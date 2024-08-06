-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Schemack.Lexer where

import Control.Applicative (many)
import Control.Lens (makePrisms)
import Data.Bifunctor (first)
import Data.Bimap (Bimap)
import Data.Bimap qualified as M
import Data.Char (isAlphaNum)
import Data.Functor (($>), (<&>))
import Data.List qualified as DL
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), getDown)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (takeWhileP, try),
    Parsec,
    PosState (
        PosState,
        pstateInput,
        pstateLinePrefix,
        pstateOffset,
        pstateSourcePos,
        pstateTabWidth
    ),
    SourcePos (sourceLine),
    Stream (
        Token,
        Tokens,
        chunkEmpty,
        chunkLength,
        chunkToTokens,
        take1_,
        takeN_,
        takeWhile_,
        tokenToChunk,
        tokensToChunk
    ),
    TraversableStream (reachOffset),
    VisualStream (showTokens, tokensLength),
    choice,
    (<?>),
 )
import Text.Megaparsec.Char (char, letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Tok
    = NameToken Name
    | Primary
    | Table
    | Trait
    | Data
    | Impl
    | Use
    | TraitAnd
    | Fn
    | Or
    | TraitIs
    | Inherit
    | OpenBrace
    | CloseBrace
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | Delimiter
    | Equal
    | Unique
    | Ref
    | Constraint
    | Placeholder
    deriving stock (Eq, Ord, Show)

data Name = Name {nameQualifier :: Vector Text, qualifiedName :: Text}
    deriving stock (Eq, Ord, Show)

makePrisms ''Tok

tokenTable :: Bimap (Down Text) Tok
tokenTable =
    M.fromList $
        [ ("primary", Primary)
        , ("table", Table)
        , ("trait", Trait)
        , ("data", Data)
        , ("impl", Impl)
        , ("use", Use)
        , ("and", TraitAnd)
        , ("fn", Fn)
        , ("or", Or)
        , ("is", TraitIs)
        , ("<:", Inherit)
        , ("{", OpenBrace)
        , ("}", CloseBrace)
        , ("(", OpenParen)
        , (")", CloseParen)
        , ("<", OpenBracket)
        , (">", CloseBracket)
        , (",", Delimiter)
        , ("=", Equal)
        , ("!", Unique)
        , ("&", Ref)
        , ("|", Constraint)
        , ("_", Placeholder)
        ]
            <&> first Down

formatToken :: Tok -> String
formatToken = T.unpack . getDown . fromJust . (`M.lookupR` tokenTable)

pToken :: Parser Tok
pToken =
    choice $
        (NameToken <$> pName) :
        (M.toList tokenTable <&> \(Down sym, tok) -> symbol sym $> tok)

pName :: Parser Name
pName =
    lexeme
        ( Name
            <$> (many (try $ pIdentifier <* char '.') <&> V.fromList)
            <*> pIdentifier
            <?> "qualified name"
        )

pIdentifier :: Parser Text
pIdentifier =
    T.cons
        <$> letterChar
        <*> takeWhileP (Just "alpha num underscore character") (\c -> isAlphaNum c || c == '_')
        <?> "identifier"

symbol :: Text -> Parser Text
symbol = L.symbol sc
{-# INLINE symbol #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
{-# INLINE lexeme #-}

sc :: Parser ()
sc =
    L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockCommentNested "/*" "*/")

data TokenStream = TokenStream
    { streamInput :: String -- for showing offending lines
    , unTokenStream :: [WithPos Tok]
    }

data WithPos a = WithPos
    { startPos :: SourcePos
    , endPos :: SourcePos
    , tokenLength :: Int
    , tokenVal :: a
    }
    deriving (Eq, Ord, Show)

instance Stream TokenStream where
    type Token TokenStream = WithPos Tok
    type Tokens TokenStream = [WithPos Tok]

    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (TokenStream _ []) = Nothing
    take1_ (TokenStream str (t : ts)) =
        Just (t, TokenStream (drop (tokensLength @TokenStream Proxy (t :| [])) str) ts)
    takeN_ n (TokenStream str s)
        | n <= 0 = Just ([], TokenStream str s)
        | null s = Nothing
        | otherwise =
            let (x, s') = splitAt n s
             in case NE.nonEmpty x of
                    Nothing -> Just (x, TokenStream str s')
                    Just nex ->
                        Just (x, TokenStream (drop (tokensLength @TokenStream Proxy nex) str) s')
    takeWhile_ f (TokenStream str s) =
        let (x, s') = DL.span f s
         in case NE.nonEmpty x of
                Nothing -> (x, TokenStream str s')
                Just nex ->
                    (x, TokenStream (drop (tokensLength @TokenStream Proxy nex) str) s')

instance VisualStream TokenStream where
    showTokens Proxy =
        unwords
            . NE.toList
            . fmap (formatToken . tokenVal)
    tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
    reachOffset o PosState{..} =
        ( Just (prefix ++ restOfLine)
        , PosState
            { pstateInput = TokenStream postStr post
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix =
            if sameLine
                then pstateLinePrefix ++ preLine
                else preLine
        sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos =
            case post of
                [] -> case unTokenStream pstateInput of
                    [] -> pstateSourcePos
                    xs -> endPos (last xs)
                (x : _) -> startPos x
        (pre, post) = splitAt (o - pstateOffset) (unTokenStream pstateInput)
        (preStr, postStr) = splitAt tokensConsumed (undefined pstateInput)
        preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
        tokensConsumed =
            case NE.nonEmpty pre of
                Nothing -> 0
                Just nePre -> tokensLength @TokenStream Proxy nePre
        restOfLine = takeWhile (/= '\n') postStr
