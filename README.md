# Schemack
A schema description language capable of defining strong data integrity checks.

✅ **DRY in Type Definitions: Across DB, backend, frontend, between languages, and microservices.**
**Let's eliminate duplication of all type and validation definitions, consolidate them in one place, and ensure consistency throughout.**

It will be compiled into SQL and other formats. Plans also include JSON Schema, GraphQL, protobuf, and the WASM Component Model.

## Status
- [ ] Parser
- [ ] Semantic Analyzer
    - [ ] Name Resolver
    - [ ] CEL interop
- [ ] Compiler to SQL
- [ ] CLI frontend

## Syntax

### `example/user.schk`
```
use std.types.{NonEmptyString, rfc.EmailAddress}

table User {
    name NonEmptyString primary,
    email EmailAddress!,    # `!` means `unique`
    | name.all(c, is_valid_username_character(c))
}

fn is_valid_username_character(c Char) -> Bool {
    /* boolean expression written in Google Common Expression Language */
}
```

### `example/company.schk`
```
use example.user
use std.types.{NonEmptyString, NonNegativeInt, Interval}

/*  `<:` means subtyping (inheritance).
 *  A `User` cannot be both an `Employee` and a `Guest` at the same time.
 */

table Employee <: User {
    real_name NonEmptyString,
    department &Department,    /*  `&` means an foreign key reference.
                                *  If `&` is not added, it appears as if the target `table` is 
                                *  defined as `data` instead.
                                */
    birth_date Date,
    hire_date Date,
    | birth_date <= hire_date
}

table Guest <: User {
    visiting_period Interval<Date>,
}

table Department {
    no NonNegativeInt primary,
    name NonEmptyString!,
}

table DepartmentManager {
    department &Department,
    manager &EmployeePerson,
}

table Salary {
    employee &Employee,
    salary NonNegativeInt,
    interval Interval<Date>,
}
```

### `std/types.schk` (within the standard library)
```
...

data NonNegativeInt { n Int | n >= 0 }

data NonEmptyString { string String | size(string) != 0 }

# Support for generics
data Interval<T: Ord> {
    start T,
    end T,
    | start <= end
}

# Support for direct sum
data Option<T> = Some(T) or None

...
```
