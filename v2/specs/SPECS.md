# Turtle Shell Scripting Language Specification 2

Second time's the Charm.

## What changed? (Brief)

* Tuples and Closures are now seperated. The Closures are {...} while the tuples remain (...).  
  The , syntax is still in-use, but it is no longer an operator. Just a seperator now.  
  This change should fix: type-checking, $, unclear evaluation time,
  unclear type-checking, shortcuts (early return and break).

* Along with the addition of closures, keywords (such as `return`, `if`, or `break`)
  are now seperated from operators.

* Explicit return type syntax is allowed but not necessary:
  `opr ... = type {...}` (implicit is `opr ... = Expression`)

* Generic syntax (array of uppercase type names to be used):
  `[T1 T2 T3] opr T1 a op T2 b = T3 {...}`  
  I may add typeclasses later if I reaaaally want to continue this project.

* All environmental variables are converted to string variables on launch.
  This gets rid of set/unset. (In real implementation it will probably be the opposite,
  using env to store strings instead of storing strings from env.)

## Shape

* Expression = `Operand` | `Operand` `Operator` `Expression`
* Operand = `Closure` | `Variable` | `Literal`
* Assignment = `Variable` `=` `Expression`
* Definition = `let` `Variable` (`:` `Type` and/or `=` `Expression`)
* Closure = `{` one or more `Expression`s intercalated with `,`s `}`.
  It returns either the final expression or the one following the `return` keyword as its output.
  Early returns from loops/ifs/elses are possible, but not from naked closures or naked values.
* Literal = `Integer` | `Double` | `Character` | `String` | `Boolean` | `Path` | `Type` | `List` | `Tuple` | `Process`
* Variables and Operator names aren't limited to certain shapes. That's up to you.
* A list contains any amount of elements with the same type. Surrounded by `[` and `]`.
  and intercalated with either ` ` or `,` if it is present.
* A tuple contains a fixed amount of elements of any type. Surrounded by `(` and `)`.
  and intercalated with either ` ` or `,` if it is present.
  Implicitly delimited if next to an `Operator` or at the start/end of a line.
* Glob patterns/Regular expressions can be used to match options or paths, turning them into arrays of that type.

## Rules

Let's write the rest down as it goes on. I think the 1.0 specs are fine for the rest.

### Keywords

* The `Closure` that follows the keyword `if` is able to return a value if
  1. It is directly followed by an `else` keyword (along with its `Closure`).
  2. The `Closure` following the `else` keyword returns a value of the same type.

### Values

* Formatted strings `f"...{...}..."` are evaluated into `String`s before use.
* Paths are stored as absolute paths. They are detected by containing `/`s or being `.`/`..`/`~`.
* Here is the type conversion table. From the vertical to the horizontal types.
  Conversion to/from composite types (`List` and `Tuples`) isn't implemented.
  | f/t | int | dub | chr | str | bln | pth | typ | prc |
  |-----|-----|-----|-----|-----|-----|-----|-----|-----|
  | int | yes | yes | no  | yes | no  | no  | yes | yes |
  | dub | yes | yes | no  | yes | no  | no  | yes | yes |
  | chr | yes | yes | yes | yes | no  | yes | yes | yes |
  | str | no  | no  | no  | yes | no  | yes | yes | yes |
  | bln | yes | yes | no  | yes | yes | no  | yes | yes |
  | pth | no  | no  | no  | yes | no  | yes | yes | yes |
  | typ | no  | no  | no  | yes | no  | no  | yes | yes |
  | prc | yes | yes | no  | yes | no  | no  | no  | yes |

  Booleans converted to string will be "true" or "false", and if converted to numbers, 1 will be true and 0 will be false.
  When a process is converted to the allowed types, it will be run and the exit code will be captured for the numbers, and
  the standard output is captured for the `String`.

## System

* The output of system-evaluated commands are stored as a `List` of `String`s seperated by newlines and whitespaces.
* `Process` values need to all be assigned to a variable. Ones that are stopped with `SIGSTOP` (C-z by default)
  are appended to the global `process` variable of type `List` of `Process`es.
  `Process`es that aren't assigned to values are run immediately. The system decides this by checking
  if the `Process` is a returned value or not. If it isn't, it is run. If it is at the top-level, it is also run.
  Example:
  ```
  > ls
  dir txt1 txt2
  > files = { ls }
  > cd dir
  > files # It was spawned in the previous directory, so the process is still in that directory.
  dir txt1 txt2
  > files
  [interactive tush]:0:0: Process `ls` ended.
  ```
* Final values of expressions values that contain a `String` or a `Path` at the front are converted to `Process`es immediately.
  Returned values that do not have their types explicitly declared go under the same treatment.

### Operators

* All operators are infix. `fun(a, b)` is in actuality `() fun (a b)`.
* All operators have their own cardinality which cannot be overridden.
* All operators can be overloaded.
* Default operators
  | name | left-hand side | right-hand side | output | description |
  |------|----------------|-----------------|--------|-------------|
  | +    | int            | int             | int    | Add the two numbers together |
  | +    | int            | dub             | dub    |             |
  | +    | dub            | int             | dub    |             |
  | +    | dub            | dub             | dub    |             |
  | +    | chr            | chr             | chr    | Add the two characters together |
  | +    | str            | str             | str    | Append the right string to the left string |
  | +    | int            | bln             | int    | Increments the left by 1 if the right is true |
  | +    | dub            | bln             | dub    |             |
  | +    | bln            | int             | int    | Increments the right by 1 if the left is true |
  | +    | bln            | dub             | dub    |             |
  | +    | bln            | bln             | bln    | Alias for `\|\|` (OR) |
  | +    | pth            | int             | pth    | The integer to the right is modulo'd with the size of the path's parent directory, then the path is changed to the path N to the right of it based on the implementation language's readDirectory. |
  | +    | [T]            | [T]             | [T]    | Append the right list to the left list |
  | -    | int            | int             | int    | Subtract the right number from the left |
  | -    | int            | dub             | dub    |             |
  | -    | dub            | int             | dub    |             |
  | -    | dub            | dub             | dub    |             |

  ...

### Variables

* Variables are statically typed.
* Variables can be shadowed. If done in the same scope, the variable name is overwritten.
