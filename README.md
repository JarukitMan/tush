```
  ,--.                 ,--.      
,-'  '-.,--.,--. ,---. |  ,---.  
'-.  .-'|  ||  |(  .-' |  .-.  | 
  |  |  '  ''  '.-'  `)|  | |  | 
  `--'   `----' `----' `--' `--'
```

# !!! Literally Unusable Right Now. Actually Still in Its Conception Stage !!!
# Turtle Shell (tush)

Simple shell with python-style string formatting and ability to do simple maths.
Shipped alongside the Erminal, (hopefully).
___

## Abnormalities:

1. (...) instead of $(...) - No idea why shells went with that approach,\
   it's a real bother to write in the shell.
2. f".{...}.." for formatted strings - Just like in python.\
   Strings in normal bash is very, very annoying.
3. Vim-style text navigation - Personal preferences.
___

## Features, period.

1. Calling other programs.
2. Navigation.
3. Input line highlighting.
4. Piping, in all kinds of ways. ( -> ; | => (<<< replaced by <- because here files are wack just redirect the cat'd here string) (...))
5. Simple regex. (? * [...] ...)
6. History.
7. Tab-completion.
8. exec/source
9. Non-interactive/Interactive session.
10. Command session. (-c)
11. Flag/Variable parsing.
12. Branching/Looping, basic logic.
13. Environment/Arguments.
14. Error messages.
15. Some fancy shit like rendering color ANSI codes or markdown in real time.
16. Blesh style auto-complete and config.
17. Config live-reloading.
18. Be modular and maintainable/extensible.
19. Variables and aliases.
20. Time.
21. Whatever bash has (that I like) and more (and easier!).
22. Sync & Async
23. Customizable prompt.
24. Flushing ANSI characters at output end.
25. Fuzzy Search.
26. Login. (Whatever that means)
27. Variable Type Inference. (command, flag, number, string, etc.)
28. Foreground/Background Processes.
___

## What can be configured? (tush.ini)

1. Styles.
    1. Indicator.
    2. Path style. (0 for none, -1 for full, positive number for that many directories deep.)
    3. Styles for certain file types (executable, link, etc.)
    4. Auto-complete style. (Underline, color, whatever. Just like the above.)
    5. Auto-complete size. (negative to show all, 0 to show none, the rest to show that many lines.)
2. Features.
    2. History-based auto-suggestions.
3. Special keybinds. (command = key)
4. Init script. Both on login and not. (profile and rc) (login means interactive)
5. Variables.
    1. Just variables.
    2. Cache path.
    3. Exec path.
    4. Config path.
    5. History path.
___

## Turtle Shell Scripting Language 1.0 Specifications.

Five token types: Literals, Variables, Operators, Closures, Pipers.

#### Literals

* int - The integer type. `(1 -2 3 ...)`
* flt - The floating number type. `(1.0 -2.0 2.5 ...)`
* bln - The boolean value type. `(true false)`
* chr - The character type. `('a' 'b' 'c')`
* tup - The tuple type, a collection of values of any types. One-long tuples are automatically unpacked, and a tuple with no elements or only empty tuples as elements are dissolved into empty tuples. `(([a b c] d 'e') (a "bc") ('d' 5))`
* arr - The array type, a collection of values from a single type. `([a1 a2 a3] [a4 a5] [a6 a7])`
* typ - The type type, an enumeration of all types.
* fmt - Formatted Strings, stores the variable name and updates alongside the variables. `f"1 + 1 = {1+1}"`
* str - Normal Strings, defined either with or without quotes. `This sentence has "four strings"`
* pth - The file path type, can be enumerated if the files are from the same directory. `(~/CloseToHome Relative/Folder /usr/bin/rm)`
* cmd - The command type represents commands. Defined by\
        1. Being the first token in a line and\
        2. Matching a filename from PATH or a shell-defined "command". `(ls which time)`

#### Variables

Defined by `let x = y`, variables are values that can be assigned to, and are replaced by literals contained within when used in an expression at runtime.
Their types are derived, but they are static and cannot be changed. However, a tuple is special in which a variable can be both (a b) and (a b c),
but there may be functions that only accept either (a b) or (a b c).

```
let one = 1 #int
let two = two #str
let three = ['t' 'h' 'r' 'e' 'e'] #[chr]
let four = ("fo" 'u' 'r') #tuple or (str chr chr)
```

#### Operations

Operations are split into two types, base and defined.

Base Operators:
* IO
    * read
* Arithmatic
    * \+ -- adds two numbers together
    * \- -- subtracts the second number from the first
    * \* -- multiplies two numbers together
    * / -- divides the first number by the second
* Boolean
    * && -- If either of the operands are false, it returns a false.
    * || -- If either of the operands are true, it returns a true.
    * == -- if the two operands are equal, it returns a true. If it's not a primitive value, it goes left to right.
    * /= -- if the two operands are unequal, it returns a false. If it's not a primitive value, it goes left to right.
    * < -- if the left operand is lesser than the right operand, it returns a true.
    * <= -- if the left operand is lesser than or equal to the right operand, it returns a true.
    * \> -- if the right operand is lesser than the left operand, it returns a true.
    * \>= -- if the right operand is lesser than or equal to the left operand, it returns a true.
    * not -- converts true to false and false to true.
* Type
    * int -- These operators cast the types where applicable, and exits with an error otherwise.
    * flt -- These only work on "primitive" types,
    * chr -- with the exception of [chr] that converts a string to an array of chrs.
    * ...
* Structural
    * \+ -- Concatenates the two of the same kind of structure, with the exception of tuples where all entries are appended as-is. `((a b) + (c d) -> (a b (c d)))`
    * \- -- Drops a specified number of elements from the back of the structure.
    * \* -- Duplicates the structure by a specified number of times.
    * / -- Divides the structure into a specified number of parts equally. (The 'Leftover' parts are put at the back. EX. `"Hello World"/3 -> ["Hel" "lo " "Wor" "ld"]`)
    * len -- Returns the length of the structure.
    * . -- accesses the structure. Can be done with either a number or an array. `let arr = [[1 2 3] [4 5 6] [7 8 9]] ; echo arr.2.[1 2] #8 9`
    * [a..b] -- creates a range of numbers or "enumerable" types (I don't know the proper word), incremented by its version of 1.
    * [a,a+d..b] -- creates a range, but with a specified increment value.
* Flow
    * if -- Executes the following {...} block depending on if the boolean statement returns true. `if **truth statement** {...}`
    * for -- Works as either a C-like for loop that takes in three statements, or a Python-like for loop that takes in an iterator.\
        `for let i = 0; i < n; i = i + 1 {...}` or `for i in [0..n] {...}`
    * while -- Executes the following {...} block while the boolean statement returns true. `while **truth statement** {...}`
    * else -- If the global truth value the interpreter keeps for checking the above functions is false, execute the {...} block following it. `else {...}`
    * return -- Returns the value immediately behind it from a function.
    * break -- Immediately terminates the loop being executed.
    * continue -- Immediately starts the next iteration of the loop being executed.
* Definition
    * let -- Defines a variable. The type of the variable should be known at this time. `let x: [type] = []; let y: type; let z = thing`
    * fn -- Defines a function, composed on these tokens in any order: `fn **tokens** {...}`
        1. function\_name (may only be used once.)
        2. type varname
        3. (type1 var1 type2 var2)

#### Closures

1. [...] <------ These two aren't really closures, but they can contain statements inside that are then packed into them.
2. (...) <--/        And tuples happen to unpack when they have a single value inside of them.
3. {...} -- Literally an alias for tuples.

#### Pipers

1. ; -- Ends an expression, used in place of newline in case the user can't or doesn't want to use a newline.
2. | -- Pipes output as argument for the next expression.
3. <- -- Here strings, replaces the stdin of the expression to the left with the string to the right.
4. -> -- Write to file, replaces the contents of the target file on the right side with the left side.
5. => -- Append to file, adds the left side to the end of the file named the right side of the piper.
6. & -- Sends a job to the background, not waiting for it to be over before accepting the next input.

#### Operator precedence. High to low.

1. Closure
2. IO
3. Structural (except for range)
4. Type
5. Defined operators (functions)
6. Arithmatic
7. Range
8. Boolean
9. Control Flow
10. Definition
11. Command
12. Pipers

#### Note:
If the expression leaves some values, those values are printed.
___

### Planned features.
- translating markdown (and colors) to ansi in formatted strings would be banger.
- rust style enum and match, which implies switch case.
- I don't think I'll go as far as struct and impl but eh we'll see.
- seperate, static string typing for inside job strings [char] -> Command fString String Flag Argument Path
- slices, anonymous functions, and dynamic input functions
