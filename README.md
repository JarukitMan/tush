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

## Dependencies
- [brick] (https://github.com/jtdaugherty/brick/tree/master?tab=readme-ov-file) Might change to vty. We'll see.

## Abnormalities:

1. (...) instead of $(...) - No idea why shells went with that approach,\
    it's a real bother to write in the shell.
2. f".{...}.." for formatted strings - Just like in python.\
    Strings in normal bash is very, very annoying.
3. Vim-style text navigation - Personal preferences.
4. Thick shell. Similar to vim's "q:" screen. The shell clears the stdin part of the screen and\
    lets processes "take over" while keeping track of the stdin and stdout to put in the buffer later.\
    It then rewrites the stdin area buffer.
___

## Features, period.

1. Calling other programs.
2. Navigation.
3. Input line highlighting.
4. Piping, in all kinds of ways. ( -> , | => (<<< replaced by <- because here files are wack just redirect the cat'd here string) (...))
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
26. Login.
27. Variable Type Inference. (command, flag, number, string, etc.)
28. Foreground/Background Processes.
___

## What can be configured? (tush.ini or md? We'll see.)

1. Styles.
    1. Indicator. (Default = "<[path]> ")
    2. Path style. (Default = 1. 0 for none, -1 for full, positive number for that many directories deep.)
    3. Styles for certain file types (executable, link, etc.)
    4. Auto-complete style. (Underline, color, whatever. Just like the above.)
    5. Auto-complete size. (Default = -1. negative to show all, 0 to show none, the rest to show that many lines.)
    6. Input height. (Default = 5. Anything less than 1 is ignored.)
    7. Scroll zone offset. (Default = 1. Negative means center at all times.)
2. Features.
    1. History-based auto-suggestions.
    2. Split input & Output. (Default = true)
    3. Sorting order.
3. Special keybinds. (command = key)
4. Init script. Both on login and not. (profile and rc) (login means the first shell)
5. Variables.
    1. Just variables.
    2. Cache path.
    3. Exec path.
    4. Config path.
    5. History path.
___

## Turtle Shell Scripting Language 1.0 Specifications.

Four token types: Literals, Variables, Operators, Closures.

#### Literals

* int - The integer type. `1 -2 3 ...`
* flt - The floating number type. `1.0 -2.0 2.5 ...`
* bln - The boolean value type. `true false`
* chr - The character type. `'a' 'b' 'c' ...`
* tup - The tuple type, a collection of values of any types.\
    One-long tuples are automatically unpacked.\
    `(([a b c] d 'e') (a "bc") ('d' 5))`
    `(1, 2 + 3) == ((1) (2 + 3)) == (1, 5) == (1 5)`
* arr - The array type, a collection of values from a single type. (They're actually lists in this implementation.) `[a1 a2 a3] [a4 a5] [b1 b2] ...`
* typ - The type type, an enumeration of all types. `int flt bln ...`
* str - Normal Strings, defined either with or without quotes. `This sentence has "four strings"`
* pth - The file path type, can be enumerated if the files are from the same directory. `~/CloseToHome Relative/Folder /root`
* Note: there are formatted strings defined by `f"..."` that are evaluated to string immediately.

#### Variables

Defined by `let x = y`, variables are values that can be assigned to,\
and are replaced by literals contained within when used in an expression at runtime\
Their types are derived, but they are static and cannot be changed.\
Variables only exist within their tuple scope or any expression nested inside.

```
let one = 1 #int
let two = two #str
let three = ['t' 'h' 'r' 'e' 'e'] #[chr]
let four = ("fo" 'u' 'r') #(str chr chr)
let fourthree = four + three #(str chr chr [chr])
four = fourthree #ERROR: Type Mismatch.
```

However, it is possible to "shadow" variables, reusing the same name for two different variables.

```
let x = int(input)
if x < 100 {
    let x = "Something else"
    echo x
}
x = x + 10
```
This snippet of code is legal, because the "let x = "Something else"" line declares a new variable with the name x within the scope\
and that x is destroyed at the end of the scope, as it is a local variable.

#### Operations

Operations are split into two types, base and defined.

Base Operators:
* IO
    * input -- reads a line of input from the stdin into a str.
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
    * \- -- Drops a specified number of elements from the back of the structure. `(a b c) - 2 -> (a) -> a`
    * \* -- Duplicates the structure by a specified number of times. `[a1 a2 a3] * 3 -> [a1 a2 a3 a1 a2 a3 a1 a2 a3]`
    * / -- Divides the structure into a specified number of parts equally. Rounded to the nearest number. `"Hello World"/4 -> ["Hel" "lo " "Wor" "ld"]`
    * len -- Returns the length of the structure. `len [1..10] -> 10`
    * . -- accesses the structure. Can be done with either a number or an array. `let arr = [[1 2 3] [4 5 6] [7 8 9]], echo arr.2.[1 2] #8 9`
    * [a..b] -- creates a range of numbers or "enumerable" types, incremented by its version of 1.
* Flow
    * if -- Executes the following tuple depending on if the boolean statement returns true. `if **truth statement** {...}`
    * for -- Works as either a C-like for loop that takes in three statements, or a Python-like for loop that takes in an iterator.\
        `for let i = 0, i < n, i = i + 1 {...}` or `for i in [0..n] {...}`
    * while -- Executes the following tuple while the boolean statement returns true. `while **truth statement** {...}`
    * else -- If the global truth value the interpreter keeps for checking the above functions is false, execute the tuple following it. `else {...}`
    * return -- Returns the value immediately behind it from a function or a tuple.\
                All possible branches must contain a return that returns the same type when used in a tuple or a function.\
                With the current implementation of `return`, since the interpreter must know the values of each function or tuple,\
                if an expression evaluates to the return type of the function or tuple, it will immediatey return.\
                To avoid this, use `let _ =` to convert the final result of the expression to `()`.
    * break -- Immediately terminates the loop being executed.
    * continue -- Immediately starts the next iteration of the loop being executed.
* Definition
    * let -- Defines a variable. If the type is obvious, the interpreter could infer it. Otherwise, denote the type. `let [Type] x = [], let Type y, let z = thing`
    * opr -- Defines a function, composed on these tokens in any order: `opr [precendence] **tokens** = (...)`
        1. function\_name (may only be used once.)
        2. type varname
        3. (type1 var1 type2 var2)
      ```
      opr 1 int a add int b = { a + b }
      opr mpy (int a int b) = {
          let acc = 0
          for i in [0..a] {
              acc = acc + b
          }
          return acc
      }
      ```
      The [precendence] number (rank) is an unsigned 8-bit integer. Operations with a higher rank will be done before operations with a rank.
      A function has access to any in-scope variables declared before it.\
      Operators can also be shadowed, though an earlier definition will be matched if the input/output types don't match.\
      It follows roughly the same rules as shadowing.\
      ```
      opr add int a int b = { return str(a + b) }
      opr add int a int b = { return a + b }
      opr add int a int b int c = { return a + b + c }
      add 1 2
      add 1 2 3
      echo "Hello, " + (add 1 2)
      ```
      Note that there is a special variadic type `Any` that consumes all input into a tuple.
      ```
      opr addall (...) nums = ( let acc = 0, for num in nums (if typ num == int || typ num == flt (acc = acc + num)), return acc)
      addall 1 2 3 4 5 #15
      ```
      It is quite error-prone. So use it carefully.
* Pipers
    * , -- Ends an expression, used in place of newline in case the user can't or doesn't want to use a newline. `echo "Hello", echo "World"`
    * | -- Pipes output as input for the next expression. `echo Hello World | cat`
    * <- -- Here strings, replaces the stdin of the expression to the left with the string to the right. `cat <- "Hello World"`
    * -> -- Write to file, replaces the contents of the target file on the right side with the left side. `echo Hello World -> hello.txt`
    * => -- Append to file, adds the left side to the end of the file named the right side of the piper. `echo "\nAnother Line" => hello.txt, cat hello.txt`
    * & -- Sends a job to the background, not waiting for it to be over before accepting the next input. `echo theBeeMovieScript.txt & vim`

    Pipers are basically whatever ; > >> <<< were in bash. I don't know their names.
    They should behave the same in which expressions that contain errors are skipped while the rest of the program is executed normally.

#### Closures

1. [...] -- arrays
2. (...) -- tuples
3. {...} -- an alias for tuples.

Tuples are evaluated when a value is needed, so not when defining a function or in a control-flow statement.
However, when a tuple is evaluated. You'd end up with a bunch of intermediate garbage. Like `((), (), 3)` when you write `{let x = 1, let y = 2, x + y}`.
That is why the `return` operator is used to replace the output value of the tuple with the value after it.
`{let x = 1, let y = 2, return x + y}` will now become just `(3)`, which is equivalent to `3`
With locality rules, x and y are deallocated after the tuple is done evaluating.

When they are used in control-flow statements or function definitions,
it would just be saved as `{let x = 1, let y = 2, return x + y}` and not evaluated until needed.

#### Operator precedence. High to low. (TODO)

Bottom level - Closure
255 - IO
254 - cmd $
17 - User-defined operators
16 - .
15 - type-casts
14 - len
13 - multiplication and division
12 - addition and subtraction
11 - ..
10 - < > <= >= == /=
9  - not
8  - && ||
7  - continue break return
6  - control-flow
5  - cd time help
4  - definition and declaration
3  - <-
2  - -> =>
1  - |
0  - , &


#### Note:

If the expression leaves some values, those values are printed unless they're strings or executable filepaths, which would be called as an external program.
Internally, all values are packed into tuples with operations as seperators. So operations that take no input on either (or both) side(s) also accept zero tuples.
```
a + b is (a) + (b)
len x is () len (x)
```
With the lack of error handling (until structs and enums are added), error messages are simply printed and the expression is aborted.\
Changes made are not reverted.\
Currently, all values are passed as copies in operations. This may lead to more memory usage,\
but I already said it would be slow and heavy, so I'm not bothering to change it.\
closures should capture the stdout (and stderr) of the commands executed into strings.
___
