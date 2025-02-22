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
4. Piping, in all kinds of ways. ( > ; | >> >>> < (...))
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
___

## More specific specifications. Not for the functionalities but for the syntax.

- int (maybe float) char
- type(x)
- == /= < <= >= >
- let x = ...
- (x y) [x1 x2 x3]
- [a1 a2] ++ [a3 a4] and maybe a3:[a1 a2]
- len(arr)
- arr[0] tup.0
- arr[a..b] slicing
- [a,a+d..b] range
- if ... { ... } [else { ... }] the else needs the if history to be saved.
- for x in y { ... } [else { ... }] else from Python. Executed when the function exits normally.
- while ... { ... } [else { ... }]
- fn [type] funcname(int a [char] b ...) { ... } "..." for dynamic arg count funcs.
- return, panic and break.
- (x y) = func( ... )
___

### Will implement if it's not too hard
- translating markdown (and colors) to ansi in formatted strings would be banger.
- rust style enum and match, which implies switch case.
- I don't think I'll go as far as struct and impl but eh we'll see.
- seperate, static string typing for inside job strings [char] -> Command fString String Flag Argument Path
