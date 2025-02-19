First, this program checks its flags to see whether it's an interactive session or not.
This is achieved by checking whether arguments are passed, and if they are, parse the arguments.
It is then handed over to the non-interactive session handler, so it won't wait for user input and waste resources on QoL features.
Then, the program should accept inputs of each character. If it's a control character or a tab or whatever,
it then goes to a handler to do stuff like printing auto-suggestions or terminating programs and lines.
Now, after that was handled, the program SHOULD go back to normal. It achieves this by logging the latest
special characters pressed, or options selected in case of auto-suggestions. It implies suggestion anyways.
Meanwhile, the shell passes the unparseed parts of the input to the parser for further proessing.
Checking for what could be suggested, what colors to show, stuff like that.

Once the user presses enter, if the user is using regular expressions, that would be caught and processed.
                             if the user is using variables, that would be caught and converted to the value contained.
                             if the user is using shell-defined keywords, that would be caught and executed. (One notable feature is simple maths!)
                             if the user is using pipes/sub-shell commands, the program would be forked, piped, and dupto'd.
