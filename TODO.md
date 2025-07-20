# FIX:
- Find a way to deal with "," and tuple appending/dropping type signatures.
       ^ Might have to dabble into Template Haskell. (decision inspired by [https://hackage.haskell.org/package/tuple-append])
- Fix all the inconsistencies.
- Unimplement Ord, move it to the operators.

# TODO:
- Implement Imp.
- Implement Tsf.
- Add globbing/regex to input. Will probably be built into Tsf...
- Add debug messages to the base operations.
- Add help, exec, and time operators.
- Write a proper README and split the current one to Tssl and Tsf.
- Write examples.
- Store handles for the processes that are in the background.

# FUTURE:
## Front part:
- Tree-sitter or language server protocol integration.
- Translate markdown (and colors) to ansi in strings.
- Include a wrapper for other interactive shells.
- Scratch paper.
- Seperate buffer for outputs, allowing for navigating and yanking the outputs.
## Lang part:
- Make "," follow spec.
  ^ So apparently, the way GHC does it is that they just brute-forced it up to a 64-tuple.
                   I will do the same.
- Rust style enum and match, which implies switch case.
- Basically, pattern matching. This applies to variable assignment too.
- Struct and typeclass/trait.
- More string types. E.G. lit string, regex string, whatever.
- Slices, functions as values, specified return type, libraries.
- Compiled version.
- Seperate IO operations and non-IO operations. This will allow for better handling of both.
- Make a "Rabbit" variant. A lazy, probably faster version.
- Proper process handling and pipelining. Might pass process handles around.
- Capturing stdout while letting it print out naturally in `cap`.
- The more I develop the more I want a type that's a tree all the way down for expressions.

# NOTES:
Currently, pipe is just eval left and pass it to system and feed out to right process.
Redirect is just eval right, NOT passing it to system, then feed to left process.
