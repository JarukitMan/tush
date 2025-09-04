# Front End

- Renderer
  - Exposes
    ```
    render(text: Vec<String>, cursor: ((u16, u16), (u16, u16)), mode: Mode, config: GraphicsConfig) -> Option<(), IOError>
    ```
  - Should be called by the `Action` part every time it finishes its action.
  - Requires the `GraphicsConfig` struct that contains information on what part of the final picture has what color.
  - How do I make the renderer not reprint the entire thing every single keystroke?
    - What if it accepts `Change`s and renders only the lines affected?  
      It implicitly keeps track of the view with where the cursor is. If explicit view change is added later, so be it.
      EX. `Change: {Cursor: Some(newanch, newpos), Text: \[Add: ( 2, "new line"), Change: (4, "babu"), Remove: (1)...]}
    - What if reprinting the entire thing (texts) is fine? The "sane" (default) usage is only 5 lines tall. Is optimization needed?
    - Let's **NOT** optimize it for now, and we will use this technique if I find the performance too slow.
  - The cursor seems to be "hidden and replaced with a rect" when it is a "block"
- Key-Mapper
  - Maps keys from the Config to functions associated with it.
  - So it should be called in Actions that contain inner modes.
  - Exposes the `mapkey` function that maps keys to the associated functions.
    ```
    mapkey(key: KeyEvent, mode: Mode, config: Config) ->
    fn(Vec<String>, ((u16, u16), (u16, u16)), Config) -> Option<Vec<String>, IOError>
    ```
- Glue
  - This module should contain the linking parts between the other modules like the `Config` struct.
  - Thinking about it more, the `Editor` should be a struct so that I wouldn't need to pass all the stuff around and  
    everything could be a method, and the previous terminal mode is restored upon dropping.
  - With the `Editor` struct, I could also record the previous version of the buffer to only render the parts that changed.
  - All "exposed functions" are now "methods that tell you what they actually use".
  - Could expose like,
    ```
    yield(mut self) -> String
    ```
    which I then wrap around with a
    ```
    read_line(config: Config)
    ```
    Yes, I wrapped around it but I still need to do the Editor struct thing internally for the drop.
- Action
  - This module should expose various actions and modes that can be accessed with the key-mapper.
    All keys should either:
      1. Be part of an enumeration
      2. or have the same return type.
    The former is a consideration because some actions don't act upon the text,
    only moving the cursor (navigation) and/or the buffer view. Different modes don't "return",
    only calling the key-mapper again with a different map.
- Config
  - This part should parse the configuration file into a struct.
  - It should also let the user save the current config to a file.
- Error reporting format: `filename:row:column:error`

# Back End

- Tokenizer
  - Split into tokens, but this time with a moving cursor instead of directly consuming characters.
  - Tokens should contain the positional information, and that should be passed to the further states as well.
- Parser
  - The implementation shouldn't be too different. Just convert a list of tokens into a tree of tokens.  
    Need to make sure to mark down the row:column for the error.
- Interpreter
  - This exposes the interpreter and the type-checker. This only changes from the initial implementation in that
    1. Error messages need to be better.
    2. Closures are no longer tuples, and tuples can no longer return.
  - The `Keyword`s are different from `Operator`s in that they directly work on the Tokens instead of values,
    while the `Operator`s work on `Value`s, translated by the `Glue` module.
- Operator
- Glue
