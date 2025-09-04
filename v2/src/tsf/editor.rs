struct Editor {
    cursor: (u16, u16),
    anchor: (u16, u16),
    text: Vec<String>,
    mode: Vec<Mode>, //Modes are in a way, in a stack. This is how it's stored.
}

impl Editor {
    fn new(conf: Config, hist: Vec<String>) -> Self {
        terminal::enable_raw_mode().unwrap();
        todo!()
    }
    fn read(self: Self) -> String {
        todo!()
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        terminal::disable_raw_mode().unwrap();
    }
}

enum Mode {
    Normal,
    Insert,
    Select,
    Repeat,
    //...
}
