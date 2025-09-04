pub struct GraphicsConfig {
    wrd: Color,
    str: Color,
    int: Color,
    dub: Color,
    chr: Color,
    bln: Color,
    cursor: SetCursorStyle,
    //...
}
pub struct Config {
    style: GraphicsConfig,
    keymap: HashMap<KeyEvent, fn()>,
}
