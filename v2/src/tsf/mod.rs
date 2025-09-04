mod render;
mod editor;
mod config;
mod keymap;

pub fn read_line(conf: config::Config, hist: Vec<String>) -> String {
    editor::Editor::new(conf, hist).read()
}
