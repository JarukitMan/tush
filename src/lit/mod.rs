use std::{fmt, path};

pub enum Lit
{
    Int(i32),
    Flt(f32),
    Chr(char),
    Bln(bool),
    Str(String),
    Pth(Box<path::Path>),
}

impl fmt::Display for Lit
{
    fn fmt(&self, f: &mut fmt::Formatter <'_>) -> Result<(), fmt::Error> {
        use crate::lit::Lit::*;
        match &self {
            Int(x) => write!(f, "{}", x),
            Flt(x) => write!(f, "{}", x),
            Chr(x) => write!(f, "{}", x),
            Bln(x) => write!(f, "{}", x),
            Str(x) => write!(f, "{}", x),
            Pth(x) => write!(f, "{}", x.display()),
        }
    }
}
