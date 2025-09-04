mod operator;
mod parse;
mod tokenize;

fn interpret() {}

struct Token {
    row: i32,
    column: i32,
    token: Data,
}

enum Data {
    Word(String),
    Keyword(Keyword),
    Operator(Vec<Operator>, u8), //Due to a lack of a better way to search for operators while generic/any exist.
    Integer(i64),
    Double(f64),
    Charactor(char),
    Boolean(bool),
    String(String),
}

enum Keyword {}

// Temporary form.
struct Operator {
    lhs: Vec<(String, Type)>,
    rhs: Vec<(String, Type)>,
    body: Vec<Token>,
}

enum Type {
    Integer,
    Double,
    Character,
    Boolean,
    String,
    Any,
    Generic(u8),
}
