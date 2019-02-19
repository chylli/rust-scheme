use std::fmt;
use self::Token::*;

fn main() {
    run("(+ 21 325)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = tokenize(s);
    println!("tokens: {:?}", tokens);
}

enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(i32),
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OpenParen => write!(f, "OpenParen"),
            CloseParen => write!(f, "CloseParen"),
            Identifier(ref v) => write!(f, "Identifier({})", v),
            Integer(ref v) => write!(f, "Integer({})", v),
        }
    }
}

fn tokenize(s: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iter = s.chars();
    loop {
        match iter.next() {
            Some(c) =>
                match c {
                    '(' => tokens.push(OpenParen),
                    ')' => tokens.push(CloseParen),
                    '+' => tokens.push(Identifier(c.to_string())),
                    '0'... '9' => {
                        let mut chars = c.to_string();
                        iter.by_ref().take_while(|c| c.is_digit(10)).for_each(|c| chars.push(c));
                        let val: i32 = chars.parse().unwrap();

                        tokens.push(Integer(val))
                    },
                    ' ' => (),
                    _   => println!("unexpected character: {}", c),
                },
            None =>
                return tokens
        }
    };
}
