use std::fmt;
use std::str;
use self::Token::*;

fn main() {
    run("(+ 21 325)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = Lexer::tokenize(s);
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

struct Lexer<'a> {
    chars: std::iter::Peekable<str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Vec<Token> {
        let mut lexer = Lexer { chars: s.chars().peekable(), current: None, tokens: Vec::new()};
        lexer.run();
        lexer.tokens
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn peek(&mut self) -> Option<char> {
        match self.chars.peek() {
            Some(c) => Some (*c),
            None => None,
        }
    }

    fn advance(&mut self) {
        self.current = self.chars.next()
    }

    fn run(&mut self) {
        self.advance();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '(' => {
                            self.tokens.push(OpenParen);
                            self.advance();
                        },
                        ')' => {
                            self.tokens.push(CloseParen);
                            self.advance();
                        },
                        '+' | '-' => {
                            match self.peek() {
                                Some('0'...'9') => {
                                    self.advance();
                                    let val = self.parse_number();
                                    self.tokens.push(Integer(if c == '-'{ -1 * val} else { val }))
                                },
                                _ => {
                                    self.tokens.push(Identifier(c.to_string()));
                                    self.advance();
                                }
                            }
                        },
                        '0' ... '9' => {
                            let val = self.parse_number();
                            self.tokens.push(Integer(val))
                        },
                        ' ' => self.advance(),
                        _   => panic!("unexpected character: {}", c),
                    }
                },
                None => return (),
            }
        };
    }

    fn parse_number(&mut self) -> i32 {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '0' ... '9' => {
                            s.push(c);
                            self.advance();
                        },
                        _ => break
                    }
                },
                None => break
            }
        }
        s.parse().unwrap()
    }
}

