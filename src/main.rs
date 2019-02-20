use std::str;
use self::Token::*;

fn main() {
    run("(+ 2 3)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = Lexer::tokenize(s);
    println!("tokens: {:?}", tokens);
}

#[derive(PartialEq, Debug)]
enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(i32),
}

struct Lexer<'a> {
    chars: std::iter::Peekable<str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Result<Vec<Token>, &'static str> {
        let mut lexer = Lexer { chars: s.chars().peekable(), current: None, tokens: Vec::new()};
        lexer.run()?;
        Ok(lexer.tokens)
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn peek(&mut self) -> Option<char> {
        match self.chars.peek() {
            Some(c) => Some (*c),
            None => None
        }
    }

    fn advance(&mut self) {
        self.current = self.chars.next()
    }

    fn run(&mut self) -> Result<(), &'static str> {
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
                                    // skip past the +/- symbol and parse the number
                                    self.advance();
                                    let val = self.parse_number();
                                    self.tokens.push(Integer(if c == '-'{ -1 * val} else { val }));
                                },
                                _ => {
                                    // not followed by a digit, must be an identifier
                                    self.tokens.push(Identifier(c.to_string()));
                                    self.advance();
                                }
                            }
                        },
                        '0' ... '9' => {
                            // don't advance -- let parse_number advance as needed
                            let val = self.parse_number();
                            self.tokens.push(Integer(val));
                        },
                        ' ' => self.advance(),
                        _   => return Err("unexpected character"),
                    }
                },
                None => break
            }
        };
        Ok(())
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

#[test]
fn test_simple_lexing() {
    assert_eq!(Lexer::tokenize("(+ 2 3)").unwrap(),
               vec![OpenParen, Identifier("+".to_string()), Integer(2), Integer(3), CloseParen]);
}

#[test]
fn test_multi_digit_integers() {
    assert_eq!(Lexer::tokenize("(+ 21 325)").unwrap(),
               vec![OpenParen, Identifier("+".to_string()), Integer(21), Integer(325), CloseParen]);
}

#[test]
fn test_subtraction(){
    assert_eq!(Lexer::tokenize("(+ -8 +2 -33)").unwrap(),
               vec![OpenParen, Identifier("+".to_string()), Integer(-8), Integer(2), Integer(-33), CloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert!(Lexer::tokenize("(&&)").is_err())
}
