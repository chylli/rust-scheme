use std::str;
use std::fmt;
use self::Token::*;

fn main() {
    run("(+ 2 3)");
    run("(22+)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = Lexer::tokenize(s);
    println!("tokens: {:?}", tokens);
}

macro_rules! parse_error {
    ($($arg:tt)*) => (
        return Err(ParseError {message: format!($($arg)*)})
    )
}

#[derive(PartialEq, Debug)]
enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(i64),
}

struct ParseError {
    message: String,
}

// it should be Display, but Vec has no display, so the token cannot
// be displayed with it. So use Debug temporarily.
impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

struct Lexer<'a> {
    chars: std::iter::Peekable<str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Result<Vec<Token>, ParseError> {
        let mut lexer = Lexer { chars: s.chars().peekable(), current: None, tokens: Vec::new()};
        lexer.run()?;
        Ok(lexer.tokens)
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn advance(&mut self) {
        self.current = self.chars.next()
    }

    fn peek(&mut self) -> Option<char> {
        match self.chars.peek() {
            Some(c) => Some (*c),
            None => None
        }
    }

    fn run(&mut self) -> Result<(), ParseError> {
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
                                    let val = self.parse_number()?;
                                    self.tokens.push(Integer(if c == '-'{ -1 * val} else { val }));
                                    self.parse_delimiter()?;
                                },
                                _ => {
                                    // not followed by a digit, must be an identifier
                                    self.tokens.push(Identifier(c.to_string()));
                                    self.advance();
                                    self.parse_delimiter()?;
                                }
                            }
                        },
                        '0' ... '9' => {
                            // don't advance -- let parse_number advance as needed
                            let val = self.parse_number()?;
                            self.tokens.push(Integer(val));
                            self.parse_delimiter()?;
                        },
                        ' ' | '\x09' | '\x0a' | '\x0d' => self.advance(),
                        _   => parse_error!("unexpected character: {}", c),
                    }
                },
                None => break
            }
        };
        Ok(())
    }

    fn parse_number(&mut self) -> Result<i64, ParseError> {
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
        Ok(s.parse().unwrap())
    }

    fn parse_delimiter(&mut self) -> Result<(), ParseError> {
        match self.current() {
            Some(c) => {
                match c {
                    ')' => {
                        self.tokens.push(CloseParen);
                        self.advance();
                    }
                    ' ' | '\x09' | '\x0a' | '\x0d' => (),
                    _ => parse_error!("unexpected character when looking for a delimiter: {}", c),
                }
            },
            None => ()
        };
        Ok(())
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
fn test_negative_integers() {
    assert_eq!(Lexer::tokenize("(+ -8 +2 -33)").unwrap(),
               vec![OpenParen, Identifier("+".to_string()), Integer(-8), Integer(2), Integer(-33), CloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert!(Lexer::tokenize("(&&)").is_err())
}

#[test]
fn test_whitespace() {
    assert_eq!(Lexer::tokenize("(+ 1 1)\n(+\n   2\t2 \n )\r\n \n").unwrap(),
               vec![OpenParen, Identifier("+".to_string()), Integer(1), Integer(1), CloseParen, OpenParen, Identifier("+".to_string()), Integer(2), Integer(2), CloseParen]);
}

#[test]
fn test_terminor_checking() {
    assert!(Lexer::tokenize("(+-)").is_err());
    assert!(Lexer::tokenize("(-22+)").is_err());
    assert!(Lexer::tokenize("(22+)").is_err());
}
