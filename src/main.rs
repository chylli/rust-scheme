use std::str;
use std::fmt;

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
    ($lexer:ident, $($arg:tt)*) => (
        return Err(ParseError {message: format!($($arg)*), line: $lexer.line, column: $lexer.column})
    )
}

#[derive(PartialEq, Debug)]
enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    String(String),
}

#[derive(Debug)]
struct ParseError {
    message: String,
    line: u64,
    column: u64,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {} (line: {}, column: {})", self.message, self.line, self.column)
    }
}

struct Lexer<'a> {
    chars: std::iter::Peekable<str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
    line: u64,
    column: u64,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Result<Vec<Token>, ParseError> {
        let mut lexer = Lexer { chars: s.chars().peekable(), current: None, tokens: Vec::new(), line: 1, column: 0};
        lexer.run()?;
        Ok(lexer.tokens)
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn advance(&mut self) {
        if self.current() == Some('\x0a'){
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
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
                            self.tokens.push(Token::OpenParen);
                            self.advance();
                        },
                        ')' => {
                            self.tokens.push(Token::CloseParen);
                            self.advance();
                        },
                        '+' | '-' => {
                            match self.peek() {
                                Some('0'...'9') => {
                                    // skip past the +/- symbol and parse the number
                                    self.advance();
                                    let val = self.parse_number()?;
                                    self.tokens.push(Token::Integer(if c == '-'{ -1 * val} else { val }));
                                    self.parse_delimiter()?;
                                },
                                _ => {
                                    // not followed by a digit, must be an identifier
                                    self.tokens.push(Token::Identifier(c.to_string()));
                                    self.advance();
                                    self.parse_delimiter()?;
                                }
                            }
                        },
                        '#' => {
                            let val = self.parse_boolean()?;
                            self.tokens.push(Token::Boolean(val));
                            self.parse_delimiter()?;
                        }
                        'A' ... 'Z' | 'a' ... 'z' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<'  | '=' | '>' | '?' | '_' | '^' => {
                            let val = self.parse_identifier()?;
                            self.tokens.push(Token::Identifier(val));
                            self.parse_delimiter()?;
                        },
                        '0' ... '9' => {
                            // don't advance -- let parse_number advance as needed
                            let val = self.parse_number()?;
                            self.tokens.push(Token::Integer(val));
                            self.parse_delimiter()?;
                        },
                        '\"' => {
                            let val = self.parse_string()?;
                            self.tokens.push(Token::String(val));
                            self.parse_delimiter()?;
                        }
                        ' ' | '\x09' | '\x0a' | '\x0d' => self.advance(),
                        _   => parse_error!(self, "Unexpected character: {}", c),
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

    fn parse_boolean(&mut self) -> Result<bool, ParseError> {
        //if self.current() != Some('#') { parse_error!(self, "Unexecpted character: {}", self.current())};
        self.advance();
        match self.current() {
            Some('t') => {
                self.advance();
                Ok(true)
            },
            Some('f') => {
                self.advance();
                Ok(false)
            },
            Some(c) => parse_error!(self, "Unexpected character when looking for t/f: {}", c)
            ,
            _ => parse_error!(self, "Unexpected EOF when looking for t/f")

        }
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        let mut s = String::new();
        loop {
            match self.current(){
                Some(c) => {
                    match c {
                        'A'...'Z' | 'a'...'z' | '0'...'9' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '_' | '^' | '+' | '-' | '#' => {
                            s.push(c);
                            self.advance();
                        },
                        _ => break

                    }
                }
                None => break
            }
        }
        Ok(s)
    }

    fn parse_string(&mut self) -> Result<String, ParseError> {
        //if self.current() != Some('\"') {parse_error!(self, "Unexpected character: {}", self.current())}
        self.advance();

        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '\"' => {
                            self.advance();
                            break;
                        },
                        _ => {
                            s.push(c);
                            self.advance();
                        }
                    }
                },
                None => parse_error!(self, "Expected end quote, but found EOF instead")
            }
        }
        Ok(s)
    }

    fn parse_delimiter(&mut self) -> Result<(), ParseError> {
        match self.current() {
            Some(c) => {
                match c {
                    ')' => {
                        self.tokens.push(Token::CloseParen);
                        self.advance();
                    }
                    ' ' | '\x09' | '\x0a' | '\x0d' => (),
                    _ => parse_error!(self, "Unexpected character when looking for a delimiter: {}", c),
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
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(2), Token::Integer(3), Token::CloseParen]);
}

#[test]
fn test_multi_digit_integers() {
    assert_eq!(Lexer::tokenize("(+ 21 325)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(21), Token::Integer(325), Token::CloseParen]);
}

#[test]
fn test_subtraction(){
    assert_eq!(Lexer::tokenize("(+ -8 +2 -33)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(-8), Token::Integer(2), Token::Integer(-33), Token::CloseParen]);
}

#[test]
fn test_negative_integers() {
    assert_eq!(Lexer::tokenize("(+ -8 +2 -33)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(-8), Token::Integer(2), Token::Integer(-33), Token::CloseParen]);
}

#[test]
fn test_booleans() {
    assert_eq!(Lexer::tokenize("#t").unwrap(),
               vec![Token::Boolean(true)]);
    assert_eq!(Lexer::tokenize("#f").unwrap(),
               vec![Token::Boolean(false)]);
    assert_eq!(Lexer::tokenize("#").err().unwrap().to_string(),
               "ParseError: Unexpected EOF when looking for t/f (line: 1, column: 2)");
    assert_eq!(Lexer::tokenize("#a").err().unwrap().to_string(),
               "ParseError: Unexpected character when looking for t/f: a (line: 1, column: 2)");

}

#[test]
fn test_identifiers() {
    for identifier in ["*", "<", "<=", "if", "while", "$t$%*=:t059s"].iter(){
        assert_eq!(Lexer::tokenize(*identifier).unwrap(),
                   vec![Token::Identifier(identifier.to_string())]);
    }
}

#[test]
fn test_strings() {
    assert_eq!(Lexer::tokenize("\"hello\"").unwrap(),
               vec![Token::String("hello".to_string())]);
    assert_eq!(Lexer::tokenize("\"a _ $ snthoeau(*&G#$()*^!\"").unwrap(),
               vec![Token::String("a _ $ snthoeau(*&G#$()*^!".to_string())]);
    assert_eq!(Lexer::tokenize("\"truncated").err().unwrap().to_string(),
               "ParseError: Expected end quote, but found EOF instead (line: 1, column: 11)")
}


#[test]
fn test_whitespace() {
    assert_eq!(Lexer::tokenize("(+ 1 1)\n(+\n   2\t2 \n )\r\n \n").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(1), Token::Integer(1), Token::CloseParen, Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(2), Token::Integer(2), Token::CloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(Lexer::tokenize("(\\)").err().unwrap().to_string(),
               "ParseError: Unexpected character: \\ (line: 1, column: 2)")
}

#[test]
fn test_delimiter_checking() {
    assert_eq!(Lexer::tokenize("(+-)").err().unwrap().to_string(),
               "ParseError: Unexpected character when looking for a delimiter: - (line: 1, column: 3)");
    assert_eq!(Lexer::tokenize("(-22+)").err().unwrap().to_string(),
               "ParseError: Unexpected character when looking for a delimiter: + (line: 1, column: 5)");
    assert_eq!(Lexer::tokenize("(22+)").err().unwrap().to_string(),"ParseError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");
    assert_eq!(Lexer::tokenize("(+ 2 3)\n(+ 1 2-)").err().unwrap().to_string(),
               "ParseError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)")

}

#[test]
fn test_complex_code_block() {
    assert_eq!(Lexer::tokenize("(define (list-of-squares n)\n  (let loop ((i n) (res (list)))\n    (if (< i 0)\n        res\n        (loop (- i 1) (cons (* i i) res)))))").unwrap(),
               vec![Token::OpenParen, Token::Identifier("define".to_string()), Token::OpenParen, Token::Identifier("list-of-squares".to_string()), Token::Identifier("n".to_string()), Token::CloseParen, Token::OpenParen, Token::Identifier("let".to_string()), Token::Identifier("loop".to_string()), Token::OpenParen, Token::OpenParen, Token::Identifier("i".to_string()), Token::Identifier("n".to_string()), Token::CloseParen, Token::OpenParen, Token::Identifier("res".to_string()), Token::OpenParen, Token::Identifier("list".to_string()), Token::CloseParen, Token::CloseParen, Token::CloseParen, Token::OpenParen, Token::Identifier("if".to_string()), Token::OpenParen, Token::Identifier("<".to_string()), Token::Identifier("i".to_string()), Token::Integer(0), Token::CloseParen, Token::Identifier("res".to_string()), Token::OpenParen, Token::Identifier("loop".to_string()), Token::OpenParen, Token::Identifier("-".to_string()), Token::Identifier("i".to_string()), Token::Integer(1), Token::CloseParen, Token::OpenParen, Token::Identifier("cons".to_string()), Token::OpenParen, Token::Identifier("*".to_string()), Token::Identifier("i".to_string()), Token::Identifier("i".to_string()), Token::CloseParen, Token::Identifier("res".to_string()), Token::CloseParen, Token::CloseParen, Token::CloseParen, Token::CloseParen, Token::CloseParen]);

}
