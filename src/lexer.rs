use std::str;
use std::fmt;

pub fn tokenize(s: &str) -> Result<Vec<Token>, SyntaxError> {
    Lexer::tokenize(s)
}

macro_rules! parse_error {
    ($lexer:ident, $($arg:tt)*) => (
        return Err(SyntaxError {message: format!($($arg)*), line: $lexer.line, column: $lexer.column})
    )
}

//pub here because main.rs used it. Maybe removed later.
#[derive(PartialEq, Debug)]
pub enum Token {
    OpenParen,
    CloseParen,
    Quote,
    Quasiquote,
    Unquote,
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    String(String),
}

//pub here because main.rs used it. Maybe removed later.
#[derive(Debug)]
pub struct SyntaxError {
    message: String,
    line: u64,
    column: u64,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SyntaxError: {} (line: {}, column: {})", self.message, self.line, self.column)
    }
}

pub struct Lexer<'a> {
    chars: std::iter::Peekable<str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
    line: u64,
    column: u64,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Result<Vec<Token>, SyntaxError> {
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

    fn run(&mut self) -> Result<(), SyntaxError> {
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
                        '\'' => {
                            self.tokens.push(Token::Quote);
                            self.advance();
                        },
                        '`' => {
                            self.tokens.push(Token::Quasiquote);
                            self.advance();
                        },
                        ',' => {
                            self.tokens.push(Token::Unquote);
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

    fn parse_number(&mut self) -> Result<i64, SyntaxError> {
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

    fn parse_boolean(&mut self) -> Result<bool, SyntaxError> {
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

    fn parse_identifier(&mut self) -> Result<String, SyntaxError> {
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

    fn parse_string(&mut self) -> Result<String, SyntaxError> {
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

    fn parse_delimiter(&mut self) -> Result<(), SyntaxError> {
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
    assert_eq!(tokenize("(+ 2 3)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(2), Token::Integer(3), Token::CloseParen]);
}

#[test]
fn test_multi_digit_integers() {
    assert_eq!(tokenize("(+ 21 325)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(21), Token::Integer(325), Token::CloseParen]);
}

#[test]
fn test_subtraction(){
    assert_eq!(tokenize("(+ -8 +2 -33)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(-8), Token::Integer(2), Token::Integer(-33), Token::CloseParen]);
}

#[test]
fn test_negative_integers() {
    assert_eq!(tokenize("(+ -8 +2 -33)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(-8), Token::Integer(2), Token::Integer(-33), Token::CloseParen]);
}

#[test]
fn test_booleans() {
    assert_eq!(tokenize("#t").unwrap(),
               vec![Token::Boolean(true)]);
    assert_eq!(tokenize("#f").unwrap(),
               vec![Token::Boolean(false)]);
    assert_eq!(tokenize("#").err().unwrap().to_string(),
               "SyntaxError: Unexpected EOF when looking for t/f (line: 1, column: 2)");
    assert_eq!(tokenize("#a").err().unwrap().to_string(),
               "SyntaxError: Unexpected character when looking for t/f: a (line: 1, column: 2)");

}

#[test]
fn test_identifiers() {
    for identifier in ["*", "<", "<=", "if", "while", "$t$%*=:t059s"].iter(){
        assert_eq!(tokenize(*identifier).unwrap(),
                   vec![Token::Identifier(identifier.to_string())]);
    }
}

#[test]
fn test_strings() {
    assert_eq!(tokenize("\"hello\"").unwrap(),
               vec![Token::String("hello".to_string())]);
    assert_eq!(tokenize("\"a _ $ snthoeau(*&G#$()*^!\"").unwrap(),
               vec![Token::String("a _ $ snthoeau(*&G#$()*^!".to_string())]);
    assert_eq!(tokenize("\"truncated").err().unwrap().to_string(),
               "SyntaxError: Expected end quote, but found EOF instead (line: 1, column: 11)")
}


#[test]
fn test_whitespace() {
    assert_eq!(tokenize("(+ 1 1)\n(+\n   2\t2 \n )\r\n \n").unwrap(),
               vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(1), Token::Integer(1), Token::CloseParen, Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(2), Token::Integer(2), Token::CloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(tokenize("(\\)").err().unwrap().to_string(),
               "SyntaxError: Unexpected character: \\ (line: 1, column: 2)")
}

#[test]
fn test_delimiter_checking() {
    assert_eq!(tokenize("(+-)").err().unwrap().to_string(),
               "SyntaxError: Unexpected character when looking for a delimiter: - (line: 1, column: 3)");
    assert_eq!(tokenize("(-22+)").err().unwrap().to_string(),
               "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 5)");
    assert_eq!(tokenize("(22+)").err().unwrap().to_string(),"SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");
    assert_eq!(tokenize("(+ 2 3)\n(+ 1 2-)").err().unwrap().to_string(),
               "SyntaxError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)")

}

#[test]
fn test_quoting() {
    assert_eq!(tokenize("'(a)").unwrap(),
               vec![Token::Quote, Token::OpenParen, Token::Identifier("a".to_string()), Token::CloseParen]);
    assert_eq!(tokenize("'('a 'b)").unwrap(),
               vec![Token::Quote, Token::OpenParen, Token::Quote, Token::Identifier("a".to_string()), Token::Quote, Token::Identifier("b".to_string()), Token::CloseParen]);
    assert_eq!(tokenize("(list 'a b)").unwrap(),
               vec![Token::OpenParen, Token::Identifier("list".to_string()), Token::Quote, Token::Identifier("a".to_string()), Token::Identifier("b".to_string()), Token::CloseParen]);
}

#[test]
fn test_quasiquoting() {
    assert_eq!(tokenize("`(,a)").unwrap(),
               vec![Token::Quasiquote, Token::OpenParen, Token::Unquote, Token::Identifier("a".to_string()), Token::CloseParen]);
    assert_eq!(tokenize("`(,a b ,c)").unwrap(),
               vec![Token::Quasiquote, Token::OpenParen, Token::Unquote, Token::Identifier("a".to_string()), Token::Identifier("b".to_string()), Token::Unquote, Token::Identifier("c".to_string()), Token::CloseParen]);
}

#[test]
fn test_complex_code_block() {
    assert_eq!(tokenize("(define (list-of-squares n)\n  (let loop ((i n) (res (list)))\n    (if (< i 0)\n        res\n        (loop (- i 1) (cons (* i i) res)))))").unwrap(),
               vec![Token::OpenParen, Token::Identifier("define".to_string()), Token::OpenParen, Token::Identifier("list-of-squares".to_string()), Token::Identifier("n".to_string()), Token::CloseParen, Token::OpenParen, Token::Identifier("let".to_string()), Token::Identifier("loop".to_string()), Token::OpenParen, Token::OpenParen, Token::Identifier("i".to_string()), Token::Identifier("n".to_string()), Token::CloseParen, Token::OpenParen, Token::Identifier("res".to_string()), Token::OpenParen, Token::Identifier("list".to_string()), Token::CloseParen, Token::CloseParen, Token::CloseParen, Token::OpenParen, Token::Identifier("if".to_string()), Token::OpenParen, Token::Identifier("<".to_string()), Token::Identifier("i".to_string()), Token::Integer(0), Token::CloseParen, Token::Identifier("res".to_string()), Token::OpenParen, Token::Identifier("loop".to_string()), Token::OpenParen, Token::Identifier("-".to_string()), Token::Identifier("i".to_string()), Token::Integer(1), Token::CloseParen, Token::OpenParen, Token::Identifier("cons".to_string()), Token::OpenParen, Token::Identifier("*".to_string()), Token::Identifier("i".to_string()), Token::Identifier("i".to_string()), Token::CloseParen, Token::Identifier("res".to_string()), Token::CloseParen, Token::CloseParen, Token::CloseParen, Token::CloseParen, Token::CloseParen]);

}
