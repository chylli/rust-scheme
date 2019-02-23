use crate::lexer::Token;
use std::fmt;
use std::slice;


pub fn parse(tokens: Vec<Token>) -> Result<Vec<Node>, ParseError> {
    Parser::parse(tokens)
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Node>),
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

macro_rules! parse_error{
    ($($arg:tt)*) => (
        return Err(ParseError { message: format!($($arg)*)})
    )
}

struct Parser<'a> {
    tokens: slice::Iter<'a, Token>
}

impl<'a> Parser<'a> {
    fn parse(tokens: Vec<Token>) -> Result<Vec<Node>, ParseError> {
        let mut parser = Parser { tokens: tokens.iter() };
        parser.run(0)
    }

    fn run(&mut self, depth: u64) -> Result<Vec<Node>, ParseError> {
        let mut vec = Vec::new();
        loop {
            match self.tokens.next() {
                Some(token) => {
                    match *token {
                        Token::OpenParen => {
                            let inner = self.run(depth + 1)?;
                            vec.push(Node::List(inner));
                        },
                        Token::CloseParen => {
                            if depth > 0 {
                                return Ok(vec);
                            } else {
                                parse_error!("Unexpected close paren, depth: {}", depth);
                            }
                        },
                        Token::Identifier(ref val) => {
                            vec.push(Node::Identifier(val.clone()));
                        },
                        Token::Integer(ref val) => {
                            vec.push(Node::Integer(val.clone()));
                        },
                        Token::Boolean(ref val) => {
                            vec.push(Node::Boolean(val.clone()));
                        },
                        Token::String(ref val) => {
                            vec.push(Node::String(val.clone()));
                        }
                    };
                },
                None => {
                    if depth == 0 {
                        return Ok(vec);
                    } else {
                        parse_error!("Unexpected end of input, depth: {}", depth);
                    }
                }
            }
        }
    }
}

#[test]
fn test_simple() {
    assert_eq!(parse(vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("+".to_string())])]);
}

#[test]
fn test_nested() {
    assert_eq!(parse(vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(1), Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(3), Token::Integer(4), Token::CloseParen, Token::CloseParen, Token::Integer(5), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("+".to_string()), Node::List(vec![Node::Identifier("+".to_string()), Node::Integer(1), Node::List(vec![Node::Identifier("+".to_string()), Node::Integer(3), Node::Integer(4)])]), Node::Integer(5)])]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(parse(vec![Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(vec![Token::OpenParen, Token::OpenParen, Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected end of input, depth: 1");
    assert_eq!(parse(vec![Token::OpenParen, Token::CloseParen, Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(vec![Token::OpenParen, Token::OpenParen, Token::CloseParen, Token::OpenParen, Token::OpenParen, Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected end of input, depth: 2");
}
