fn main() {
    run("(+ 2 3)");
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
    Integer(u32),
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Token::OpenParen => write!(f, "OpenParen"),
            Token::CloseParen => write!(f, "CloseParen"),
            Token::Identifier(ref v) => write!(f, "Identifier({})", v),
            Token::Integer(ref v) => write!(f, "Integer({})", v),
        }
    }
}

fn tokenize(s: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    for c in s.chars() {
        match c {
            '(' => tokens.push(Token::OpenParen),
            ')' => tokens.push(Token::CloseParen),
            '+' => tokens.push(Token::Identifier(c.to_string())),
            '0'...'9' => tokens.push(Token::Integer(c.to_digit(10).unwrap())),
            ' ' => (),
            _   => println!("other"),
        };
    }
    tokens
}
