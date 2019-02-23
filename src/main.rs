mod lexer;
mod parser;

fn main() {
    run("(+ 2 3)");
    run("(22+)");
    run("(+ 2 3)\n(+ 1 2-)");
    run("(+ 2 (- (+ 9 1) 4))");
}

fn run(s: &str) {
    match lexer::tokenize(s){
        Ok(tokens) => {
            println!("tokens: {:?}", tokens);
            let ast = parser::parse(tokens);
            println!("ast: {:?}", ast);
        },
        Err(err) => {
            println!("Syntax error: {}", err);
            return;
        }
    }
}
