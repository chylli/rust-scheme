mod lexer;
mod parser;
mod interpreter;

fn main() {
    run("(+ 2 3)");
    run("(22+)");
    run("(+ 2 3)\n(+ 1 2-)");
    run("(+ 2 (- (+ 9 1) 4))");
    run("(define x 2) (+ x x x)");
}

fn run(input: &str) {
    println!("input: \"{:?}\"", input);
    println!("result: \"{:?}\"", execute(input));
}

fn execute(input: &str) -> Result<String, String> {
    let tokens = match lexer::tokenize(input) {
        Ok(t) => t,
        Err(e) => return Err(e.to_string())
    };
    println!("tokens: {:?}", tokens);

    let ast = match parser::parse(&tokens) {
        Ok(t) => t,
        Err(e) => return Err(e.to_string())
    };
    println!("ast: {:?}", ast);

    let result = match interpreter::interpret(&ast) {
        Ok(t) => t,
        Err(e) => return Err(e.to_string())
    };

    Ok(format!("{:?}", result))
}
