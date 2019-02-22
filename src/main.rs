mod lexer;

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = lexer::tokenize(s);
    println!("tokens: {:?}", tokens);
}


fn main() {
    run("(+ 2 3)");
    run("(22+)");
}

