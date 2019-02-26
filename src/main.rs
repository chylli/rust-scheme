mod lexer;
mod parser;
mod interpreter;

fn main() {
    run("(+ 2 3)");
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

    Ok(format!("{}", result))
}

#[test]
fn test_basic_identities() {
    assert_eq!(execute("1").unwrap().as_str(),
               "1");
    assert_eq!(execute("#f").unwrap().as_str(),
               "#f");
    assert_eq!(execute("\"hi\"").unwrap().as_str(),
               "\"hi\"");
    assert_eq!(execute("(lambda (x) x)").unwrap().as_str(),
               "#<procedure>");
}

#[test]
fn test_simple_function() {
    assert_eq!(execute("(+ 2 3)").unwrap().as_str(),
               "5");
}

#[test]
fn test_multiple_expression_return() {
    assert_eq!(execute("(+ 2 3)\n(+ 1 2)").unwrap().as_str(),
               "3");
}

#[test]
fn test_nested_expressions() {
    assert_eq!(execute("(+ 2 (- (+ 9 1) 4))").unwrap().as_str(),
               "8");
}

#[test]
fn test_variable_definition() {
    assert_eq!(execute("(define x 2) (+ x x x)").unwrap().as_str(),
               "6");
}

#[test]
fn test_duplicate_variable_definition() {
    assert_eq!(execute("(define x 2) (define x 3)").err().unwrap().as_str(),
               "RuntimeError: Duplicate define: x");
}

#[test]
fn test_variable_modification() {
    assert_eq!(execute("(define x 2) (set! x 3) (+ x x x)").unwrap().as_str(),
               "9");
}

#[test]
fn test_unknown_variable_modification() {
    assert_eq!(execute("(set! x 3)").err().unwrap().as_str(),
               "RuntimeError: Can't set! an undefined variable: x");
}

#[test]
fn test_procedure_definition() {
    assert_eq!(execute("(define double (lambda (x) (+ x x))) (double 8)").unwrap().as_str(),
               "16");
}

#[test]
fn test_bad_syntax() {
    assert_eq!(execute("(22+)").err().unwrap().as_str(),
               "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");
    assert_eq!(execute("(+ 2 3)\n(+ 1 2-)").err().unwrap().as_str(),
               "SyntaxError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)");
}
