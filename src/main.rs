use std::fs::File;
use std::env;
use std::path::Path;
use std::io::Read;

mod lexer;
mod parser;
mod interpreter;

mod repl;

//TODO https://github.com/rust-lang/rfcs/blob/master/text/0243-trait-based-exception-handling.md  rewrite it to use exception type casting
macro_rules! try_or_err_to_str{
    ($inp:expr) => (
        match $inp {
            Ok(v) => v,
            Err(e) => return Err(e.to_string())
        })
}

//#[cfg(not(test))]
fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => start_repl(),
        2 => run_file(&args[1]),
        _ => panic!("Your must provide 0 or 1 arguments to RustyScheme: {:?}", args)
    }
}

fn run_file(filename: &String) {
    let path = Path::new(filename);
    let mut file = File::open(&path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    let ctx = interpreter::Interpreter::new();
    execute(content.as_str(), ctx).unwrap();
}
fn start_repl() {
    let ctx = interpreter::Interpreter::new();
    println!("\nWelcome to the RustyScheme REPL!");
    repl::start("> ", |s| execute(s.as_str(), ctx.clone())); // why clone ?
}


fn execute(input: &str, ctx: interpreter::Interpreter) -> Result<String, String> {
    let tokens = try_or_err_to_str!(lexer::tokenize(input));
    let ast = try_or_err_to_str!(parser::parse(&tokens));
    let result = try_or_err_to_str!(ctx.run(&ast));

    Ok(format!("{}", result))
}

#[cfg(test)] // compile only when test
macro_rules! assert_execute{
    ($src:expr, $res:expr) => (
        assert_eq!(execute($src, interpreter::Interpreter::new()).unwrap().as_str(), $res)
    )
}

#[cfg(test)]
macro_rules! assert_execute_fail{
    ($src:expr, $res:expr) => (
        assert_eq!(execute($src, interpreter::Interpreter::new()).err().unwrap().as_str(), $res)
    )
}

#[test]
fn test_basic_identities() {
    assert_execute!("1", "1");
    assert_execute!("#f", "#f");
    assert_execute!("\"hi\"", "\"hi\"");
    assert_execute!("(lambda (x) x)", "#<procedure>");
}

#[test]
fn test_simple_function() {
    assert_execute!("(+ 2 3)", "5");
}

#[test]
fn test_multiple_expression_return() {
    assert_execute!("(+ 2 3)\n(+ 1 2)", "3");
}

#[test]
fn test_nested_expressions() {
    assert_execute!("(+ 2 (- (+ 9 1) 4))", "8");
}

#[test]
fn test_list_creation() {
    assert_execute!("(list)", "()");
    assert_execute!("(list 1 2 3)", "(1 2 3)");
    assert_execute!("(list 1 (list 2 3) (list 4) (list))", "(1 (2 3) (4) ())");
}

#[test]
fn test_variable_definition() {
    assert_execute!("(define x 2) (+ x x x)", "6");
}

#[test]
fn test_duplicate_variable_definition() {
    assert_execute_fail!("(define x 2) (define x 3)", "RuntimeError: Duplicate define: x");
}

#[test]
fn test_variable_modification() {
    assert_execute!("(define x 2) (set! x 3) (+ x x x)", "9");
}

#[test]
fn test_unknown_variable_modification() {
    assert_execute_fail!("(set! x 3)", "RuntimeError: Can't set! an undefined variable: x");
}

#[test]
fn test_procedure_definition() {
    assert_execute!("(define double (lambda (x) (+ x x))) (double 8)", "16");
    assert_execute!("(define twice (lambda (f v) (f (f v)))) (twice (lambda (x) (+ x x)) 8)", "32");
    assert_execute!("(define twice (λ (f v) (f (f v)))) (twice (λ (x) (+ x x)) 8)", "32");
    assert_execute!("((λ (x) (+ x x)) 8)", "16");
    assert_execute!("(define (twice f v) (f (f v))) (twice (lambda (x) (+ x x)) 8)", "32");
}

#[test]
fn test_conditional_execution() {
    assert_execute!("(if #t 1 2)", "1");
    assert_execute!("(if #f 1 2)", "2");
    assert_execute!("(if 0 1 2)", "1");
    assert_execute!("(if \"\" 1 2)", "1");
}

#[test]
fn test_conditional_execution_doesnt_run_other_case() {
    assert_execute!("(if #t 1 (error \"bad\"))", "1");
    assert_execute!("(if #f (error \"bad\") 2)", "2");
}

#[test]
fn test_boolean_operators() {
    assert_execute!("(and)", "#t");
    assert_execute!("(and #t)", "#t");
    assert_execute!("(and 1)", "1");
    assert_execute!("(and 1 2 3)", "3");
    assert_execute!("(and 1 #f 3)", "#f");
    assert_execute!("(and 1 #f (error \"bad\"))", "#f");
    assert_execute!("(or)", "#f");
    assert_execute!("(or #f)", "#f");
    assert_execute!("(or 1)", "1");
    assert_execute!("(or 1 2)", "1");
    assert_execute!("(or 1 #f)", "1");
    assert_execute!("(or #f 3)", "3");
    assert_execute!("(or #f #f)", "#f");
    assert_execute!("(or 1 (error \"bad\"))", "1");
}

#[test]
fn test_quoting() {
    assert_execute!("(quote #t)", "#t");
    assert_execute!("(quote 1)", "1");
    assert_execute!("(quote sym)", "sym");
    assert_execute!("(quote \"hi\")", "\"hi\"");
    assert_execute!("(quote (1 2))", "(1 2)");
    assert_execute!("(quote (a b))", "(a b)");
    assert_execute!("(quote (a b (c (d) e ())))", "(a b (c (d) e ()))");
    assert_execute!("(quote (a (quote b)))", "(a (quote b))");
    assert_execute!("'(1 2)", "(1 2)");
    assert_execute!("'(a b (c (d) e ()))", "(a b (c (d) e ()))");
    assert_execute!("'(1 '2)", "(1 (quote 2))");

}

#[test]
fn test_quasiquoting() {
    assert_execute!("(quasiquote (1 2))", "(1 2)");
    assert_execute!("`(2 ,(+ 1 2) 4)", "(2 3 4)");
}

#[test]
fn test_apply() {
    assert_execute!("(apply + '(1 2 3))", "6");
    assert_execute!("(define foo (lambda (f) (lambda (x y) (f (f x y) y)))) (apply (apply foo '(+)) '(5 3))", "11");
}

#[test]
fn test_eval() {
    assert_execute!("(eval '(+ 1 2 3))", "6");
    assert_execute!("(define eval-formula (lambda (formula) (eval `((lambda (x y) ,formula) 2 3)))) (eval-formula '(+ (- y x) y))", "4");
    assert_execute_fail!("(define bad-eval-formula (lambda (formula) ((lambda (x y) (eval formula)) 2 3))) (bad-eval-formula '(+ x y))", "RuntimeError: Identifier not found: x");
}

#[test]
fn test_bad_syntax() {
    assert_execute_fail!("(22+)",
               "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");
    assert_execute_fail!("(+ 2 3)\n(+ 1 2-)",
               "SyntaxError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)");
}

#[test]
fn test_unicode_identifiers() {
    assert_execute!("(define ★ 3) (define ♫ 4) (+ ★ ♫)", "7");
}
