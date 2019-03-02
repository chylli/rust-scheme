use crate::parser::Node;

use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::parser::*;
use self::Value::*;

pub fn interpret(nodes: &Vec<Node>) -> Result<Value, RuntimeError> {
    let env = Environment::new_root();
    evaluate_nodes(nodes, env)
}

#[derive(PartialEq, Clone)]
pub enum Value {
    VSymbol(String),
    VInteger(i64),
    VBoolean(bool),
    VString(String),
    VList(Vec<Value>),
    VProcedure(Function),
}

// null == empty list
macro_rules! null { () => (VList(Vec::new()))}

#[derive(Clone)]
pub enum Function {
    NativeFunction(ValueOperation),
    SchemeFunction(Vec<String>, Vec<Node>, Rc<RefCell<Environment>>),
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        self == other
    }
}


// type signature for all native functions
pub type ValueOperation = fn(&[Node], Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VSymbol(val) => write!(f, "{}", val),
            VInteger(val) => write!(f, "{}", val),
            VBoolean(val) => write!(f, "#{}", if *val {"t"} else {"f"}),
            VString(val) => write!(f, "\"{}\"", val),
            VList(list)  => {
                let strs: Vec<String> = list.iter().map(|v| format!("{}", v)).collect();
                write!(f, "({})", &strs.join(" "))
            },
            VProcedure(_) => write!(f, "#<procedure>")
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    message: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.message)
    }
}

macro_rules! runtime_error{
    ($($arg:tt)*) => (
        return Err(RuntimeError { message: format!($($arg)*)})
    )
}

pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new_root() -> Rc<RefCell<Environment>> {
        let env = PREDEFINED_FUNCTIONS.with(|f|{
            let mut env = Environment { parent: None, values: HashMap::new()};
            for item in f.iter() {
                let (name, func) = item;
                env.set(name.to_string(), VProcedure(func.clone()));
            }
            env
        });

        Rc::new(RefCell::new(env))
    }

    //define a new type for Rc<RefCell<Environment>> ?
    fn new_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = Environment { parent: Some(parent), values: HashMap::new()};
        Rc::new(RefCell::new(env))
    }
    fn set(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    fn has(&self, key: &String) -> bool {
        self.values.contains_key(key)
    }

    fn get(&self, key: &String) -> Option<Value> {
        match self.values.get(key) {
            Some(val) => Some(val.clone()),
            None => {
                // recurse up the environment tree until a value is found or the end is reached
                match &self.parent {
                    Some(parent) => parent.borrow().get(key),
                    None => None
                }
            }
        }
    }
}

fn evaluate_nodes(nodes: &Vec<Node>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut result = null!();
    for node in nodes.iter() {
        result = evaluate_node(node, env.clone())?;
    };
    Ok(result)
}

fn evaluate_node(node: &Node, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match node {
        NIdentifier(v) => {
            match env.borrow().get(v) {
                Some(val) => Ok(val),
                None => runtime_error!("Identifier not found: {:?}", node)
            }
        }
        NInteger(v) => Ok(VInteger(*v)),
        NBoolean(v) => Ok(VBoolean(*v)),
        NString(v) => Ok(VString(v.clone())),
        NList(vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec, env)
            } else {
                Ok(null!())
            }
        }
    }
}

// TODO create an alias type for environment ? 
fn quote_node(node: &Node, quasi: bool, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match node {
        NIdentifier(v) => Ok(VSymbol(v.clone())),
        NInteger(v) => Ok(VInteger(*v)),
        NBoolean(v) => Ok(VBoolean(*v)),
        NString(v) => Ok(VString(v.clone())),
        NList(vec) => {
            // check if we are unquoting inside a quasiquote
            if quasi && vec.len() > 0 && *vec.get(0).unwrap() == NIdentifier("unquote".to_string()){
                if vec.len() != 2 {
                    runtime_error!("Must supply exactly one argument to unquote: {:?}", vec);
                }
                evaluate_node(vec.get(1).unwrap(), env.clone())
            }
            else {
                let mut res = vec![];
                for n in vec.iter() {
                    let v = quote_node(n, quasi, env.clone())?;
                    res.push(v);
                }
                Ok(VList(res))
            }

        }
    }
}

fn evaluate_expression(nodes: &Vec<Node>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if nodes.len() == 0 {
        runtime_error!("Can't evaluate an empty expression: {:?}", nodes);
    }
    let first = evaluate_node(nodes.first().unwrap(), env.clone())?;
    match first {
        VProcedure(f) => apply_function(&f, &nodes[1..], env.clone()),
        _ => runtime_error!("First element in an expression must be a procedure: {}", first)
    }
}

fn apply_function(func: &Function, args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match func {
        Function::NativeFunction(native_fn) => {
            native_fn(args, env)
        },
        Function::SchemeFunction(arg_names, body, func_env) => { //TODO  remove ref?
            if arg_names.len() != args.len() {
                runtime_error!("Must supply exactly {} arguments to function: {:?}", arg_names.len(), args);
            }
            // create a new, child environment for the procedure and define the arguments as local variables
            let proc_env = Environment::new_child(func_env.clone());
            for (name, arg) in arg_names.iter().zip(args.iter()) {
                            let val = evaluate_node(arg, env.clone())?;
                            proc_env.borrow_mut().set(name.clone(), val);
                        }
            Ok(evaluate_nodes(body, proc_env)?)
        }
    }
}

// we cannot use global variable directly because it is not thread safe
// limit it in thread
thread_local!(static PREDEFINED_FUNCTIONS: &'static[(&'static str, Function)] = &[
    ("define", Function::NativeFunction(native_define)),
    ("set!", Function::NativeFunction(native_set)),
    ("lambda", Function::NativeFunction(native_lambda)),
    ("λ", Function::NativeFunction(native_lambda)),
    ("if", Function::NativeFunction(native_if)),
    ("+", Function::NativeFunction(native_plus)),
    ("-", Function::NativeFunction(native_minus)),
    ("and", Function::NativeFunction(native_and)),
    ("or", Function::NativeFunction(native_or)),
    ("list", Function::NativeFunction(native_list)),
    ("quote", Function::NativeFunction(native_quote)),
    ("quasiquote", Function::NativeFunction(native_quasiquote)),
    ("error", Function::NativeFunction(native_error)),
]);


//TODO move common function to lib.scm
fn native_define(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() !=2 {
        runtime_error!("Must supply exactly two arguments to define: {:?}", args);
    }
    let name = match args.first().unwrap() {
        NIdentifier(x) => x,
        _ => runtime_error!("Unexpected node for name in define {:?}", args)};
    let already_defined = env.borrow().has(name);
    if !already_defined {
        let val = evaluate_node(args.last().unwrap(), env.clone())?;
        env.borrow_mut().set(name.clone(),val);
        Ok(null!())
    } else{
        runtime_error!("Duplicate define: {}", name)
    }
}

fn native_set(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply excactly two arguments to set!: {:?}", args);
    }
    let name = match args.first().unwrap() {
        NIdentifier(x) => x,
        _ => runtime_error!("Unexpected node for name in set!: {:?}", args)
    };
    let already_defined = env.borrow().has(name);
    if already_defined {
        let val = evaluate_node(args.get(1).unwrap(), env.clone())?;
        env.borrow_mut().set(name.clone(), val);
        Ok(null!())
    } else {
        runtime_error!("Can't set! an undefined variable: {}",name)
    }
}

fn native_lambda(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to lambda: {:?}", args)
    }
    let arg_names = match args.first().unwrap(){
        NList(list) => {
            let mut names = Vec::new();
            for item in list.iter(){
                match item {
                    NIdentifier(s) => names.push(s.clone()),
                    _ => runtime_error!("Unexpected argument  in lambda arguments: {:?}", item)
                };
            }
            names
        }
        _ => runtime_error!("Unexpected node for arguments in lambda: {:?}", args)
    };
    let body = args[1..].to_vec();
    Ok(VProcedure(Function::SchemeFunction(arg_names, body, env.clone())))
}

fn native_if(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        runtime_error!("Must supply exactly three arguments to if: {:?}", args);
    }
    let condition = evaluate_node(args.first().unwrap(), env.clone())?;
    match condition {
        VBoolean(false) => evaluate_node(args.get(2).unwrap(), env.clone()),
        _ => evaluate_node(args.get(1).unwrap(), env.clone())
    }
}

fn native_and(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut res = VBoolean(true);
    for n in args.iter() {
        let v = evaluate_node(n, env.clone())?;
        match v {
            VBoolean(false) => return Ok(v),
            _ => res = v
        }
    }
    Ok(res)
}

fn native_or(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    for n in args.iter() {
        let v = evaluate_node(n, env.clone())?;
        match v {
            VBoolean(false) => (),
            _ => return Ok(v)
        }
    }
    Ok(VBoolean(false))
}

fn native_plus(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to +: {:?}", args);
    }
    let mut sum = 0;
    for n in args[..].iter() {  // todo here slice
        let v = evaluate_node(n, env.clone())?;
        match v {
            VInteger(x) => sum += x,
            _ => runtime_error!("Unexpected node during +: {:?}", n)
        };
    };
    Ok(VInteger(sum))
}

fn native_minus(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to -: {:?}", args);
    }
    let v1 = evaluate_node(args.first().unwrap(), env.clone())?;
    let v2 = evaluate_node(args.last().unwrap(), env.clone())?;
    let mut result = match v1 {
        VInteger(x) => x,
        _ => runtime_error!("Unexpected node during -: {:?}", args)
    };
    result -= match v2 {
        VInteger(x) => x,
        _ => runtime_error!("Unexpected node during -: {:?}", args)
    };
    Ok(VInteger(result))
}

fn native_list(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut elements = vec![];
    for n in args[..].iter() {
        let v = evaluate_node(n, env.clone())?;
        elements.push(v);
    }
    Ok(VList(elements))
}

fn native_quote(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quote: {:?}", args);
    }
    quote_node(args.first().unwrap(), false, env.clone())

}

fn native_quasiquote(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quasiquote: {:?}", args);
    }
    quote_node(args.first().unwrap(), true, env.clone())

}

fn native_error(args: &[Node], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must suppply exactly one arguments to  error: {:?}", args);
    }
    let e = evaluate_node(args.first().unwrap(), env.clone())?;
    runtime_error!("{}", e);
}

#[test]
fn test_global_variables() {
    assert_eq!(interpret(&vec![NList(vec![NIdentifier("define".to_string()), NIdentifier("x".to_string()), NInteger(2)]), NList(vec![NIdentifier("+".to_string()), NIdentifier("x".to_string()), NIdentifier("x".to_string()), NIdentifier("x".to_string())])]).unwrap(),
               VInteger(6));
}

#[test]
fn test_global_function_definition() {
    assert_eq!(interpret(&vec![NList(vec![NIdentifier("define".to_string()), NIdentifier("double".to_string()), NList(vec![NIdentifier("lambda".to_string()), NList(vec![NIdentifier("x".to_string())]), NList(vec![NIdentifier("+".to_string()), NIdentifier("x".to_string()), NIdentifier("x".to_string())])])]), NList(vec![NIdentifier("double".to_string()), NInteger(8)])]).unwrap(),
               VInteger(16));
}

