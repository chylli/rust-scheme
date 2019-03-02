use crate::parser::Node;

use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::parser::*;
use self::Value::*;
use self::Function::*;

pub fn interpret(nodes: &[Node]) -> Result<Value, RuntimeError> {
    let env = Environment::new_root();
    let values = Value::from_nodes(nodes);
    evaluate_values(&values, env)
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

impl Value {
    fn from_nodes(nodes: &[Node]) -> Vec<Value> { //try to rewrite it as Value::from ?
        let mut values = vec![];
        for node in nodes.iter() {
            values.push(Value::from_node(node));
        }
        values
    }

    fn from_node(node: &Node) -> Value {
        match node {
            NIdentifier(ref val) => VSymbol(val.clone()),
            NInteger(val) => VInteger(*val),
            NBoolean(val) => VBoolean(*val),
            NString(val) => VString(val.clone()),
            NList(nodes) => VList(Value::from_nodes(nodes))
        }
    }
}

// null == empty list
macro_rules! null { () => (VList(Vec::new()))}

#[derive(Clone)]
pub enum Function {
    NativeFunction(ValueOperation),
    SchemeFunction(Vec<String>, Vec<Value>, Rc<RefCell<Environment>>),
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        self == other
    }
}


// type signature for all native functions
pub type ValueOperation = fn(&[Value], Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;


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
    fn get_root(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        match env.borrow().parent {
            Some(ref parent) => Environment::get_root(parent.clone()),
            None => env.clone()
        }
    }
}

fn evaluate_values(values: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut result = null!();
    for value in values.iter() {
        result = evaluate_value(value, env.clone())?;
    };
    Ok(result)
}

fn evaluate_value(value: &Value, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match value {
        VSymbol(v) => {
            match env.borrow().get(v) {
                Some(val) => Ok(val),
                None => runtime_error!("Identifier not found: {:?}", value)
            }
        }
        VInteger(v) => Ok(VInteger(*v)),
        VBoolean(v) => Ok(VBoolean(*v)),
        VString(v) => Ok(VString(v.clone())),
        VList(vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec, env)
            } else {
                Ok(null!())
            }
        },
        VProcedure(v) => Ok(VProcedure(v.clone()))
    }
}

// TODO create an alias type for environment ? 
fn quote_value(value: &Value, quasi: bool, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match value {
        VSymbol(v) => Ok(VSymbol(v.clone())),
        VInteger(v) => Ok(VInteger(*v)),
        VBoolean(v) => Ok(VBoolean(*v)),
        VString(v) => Ok(VString(v.clone())),
        VList(vec) => {
            // check if we are unquoting inside a quasiquote
            if quasi && vec.len() > 0 && *vec.get(0).unwrap() == VSymbol("unquote".to_string()){
                if vec.len() != 2 {
                    runtime_error!("Must supply exactly one argument to unquote: {:?}", vec);
                }
                evaluate_value(vec.get(1).unwrap(), env.clone())
            }
            else {
                let mut res = vec![];
                for n in vec.iter() {
                    let v = quote_value(n, quasi, env.clone())?;
                    res.push(v);
                }
                Ok(VList(res))
            }

        },
        VProcedure(v) => Ok(VProcedure(v.clone()))
    }
}

fn evaluate_expression(values: &Vec<Value>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if values.len() == 0 {
        runtime_error!("Can't evaluate an empty expression: {:?}", values);
    }
    let first = evaluate_value(values.first().unwrap(), env.clone())?;
    match first {
        VProcedure(f) => apply_function(&f, &values[1..], env.clone()),
        _ => runtime_error!("First element in an expression must be a procedure: {}", first)
    }
}

fn apply_function(func: &Function, args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match func {
        NativeFunction(native_fn) => {
            native_fn(args, env)
        },
        SchemeFunction(arg_names, body, func_env) => { //TODO  remove ref?
            if arg_names.len() != args.len() {
                runtime_error!("Must supply exactly {} arguments to function: {:?}", arg_names.len(), args);
            }
            // create a new, child environment for the procedure and define the arguments as local variables
            let proc_env = Environment::new_child(func_env.clone());
            for (name, arg) in arg_names.iter().zip(args.iter()) {
                            let val = evaluate_value(arg, env.clone())?;
                            proc_env.borrow_mut().set(name.clone(), val);
                        }
            Ok(evaluate_values(body, proc_env)?)
        }
    }
}

// we cannot use global variable directly because it is not thread safe
// limit it in thread
thread_local!(static PREDEFINED_FUNCTIONS: &'static[(&'static str, Function)] = &[
    ("define", NativeFunction(native_define)),
    ("set!", NativeFunction(native_set)),
    ("lambda", NativeFunction(native_lambda)),
    ("λ", NativeFunction(native_lambda)),
    ("if", NativeFunction(native_if)),
    ("+", NativeFunction(native_plus)),
    ("-", NativeFunction(native_minus)),
    ("and", NativeFunction(native_and)),
    ("or", NativeFunction(native_or)),
    ("list", NativeFunction(native_list)),
    ("quote", NativeFunction(native_quote)),
    ("quasiquote", NativeFunction(native_quasiquote)),
    ("error", NativeFunction(native_error)),
    ("apply", NativeFunction(native_apply)),
    ("eval", NativeFunction(native_eval)),
]);


//TODO move common function to lib.scm
fn native_define(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() !=2 {
        runtime_error!("Must supply exactly two arguments to define: {:?}", args);
    }
    let name = match args.first().unwrap() {
        VSymbol(x) => x,
        _ => runtime_error!("Unexpected value for name in define {:?}", args)};
    let already_defined = env.borrow().has(name);
    if !already_defined {
        let val = evaluate_value(args.last().unwrap(), env.clone())?;
        env.borrow_mut().set(name.clone(),val);
        Ok(null!())
    } else{
        runtime_error!("Duplicate define: {}", name)
    }
}

fn native_set(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply excactly two arguments to set!: {:?}", args);
    }
    let name = match args.first().unwrap() {
        VSymbol(x) => x,
        _ => runtime_error!("Unexpected value for name in set!: {:?}", args)
    };
    let already_defined = env.borrow().has(name);
    if already_defined {
        let val = evaluate_value(args.get(1).unwrap(), env.clone())?;
        env.borrow_mut().set(name.clone(), val);
        Ok(null!())
    } else {
        runtime_error!("Can't set! an undefined variable: {}",name)
    }
}

fn native_lambda(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to lambda: {:?}", args)
    }
    let arg_names = match args.first().unwrap(){
        VList(list) => {
            let mut names = Vec::new();
            for item in list.iter(){
                match item {
                    VSymbol(s) => names.push(s.clone()),
                    _ => runtime_error!("Unexpected argument in lambda arguments: {:?}", item)
                };
            }
            names
        }
        _ => runtime_error!("Unexpected value for arguments in lambda: {:?}", args)
    };
    let body = args[1..].to_vec();
    Ok(VProcedure(SchemeFunction(arg_names, body, env.clone())))
}

fn native_if(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        runtime_error!("Must supply exactly three arguments to if: {:?}", args);
    }
    let condition = evaluate_value(args.first().unwrap(), env.clone())?;
    match condition {
        VBoolean(false) => evaluate_value(args.get(2).unwrap(), env.clone()),
        _ => evaluate_value(args.get(1).unwrap(), env.clone())
    }
}

fn native_and(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut res = VBoolean(true);
    for n in args.iter() {
        let v = evaluate_value(n, env.clone())?;
        match v {
            VBoolean(false) => return Ok(v),
            _ => res = v
        }
    }
    Ok(res)
}

fn native_or(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    for n in args.iter() {
        let v = evaluate_value(n, env.clone())?;
        match v {
            VBoolean(false) => (),
            _ => return Ok(v)
        }
    }
    Ok(VBoolean(false))
}

fn native_plus(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to +: {:?}", args);
    }
    let mut sum = 0;
    for n in args[..].iter() {  // todo here slice
        let v = evaluate_value(n, env.clone())?;
        match v {
            VInteger(x) => sum += x,
            _ => runtime_error!("Unexpected value during +: {:?}", n)
        };
    };
    Ok(VInteger(sum))
}

fn native_minus(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to -: {:?}", args);
    }
    let v1 = evaluate_value(args.first().unwrap(), env.clone())?;
    let v2 = evaluate_value(args.last().unwrap(), env.clone())?;
    let mut result = match v1 {
        VInteger(x) => x,
        _ => runtime_error!("Unexpected value during -: {:?}", args)
    };
    result -= match v2 {
        VInteger(x) => x,
        _ => runtime_error!("Unexpected value during -: {:?}", args)
    };
    Ok(VInteger(result))
}

fn native_list(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut elements = vec![];
    for n in args[..].iter() {
        let v = evaluate_value(n, env.clone())?;
        elements.push(v);
    }
    Ok(VList(elements))
}

fn native_quote(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quote: {:?}", args);
    }
    quote_value(args.first().unwrap(), false, env.clone())

}

fn native_quasiquote(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quasiquote: {:?}", args);
    }
    quote_value(args.first().unwrap(), true, env.clone())

}

fn native_error(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must suppply exactly one arguments to error: {:?}", args);
    }
    let e = evaluate_value(args.first().unwrap(), env.clone())?;
    runtime_error!("{}", e);
}

fn native_apply(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to apply: {:?}", args);
    }
    let func = match evaluate_value(args.get(0).unwrap(), env.clone())? {
        VProcedure(func) => func,
        _ => runtime_error!("First argument to apply must be a procedure: {:?}", args)
    };
    let funcArgs = match evaluate_value(args.get(1).unwrap(), env.clone())? {
        VList(funcArgs) => funcArgs,
        _ => runtime_error!("Second argument to apply must be a list of arguments: {:?}", args)
    };
    apply_function(&func, funcArgs.as_slice(), env.clone())
}

fn native_eval(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to eval: {:?}", args);
    }

    // eval is basically just a double-evaluation -- the first evaluate returns the data using the local envirnoment, and the second evaluate evaluates the data as code using the global environment
    let res = evaluate_value(args.get(0).unwrap(), env.clone())?;
    evaluate_value(&res, Environment::get_root(env))
}

#[test]
fn test_global_variables() {
    assert_eq!(interpret(&[NList(vec![NIdentifier("define".to_string()), NIdentifier("x".to_string()), NInteger(2)]), NList(vec![NIdentifier("+".to_string()), NIdentifier("x".to_string()), NIdentifier("x".to_string()), NIdentifier("x".to_string())])]).unwrap(),
               VInteger(6));
}

#[test]
fn test_global_function_definition() {
    assert_eq!(interpret(&vec![NList(vec![NIdentifier("define".to_string()), NIdentifier("double".to_string()), NList(vec![NIdentifier("lambda".to_string()), NList(vec![NIdentifier("x".to_string())]), NList(vec![NIdentifier("+".to_string()), NIdentifier("x".to_string()), NIdentifier("x".to_string())])])]), NList(vec![NIdentifier("double".to_string()), NInteger(8)])]).unwrap(),
               VInteger(16));
}

