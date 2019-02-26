use crate::parser::Node;

use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub fn interpret(nodes: &Vec<Node>) -> Result<Value, RuntimeError> {
    let env = Environment::new_root();
    evaluate_nodes(nodes, env)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    Procedure(Vec<String>, Vec<Node>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integer(val) => write!(f, "{}", val),
            Value::Boolean(val) => write!(f, "#{}", if *val {"t"} else {"f"}),
            Value::String(val) => write!(f, "\"{}\"", val),
            Value::List(list)  => {
                let strs: Vec<String> = list.iter().map(|v| format!("{}", v)).collect();
                write!(f, "({})", &strs.join(" "))
            },
            Value::Procedure(_, _) => write!(f, "#<procedure>")
        }
    }
}

// null == empty list
macro_rules! null { () => (Value::List(Vec::new()))}

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

struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new_root() -> Rc<RefCell<Environment>> {
        let env = Environment { parent: None, values: HashMap::new()};
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
        Node::Identifier(v) => {
            match env.borrow().get(v) {
                Some(val) => Ok(val),
                None => runtime_error!("Identifier not found: {:?}", node)
            }
        }
        Node::Integer(v) => Ok(Value::Integer(*v)),
        Node::Boolean(v) => Ok(Value::Boolean(*v)),
        Node::String(v) => Ok(Value::String(v.clone())),
        Node::List(vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec, env)
            } else {
                Ok(null!())
            }
        }
    }
}

fn evaluate_expression(nodes: &Vec<Node>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let (first, others) = match nodes.split_first() {
        Some(v) => v,
        None => runtime_error!("Can't evaluate an empty expression: {:?}", nodes)
    };
    //TODO refactor it. like :
    //let func = match first { Node::Identifier(func) => func, _ => runtime_error!}
    match first {
        Node::Identifier(func) => {
            //match func as &str {
            match func.as_ref(){
            //match &(func[..]) {
            //match &func[..] {
                "define" => {
                    if nodes.len() !=3 {
                        runtime_error!("Must supply exactly two arguments to define: {:?}", nodes);
                    }
                    let name = match others.first().unwrap() {
                        Node::Identifier(x) => x,
                        _ => runtime_error!("Unexpected node for name in define {:?}", nodes)};
                    let val = evaluate_node(others.last().unwrap(), env.clone())?;
                    env.borrow_mut().set(name.clone(),val);
                    Ok(null!())
                },
                "lambda" => {
                    if nodes.len() < 3 {
                        runtime_error!("Must supply at least two arguments to lambda: {:?}", nodes)
                    }
                    let args = match nodes.get(1).unwrap(){
                        Node::List(list) => {
                            let mut names = Vec::new();
                            for item in list.iter(){
                                match item {
                                    Node::Identifier(s) => names.push(s.clone()),
                                    _ => runtime_error!("Unexpected argument  in lambda arguments: {:?}", item)
                                };
                            }
                            names
                        }
                        _ => runtime_error!("Unexpected node for arguments in lambda: {:?}", nodes)
                    };
                    let expressions = nodes[2..].to_vec();
                    Ok(Value::Procedure(args, expressions))
                },
                "+" => {
                    if nodes.len() < 2 {
                        runtime_error!("Must supply at least two arguments to +: {:?}", nodes);
                    }
                    let mut sum = 0;
                    for n in others.iter() { 
                        let v = evaluate_node(n, env.clone())?;
                        match v {
                            Value::Integer(x) => sum += x,
                            _ => runtime_error!("Unexpected node during +: {:?}", n)
                        };
                    };
                    Ok(Value::Integer(sum))
                },
                "-" => {
                    if others.len() != 2 {
                        runtime_error!("Must supply exactly two arguments to -: {:?}", nodes);
                    }
                    let v1 = evaluate_node(others.first().unwrap(), env.clone())?;
                    let v2 = evaluate_node(others.last().unwrap(), env.clone())?;
                    let mut result = match v1 {
                        Value::Integer(x) => x,
                        _ => runtime_error!("Unexpected node during -: {:?}", nodes)
                    };
                    result -= match v2 {
                        Value::Integer(x) => x,
                        _ => runtime_error!("Unexpected node during -: {:?}", nodes)
                    };
                    Ok(Value::Integer(result))
                },
                _ => {
                    match env.borrow().get(func) {
                        Some(Value::Procedure(args, body)) => {
                            if nodes.len() != args.len() + 1 {
                                runtime_error!("Must supply exactly {} arguments to {}: {:?}", args.len(), func, nodes);
                            }
                            //create a new child enviornment for the procedure and define the arguments as local variables
                            let proc_env = Environment::new_child(env.clone());
                            for (arg, node) in args.iter().zip(nodes[1..].iter()) { //TODO use nodes split 
                                let val = evaluate_node(node, env.clone())?;
                                proc_env.borrow_mut().set(arg.clone(), val);
                            }

                            Ok(evaluate_nodes(&body, proc_env)?)
                        },
                        Some(other) => runtime_error!("Can't execute a non-procedure: {}", other),
                        None => runtime_error!("Unknown function: {}",func)
                    }
                }
            }
        },
        _ => {
            runtime_error!("First element in an expression must be an identifier: {:?}", first);
        }
    }
}

#[test]
fn test_global_variables() {
    assert_eq!(interpret(&vec![Node::List(vec![Node::Identifier("define".to_string()), Node::Identifier("x".to_string()), Node::Integer(2)]), Node::List(vec![Node::Identifier("+".to_string()), Node::Identifier("x".to_string()), Node::Identifier("x".to_string()), Node::Identifier("x".to_string())])]).unwrap(),
               Value::Integer(6));
}

#[test]
fn test_global_function_definition() {
    assert_eq!(interpret(&vec![Node::List(vec![Node::Identifier("define".to_string()), Node::Identifier("double".to_string()), Node::List(vec![Node::Identifier("lambda".to_string()), Node::List(vec![Node::Identifier("x".to_string())]), Node::List(vec![Node::Identifier("+".to_string()), Node::Identifier("x".to_string()), Node::Identifier("x".to_string())])])]), Node::List(vec![Node::Identifier("double".to_string()), Node::Integer(8)])]).unwrap(),
               Value::Integer(16));
}

