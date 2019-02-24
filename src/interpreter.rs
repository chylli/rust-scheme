use crate::parser::Node;

use std::fmt;
use std::collections::HashMap;

pub fn interpret(nodes: &Vec<Node>) -> Result<Value, RuntimeError> {
    let mut env = Environment::root();
    evaluate_nodes(nodes, &mut env)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
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
    values: HashMap<String, Value>,
}

impl Environment {
    fn root() -> Environment {
        Environment { values: HashMap::new()}
    }

    fn set(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    fn get(&self, key: &String) -> Option<Value> {
        match self.values.get(key) {
            Some(val) => Some(val.clone()),
            None => None
        }
    }
}

fn evaluate_nodes(nodes: &Vec<Node>, env: &mut Environment) -> Result<Value, RuntimeError> {
    let mut result = null!();
    for node in nodes.iter() {
        result = evaluate_node(node, env)?;
    };
    Ok(result)
}

fn evaluate_node(node: &Node, env: &mut Environment) -> Result<Value, RuntimeError> {
    match node {
        Node::Identifier(v) => {
            match env.get(v) {
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

fn evaluate_expression(nodes: &Vec<Node>, env: &mut Environment) -> Result<Value, RuntimeError> {
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
                    let val = evaluate_node(others.last().unwrap(), env)?;
                    env.set(name.clone(),val);
                    Ok(null!()) // TODO change to more sensible return value
                },
                "+" => {
                    if nodes.len() < 2 {
                        runtime_error!("Must supply at least two arguments to +: {:?}", nodes);
                    }
                    let mut sum = 0;
                    for n in others.iter() { 
                        let v = evaluate_node(n, env)?;
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
                    let v1 = evaluate_node(others.first().unwrap(), env)?;
                    let v2 = evaluate_node(others.last().unwrap(), env)?;
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
                    runtime_error!("Unknown function: {}", func);
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
