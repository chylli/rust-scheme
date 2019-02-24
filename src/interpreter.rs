use crate::parser::Node;

use std::fmt;

pub fn interpret(nodes: &Vec<Node>) -> Result<Vec<Value>, RuntimeError> {
    evaluate_nodes(nodes)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
}

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

fn evaluate_nodes(nodes: &Vec<Node>) -> Result<Vec<Value>, RuntimeError> {
    let mut results = Vec::new();
    for node in nodes.iter() {
        let res = evaluate_node(node)?;
        results.push(res);
    };
    Ok(results)
}

fn evaluate_node(node: &Node) -> Result<Value, RuntimeError> {
    match node {
        Node::Integer(v) => Ok(Value::Integer(*v)),
        Node::Boolean(v) => Ok(Value::Boolean(*v)),
        Node::String(v) => Ok(Value::String(v.clone())),
        Node::List(vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec)
            } else {
                Ok(Value::List(Vec::new()))
            }
        },
        _ => runtime_error!("Can't evaluate node: {:?}", node)
    }
}

fn evaluate_expression(nodes: &Vec<Node>) -> Result<Value, RuntimeError> {
    let (first, others) = match nodes.split_first() {
        Some(v) => v,
        None => runtime_error!("Can't evaluate an empty expression: {:?}", nodes)
    };
    //TODO refactor it. like :
    //let func = match first { Node::Identifier(func) => func, _ => runtime_error!}
    match first {
        Node::Identifier(func) => {
            match func.as_ref() {
                "+" => {
                    if nodes.len() < 2 {
                        runtime_error!("Must supply at least two arguments to +: {:?}", nodes);
                    }
                    let mut sum = 0;
                    for n in others.iter() { 
                        let v = evaluate_node(n)?;
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
                    let v1 = evaluate_node(others.first().unwrap())?;
                    let v2 = evaluate_node(others.last().unwrap())?;
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
