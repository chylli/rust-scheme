use crate::parser::Node;

use std::fmt;

pub fn interpret(nodes: Vec<Node>) -> Result<Vec<Node>, RuntimeError> {
    evaluate_nodes(nodes)
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

fn evaluate_nodes(nodes: Vec<Node>) -> Result<Vec<Node>, RuntimeError> {
    let mut results = Vec::new();
    for node in nodes.into_iter() {
        let res = evaluate_node(node)?;
        results.push(res);
    };
    Ok(results)
}

fn evaluate_node(node: Node) -> Result<Node, RuntimeError> {
    match node {
        Node::List(vec) => {
            if vec.len() > 0 {
                let evaluated = evaluate_nodes(vec)?;
                evaluate_expression(evaluated)
            } else {
                Ok(Node::List(vec))
            }
        },
        _ => Ok(node)
    }
}

fn evaluate_expression(nodes: Vec<Node>) -> Result<Node, RuntimeError> {
    let (first, others) = match nodes.split_first() {
        Some(v) => v,
        None => runtime_error!("Can't evaluate an empty expression: {:?}", nodes)
    };
    //TODO refactor it. like :
    //let func = match first { Node::Identifier(func) => func, _ => runtime_error!}
    //match func.as_ref(){ "+"=> ...}
    match first {
        Node::Identifier(func) => {
            if *func == "+".to_string() {
                if nodes.len() < 2 {
                    runtime_error!("Must supply at least two arguments to +: {:?}", nodes);
                }
                let mut sum = 0;
                for n in others.iter() {
                    match *n {
                        Node::Integer(x) => sum += x,
                        _ => runtime_error!("Unexpected node during +: {:?}", n)
                    };
                };
                Ok(Node::Integer(sum))
            } else if *func == "-".to_string() {
                if others.len() != 2 {
                    runtime_error!("Must supply exactly two arguments to -: {:?}", nodes);
                }
                let mut result = match others.first() {
                    Some(Node::Integer(x)) => *x,
                    _ => runtime_error!("Unexpected node during -: {:?}", nodes)
                };
                result -= match others.last() {
                    Some(Node::Integer(x)) => *x,
                    _ => runtime_error!("Unexpected node during -: {:?}", nodes)
                };
                Ok(Node::Integer(result))
            } else {
                runtime_error!("Unknown function: {}", func);
            }
        },
        _ => {
            runtime_error!("First element in an expression must be an identifier: {:?}", first);
        }
    }
}
