use crate::{EvalResult, errors::Error};

#[derive(Debug, Clone)]
pub enum Variable { 
    Array(),
    Value(Value)
}

#[derive(Debug, Clone)]
pub enum Value { 
    Integer(i64),
    Float(f64),
    Char(char),
    Boolean(bool),
    Structure()
}

impl Value { 
    fn add(&self, rhs: &Self) -> EvalResult<Self> { 
        match (self, rhs) { 
            (Value::Integer(lhs), Value::Integer(rhs)) =>{
                Ok(Value::Integer(lhs + rhs))
            }
            (Value::Float(lhs), Value::Float(rhs)) => { 
                Ok(Value::Float(lhs + rhs))
            }
            _ => Err(Error::RuntimeError("OperatorError".to_string(), format!("Incompatable types {:?} with {:?}", self, rhs.clone())))
        }
    }
}