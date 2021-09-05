use parser::Body;

use crate::{EvalResult, errors::Error};

use truthy::Truthy;

#[derive(Debug, Clone)]
pub enum Variable { 
    Array(),
    Value(Value)
}

impl Truthy for Variable { 
    fn truthy(&self) -> bool {
        match self { 
            Variable::Array() => todo!(),
            Variable::Value(v) => v.truthy() 
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value { 
    Integer(i64),
    Float(f64),
    Char(StringValue),
    Boolean(bool),
    Structure(),
    Function()
}

#[derive(Debug, Clone)]
pub struct StringValue(pub String);

impl Truthy for StringValue{ 
    fn truthy(&self) -> bool {
        self.0.len() > 0 
    }
}

impl From<String> for StringValue { 
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for StringValue { 
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl Truthy for Value { 
    fn truthy(&self) -> bool {
        match self { 
            Self::Integer(i) => i.truthy(),
            Self::Float(f) => f.truthy(),
            Self::Char(c) => c.truthy(),
            Self::Boolean(b) => *b,
            _ => todo!()
        }
    }
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