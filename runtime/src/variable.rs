

use std::fmt::{Debug, Display};

use crate::{EvalResult, Runtime, errors::Error};

use colored::Colorize;
pub use truthy::Truthy;

#[derive(Debug, Clone)]
pub enum Variable { 
    Array(),
    Value(Value),
    Null,
    Undefined,
    Result(Result<Box<Variable>, String>),
}

impl Display for Variable { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { 
            Self::Array() => todo!(),
            Self::Null => write!(f, "null"),
            Self::Undefined => write!(f, "undefined"),
            Self::Value(v) => write!(f, "{}", v),
            Self::Result(r) => match r { 
                Ok(r) => write!(f, "{}", r),
                Err(e) => write!(f, "err: {}", e)
            }
        }
    }
}

impl Display for Value { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { 
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(b) => write!(f, "{}", b),
            Value::Integer(b) => write!(f, "{}", b),
            Value::Float(b) => write!(f, "{}", b),
            Value::Char(b) => write!(f, "{}", b.0),
            Value::Function(b) => write!(f, "{}", b),
            Value::Structure() => write!(f, "struct {{}}"),
        }
    }
}

impl Display for FunctionValue { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { 
            FunctionValue::Native(_) => write!(f, "function() {{ {} }}", "[native code]".italic())
        }
    }
}

impl Truthy for Variable { 
    
    fn truthy(&self) -> bool {
        match self { 
            Variable::Array() => todo!(),
            Variable::Value(v) => v.truthy(),
            Variable::Null => false,
            Variable::Undefined => false,
            Variable::Result(r) => match r { 
                Ok(_) => true,
                Err(_) => false
            }
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
    Function(FunctionValue),
    String(String),
}

pub type FunctionResult = Result<Option<Variable>, Error>;
pub type CallableFunction = fn(&mut Runtime, Vec<Variable>) -> FunctionResult;

#[derive(Clone)]
pub enum FunctionValue { 
    Native(CallableFunction)
}

impl Debug for FunctionValue { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function() {{ [Native Function] }}")
    }
}

#[derive(Debug, Clone)]
pub struct StringValue(pub String);

impl Truthy for StringValue{ 
    fn truthy(&self) -> bool {
        !self.0.is_empty() 
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
            Self::String(s) => !s.is_empty(),
            Self::Function(_) => true,
            Self::Structure() => true,
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