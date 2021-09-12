use std::{any::Any, fmt::{Debug, Display}, sync::{Arc, Mutex}};
use hashbrown::HashMap;
use derive_new::new;
pub use truthy::Truthy;

use crate::{Runtime, RuntimeError};

#[derive(Debug, new)]
pub struct Variable { 
    pub id: Option<String>,
    pub value: Arc<Mutex<Value>>,
    #[new(default)]
    pub constant: bool,
}

impl Display for Variable { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = self.value.lock().unwrap();

        match &*value { 
            Value::String(s) => write!(f, "{}", s),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(i) => write!(f, "{}", i),
            Value::Float(i) => write!(f, "{}", i),
            Value::Undefined => write!(f, "undefined"),
            Value::Null => write!(f, "null"),
            _ => todo!()
        }
    }
}

impl Truthy for Variable { 
    fn truthy(&self) -> bool {
        let value = self.value.clone();
        let value = value.lock().unwrap();

        value.truthy()
    }
}

impl Default for Variable { 
    fn default() -> Self {
        Self { 
            value: Arc::new(Mutex::new(Value::Undefined)),
            id: None,
            constant: false
        }
    }
} 

impl Variable { 
    pub fn with_value(id: Option<String>, value: Value) -> Self { 
        Self::new(id, Arc::new(Mutex::new(value)))
    }
}

#[derive(Debug)]
pub enum Value { 
    Native(Box<dyn Any>),
    Array(Vec<Value>),
    Null,
    Undefined,
    String(String),
    Integer(i64),
    Boolean(bool),
    Float(f64),
    Map(HashMap<String, Box<Arc<Mutex<Value>>>>),
    Function(FunctionValue)
}

impl Truthy for Value{ 
    fn truthy(&self) -> bool {
        match self { 
            Value::Undefined => false,
            Value::Boolean(b) => *b,
            Value::Array(c) => !c.is_empty(),
            Value::Null => false,
            Value::Float(f) => *f != 0.0,
            Value::Integer(i) => *i != 0,
            Value::Function(f) => todo!(),
            Value::Native(n) => true,
            Value::Map(m) => !m.is_empty(),
            Value::String(s) => !s.is_empty()
        }
    }
}

impl Value { 
    fn parse_variable(&mut self, variable: &Variable) -> Value { 
        
    }
}

pub type FunctionResult = Result<Option<Variable>, RuntimeError>;
pub type CallableFunction = fn(&mut Runtime, Vec<Arc<Mutex<Variable>>>) -> FunctionResult;

pub enum FunctionValue { 
    Native(CallableFunction)
}

impl Debug for FunctionValue { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
        // match self { 
        //     // Self::Native(f) => f.typ
        // }
    }
}

#[test]
fn test_native_value() { 
    let mut variable = Variable::default();
    variable.value = Arc::new(Mutex::new(Value::Native(Box::new("Hello, World"))));

    println!("{:?}", variable);
}