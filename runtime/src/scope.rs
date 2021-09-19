use std::{borrow::Borrow, cell::RefCell, rc::Rc, sync::{Arc, Mutex}};


use hashbrown::HashMap;

use crate::variable::{Value, Variable};
use log::{ trace, debug, warn, info, error };
// use anyhow::{ Result, bail, Context, };

#[derive(Debug, Clone,)]
pub enum SetError {
    VariableIsConstant,
    UnnamedVariable,
}

#[derive(Debug)]
pub struct ScopeManager { 
    scopes: Vec<Scope>,
}

pub type Scope = HashMap<String, Arc<Mutex<Variable>>>;

impl Default for ScopeManager { 
    fn default() -> Self {
        Self { 
            scopes: vec![HashMap::default()]
        }
    }
}

impl ScopeManager {
    pub fn get(&self, identifier: String) -> Option<Arc<Mutex<Variable>>> {
        for scope in self.scopes.iter().rev() { 
            if !scope.contains_key(&identifier) { 
                continue;
            }

            let var = scope.get(&identifier);
            let var = var.unwrap().clone();
            return Some(var)
        }
        
        None
    }

    pub fn set(&mut self, variable: Variable) -> Result<(), SetError> { 
        let identifier = match variable.id.clone() {
            Some(s) => s,
            None => return Err(SetError::UnnamedVariable)
        };

        
        for (index, scope) in self.scopes.iter_mut().enumerate().rev() { 
            if scope.contains_key(&identifier) { 
                trace!("Setting variable {} to scope {}", identifier, index);
                scope.insert(identifier.clone(), Arc::new(Mutex::new(variable)));
                return Ok(())
            }
        }
        
        self.scopes[0].insert(identifier.clone(), Arc::new(Mutex::new(variable)));

        Ok(())
    }
    

    pub(crate) fn set_with_mutex(&mut self, variable: Arc<Mutex<Variable>>) -> Result<(), SetError> { 
        let var = variable.clone();
        let var = var.lock().unwrap();
        
        let identifier = match var.id.clone() {
            Some(s) => s,
            None => return Err(SetError::UnnamedVariable)
        };

        drop(var);
        
        for (index, scope) in self.scopes.iter_mut().enumerate().rev() { 
            if scope.contains_key(&identifier) { 
                trace!("Setting variable {} to scope {}", identifier, index);
                scope.insert(identifier.clone(), variable);
                return Ok(())
            }
        }
        
        self.scopes[0].insert(identifier.clone(), variable);

        Ok(())
    }

    pub fn pop(&mut self) -> Option<HashMap<String, Arc<Mutex<Variable>>>> { 
        self.scopes.pop()
    }

    pub fn create(&mut self) { 
        self.scopes.push(Default::default())
    }
}

#[test]
fn test_scope_manager() { 
    let mut scopes = ScopeManager::default();

    // scopes.set(Variable::new(Some("hello".to_string()), Value::Undefined));

    // println!("{:?}", scopes);
}

