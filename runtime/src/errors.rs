use serde::{ Serialize, Deserialize };

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Error { 
    RuntimeError(String, String),
    VariableAssignmentError(String,),
    ReferenceError(String,),
    DeclarationError(String, String)
}