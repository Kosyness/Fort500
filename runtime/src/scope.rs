use std::{
    collections::{BTreeMap},
};

use lexer::{
    token::{Identifier, Keyword, Token, Word},
    TokenSpan,
};
use parser::{
    BasicVarDeclaration, Declaration, DeclarationType, Declarations, VarDeclaration, ID,
};

use log::{trace};

use crate::{errors::Error, variable::{Value, Variable}};

use colored::*;

pub struct ScopeManager {
    pub scopes: Vec<BTreeMap<String, Variable>>,
    pub length: usize,
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            scopes: vec![Default::default()],
            length: 1,
        }
    }

    pub fn declarations(&mut self, declarations: &Declarations) {
        for declaration in &declarations.0 {
            self.declaration(declaration);
        }
    }

    pub fn declaration(&mut self, declaration: &Declaration) {
        match declaration {
            Declaration::Basic(basic) => self.add_variables(basic),
            _ => todo!("Add support for adding structures"),
        }
    }

    pub fn get(&mut self, identifier: Identifier) -> Result<Variable, Error> {
        for scope in self.scopes.iter().rev() {
            match scope.get(&identifier.0) {
                Some(var) => {
                    let var = var.to_owned();

                    trace!(
                        "Found variable {}: {:?}",
                        identifier.0.magenta().bold(),
                        var
                    );
                    return Ok(var);
                }
                None => continue,
            }
        }

        Err(Error::ReferenceError(format!("{} variable is not defined", identifier.0)))
    }

    pub(crate) fn set_unchecked(&mut self, identifier: Identifier, var: Variable) { 
        let length = self.scopes.len();
        self.scopes[length - 1].insert(identifier.0, var);
    }

    pub fn set(&mut self, identifier: Identifier, var: Variable) -> Result<(), Error> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get(&identifier.0) {
                Some(_) => {
                    // Todo
                    // 
                    // Add Type Checking to see if the variable actually accepts that value
                    scope.insert(identifier.0, var);
                    return Ok(())
                }
                None => continue,
            }
        }

        Err(Error::VariableAssignmentError("Tried to assign a variable which was not declared".into()))
    }

    pub fn set_global(&mut self, identifier: Identifier, var: Variable) {
        self.scopes[0].insert(identifier.0, var);
    }

    pub(crate) fn add_variables(&mut self, variables: &BasicVarDeclaration) {
        for variable in &variables.vars.0 {
            self.add_variable(&variables.declaration_type, variable.clone());
        }
    }


    pub(crate) fn add_variable(&mut self, var_type: &DeclarationType, variable: VarDeclaration) {
        match variable {
            VarDeclaration::Single(ID(TokenSpan {
                token: Token::Word(Word::Identifier(id)),
                ..
            })) => {
                trace!(
                    "Adding a variable of type {}: {}",
                    var_type.0.token.clone().value().yellow(),
                    id.0.magenta().bold()
                );
                match var_type.0.token {
                    Token::Word(Word::Keyword(Keyword::Integer)) => {
                        self.set_unchecked(id, Variable::Value(Value::Integer(0)));
                    }
                    Token::Word(Word::Keyword(Keyword::String)) => {
                        self.set_unchecked(id, Variable::Value(Value::String("".into())));
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}
