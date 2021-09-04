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

use crate::variable::{Value, Variable};

use colored::*;

pub struct ScopeManager {
    // pub scopes: Vec<Arc<Mutex<Scope>>>
    pub scopes: Vec<BTreeMap<String, Variable>>,
    length: usize,
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

    pub fn get(&mut self, identifier: Identifier) -> Option<Variable> {
        for scope in self.scopes.iter().rev() {
            match scope.get(&identifier.0) {
                Some(var) => {
                    let var = var.to_owned();

                    trace!(
                        "Found variable {}: {:?}",
                        identifier.0.magenta().bold(),
                        var
                    );
                    return Some(var);
                }
                None => continue,
            }
        }

        None
    }

    pub fn set(&mut self, identifier: Identifier, var: Variable) {
        self.scopes[self.length - 1].insert(identifier.0, var);
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
                        self.set_global(id, Variable::Value(Value::Integer(0)));
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}
