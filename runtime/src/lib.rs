use std::{fmt::Debug, net::{TcpListener, TcpStream}, sync::{Arc, Mutex}};

use colored::*;
use log_derive::{ logfn, logfn_inputs };
use log::{ error, warn, info, trace };



use lexer::{
    token::{Identifier, Token, Word},
    Lexer, Span, TokenError, TokenSpan,
};
use parser::{Assignment, BasicVarDeclaration, Body, Constant, Declaration, DeclarationType, Declarations, Expression, FunctionCall, ID, IfStatement, IoStatement, LabeledStatement, ParseError, Program, Statement, StatementList, VarDeclaration, Variable as ParsedVariable, WriteItem, WriteList, cursor::Cursor};
use scope::ScopeManager;
use truthy::Truthy;

pub mod scope;
pub mod variable;

use anyhow::{bail, Chain, Context, Error as AnyError};

use serde::{Deserialize, Serialize};
use variable::{CallableFunction, Value, Variable};

use crate::variable::FunctionValue;

#[derive(Default, Debug)]
pub struct Runtime {
    scopes: ScopeManager,
}

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

impl Runtime {
    
    
    #[logfn(Trace)]
    pub fn eval(&mut self, input: String) -> RuntimeResult {
        let mut cursor = match Cursor::from_str(input.clone()) {
            Ok(c) => c,
            Err(e) => return Err(RuntimeError::LexerError(e)),
        };

        let program = match Program::parse(&mut cursor) {
            Ok(p) => p,
            Err(e) => return Err(RuntimeError::ParseError(e)),
        };

        self.eval_program(program)
    }
    
    
    #[logfn(Trace)]
    pub fn eval_program(&mut self, program: Program) -> RuntimeResult {
        self.handle_program(program)?;
        Ok(Default::default())
    }

    
    
    #[logfn(Trace)]
    pub fn eval_expression(&mut self, expression: &Expression) -> RuntimeResult<Arc<Mutex<Variable>>> {
        Ok(Arc::new(Mutex::new(Variable::new(
            None,
            Arc::new(Mutex::new(match expression {
                Expression::Constant(c) => match c {
                    Constant::Boolean(b) => Value::Boolean(match b.token {
                        Token::Boolean(b) => b,
                        _ => unreachable!(),
                    }),
                    Constant::Character(c) => Value::String(c.token.value()),
                    Constant::Integer(i) => Value::Integer(match i.token {
                        Token::Integer(i) => i,
                        _ => unreachable!(),
                    }),
                    Constant::Real(f) => Value::Float(match f.token {
                        Token::Float(f) => f,
                        _ => unreachable!(),
                    }),
                },
                Expression::Variable(ParsedVariable::Readable(c)) => {
                    return match self.scopes.get(c.0.token.value()) {
                        Some(v) => return Ok(v),
                        None => {
                            return Err(RuntimeError::ReferenceError(format!(
                                "{} is not defined",
                                c.0.token.value()
                            )))
                        }
                    }
                },
                Expression::String(s) => Value::String(s.token.value()),
                Expression::Function(f) => return self.handle_function_call(&f),
                Expression::Variable(ParsedVariable::Callable(f)) => return self.handle_function_call(&f),
                e => todo!("Add support in Runtime for expression: {:?}", e),
            })),
        ))))
    }

    
    
    #[logfn(Trace)]
    pub fn add_function<S: ToString + Debug>(&mut self, identifier: S, function: CallableFunction) -> RuntimeResult { 
        self.scopes.set(Variable::new(Some(identifier.to_string()), Arc::new(Mutex::new(Value::Function(FunctionValue::Native(function))))));
        Ok(())
    }

    
    
    #[logfn(Trace)]
    fn handle_function_call(&mut self, function: &FunctionCall) -> RuntimeResult<Arc<Mutex<Variable>>> { 
        let FunctionCall {
            identifier: ID(TokenSpan { token, .. }),
            ..
        } = function;

        let stored_variable = match self.scopes.get(token.value()) {
            Some(s) => s,
            None => todo!(),
        };

        let mut vars = vec![];

        for expr in function.variables.0.clone() { 
            let result = self.eval_expression(&expr)?;

            vars.push(result);
        }

        let variable = stored_variable.clone();
        let variable = variable.lock().unwrap();
        let variable = &*variable;

        let value = variable.value.clone();
        let value = value.lock().unwrap();
        let value = &*value;

        match value { 
            Value::Function(FunctionValue::Native(f)) => match f(self, vars)? { 
                None => Ok(Arc::new(Mutex::new(Variable::new(None, Arc::new(Mutex::new(Value::Undefined)))))),
                Some(v) => Ok(Arc::new(Mutex::new(v))),
            },
            _ => todo!()
        }
    }

    
    
    #[logfn(Trace)]
    fn handle_statements(&mut self, statements: &StatementList) -> RuntimeResult {
        for statement in &statements.0 {
            self.handle_labeled_statement(statement);
        }

        Ok(())
    }

    
    
    #[logfn(Trace)]
    fn handle_labeled_statement(&mut self, statement: &LabeledStatement) -> RuntimeResult {
        self.handle_statement(&statement.statement);
        Ok(())
    }

    
    #[logfn(Trace)]
    fn handle_statement(&mut self, statement: &Statement) -> RuntimeResult {
        match statement {
            Statement::Io(IoStatement::Write(write)) => self.handle_write_list(write),
            Statement::If(if_statement) => self.handle_if_statement(if_statement),
            Statement::Assignment(assignment) => self.handle_assignments(assignment),
            Statement::FunctionCall(f) => {
                match self.handle_function_call(f) {
                    Ok(_) => Ok(()),
                    Err(e) => return Err(e)
                }
            }
            c => todo!("Add support for statement {:?}", c),
        }
    }

    
    
    #[logfn(Trace)]
    fn handle_if_statement(&mut self, statement: &IfStatement) -> RuntimeResult {
        let var = self.eval_expression(&statement.expression)?;

        let var = var.lock().unwrap();
        if (&*var).truthy() {
            self.handle_body(&statement.body);
        } else if let Some(_else_body) = &statement.tail {
            todo!("Handle Else Statement");
        } else {
            // do nothing
        }

        Ok(())
    }

    
    #[logfn(Trace)]
    fn handle_write_list(&mut self, list: &WriteList) -> RuntimeResult {
        for item in &list.0 {
            self.handle_write_item(item)?;
        }

        Ok(())
    }

    
    
    #[logfn(Trace)]
    fn handle_write_item(&mut self, item: &WriteItem) -> RuntimeResult {
        match item {
            WriteItem::String(string) => {
                trace!(target: "runtime::write_item", "Printing a String to the Display '{}'", string.magenta().bold());

                println!("{}  {}", "->".green(), string)
            },
            WriteItem::Expression(expression) => { 
                let result = self.eval_expression(&expression)?;
                let result = result.lock().unwrap();

                println!("{}  {}", "->".green(), result);
            }
            c => todo!("Add suppot for {:?}", c),
        }

        Ok(())
    }

    
    
    #[logfn(Trace)]
    fn handle_assignments(&mut self, assignment: &Assignment) -> RuntimeResult {
        let id = match assignment.variable.clone() { 
            ParsedVariable::Callable(c) => unreachable!(),
            ParsedVariable::Readable(r) => r.clone()
        }.0.token;

        let expression = &assignment.expression;
        let var = self.eval_expression(expression)?;
        let var = var.lock().unwrap();
        let value = var.value.clone();

        drop(var);

        let id = if let Token::Word(Word::Identifier(id)) = id {
            id
        } else {
            unreachable!();
        };

        self.scopes.set(Variable::new(Some(id.0), value)).unwrap();

        Ok(())
    }

    
    #[logfn(Trace)]
    fn handle_program(&mut self, program: Program) -> RuntimeResult {
        self.handle_body(&program.body)
    }

    
    
    #[logfn(Trace)]
    fn handle_body(&mut self, body: &Body) -> RuntimeResult {
        self.handle_declarations(&body.declarations)?;
        self.handle_statements(&body.statements)
    }

    
    #[logfn(Trace)]
    fn handle_declarations(&mut self, declarations: &Declarations) -> RuntimeResult {
        for declaration in &declarations.0 {
            self.handle_declaration(declaration)?;
        }

        Ok(Default::default())
    }

    
    
    #[logfn(Trace)]
    fn handle_declaration(&mut self, declaration: &Declaration) -> RuntimeResult {
        match declaration {
            Declaration::Basic(b) => self.handle_basic_declaration(b)?,
            _ => todo!(),
        }
        Ok(Default::default())
    }

    
    
    #[logfn(Trace)]
    fn handle_basic_declaration(&mut self, declaration: &BasicVarDeclaration) -> RuntimeResult {
        let BasicVarDeclaration {
            declaration_type: _,
            vars,
        } = declaration;

        for var in &vars.0 {
            match var {
                VarDeclaration::Single(s) => self
                    .scopes
                    .set(Variable::new(
                        Some(s.0.token.value()),
                        Arc::new(Mutex::new(Value::Undefined)),
                    ))
                    .expect("Add Error checking for SetError"),
                VarDeclaration::Array { id, dimensions } => todo!(),
            }
        }

        Ok(Default::default())
    }
}

#[test]
fn test_runtime() {
    let _ = env_logger::try_init();
    let mut runtime = Runtime::default();

}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RuntimeError {
    RuntimeError(String, String),
    VariableAssignmentError(String),
    TypeError(String),
    ReferenceError(String),
    DeclarationError(String, String),
    LexerError(TokenError),
    ParseError(ParseError),
}
