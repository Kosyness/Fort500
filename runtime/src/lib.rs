use std::{fmt::Debug};

use errors::Error;

use parser::{Body, Constant, Declarations, Expression, IfStatement, IoStatement, LabeledStatement, Program, Statement, StatementList, WriteItem, WriteList};

use log::{ trace, info, };
use log_derive::{logfn_inputs};


use colored::*;

use lexer::{Lexer, token::Token};
use parser::cursor::Cursor;
use scope::{ScopeManager};
use truthy::Truthy;
use variable::{Value, Variable};

pub type EvalResult<T> = Result<T, Error>;

pub mod scope;
pub mod errors;
pub mod variable;

pub struct Runtime {
    global: ScopeManager,
}

impl Debug for Runtime { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime {{}}")
    }
}


impl Runtime { 
    pub fn new() -> Self { 
        Self { 
            global: ScopeManager::new(),
        }
    }

    pub fn eval(&mut self, input: String) { 
        let mut lexer = Lexer::new(input.chars().peekable());

        let tokens = lexer.lex().expect("Failed to evaluate a string");

        let mut cursor = Cursor::new(tokens);
        let program = Program::parse(&mut cursor).expect("Failed to parse program from input");

        self.eval_program(program);
    }

    #[logfn_inputs(Trace)]
    fn eval_program(&mut self, program: Program) {
        // TODO
        // First handle the subprograms in order to add the
        // Function declarations to the global scope
        self.handle_body(&program.body);
    }

    fn eval_expression(&mut self, expression: &Expression) -> Variable { 
        match expression { 
            Expression::Constant(c) => Variable::Value( match c { 
                Constant::Integer(i) => Value::Integer(i.token.value().parse().unwrap()),
                Constant::Real(r) => Value::Float(r.token.value().parse().unwrap()),
                Constant::Boolean(b) => Value::Boolean(match b.token { 
                    Token::Boolean(b) => b,
                    _ => unreachable!()
                }),
                Constant::Character(c) => Value::Char(c.token.value().into()),
            }),
            _ => todo!()
        }
    }

    #[logfn_inputs(Trace)]
    fn handle_body(&mut self, body: &Body) { 
        self.handle_declarations(&body.declarations);
        self.handle_statements(&body.statements);
    }

    #[logfn_inputs(Trace)]
    fn handle_declarations(&mut self, declarations: &Declarations) { 
        self.global.declarations(declarations);
    }

    #[logfn_inputs(Trace)]
    fn handle_statements(&mut self, statements: &StatementList) { 
        for statement in &statements.0 { 
            self.handle_labeled_statement(statement);
        }
    }

    #[logfn_inputs(Trace)]    
    fn handle_labeled_statement(&mut self, statement: &LabeledStatement) {
        self.handle_statement(&statement.statement);
    }

    #[logfn_inputs(Trace)]
    fn handle_statement(&mut self, statement: &Statement) { 
        match statement { 
            Statement::Io(IoStatement::Write(write)) => { 
                self.handle_write_list(write);
            }
            Statement::If(if_statement) => { 
                self.handle_if_statement(if_statement)
            }
            c => todo!("Add support for statement {:?}", c)
        }
    }

    fn handle_if_statement(&mut self, statement: &IfStatement) { 
        let var = self.eval_expression(&statement.expression);

        if var.truthy() { 
            self.handle_body(&statement.body);
        } else if let Some(_else_body) = &statement.tail { 
            todo!("Handle Else Statement");
        } else { 
            // do nothing
        }
    }

    #[logfn_inputs(Trace)]
    fn handle_write_list(&mut self, list: &WriteList) { 
        for item in &list.0 { 
            self.handle_write_item(item);
        }
    }

    #[logfn_inputs(Trace)]
    fn handle_write_item(&mut self, item: &WriteItem) { 
        match item { 
            WriteItem::String(string) => {
                trace!(target: "runtime::write_item", "Printing a String to the Display '{}'", string.magenta().bold());
                
                println!("{}", string)},
            c => todo!("Add suppot for {:?}", c)
        }
    }


}

#[cfg(test)]
fn to_cursor(input: &str) -> Cursor {
    let input = input.to_string();
    let mut lexer = Lexer::new(input.chars().peekable());
    let lex = lexer.lex().unwrap();

    
    Cursor::new(lex)
}

#[test]
fn basic_program() { 
    let _ = env_logger::try_init();
    let program = r#"if ( 123 ) then write "hello" endif"#;
    info!("Running program {}", program.to_string().blue());
    let mut cursor = to_cursor(program);

    let program = Program::parse(&mut cursor).unwrap();

    let mut runtime = Runtime::new();

    runtime.eval_program(program);

    runtime.global.get("x".into());
}