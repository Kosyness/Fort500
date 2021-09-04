use std::{collections::{BTreeMap, HashMap}, fmt::Debug};

use errors::Error;
use lexer::token::{Identifier, Keyword, Token, Word};
use parser::{BasicVarDeclaration, Body, Declaration, DeclarationType, Declarations, Dimension, Dimensions, IoStatement, LabeledStatement, Program, Statement, StatementList, VarDeclaration, WriteItem, WriteList};

use log::{ trace, debug, info, error, warn, };
use log_derive::{logfn, logfn_inputs};


use colored::*;

use lexer::Lexer;
use parser::cursor::Cursor;
use scope::{ScopeManager};

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

    pub fn eval_string(&mut self, input: String) { 
        let mut lexer = Lexer::new(input.chars().peekable());

        let tokens = lexer.lex().expect("Failed to evaluate a string");

        let mut cursor = Cursor::new(tokens);
        let program = Program::parse(&mut cursor).expect("Failed to parse program from input");

        self.eval_program(program);
    }

    #[logfn_inputs(Trace)]
    fn eval_program(&mut self, program: Program) {
        info!("handling body");
        self.handle_body(&program.body);
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
            c => todo!("Add support for statement {:?}", c)
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

    let cursor = Cursor::new(lex);
    cursor
}

#[test]
fn basic_program() { 
    let _ = env_logger::try_init();
    let program = r#"integer x, y, z write "hello, world" "#;
    info!("Running program {}", program.to_string().blue());
    let mut cursor = to_cursor(program);

    let program = Program::parse(&mut cursor).unwrap();

    let mut runtime = Runtime::new();

    runtime.eval_program(program);

    runtime.global.get("x".into());
}