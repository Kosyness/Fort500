use std::collections::{BTreeMap, HashMap};

use errors::Error;
use lexer::token::{Identifier, Keyword, Token, Word};
use parser::{BasicVarDeclaration, Body, Declaration, DeclarationType, Declarations, Dimension, Dimensions, Program, VarDeclaration};

use log::{ trace, debug, info, error, warn, };

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


impl Runtime { 
    fn new() -> Self { 
        Self { 
            global: ScopeManager::new(),
        }
    }

    fn eval_string(&mut self, input: String) { 
        let mut lexer = Lexer::new(input.chars().peekable());

        let tokens = lexer.lex().expect("Failed to evaluate a string");

        let mut cursor = Cursor::new(tokens);
        let program = Program::parse(&mut cursor).expect("Failed to parse program from input");

        self.eval_program(program);
    }

    fn eval_program(&mut self, program: Program) {
        self.handle_body(&program.body);
    }

    fn handle_body(&mut self, body: &Body) { 
        self.handle_declarations(&body.declarations);
    }

    fn handle_declarations(&mut self, declarations: &Declarations) { 
        self.global.declarations(declarations);
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
    let program = "integer x, y, z";
    warn!("Running program {}", program.to_string().blue());
    let mut cursor = to_cursor(program);

    let program = Program::parse(&mut cursor).unwrap();

    let mut runtime = Runtime::new();

    runtime.eval_program(program);

    runtime.global.get("x".into());
}