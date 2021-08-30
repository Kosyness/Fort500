use std::collections::{BTreeMap, HashMap};

use lexer::token::{Identifier, Keyword, Token, Word};
use parser::{BasicVarDeclaration, Declaration, DeclarationType, Program, VarDeclaration};

use log::{ trace, debug, info, error, warn, };

use lexer::Lexer;
use parser::cursor::Cursor;

pub struct Runtime {
    program: Program,

    global: Global,
}


impl Runtime { 
    fn new(program: Program) -> Self { 
        Self { 
            program,
            global: Default::default(),
        }
    }

    fn run(&mut self) { 
        let declarations = self.program.body.declarations.clone();

        for declaration in declarations.0 { 
            match declaration { 
                Declaration::Basic(BasicVarDeclaration { declaration_type, vars}) => for var in &vars.0 { 
                    self.add_global_variable(&declaration_type, var);
                }
                _ => todo!()
            }
        }
    }

    fn add_global_variable(&mut self, variable_type: &DeclarationType, variable: &VarDeclaration) { 
        trace!("Adding global variable {:?} of type {:?}", variable, variable_type);
        match variable_type.0.token { 
            Token::Word(Word::Keyword(Keyword::Integer)) => { 
            },
            _ => todo!("Currently only supporting integers")
        }
    }

    fn add_global_variable_single(&mut self, variable_type: &DeclarationType, variable: &VarDeclaration) {
        
    }
}

pub type Global = HashMap<String, Variable>;

pub enum Variable {
    Integer(IntVariable),
}

pub struct IntVariable { 
    identifier: Identifier,

    value: i64,
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
    warn!("Hello");
    let mut cursor = to_cursor("integer x, y, z");

    let program = Program::parse(&mut cursor).unwrap();

    let mut runtime = Runtime::new(program);

    runtime.run();
}