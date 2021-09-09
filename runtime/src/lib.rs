use std::fmt::Debug;

use errors::Error;

use parser::{
    Assignment, Body, Constant, Declarations, Expression, FunctionCall, IfStatement, IoStatement,
    LabeledStatement, Program, Statement, StatementList, Variable as ParsedVariable, WriteItem,
    WriteList, ID,
};

use log::{error, info, trace, warn};
use log_derive::logfn_inputs;

use colored::*;

use lexer::{
    token::{Identifier, Token, Word},
    Lexer, TokenSpan,
};
use parser::cursor::Cursor;
use scope::ScopeManager;
use truthy::Truthy;
use variable::{CallableFunction, FunctionResult, FunctionValue, Value, Variable};

pub type EvalResult<T> = Result<T, Error>;

pub mod errors;
pub mod scope;
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

    pub fn add_function(&mut self, identifier: Identifier, function: CallableFunction) {
        self.global.set_global(
            identifier,
            Variable::Value(Value::Function(FunctionValue::Native(function))),
        )
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
            Expression::Constant(c) => Variable::Value(match c {
                Constant::Integer(i) => Value::Integer(i.token.value().parse().unwrap()),
                Constant::Real(r) => Value::Float(r.token.value().parse().unwrap()),
                Constant::Boolean(b) => Value::Boolean(match b.token {
                    Token::Boolean(b) => b,
                    _ => unreachable!(),
                }),
                Constant::Character(c) => Value::Char(c.token.value().into()),
            }),
            Expression::Variable(ParsedVariable::Readable(var)) => match var.clone().0.token {
                Token::Word(Word::Identifier(id)) => self.global.get(id).unwrap(),
                _ => unreachable!(),
            },
            Expression::Variable(ParsedVariable::Callable(function_call)) => match self.handle_function_call(function_call) { 
                Ok(var) => match var { 
                    Some(v) => v,
                    None => Variable::Undefined
                },
                Err(e) => {
                    error!("RuntimeError: called function threw an exception: {:?}", e);
                    Variable::Undefined
                }
            }
            Expression::Function(f) => match self.handle_function_call(f) { 
                Ok(r) => match r {
                    Some(s) => s,
                    None => Variable::Undefined
                },
                Err(e) => {
                    error!("FunctionCallError: ${:?}", e);
                    Variable::Undefined
                }
            }
            Expression::String(s) => Variable::Value(Value::String(s.token.value())),
            Expression::Empty => {
                warn!("Empty Expression");
                Variable::Undefined
            }
            e => todo!("Implement Expression Handling for {:?}", e),
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
            Statement::Io(IoStatement::Write(write)) => self.handle_write_list(write),
            Statement::If(if_statement) => self.handle_if_statement(if_statement),
            Statement::Assignment(assignment) => self.handle_assignments(assignment),
            Statement::FunctionCall(f) => {
                match self.handle_function_call(f) {
                    _ => {} // Err(e) => //println!("{}", e.n)
                }
            }
            c => todo!("Add support for statement {:?}", c),
        }
    }

    fn handle_function_call(&mut self, function_call: &FunctionCall) -> FunctionResult {
        let FunctionCall {
            identifier: ID(TokenSpan { token, .. }),
            ..
        } = function_call;

        let stored_variable = match self.global.get(token.value().into()) {
            Ok(s) => s,
            Err(e) => return Err(e),
        };

        let mut vars = vec![];

        for expr in function_call.variables.0.clone() { 
            let result = self.eval_expression(&expr);

            vars.push(result);
        }

        match stored_variable {
            Variable::Value(Value::Function(FunctionValue::Native(native))) => {
                return native(self, vars)
            }
            // Variable::Value(Value::Function(FunctionValue::Native(native))) => {

            // }
            e => {
                error!("Expected a Callable. Got {:?}", e);
                return Err(Error::TypeError(format!(
                    "Expected a Callable. Got {:?}",
                    e
                )))
            }
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

                println!("{}", string)
            },
            WriteItem::Expression(expression) => { 
                let result = self.eval_expression(&expression);

                println!("{}", result);
            }
            c => todo!("Add suppot for {:?}", c),
        }
    }

    fn handle_assignments(&mut self, assignment: &Assignment) {
        let id = match assignment.variable.clone() { 
            ParsedVariable::Callable(c) => unreachable!(),
            ParsedVariable::Readable(r) => r.clone()
        }.0.token;

        let expression = &assignment.expression;
        let value = self.eval_expression(expression);

        let id = if let Token::Word(Word::Identifier(id)) = id {
            id
        } else {
            unreachable!();
        };

        self.global.set(id, value).unwrap();
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
    let program = r#"integer x, y, z
    write "hello, world"
    if (.true.) then
        write "entering an if statement"
    endif
    if (.false.) then
        write "you can not enter in here"
    endif
    write "end of program"
    "#;
    info!("Running program {}", program.to_string().blue());
    let mut cursor = to_cursor(program);

    let program = Program::parse(&mut cursor).unwrap();

    let mut runtime = Runtime::new();

    runtime.eval_program(program);

    runtime.global.get("x".into());
}
