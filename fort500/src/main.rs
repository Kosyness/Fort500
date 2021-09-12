use std::{env, fs, process::exit, sync::{Arc, Mutex}};

use colored::*;
use parser::{Program, cursor::Cursor};
use read_input::prelude::*;
use runtime::{Runtime, variable::{Truthy, Value, Variable}, RuntimeError, RuntimeResult};
use log::{ error, warn, info, };

fn init_runtime(runtime: &mut Runtime) {
    runtime.add_function("print", |_, args| { 
        
        for arg in &args { 
            let arg = arg.clone();
            let arg = arg.lock().unwrap();
            print!("{}", &*arg);
        }

        Ok(None)
    });

    runtime.add_function("println", |_, args| { 
        for arg in &args { 
            let arg = arg.clone();
            let arg = arg.lock().unwrap();
            print!("{}", &*arg);
        }

        println!();

        Ok(None)
    });

    runtime.add_function("input", |_, args| { 
        let mut line = input::<String>().get();
        
        let line = line.trim_end_matches("\n");
        Ok(Some(Variable::with_value(None, Value::String(line.to_string()))))
    });

    runtime.add_function("clear", |_, _| { 
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
        Ok(None)
    });


    
}

fn main() { 
    let _ = env_logger::try_init();
    let mut runtime = Runtime::default();

    init_runtime(&mut runtime);

    let args: Vec<String> = env::args().collect();
    if args.len() >= 2 { 
        let input = &args[1];

        runtime.eval(fs::read_to_string(input).unwrap());

        return;
    }

    let mut multiline = false;
    let mut multilined_input = "".to_string();
    loop {
        match multiline {
            true => print!("{} ", ">>>".blue().bold()),
            false => print!("{}   ", ">".blue().bold()),
        };

        let line = input::<String>().get();
        multilined_input += format!("\n{}", line).as_str();

        let mut cursor = match Cursor::from_str(multilined_input.clone()) { 
            Ok(res) => res,
            Err(e) => {
                error!("SyntaxError: {:?}", e);
                multilined_input = "".into();
                multiline = false;
                continue;
            }
        };

        let program = match Program::parse(&mut cursor) { 
            Ok(program) => program,
            Err(parser::ParseError::Expected(_, _, None)) => { 
                multiline = true;
                continue;
            },
            Err(e) => {
                error!("SyntaxError: {:?}", e);
                multilined_input = "".into();
                continue;
            }
        };

        runtime.eval_program(program);
        multilined_input = "".into();
        multiline = false;

    }
}