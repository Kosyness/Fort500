use colored::*;
use parser::{Program, cursor::Cursor};
use read_input::prelude::*;
use runtime::{Runtime, variable::{Truthy, Value, Variable}};
use log::{ error, warn, info, };

fn print_help() {
    println!(
        "{}",
        r#"Type "--" in order to enter multiline mode, and then "--" to evaluate the expression"#
            .blue()
            .bold()
    );
}


fn init_runtime(runtime: &mut Runtime) { 
    runtime.add_function("Clear".into(), |_, _| { 
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
        Ok(None)
    });

    runtime.add_function("FileToString".into(), |_, args| {
        let path = match args[0].clone() { 
            Variable::Value(Value::String(s)) => s,
            e => return Err(runtime::errors::Error::TypeError(format!("Expected String, got {:?}", e)))
        };

        match std::fs::read_to_string(path) { 
            Ok(s) => Ok(Some(Variable::Value(Value::String(s)))),
            Err(_) => Ok(Some(Variable::Result(Err("FileNotFound".into()))))
        }
    });

    runtime.add_function("StringToFile".into(), |_, args| {
        if args.len() != 2 { 
            return Ok(Some(Variable::Result(Err("NotEnoughArguments".into()))));
        }
        
        let path = match args[0].clone() { 
            Variable::Value(Value::String(s)) => s,
            e => return Err(runtime::errors::Error::TypeError(format!("Expected String, got {:?}", e)))
        };

        let data = match args[1].clone() { 
            Variable::Value(Value::String(s)) => s,
            e => return Err(runtime::errors::Error::TypeError(format!("Expected String, got {:?}", e)))
        };

        std::fs::write(path, data).unwrap();

        Ok(Some(Variable::Value(Value::Boolean(true))))
    });

    runtime.add_function("IsError".into(), |_, args| {  
        for arg in &args { 
            if !arg.truthy() { 
                return Ok(Some(Variable::Value(Value::Boolean(true))))
            }
        }

        Ok(Some(Variable::Value(Value::Boolean(false))))
    });

    runtime.add_function("IsOk".into(), |_, args| {  
        for arg in &args { 
            if !arg.truthy() { 
                return Ok(Some(Variable::Value(Value::Boolean(false))))
            }
        }

        Ok(Some(Variable::Value(Value::Boolean(true))))
    });

    runtime.add_function("Unwrap".into(), |_ , args| { 
        match args[0].clone() { 
            Variable::Result(r) => match r { 
                Ok(r) => Ok(Some(*r)),
                Err(e) => panic!("Error unwrapping an error value {}", e)
            },
            e => Ok(Some(e))
        }
    });

    runtime.add_function("UnwrapErr".into(), |_ , args| { 
        match args[0].clone() { 
            Variable::Result(r) => match r { 
                Err(r) => Ok(Some(Variable::Value(Value::String(r)))),
                Ok(e) => panic!("Tried to unwrap an error, got {}", e)
            },
            e => panic!("Tried to unwrap an error, got {}", e)
        }
    });

    runtime.add_function("Concat".into(), |_ , args| { 
        let mut output = "".to_string();

        for arg in &args { 
            output = format!("{}{}", output, arg)
        }

        Ok(Some(Variable::Value(Value::String(output))))
    })
}

fn main() {
    let _ = env_logger::try_init();
    let mut runtime = Runtime::new();

    init_runtime(&mut runtime);

    // runtime.eval("string myinput myinput = FileToString(\"n\") if ( IsError(myinput) ) then write \"hi\" endif".into());

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
        continue;

        let line = input::<String>().get();

        match line.as_str() {
            "?" => print_help(),
            "--" if !multiline => multiline = true,
            "--" if multiline => {
                multiline = false;
                // let mut cursor = Cursor::from_str(mulit)
                runtime.eval(multilined_input);
                multilined_input = "".to_string();
            }
            line if multiline => multilined_input += format!("\n{}", line).as_str(),
            line => runtime.eval(line.to_string()),
        }
    }
}

// use std::io::{self, BufRead};

// fn main() {
//         let _ = env_logger::try_init();
//     let mut runtime = Runtime::new();

//     let stdin = io::stdin();
//     print!("{} ", ">>>".blue().bold());
//     for line in stdin.lock().lines() {
//         let line = line.expect("Could not read line from standard in");
//         print!("{} ", ">>>".blue().bold());
//     }
// }
