use colored::*;
use parser::{Program, cursor::Cursor};
use read_input::prelude::*;
use runtime::{Runtime, variable::{Value, Variable}};

fn print_help() {
    println!(
        "{}",
        r#"Type "--" in order to enter multiline mode, and then "--" to evaluate the expression"#
            .blue()
            .bold()
    );
}

fn main() {
    let _ = env_logger::try_init();
    let mut runtime = Runtime::new();

    runtime.add_function("clear".into(), |_, _ | { 
        println!("Calling native function");
        Ok(None)
    });

    runtime.add_function("ReadFileToString".into(), |_, input| { 
        let path = match input[0].clone() { 
            Variable::Value(Value::String(s)) => s,
            e => return Err(runtime::errors::Error::TypeError(format!("Expected String, got {:?}", e)))
        };

        let data = std::fs::read_to_string(path).unwrap();
        Ok(Some(Variable::Value(Value::String(data))))
    });

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
