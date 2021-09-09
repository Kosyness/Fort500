use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer::Lexer;
use std::{env, process::exit, fs};

use colored::*;
use parser::{Program, cursor::Cursor};
use runtime::{Runtime, variable::{Truthy, Value, Variable}, errors::Error};
use log::{ error, warn, info, };

fn init_runtime(runtime: &mut Runtime) { 
    runtime.add_function("eq".into(), |_, args| { 
        if args.len() != 2 { 
            return Err(Error::RuntimeError("InvalidArgumentLenght".into(), "Expected 2 Arguments".into()))
        }

        Ok(Some(Variable::Value(Value::Boolean(args[0] == args[1]))))
    });

    runtime.add_function("print".into(), |_, args| { 
        
        for arg in &args { 
            // print!("{}", arg);
        }

        Ok(None)
    });

    runtime.add_function("println".into(), |_, args| { 
        for arg in &args { 
            // print!("{}", arg);
        }

        // println!();

        Ok(None)
    });

    runtime.add_function("input".into(), |_, args| { 
        // let mut line = input::<String>().get();
        
        // let line = line.trim_end_matches("\n");
        // Ok(Some(Variable::Value(Value::String(line.to_string()))))
        Ok(None)
    });

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

fn runtime_speed() {
    let input = r#"
    string name
integer age
string file_path
string output

print("What is your name?: ")

if ( eq("easter egg", name) ) then 
    println("You found the Easter Egg!")
endif

print("Welcome ", name, ". What is your age?: ")

println("So you are ", age, " years old, I see!")

print("Where would you like to store that information?: ")

output = Concat("Name: ", name, ", Age: ", age)
    "#;

    let mut cursor = Cursor::from_str(input.to_string()).unwrap();
    let parser = Program::parse(&mut cursor).unwrap();

    let mut runtime = Runtime::new();

    init_runtime(&mut runtime);
    runtime.eval_program(parser);
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("runtime speed", |b| b.iter(|| runtime_speed()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);