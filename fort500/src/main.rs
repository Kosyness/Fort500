use std::{io::{self, BufRead}, process::exit};

use read_input::prelude::*;
use runtime::Runtime;

fn main() {
    let _ = env_logger::try_init();
    let mut runtime = Runtime::new();

    let mut program_str = "".to_string();

    loop { 
        let line = input::<String>().get();
        // if line != "eval" {
        //     program_str += line.as_str();
        // } else if line == "shutdown" { 
        //     exit(0);
        // } else {
            runtime.eval_string(line);
    
            program_str = "".to_string();
        // }
    }
}
