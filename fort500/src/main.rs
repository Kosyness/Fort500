use runtime::Runtime;
use read_input::prelude::*;
use colored::*;


fn main() {
    let _ = env_logger::try_init();
    let mut runtime = Runtime::new();

    loop { 
        print!("{} ", ">>>".blue().bold());
        let line = input::<String>().get();
        runtime.eval(line);
    }
}
