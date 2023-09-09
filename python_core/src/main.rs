
use python_runtime_engine::parser::lexical_analyzer::lexer;

fn main() {
    println!("Hello, world!");
    let source = "Test5".to_string();
    let res = lexer(&source);
    match res {
        Ok(x) => println!("Ok!"),
        Err(e) =>   println!("Error!")
    }

}
