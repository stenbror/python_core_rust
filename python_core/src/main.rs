
use python_runtime_engine::parser::lexical_analyzer::is_reserved_keyword_or_name_from_buffer;
use python_runtime_engine::parser::source_buffer::*;

fn main() {
    println!("Hello, world!");
    
    let mut buffer = SourceBuffer::new();
    buffer.from_text("False");
        
    let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
    match res {
        Some(x) => {
            println!("Success!")
        },
        _ => println!("Failure!")
    }
}
