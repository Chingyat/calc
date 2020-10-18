use calc::parse_and_eval_expr;
use std::io::{self, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    ctrlc::set_handler(move || {
        println!("received Ctrl+C!");
        std::process::exit(0);
    })
    .expect("Error setting Ctrl-C handler");

    let mut expr = String::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut counter = 0;
    loop {
        print!("IN  [{}]: ", counter);
        stdout.flush()?;
        stdin.read_line(&mut expr)?;
        if expr.trim().is_empty() {
            continue;
        }

        let result = parse_and_eval_expr(expr.as_str());
        match result {
            Ok(n) => {
                println!("OUT [{}]: {:?}", counter, n);
            }

            Err(e) => {
                eprintln!("OUT [{}]: {:?}", counter, e);
            }
        }
        expr.clear();
        counter += 1;
    }
}
