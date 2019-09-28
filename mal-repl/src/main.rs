use rustyline::{error::ReadlineError, Editor};

use mal::{repl_parse, Err as ParseErr, Node};

#[derive(Clone, Debug)]
enum ReplError {
    Incomplete,
    Parse,
    Runtime,
}

#[derive(Clone, Debug, Default)]
struct ReplState {
    line_number: usize,
    lines: Vec<String>,
}

fn read(state: &mut ReplState, text: String) -> Result<Node, ReplError> {
    state.lines.push(text);

    let current_lines = state.lines.join("\n");

    dbg!(&current_lines);

    dbg!(repl_parse(&current_lines))
        .map(|expr| {
            state.lines.clear();

            expr
        })
        .map_err(|err| match err {
            ParseErr::Incomplete(_) => ReplError::Incomplete,
            ParseErr::Error(err) => {
                println!("{:#?}", err);

                ReplError::Parse
            }
            ParseErr::Failure(_) => ReplError::Runtime,
        })
        .map_err(|err| {
            match err {
                ReplError::Incomplete => (),
                _ => state.lines.clear(),
            };

            err
        })
}

fn eval() {
    unimplemented!()
}

fn print() {
    unimplemented!()
}

fn main() {
    let mut rl = Editor::<()>::new();
    let version = env!("CARGO_PKG_VERSION");

    println!("Make A Lisp v{} - Rust Edition", version);

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let mut state = ReplState::default();

    loop {
        let prefix = if state.lines.is_empty() {
            state.line_number += 1;
            format!("rmal({})> ", state.line_number)
        } else {
            format!("....({})> ", state.line_number)
        };

        match rl.readline(&prefix) {
            Ok(line) => {
                if !line.is_empty() {
                    rl.add_history_entry(line.as_str());
                }

                let next_line = line.trim_end().to_string();

                match read(&mut state, next_line) {
                    Ok(expr) => println!("{:?}", expr),
                    Err(err) => match err {
                        ReplError::Incomplete => (),
                        ReplError::Parse => println!("Parse error"),
                        ReplError::Runtime => println!("Whoops"),
                    },
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history("history.txt").unwrap();
}
