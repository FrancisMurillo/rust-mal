#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
extern crate rand;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

mod ast;
mod parser;

pub use nom::{
    error::{ErrorKind, ParseError, VerboseError},
    Err, IResult,
};

pub use ast::Node;
pub use parser::{parse, repl_parse};
