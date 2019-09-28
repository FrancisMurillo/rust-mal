use mal::{repl_parse, Err as ParseErr, Node};

#[test]
fn delimited_expressions_can_be_incomplete() {
    for ch in "([{\"".chars() {
        match repl_parse(&ch.to_string()) {
            Err(ParseErr::Incomplete(_)) => (),
            _ => panic!("{} should be incomplete", ch),
        }
    }
}

#[test]
fn parsing_should_pass_acceptance_test() {
    assert_eq!(Ok(Node::Blank), repl_parse(""));
}
