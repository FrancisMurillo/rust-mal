use std::fs;

use mal::{parse, Node};

const TEST_FILE: &str = "./tests/step1_read_print.mal";

fn exprs_print(exprs: &[Node]) -> String {
    exprs
        .iter()
        .map(|inner_expr| expr_print(inner_expr))
        .collect::<Vec<String>>()
        .join(" ")
}

fn expr_print(expr: &Node) -> String {
    match expr {
        Node::Comment(_) | Node::Blank => String::default(),
        Node::Integer(value) => value.to_string(),
        Node::Float(value) => value.to_string(),
        Node::Variable(symbol) => symbol.to_string(),
        Node::Keyword(symbol) => format!(":{}", symbol),
        Node::String(string) => format!("\"{}\"", string),
        Node::DerefExpr(expr) => format!("(deref {})", expr_print(expr)),
        Node::QuotedExpr(expr) => format!("(quote {})", expr_print(expr)),
        Node::QuasiQuotedExpr(expr) => format!("(quasiquote {})", expr_print(expr)),
        Node::UnquotedExpr(expr) => format!("(unquote {})", expr_print(expr)),
        Node::SpliceUnquotedExpr(expr) => format!("(splice-unquote {})", expr_print(expr)),
        Node::List(exprs) => format!("({})", exprs_print(exprs)),
        Node::Vector(exprs) => format!("[{}]", exprs_print(exprs)),
        Node::HashMap(exprs) => format!("{{{}}}", exprs_print(exprs)),
    }
}

#[test]
fn should_pass_acceptance_test() {
    fs::read_to_string(TEST_FILE)
        .unwrap()
        .lines()
        .enumerate()
        .filter(|(_index, line)| {
            !(line.is_empty() || line.starts_with(";;") || line.starts_with(";>"))
        })
        .scan(None, |state, (index, line)| match state.clone() {
            None => {
                *state = Some((index, line.to_string()));

                Some(None)
            }
            Some((index, prev_line)) => {
                *state = None;

                if line.starts_with(";=>") {
                    Some(Some((
                        index,
                        prev_line,
                        Some(line.trim_start_matches(";=>").trim().to_string()),
                    )))
                } else if line.starts_with(";/") {
                    Some(Some((index, prev_line, None)))
                } else {
                    *state = Some((index, format!("{}\n{}", prev_line, line)));

                    Some(None)
                }
            }
        })
        .filter(|item| item.is_some())
        .map(|item| item.unwrap())
        .for_each(|(index, line, expected_opt)| {
            let line_number = index + 1;

            match expected_opt {
                None => assert!(
                    parse(&line).is_err(),
                    "Test at line {} should be a parse error: {}",
                    line_number,
                    line
                ),
                Some(expected_line) => {
                    let result = parse(&line);

                    assert!(
                        result.is_ok(),
                        "Test at line {} should parse: {}",
                        line_number,
                        line.clone()
                    );

                    let expr = result.unwrap();
                    assert_eq!(
                        expected_line,
                        expr_print(&expr),
                        "Test at line {} should parse correctly: {} vs {}",
                        line_number,
                        line.clone(),
                        expr_print(&expr)
                    );
                }
            }
        });
}
