use nom::{
    branch::alt,
    bytes::complete::{escaped, take_while1},
    bytes::streaming::tag,
    character::complete::{none_of, not_line_ending, one_of},
    combinator::{map, opt, verify},
    error::{context, ErrorKind, ParseError},
    multi::many0,
    sequence::{delimited, preceded},
    AsChar, Err, IResult, InputTakeAtPosition,
};

use crate::ast::Node;

pub fn wsc<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| {
        let ch = item.as_char();

        !(ch.is_whitespace() || ch == ',')
    })
}

fn comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, o) = context("comment", preceded(tag(";"), not_line_ending))(i)?;

    if i.is_empty() {
        Ok((i, Node::Comment(o.to_string())))
    } else {
        let (i, _) = wsc(i)?;

        Ok((i, Node::Comment(o.to_string())))
    }
}

fn variable<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, text) = context("variable", variable_name)(i)?;

    Ok((i, Node::Variable(text.to_string())))
}

fn keyword<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, text) = context("keyword", preceded(tag(":"), variable_name))(i)?;

    Ok((i, Node::Keyword(text.to_string())))
}

fn variable_name<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&str, &str, E> {
    take_while1(is_variable_char)(i)
}

fn is_variable_char(ch: char) -> bool {
    !(is_special_char(ch) || ch.is_control() || ch.is_whitespace())
}

fn is_special_char(ch: char) -> bool {
    ch == '('
        || ch == ')'
        || ch == '['
        || ch == ']'
        || ch == '"'
        || ch == '\''
        || ch == '`'
        || ch == ';'
        || ch == '~'
        || ch == '@'
        || ch == ':'
        || ch == ','
        || ch == '{'
        || ch == '}'
}

fn number<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, var) = context("number", verify(variable, is_numeric_symbol))(i)?;

    let text = match var {
        Node::Variable(text) => text,
        _ => String::default(),
    };

    Ok((i, as_number(text)))
}

fn is_numeric_symbol(sym: &Node) -> bool {
    match sym {
        Node::Variable(input) => input.parse::<f64>().is_ok() || input.parse::<i32>().is_ok(),
        _ => false,
    }
}

fn as_number(input: String) -> Node {
    if input.contains('.') {
        input.parse::<f64>().map(Node::Float).unwrap()
    } else {
        input
            .parse::<i32>()
            .map(Node::Integer)
            .or_else(|_| input.parse::<f64>().map(Node::Float))
            .unwrap()
    }
}

fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, text) = context(
        "string",
        delimited(
            tag("\""),
            map(
                opt(escaped(none_of("\\\""), '\\', one_of("\\\"nrt"))),
                |opt| opt.unwrap_or(""),
            ),
            tag("\""),
        ),
    )(i)?;

    Ok((i, Node::String(text.to_string())))
}

fn list<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, exprs) = context(
        "list",
        delimited(
            tag("("),
            |ie| {
                let (ie, _) = opt(many0(comment))(ie)?;
                let (ie, _) = wsc(ie)?;
                let (ie, o) = many0(expr)(ie)?;
                let (ie, _) = opt(many0(comment))(ie)?;
                let (ie, _) = wsc(ie)?;

                Ok((ie, o))
            },
            tag(")"),
        ),
    )(i)?;
    let (i, _) = wsc(i)?;

    Ok((i, Node::List(exprs)))
}

fn vector<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, exprs) = context(
        "vector",
        delimited(
            tag("["),
            |ie| {
                let (ie, _) = opt(many0(comment))(ie)?;
                let (ie, _) = wsc(ie)?;
                let (ie, o) = many0(expr)(ie)?;
                let (ie, _) = opt(many0(comment))(ie)?;
                let (ie, _) = wsc(ie)?;

                Ok((ie, o))
            },
            tag("]"),
        ),
    )(i)?;
    let (i, _) = wsc(i)?;

    Ok((i, Node::Vector(exprs)))
}

fn hash_map<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, exprs) = context(
        "hash_map",
        delimited(
            tag("{"),
            |ie| {
                let (ie, _) = opt(many0(comment))(ie)?;
                let (ie, _) = wsc(ie)?;
                let (ie, o) = many0(expr)(ie)?;
                let (ie, _) = opt(many0(comment))(ie)?;
                let (ie, _) = wsc(ie)?;

                Ok((ie, o))
            },
            tag("}"),
        ),
    )(i)?;
    let (i, _) = wsc(i)?;

    Ok((i, Node::HashMap(exprs)))
}

fn expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = opt(many0(comment))(i)?;
    let (i, inner_expr) = context(
        "expr",
        alt((
            list,
            vector,
            hash_map,
            string,
            splice_unquoted_expr,
            deref_expr,
            quoted_expr,
            quasi_quoted_expr,
            unquoted_expr,
            number,
            keyword,
            variable,
        )),
    )(i)?;

    Ok((i, inner_expr))
}

fn deref_expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, expr) = context("deref_expr", preceded(tag("@"), expr))(i)?;

    Ok((i, Node::DerefExpr(Box::new(expr))))
}

fn quoted_expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, expr) = context("quoted_expr", preceded(tag("'"), expr))(i)?;

    Ok((i, Node::QuotedExpr(Box::new(expr))))
}

fn quasi_quoted_expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, expr) = context("quasi_quoted_expr", preceded(tag("`"), expr))(i)?;

    Ok((i, Node::QuasiQuotedExpr(Box::new(expr))))
}

fn unquoted_expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, expr) = context("unquoted_expr", preceded(tag("~"), expr))(i)?;

    Ok((i, Node::UnquotedExpr(Box::new(expr))))
}

fn splice_unquoted_expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    let (i, _) = wsc(i)?;
    let (i, expr) = context("splice_unquoted_expr", preceded(tag("~@"), expr))(i)?;

    Ok((i, Node::SpliceUnquotedExpr(Box::new(expr))))
}

fn blank<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    if i.is_empty() {
        return Ok((i, Node::Blank));
    }

    let (i, _) = opt(many0(comment))(i)?;
    let (i, _) = wsc(i)?;

    if i.is_empty() {
        Ok((i, Node::Blank))
    } else {
        Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Eof)))
    }
}

fn repl_expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Node, E> {
    context("repl_expr", alt((blank, expr)))(i)
}

pub fn parse(input: &str) -> Result<Node, Err<(&str, ErrorKind)>> {
    expr(input).map(|(_, expr)| expr)
}

pub fn repl_parse(input: &str) -> Result<Node, Err<(&str, ErrorKind)>> {
    repl_expr(input).map(|(_, expr)| expr)
}

#[cfg(test)]
mod properties {
    use std::fmt;

    use nom::error::VerboseError;

    use quickcheck::{Arbitrary, Gen, TestResult};

    use crate::ast::Node;

    use super::*;

    fn format_exprs(exprs: &[Node]) -> String {
        exprs
            .iter()
            .map(|expr| format!("{}", expr))
            .collect::<Vec<String>>()
            .join(" ")
    }

    impl fmt::Display for Node {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self {
                Node::Blank => write!(f, ""),
                Node::Comment(comment) => writeln!(f, ";{}", comment),
                Node::Integer(value) => write!(f, "{}", value),
                Node::Float(value) => write!(f, "{}", value),
                Node::Variable(symbol) => write!(f, "{}", symbol),
                Node::Keyword(symbol) => write!(f, ":{}", symbol),
                Node::String(string) => write!(f, "\"{}\"", string),
                Node::DerefExpr(expr) => write!(f, "@{}", expr),
                Node::QuotedExpr(expr) => write!(f, "'{}", expr),
                Node::QuasiQuotedExpr(expr) => write!(f, "`{}", expr),
                Node::UnquotedExpr(expr) => write!(f, "~{}", expr),
                Node::SpliceUnquotedExpr(expr) => write!(f, "~@{}", expr),
                Node::List(exprs) => write!(f, "({})", format_exprs(exprs)),
                Node::Vector(exprs) => write!(f, "[{}]", format_exprs(exprs)),
                Node::HashMap(exprs) => write!(f, "{{{}}}", format_exprs(exprs)),
            }
        }
    }

    #[quickcheck]
    fn number_parser_should_parse_f64(value: f64) -> bool {
        match number::<VerboseError<&str>>(&value.to_string()) {
            Ok((remaining, Node::Float(float))) => {
                remaining.is_empty() && (value - float).abs() < std::f64::EPSILON
            }
            _ => false,
        }
    }

    #[quickcheck]
    fn number_parser_should_parsei32(value: i32) -> bool {
        match number::<VerboseError<&str>>(&value.to_string()) {
            Ok((remaining, Node::Integer(int))) => remaining.is_empty() && (value == int),
            _ => false,
        }
    }

    #[quickcheck]
    fn number_parser_can_fail(text: String) -> TestResult {
        if text.parse::<f64>().is_err() && text.parse::<i32>().is_err() {
            TestResult::from_bool(match number::<VerboseError<&str>>(&text) {
                Ok((remaining, Node::Variable(..))) => remaining.is_empty(),
                Ok((remaining, Node::Float(..))) => !remaining.is_empty(),
                Ok((remaining, Node::Integer(..))) => !remaining.is_empty(),
                Err(..) => true,
                _ => false,
            })
        } else {
            TestResult::discard()
        }
    }

    #[quickcheck]
    fn number_parser_can_fail_with_numerical_prefix(value: f64, text: String) -> TestResult {
        if !text.trim().is_empty() && text.parse::<f64>().is_err() && text.parse::<i32>().is_err() {
            let prefixed_text = format!("{}{}", value, text);

            TestResult::from_bool(match number::<VerboseError<&str>>(&prefixed_text) {
                Ok((remaining, Node::Variable(..))) => remaining.is_empty(),
                Ok((remaining, Node::Float(..))) => !remaining.is_empty(),
                Ok((remaining, Node::Integer(..))) => !remaining.is_empty(),
                Err(..) => true,
                _ => false,
            })
        } else {
            TestResult::discard()
        }
    }

    #[quickcheck]
    fn variable_parser_should_parse_text(text: String) -> TestResult {
        if text.is_empty() || text.chars().any(|ch| !is_variable_char(ch)) {
            TestResult::discard()
        } else {
            TestResult::from_bool(match variable::<VerboseError<&str>>(&text) {
                Ok((_, Node::Variable(ref symbol))) if text.eq(symbol) => true,
                _ => false,
            })
        }
    }

    #[quickcheck]
    fn keywork_parser_should_parse_text(text: String) -> TestResult {
        if text.is_empty() || text.chars().any(|ch| !is_variable_char(ch)) || !text.starts_with(':')
        {
            TestResult::discard()
        } else {
            TestResult::from_bool(match keyword::<VerboseError<&str>>(&text) {
                Ok((_, Node::Keyword(ref symbol))) if text.eq(symbol) => true,
                _ => false,
            })
        }
    }

    #[quickcheck]
    fn string_parser_should_parse_text(text: String) -> TestResult {
        if text.chars().any(|ch| ch == '"' || ch == '\\') {
            TestResult::discard()
        } else {
            let quoted_text = format!("\"{}\"", text);

            TestResult::from_bool(match string::<VerboseError<&str>>(&quoted_text) {
                Ok((_, Node::String(ref string))) if text.eq(string) => true,
                _ => false,
            })
        }
    }

    #[quickcheck]
    fn string_parser_should_parse_escaped_text(text: String) -> TestResult {
        if text.chars().any(|ch| ch == '"' || ch == '\\') {
            TestResult::discard()
        } else if text.contains("\\\"")
            || text.contains("\\\\")
            || text.contains("\\\n")
            || text.contains("\\\r")
            || text.contains("\\\t")
        {
            let quoted_text = format!("\"{}\"", text);

            TestResult::from_bool(match string::<VerboseError<&str>>(&quoted_text) {
                Ok((_, Node::String(ref string))) if text.eq(string) => true,
                _ => false,
            })
        } else {
            TestResult::discard()
        }
    }

    impl Arbitrary for Node {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            match u8::arbitrary(g) % 14 {
                0 => Node::Integer(i32::arbitrary(g)),
                1 => Node::Float(f64::arbitrary(g)),
                2 => {
                    let symbol_name = String::arbitrary(g)
                        .replace(|ch: char| !is_variable_char(ch), "_")
                        .replace(|ch: char| !ch.is_ascii(), "=")
                        .replace(|ch: char| ch.is_digit(10), "o");

                    if symbol_name.is_empty() {
                        Node::Variable(String::from("_"))
                    } else {
                        Node::Variable(symbol_name)
                    }
                }
                3 => {
                    let symbol_name = String::arbitrary(g)
                        .replace(|ch: char| !is_variable_char(ch), "_")
                        .replace(|ch: char| !ch.is_ascii(), "=")
                        .replace(|ch: char| ch.is_digit(10), "o");

                    if symbol_name.is_empty() {
                        Node::Keyword(String::from("_"))
                    } else {
                        Node::Keyword(symbol_name)
                    }
                }
                4 | 5 => {
                    let text = String::arbitrary(g)
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"");

                    Node::String(text)
                }
                6 => {
                    let size = u8::arbitrary(g) % 5;

                    if size <= 2 {
                        Node::List(Vec::default())
                    } else {
                        Node::List(
                            (0..(size - 2))
                                .map(|_| Self::arbitrary(g))
                                .collect::<Vec<_>>(),
                        )
                    }
                }
                7 => {
                    let size = u8::arbitrary(g) % 5;

                    if size <= 2 {
                        Node::Vector(Vec::default())
                    } else {
                        Node::Vector(
                            (0..(size - 2))
                                .map(|_| Self::arbitrary(g))
                                .collect::<Vec<_>>(),
                        )
                    }
                }
                8 | 9 => {
                    let size = u8::arbitrary(g) % 5;

                    if size <= 2 {
                        Node::Vector(Vec::default())
                    } else {
                        Node::Vector(
                            (0..(size - 2))
                                .map(|_| Self::arbitrary(g))
                                .collect::<Vec<_>>(),
                        )
                    }
                }
                10 => Node::QuotedExpr(Box::new(Self::arbitrary(g))),
                11 => Node::QuasiQuotedExpr(Box::new(Self::arbitrary(g))),
                12 => Node::UnquotedExpr(Box::new(Self::arbitrary(g))),
                13 => Node::SpliceUnquotedExpr(Box::new(Self::arbitrary(g))),
                _ => panic!("Should not happen"),
            }
        }
    }

    #[quickcheck]
    fn list_parser_should_parse_list_expressions(expr: Node) -> TestResult {
        if let Node::List(..) = expr {
            let text = format!("{}", expr);

            TestResult::from_bool(match list::<VerboseError<&str>>(&text) {
                Ok((_, Node::List(..))) => true,
                _ => false,
            })
        } else {
            TestResult::discard()
        }
    }

    #[quickcheck]
    fn vector_parser_should_parse_list_expressions(expr: Node) -> TestResult {
        if let Node::Vector(..) = expr {
            let text = format!("{}", expr);

            TestResult::from_bool(match vector::<VerboseError<&str>>(&text) {
                Ok((_, Node::Vector(..))) => true,
                _ => false,
            })
        } else {
            TestResult::discard()
        }
    }

    #[quickcheck]
    fn hash_map_parser_should_parse_list_expressions(expr: Node) -> TestResult {
        if let Node::HashMap(..) = expr {
            let text = format!("{}", expr);

            TestResult::from_bool(match hash_map::<VerboseError<&str>>(&text) {
                Ok((_, Node::HashMap(..))) => true,
                _ => false,
            })
        } else {
            TestResult::discard()
        }
    }

    #[quickcheck]
    fn expr_parser_should_parse_expressions(root_expr: Node) -> bool {
        let text = format!("{}", root_expr);

        match expr::<VerboseError<&str>>(&text) {
            Ok((_, expr)) => expr == root_expr,
            _ => false,
        }
    }

    #[quickcheck]
    fn deref_expr_parser_should_parse_expressions(root_expr: Node) -> bool {
        let text = format!("@{}", root_expr);

        match deref_expr::<VerboseError<&str>>(&text) {
            Ok((_, Node::DerefExpr(expr))) => *expr == root_expr,
            _ => false,
        }
    }

    #[quickcheck]
    fn quoted_expr_parser_should_parse_expressions(root_expr: Node) -> bool {
        let text = format!("'{}", root_expr);

        match quoted_expr::<VerboseError<&str>>(&text) {
            Ok((_, Node::QuotedExpr(expr))) => *expr == root_expr,
            _ => false,
        }
    }

    #[quickcheck]
    fn quasi_quoted_expr_parser_should_parse_expressions(root_expr: Node) -> bool {
        let text = format!("`{}", root_expr);

        match quasi_quoted_expr::<VerboseError<&str>>(&text) {
            Ok((_, Node::QuasiQuotedExpr(expr))) => *expr == root_expr,
            _ => false,
        }
    }

    #[quickcheck]
    fn unquoted_expr_parser_should_parse_expressions(root_expr: Node) -> bool {
        let text = format!("~{}", root_expr);

        match unquoted_expr::<VerboseError<&str>>(&text) {
            Ok((_, Node::UnquotedExpr(expr))) => *expr == root_expr,
            _ => false,
        }
    }

    #[quickcheck]
    fn splice_unquoted_expr_parser_should_parse_expressions(root_expr: Node) -> bool {
        let text = format!("~@{}", root_expr);

        match splice_unquoted_expr::<VerboseError<&str>>(&text) {
            Ok((_, Node::SpliceUnquotedExpr(expr))) => *expr == root_expr,
            _ => false,
        }
    }
}
