#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Blank,
    Comment(String),
    Integer(i32),
    Float(f64),
    String(String),
    Variable(String),
    Keyword(String),
    DerefExpr(Box<Node>),
    QuotedExpr(Box<Node>),
    QuasiQuotedExpr(Box<Node>),
    UnquotedExpr(Box<Node>),
    SpliceUnquotedExpr(Box<Node>),
    List(Vec<Node>),
    Vector(Vec<Node>),
    HashMap(Vec<Node>),
}
