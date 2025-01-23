use std::fmt::Display;

use crate::token::Token;

#[derive(Debug)]
pub struct Program {
    body: Vec<StmtFunc>,
}

impl Program {
    pub fn new(body: Vec<StmtFunc>) -> Self {
        Self { body }
    }

    pub fn body(&self) -> &[StmtFunc] {
        &self.body
    }
}

#[derive(Debug)]
pub enum StmtFunc {
    Statement(Statement),
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    name: String,
    args: Vec<String>,
    body: Vec<Statement>,
}

impl Function {
    pub fn new(name: String, args: Vec<String>, body: Vec<Statement>) -> Self {
        Self { name, args, body }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> &[String] {
        &self.args
    }

    pub fn body(&self) -> &[Statement] {
        &self.body
    }
}

#[derive(Debug)]
pub enum Statement {
    Simple(SimpleStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Break,
}

#[derive(Debug)]
pub enum SimpleStmt {
    VarDef(VarStmt),
    Assign(AsgStmt),
    Expr(Node),
}

#[derive(Debug)]
pub struct IfStmt {
    condition: Node,
    true_case: Vec<Statement>,
    false_case: Option<Vec<Statement>>,
}

impl IfStmt {
    pub fn new(
        condition: Node,
        true_case: Vec<Statement>,
        false_case: Option<Vec<Statement>>,
    ) -> Self {
        Self {
            condition,
            true_case,
            false_case,
        }
    }

    pub fn condition(&self) -> &Node {
        &self.condition
    }

    pub fn true_case(&self) -> &[Statement] {
        &self.true_case
    }

    pub fn false_case(&self) -> Option<&[Statement]> {
        self.false_case.as_deref()
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    condition: Node,
    body: Vec<Statement>,
}

impl WhileStmt {
    pub fn new(condition: Node, body: Vec<Statement>) -> Self {
        Self { condition, body }
    }

    pub fn condition(&self) -> &Node {
        &self.condition
    }

    pub fn body(&self) -> &[Statement] {
        &self.body
    }
}

#[derive(Debug)]
pub struct ForStmt {
    counter: String,
    init: Node,
    condition: Node,
    step: Option<Node>,
    body: Vec<Statement>,
}

impl ForStmt {
    pub fn new(
        counter: String,
        init: Node,
        condition: Node,
        step: Option<Node>,
        body: Vec<Statement>,
    ) -> Self {
        Self {
            counter,
            init,
            condition,
            step,
            body,
        }
    }

    pub fn counter(&self) -> &str {
        &self.counter
    }

    pub fn init(&self) -> &Node {
        &self.init
    }

    pub fn condition(&self) -> &Node {
        &self.condition
    }

    pub fn step(&self) -> Option<&Node> {
        self.step.as_ref()
    }

    pub fn body(&self) -> &[Statement] {
        &self.body
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    expr: Option<Node>,
}

impl ReturnStmt {
    pub fn new(expr: Option<Node>) -> Self {
        Self { expr }
    }

    pub fn expr(&self) -> Option<&Node> {
        self.expr.as_ref()
    }
}

#[derive(Debug)]
pub struct VarStmt {
    lhs: String,
    rhs: Node,
}

impl VarStmt {
    pub fn new(lhs: String, rhs: Node) -> Self {
        Self { lhs, rhs }
    }

    pub fn lhs(&self) -> &str {
        &self.lhs
    }

    pub fn rhs(&self) -> &Node {
        &self.rhs
    }
}

#[derive(Debug)]
pub struct AsgStmt {
    lhs: String,
    rhs: Node,
}

impl AsgStmt {
    pub fn new(lhs: String, rhs: Node) -> Self {
        Self { lhs, rhs }
    }

    pub fn lhs(&self) -> &str {
        &self.lhs
    }

    pub fn rhs(&self) -> &Node {
        &self.rhs
    }
}

#[derive(Debug)]
pub enum AsgKind {
    None,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl From<Token> for AsgKind {
    fn from(value: Token) -> Self {
        match value {
            Token::AsgAdd => Self::Add,
            Token::AsgSub => Self::Sub,
            Token::AsgMul => Self::Mul,
            Token::AsgDiv => Self::Div,
            Token::AsgRem => Self::Rem,
            Token::Asg => Self::None,
            _ => panic!("invalid assignment operator"),
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Literal(LiteralNode),
    Variable(VariableNode),
    BiOp(BiOpNode),
    FuncCall(FuncCallNode),
}

#[derive(Debug)]
pub enum LiteralNode {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
pub struct VariableNode {
    name: String,
}

impl VariableNode {
    pub fn new(name: String) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct BiOpNode {
    kind: BiOpKind,
    lhs: Box<Node>,
    rhs: Box<Node>,
}

impl BiOpNode {
    pub fn new(kind: BiOpKind, lhs: Node, rhs: Node) -> Self {
        Self {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn kind(&self) -> &BiOpKind {
        &self.kind
    }

    pub fn lhs(&self) -> &Node {
        &self.lhs
    }

    pub fn rhs(&self) -> &Node {
        &self.rhs
    }
}

#[derive(Debug)]
pub enum BiOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Xor,
}

impl From<Token> for BiOpKind {
    fn from(value: Token) -> Self {
        match value {
            Token::AsgAdd => Self::Add,
            Token::AsgSub => Self::Sub,
            Token::AsgMul => Self::Mul,
            Token::AsgDiv => Self::Div,
            Token::AsgRem => Self::Rem,
            _ => panic!("this method is only for assignment"),
        }
    }
}

impl Display for BiOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::Le => write!(f, "<="),
            Self::Ge => write!(f, ">="),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Xor => write!(f, "^"),
        }
    }
}

#[derive(Debug)]
pub struct FuncCallNode {
    name: String,
    args: Vec<Node>,
}

impl FuncCallNode {
    pub fn new(name: String, args: Vec<Node>) -> Self {
        Self { name, args }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> &[Node] {
        &self.args
    }
}
