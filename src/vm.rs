use core::panic;
use std::{collections::HashMap, fmt::Debug};

use crate::ast::{
    AsgStmt, BiOpKind, BiOpNode, ForStmt, FuncCallNode, Function, IfStmt, LiteralNode, Node,
    Program, ReturnStmt, SimpleStmt, Statement, StmtFunc, VarStmt, VariableNode, WhileStmt,
};

#[derive(Debug)]
pub struct VM<'a> {
    environment: Vec<HashMap<String, Value<'a>>>,
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        let environment = vec![HashMap::from([
            (
                BiOpKind::Add.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Add.to_string(),
                    arity: 2,
                    func: lib_add,
                })),
            ),
            (
                BiOpKind::Sub.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Sub.to_string(),
                    arity: 2,
                    func: lib_sub,
                })),
            ),
            (
                BiOpKind::Mul.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Mul.to_string(),
                    arity: 2,
                    func: lib_mul,
                })),
            ),
            (
                BiOpKind::Div.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Div.to_string(),
                    arity: 2,
                    func: lib_div,
                })),
            ),
            (
                BiOpKind::Rem.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Rem.to_string(),
                    arity: 2,
                    func: lib_rem,
                })),
            ),
            (
                BiOpKind::Eq.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Eq.to_string(),
                    arity: 2,
                    func: lib_eq,
                })),
            ),
            (
                BiOpKind::Ne.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Ne.to_string(),
                    arity: 2,
                    func: lib_ne,
                })),
            ),
            (
                BiOpKind::Lt.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Lt.to_string(),
                    arity: 2,
                    func: lib_lt,
                })),
            ),
            (
                BiOpKind::Gt.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Gt.to_string(),
                    arity: 2,
                    func: lib_gt,
                })),
            ),
            (
                BiOpKind::Le.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Le.to_string(),
                    arity: 2,
                    func: lib_le,
                })),
            ),
            (
                BiOpKind::Ge.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Ge.to_string(),
                    arity: 2,
                    func: lib_ge,
                })),
            ),
            (
                BiOpKind::And.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::And.to_string(),
                    arity: 2,
                    func: lib_and,
                })),
            ),
            (
                BiOpKind::Or.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Or.to_string(),
                    arity: 2,
                    func: lib_or,
                })),
            ),
            (
                BiOpKind::Xor.to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: BiOpKind::Xor.to_string(),
                    arity: 2,
                    func: lib_xor,
                })),
            ),
            (
                "print".to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: "print".to_string(),
                    arity: 1,
                    func: lib_print,
                })),
            ),
            (
                "println".to_string(),
                Value::Function(FuncValue::Native(NativeFunc {
                    name: "println".to_string(),
                    arity: 1,
                    func: lib_println,
                })),
            ),
        ])];
        Self { environment }
    }

    pub fn run(&mut self, program: &'a Program) -> Result<(), &'static str> {
        for stmt in program.body() {
            match stmt {
                StmtFunc::Function(func) => {
                    self.environment.last_mut().unwrap().insert(
                        func.name().to_string(),
                        Value::Function(FuncValue::User(func)),
                    );
                }
                StmtFunc::Statement(stmt) => match self.visit_stmt(stmt)? {
                    StmtResult::None => (),
                    StmtResult::Return(_) => return Err("cannot return from top level"),
                    StmtResult::Break => return Err("cannot break from top level"),
                },
            }
        }
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Statement) -> Result<StmtResult<'a>, &'static str> {
        match stmt {
            Statement::Simple(stmt) => self.visit_simple_stmt(stmt),
            Statement::If(stmt) => self.visit_if_stmt(stmt),
            Statement::While(stmt) => self.visit_while_stmt(stmt),
            Statement::For(stmt) => self.visit_for_stmt(stmt),
            Statement::Return(stmt) => self.visit_return_stmt(stmt),
            Statement::Break => Ok(StmtResult::Break),
        }
    }

    fn visit_simple_stmt(&mut self, stmt: &SimpleStmt) -> Result<StmtResult<'a>, &'static str> {
        match stmt {
            SimpleStmt::VarDef(stmt) => self.visit_var_def(stmt)?,
            SimpleStmt::Assign(stmt) => self.visit_assign(stmt)?,
            SimpleStmt::Expr(expr) => {
                self.eval_expr(expr)?;
            }
        }
        Ok(StmtResult::None)
    }

    fn visit_var_def(&mut self, stmt: &VarStmt) -> Result<(), &'static str> {
        let name = stmt.lhs();
        if self.find_var(name).is_some() {
            return Err("variable already defined");
        }
        let value = self.eval_expr(stmt.rhs())?;
        self.environment
            .last_mut()
            .unwrap()
            .insert(name.to_string(), value);
        Ok(())
    }

    fn visit_assign(&mut self, stmt: &AsgStmt) -> Result<(), &'static str> {
        let name = stmt.lhs();
        let index = self.find_var(name).ok_or("variable not defined")?;
        let value = self.eval_expr(stmt.rhs())?;
        self.environment[index].insert(name.to_string(), value);
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<StmtResult<'a>, &'static str> {
        let Value::Boolean(condition) = self.eval_expr(stmt.condition())? else {
            return Err("condition must be a boolean");
        };
        let ret = if condition {
            self.visit_block(stmt.true_case())?
        } else if let Some(false_case) = stmt.false_case() {
            self.visit_block(false_case)?
        } else {
            StmtResult::None
        };
        Ok(ret)
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<StmtResult<'a>, &'static str> {
        while {
            let Value::Boolean(condition) = self.eval_expr(stmt.condition())? else {
                return Err("condition must be a boolean");
            };
            condition
        } {
            match self.visit_block(stmt.body())? {
                StmtResult::None => (),
                StmtResult::Return(value) => return Ok(StmtResult::Return(value)),
                StmtResult::Break => return Ok(StmtResult::None),
            }
        }
        Ok(StmtResult::None)
    }

    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> Result<StmtResult<'a>, &'static str> {
        let Value::Number(init) = self.eval_expr(stmt.init())? else {
            return Err("counter must be a number");
        };
        let env_level = self.environment.len();
        self.environment.push(HashMap::from([(
            stmt.counter().to_string(),
            Value::Number(init),
        )]));
        while {
            let Value::Boolean(condition) = self.eval_expr(stmt.condition())? else {
                return Err("condition must be a boolean");
            };
            condition
        } {
            match self.visit_block(stmt.body())? {
                StmtResult::None => {
                    let &Value::Number(counter) =
                        self.environment[env_level].get(stmt.counter()).unwrap()
                    else {
                        return Err("counter must be a number");
                    };
                    if let Some(step) = stmt.step() {
                        let Value::Number(step) = self.eval_expr(step)? else {
                            return Err("step must be a number");
                        };
                        self.environment[env_level]
                            .insert(stmt.counter().to_string(), Value::Number(counter + step));
                    } else {
                        self.environment[env_level]
                            .insert(stmt.counter().to_string(), Value::Number(counter + 1.0));
                    }
                }
                StmtResult::Return(value) => {
                    self.environment.pop();
                    return Ok(StmtResult::Return(value));
                }
                StmtResult::Break => {
                    self.environment.pop();
                    return Ok(StmtResult::None);
                }
            }
        }
        self.environment.pop();
        Ok(StmtResult::None)
    }

    fn visit_block(&mut self, stmts: &[Statement]) -> Result<StmtResult<'a>, &'static str> {
        self.environment.push(HashMap::new());
        for stmt in stmts {
            match self.visit_stmt(stmt)? {
                StmtResult::None => (),
                StmtResult::Return(value) => {
                    self.environment.pop();
                    return Ok(StmtResult::Return(value));
                }
                StmtResult::Break => {
                    self.environment.pop();
                    return Ok(StmtResult::Break);
                }
            }
        }
        self.environment.pop();
        Ok(StmtResult::None)
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<StmtResult<'a>, &'static str> {
        let value = if let Some(expr) = stmt.expr() {
            self.eval_expr(expr)?
        } else {
            Value::Nil
        };
        Ok(StmtResult::Return(value))
    }

    fn eval_expr(&mut self, expr: &Node) -> Result<Value<'a>, &'static str> {
        match expr {
            Node::Literal(lit) => self.eval_literal(lit),
            Node::Variable(var) => self.eval_variable(var),
            Node::BiOp(op) => self.eval_binary_op(op),
            Node::FuncCall(call) => self.eval_func_call(call),
        }
    }

    fn eval_literal(&mut self, literal: &LiteralNode) -> Result<Value<'a>, &'static str> {
        let lit = match literal {
            LiteralNode::Number(n) => Value::Number(*n),
            LiteralNode::String(s) => Value::String(s.to_string()),
            LiteralNode::Boolean(b) => Value::Boolean(*b),
            LiteralNode::Nil => Value::Nil,
        };
        Ok(lit)
    }

    fn eval_variable(&mut self, variable: &VariableNode) -> Result<Value<'a>, &'static str> {
        let name = variable.name();
        let index = self.find_var(name).ok_or("variable not defined")?;
        Ok(self.environment[index][name].clone())
    }

    fn eval_binary_op(&mut self, op: &BiOpNode) -> Result<Value<'a>, &'static str> {
        let lhs = self.eval_expr(op.lhs())?;
        let rhs = self.eval_expr(op.rhs())?;
        let Value::Function(FuncValue::Native(NativeFunc { func, .. })) =
            self.environment[0].get(&op.kind().to_string()).unwrap()
        else {
            panic!("invalid binary operator");
        };
        func(vec![lhs, rhs])
    }

    fn eval_func_call(&mut self, func_call: &FuncCallNode) -> Result<Value<'a>, &'static str> {
        let name = func_call.name();
        let index = self.find_var(name).ok_or("function not defined")?;
        let Value::Function(func) = self.environment[index][name].clone() else {
            return Err("variable is not a function");
        };
        let args = func_call.args();
        let arity = match func {
            FuncValue::Native(NativeFunc { arity, .. }) => arity,
            FuncValue::User(func) => func.args().len(),
        };
        if args.len() != arity {
            return Err("invalid number of arguments");
        }
        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.eval_expr(arg)?);
        }
        match func {
            FuncValue::Native(NativeFunc { func, .. }) => func(arg_values),
            FuncValue::User(func) => {
                self.environment
                    .push(func.args().iter().cloned().zip(arg_values).collect());
                let ret = self.visit_block(func.body())?;
                self.environment.pop();
                match ret {
                    StmtResult::None => Ok(Value::Nil),
                    StmtResult::Return(value) => Ok(value),
                    StmtResult::Break => Err("cannot break from function"),
                }
            }
        }
    }

    fn find_var(&self, name: &str) -> Option<usize> {
        self.environment
            .iter()
            .rposition(|env| env.contains_key(name))
    }
}

#[derive(Debug)]
enum StmtResult<'a> {
    None,
    Return(Value<'a>),
    Break,
}

#[derive(Debug, Clone)]
enum Value<'a> {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(FuncValue<'a>),
    Nil,
}

#[derive(Debug, Clone)]
enum FuncValue<'a> {
    Native(NativeFunc),
    User(&'a Function),
}

#[derive(Debug, Clone)]
struct NativeFunc {
    name: String,
    arity: usize,
    func: fn(Vec<Value>) -> Result<Value, &'static str>,
}

fn lib_add(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
        (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs.to_string() + rhs)),
        (Value::String(lhs), Value::Number(rhs)) => {
            Ok(Value::String(lhs.to_string() + &rhs.to_string()))
        }
        (Value::Number(lhs), Value::String(rhs)) => {
            Ok(Value::String(lhs.to_string() + &rhs.to_string()))
        }
        (Value::String(lhs), Value::Boolean(rhs)) => {
            Ok(Value::String(lhs.to_string() + &rhs.to_string()))
        }
        (Value::Boolean(lhs), Value::String(rhs)) => {
            Ok(Value::String(lhs.to_string() + &rhs.to_string()))
        }
        _ => Err("type mismatch in addition"),
    }
}

fn lib_sub(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
        _ => Err("type mismatch in subtraction"),
    }
}

fn lib_mul(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
        (Value::String(s), Value::Number(n)) => Ok(Value::String(s.repeat(*n as usize))),
        _ => Err("type mismatch in multiplication"),
    }
}

fn lib_div(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
        _ => Err("type mismatch in division"),
    }
}

fn lib_rem(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => {
            Ok(Value::Number(lhs - (lhs / rhs).floor() * rhs))
        }
        _ => Err("type mismatch in remainder"),
    }
}

fn lib_eq(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs == rhs)),
        (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs == rhs)),
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs == rhs)),
        (Value::Nil, Value::Nil) => Ok(Value::Boolean(true)),
        _ => Ok(Value::Boolean(false)),
    }
}

fn lib_ne(args: Vec<Value>) -> Result<Value, &'static str> {
    match lib_eq(args)? {
        Value::Boolean(b) => Ok(Value::Boolean(!b)),
        _ => panic!("unexpected value"),
    }
}

fn lib_lt(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs < rhs)),
        (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs < rhs)),
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs < rhs)),
        _ => Err("type mismatch in less than"),
    }
}

fn lib_gt(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs > rhs)),
        (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs > rhs)),
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs > rhs)),
        _ => Err("type mismatch in greater than"),
    }
}

fn lib_le(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
        (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
        _ => Err("type mismatch in less than or equal"),
    }
}

fn lib_ge(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
        (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
        _ => Err("type mismatch in greater than or equal"),
    }
}

fn lib_and(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(*lhs && *rhs)),
        _ => Err("type mismatch in logical and"),
    }
}

fn lib_or(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(*lhs || *rhs)),
        _ => Err("type mismatch in logical or"),
    }
}

fn lib_xor(args: Vec<Value>) -> Result<Value, &'static str> {
    match (&args[0], &args[1]) {
        (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(*lhs ^ *rhs)),
        _ => Err("type mismatch in logical xor"),
    }
}

fn lib_print(args: Vec<Value>) -> Result<Value, &'static str> {
    for arg in args {
        match arg {
            Value::Number(n) => print!("{}", n),
            Value::String(s) => print!("{}", s),
            Value::Boolean(b) => print!("{}", b),
            Value::Function(FuncValue::Native(NativeFunc { name, .. })) => {
                print!("<function {}>", name)
            }
            Value::Function(FuncValue::User(f)) => print!("<function {}>", f.name()),
            Value::Nil => print!("nil"),
        }
    }
    Ok(Value::Nil)
}

fn lib_println(args: Vec<Value>) -> Result<Value, &'static str> {
    lib_print(args)?;
    println!();
    Ok(Value::Nil)
}
