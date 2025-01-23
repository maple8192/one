use crate::{
    ast::{
        AsgStmt, BiOpKind, BiOpNode, ForStmt, FuncCallNode, Function, IfStmt, LiteralNode, Node,
        Program, ReturnStmt, SimpleStmt, Statement, StmtFunc, VarStmt, VariableNode, WhileStmt,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<Program, &'static str> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, &'static str> {
        let mut body = Vec::new();
        loop {
            match self.lexer.first()? {
                Token::Fn => body.push(StmtFunc::Function(self.parse_function()?)),
                Token::Eof => break,
                _ => body.push(StmtFunc::Statement(self.parse_statement()?)),
            }
        }
        Ok(Program::new(body))
    }

    fn parse_function(&mut self) -> Result<Function, &'static str> {
        assert!(matches!(self.lexer.next(), Ok(Token::Fn)));
        let Token::Ident(name) = self.lexer.next()? else {
            return Err("identifier expected in parsing function");
        };
        if !matches!(self.lexer.next()?, Token::ParL) {
            return Err("'(' expected in parsing function");
        }
        let mut args = Vec::new();
        if matches!(self.lexer.first()?, Token::ParR) {
            self.lexer.next()?;
        } else {
            let Token::Ident(first_arg) = self.lexer.next()? else {
                return Err("identifier expected in parsing function");
            };
            args.push(first_arg);
            while matches!(self.lexer.first()?, Token::Comma) {
                self.lexer.next()?;
                let Token::Ident(arg) = self.lexer.next()? else {
                    return Err("identifier expected in parsing function");
                };
                args.push(arg);
            }
            if !matches!(self.lexer.next()?, Token::ParR) {
                return Err("')' expected in parsing function");
            }
        }
        if !matches!(self.lexer.next()?, Token::CurL) {
            return Err("'{' expected in parsing function");
        }
        let mut body = Vec::new();
        loop {
            match self.lexer.first()? {
                Token::CurR => {
                    self.lexer.next()?;
                    break;
                }
                _ => body.push(self.parse_statement()?),
            }
        }
        Ok(Function::new(name, args, body))
    }

    fn parse_statement(&mut self) -> Result<Statement, &'static str> {
        match self.lexer.first()? {
            Token::If => Ok(Statement::If(self.parse_if_stmt()?)),
            Token::While => Ok(Statement::While(self.parse_while_stmt()?)),
            Token::For => Ok(Statement::For(self.parse_for_stmt()?)),
            Token::Return => Ok(Statement::Return(self.parse_return_stmt()?)),
            Token::Break => {
                self.lexer.next()?;
                Ok(Statement::Break)
            }
            _ => Ok(Statement::Simple(self.parse_simple_stmt()?)),
        }
    }

    fn parse_simple_stmt(&mut self) -> Result<SimpleStmt, &'static str> {
        let ret = match (self.lexer.first()?.clone(), self.lexer.second()?.clone()) {
            (Token::Ident(name), Token::Var) => {
                self.lexer.next()?;
                self.lexer.next()?;
                SimpleStmt::VarDef(VarStmt::new(name.clone(), self.parse_expression()?))
            }
            (Token::Ident(name), Token::Asg) => {
                self.lexer.next()?;
                self.lexer.next()?;
                SimpleStmt::Assign(AsgStmt::new(name.clone(), self.parse_expression()?))
            }
            (Token::Ident(name), asg)
                if matches!(
                    asg,
                    Token::AsgAdd | Token::AsgSub | Token::AsgMul | Token::AsgDiv | Token::AsgRem
                ) =>
            {
                self.lexer.next()?;
                self.lexer.next()?;
                SimpleStmt::Assign(AsgStmt::new(
                    name.clone(),
                    Node::BiOp(BiOpNode::new(
                        asg.into(),
                        Node::Variable(VariableNode::new(name)),
                        self.parse_expression()?,
                    )),
                ))
            }
            _ => SimpleStmt::Expr(self.parse_expression()?),
        };
        if !matches!(self.lexer.next()?, Token::Semi) {
            return Err("';' expected in parsing simple statement");
        }
        Ok(ret)
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, &'static str> {
        assert!(matches!(self.lexer.next(), Ok(Token::If)));
        let condition = self.parse_expression()?;
        if !matches!(self.lexer.next()?, Token::CurL) {
            return Err("'{' expected in parsing if statement");
        }
        let mut true_case = Vec::new();
        loop {
            match self.lexer.first()? {
                Token::CurR => {
                    self.lexer.next()?;
                    break;
                }
                _ => true_case.push(self.parse_statement()?),
            }
        }
        let false_case = if matches!(self.lexer.first()?, Token::Else) {
            self.lexer.next()?;
            if matches!(self.lexer.first()?, Token::If) {
                Some(vec![Statement::If(self.parse_if_stmt()?)])
            } else {
                if !matches!(self.lexer.next()?, Token::CurL) {
                    return Err("'{' expected in parsing if statement");
                }
                let mut false_case = Vec::new();
                loop {
                    match self.lexer.first()? {
                        Token::CurR => {
                            self.lexer.next()?;
                            break;
                        }
                        _ => false_case.push(self.parse_statement()?),
                    }
                }
                Some(false_case)
            }
        } else {
            None
        };
        Ok(IfStmt::new(condition, true_case, false_case))
    }

    fn parse_while_stmt(&mut self) -> Result<WhileStmt, &'static str> {
        assert!(matches!(self.lexer.next(), Ok(Token::While)));
        let condition = self.parse_expression()?;
        if !matches!(self.lexer.next()?, Token::CurL) {
            return Err("'{' expected in parsing while statement");
        }
        let mut body = Vec::new();
        loop {
            match self.lexer.first()? {
                Token::CurR => {
                    self.lexer.next()?;
                    break;
                }
                _ => body.push(self.parse_statement()?),
            }
        }
        Ok(WhileStmt::new(condition, body))
    }

    fn parse_for_stmt(&mut self) -> Result<ForStmt, &'static str> {
        assert!(matches!(self.lexer.next(), Ok(Token::For)));
        let Token::Ident(counter) = self.lexer.next()? else {
            return Err("identifier expected in parsing for statement");
        };
        if !matches!(self.lexer.next()?, Token::Var) {
            return Err("':=' expected in parsing for statement");
        }
        let init = self.parse_expression()?;
        if !matches!(self.lexer.next()?, Token::Comma) {
            return Err("',' expected in parsing for statement");
        }
        let condition = self.parse_expression()?;
        let step = if matches!(self.lexer.first()?, Token::Comma) {
            self.lexer.next()?;
            Some(self.parse_expression()?)
        } else {
            None
        };
        if !matches!(self.lexer.next()?, Token::CurL) {
            return Err("'{' expected in parsing for statement");
        }
        let mut body = Vec::new();
        loop {
            match self.lexer.first()? {
                Token::CurR => {
                    self.lexer.next()?;
                    break;
                }
                _ => body.push(self.parse_statement()?),
            }
        }
        Ok(ForStmt::new(counter, init, condition, step, body))
    }

    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, &'static str> {
        assert!(matches!(self.lexer.next(), Ok(Token::Return)));
        if matches!(self.lexer.first()?, Token::Semi) {
            self.lexer.next()?;
            Ok(ReturnStmt::new(None))
        } else {
            let expr = self.parse_expression()?;
            if !matches!(self.lexer.next()?, Token::Semi) {
                return Err("';' expected in parsing return statement");
            }
            Ok(ReturnStmt::new(Some(expr)))
        }
    }

    fn parse_expression(&mut self) -> Result<Node, &'static str> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Node, &'static str> {
        let mut node = self.parse_xor_expr()?;
        while matches!(self.lexer.first()?, Token::Or) {
            self.lexer.next()?;
            node = Node::BiOp(BiOpNode::new(BiOpKind::Or, node, self.parse_xor_expr()?));
        }
        Ok(node)
    }

    fn parse_xor_expr(&mut self) -> Result<Node, &'static str> {
        let mut node = self.parse_and_expr()?;
        while matches!(self.lexer.first()?, Token::Xor) {
            self.lexer.next()?;
            node = Node::BiOp(BiOpNode::new(BiOpKind::Xor, node, self.parse_and_expr()?));
        }
        Ok(node)
    }

    fn parse_and_expr(&mut self) -> Result<Node, &'static str> {
        let mut node = self.parse_eq_expr()?;
        while matches!(self.lexer.first()?, Token::And) {
            self.lexer.next()?;
            node = Node::BiOp(BiOpNode::new(BiOpKind::And, node, self.parse_eq_expr()?));
        }
        Ok(node)
    }

    fn parse_eq_expr(&mut self) -> Result<Node, &'static str> {
        let lhs = self.parse_ineq_expr()?;
        let kind = match self.lexer.first()? {
            Token::Eq => BiOpKind::Eq,
            Token::Ne => BiOpKind::Ne,
            _ => return Ok(lhs),
        };
        self.lexer.next()?;
        Ok(Node::BiOp(BiOpNode::new(
            kind,
            lhs,
            self.parse_ineq_expr()?,
        )))
    }

    fn parse_ineq_expr(&mut self) -> Result<Node, &'static str> {
        let lhs = self.parse_add_expr()?;
        let kind = match self.lexer.first()? {
            Token::Lt => BiOpKind::Lt,
            Token::Gt => BiOpKind::Gt,
            Token::Le => BiOpKind::Le,
            Token::Ge => BiOpKind::Ge,
            _ => return Ok(lhs),
        };
        self.lexer.next()?;
        Ok(Node::BiOp(BiOpNode::new(kind, lhs, self.parse_add_expr()?)))
    }

    fn parse_add_expr(&mut self) -> Result<Node, &'static str> {
        let mut node = self.parse_mul_expr()?;
        while matches!(self.lexer.first()?, Token::Add | Token::Sub) {
            let kind = match self.lexer.next()? {
                Token::Add => BiOpKind::Add,
                Token::Sub => BiOpKind::Sub,
                _ => unreachable!(),
            };
            node = Node::BiOp(BiOpNode::new(kind, node, self.parse_mul_expr()?));
        }
        Ok(node)
    }

    fn parse_mul_expr(&mut self) -> Result<Node, &'static str> {
        let mut node = self.parse_unary_expr()?;
        while matches!(self.lexer.first()?, Token::Mul | Token::Div | Token::Rem) {
            let kind = match self.lexer.next()? {
                Token::Mul => BiOpKind::Mul,
                Token::Div => BiOpKind::Div,
                Token::Rem => BiOpKind::Rem,
                _ => unreachable!(),
            };
            node = Node::BiOp(BiOpNode::new(kind, node, self.parse_unary_expr()?));
        }
        Ok(node)
    }

    fn parse_unary_expr(&mut self) -> Result<Node, &'static str> {
        match self.lexer.first()? {
            Token::Not => {
                self.lexer.next()?;
                Ok(Node::BiOp(BiOpNode::new(
                    BiOpKind::Eq,
                    self.parse_primary_expr()?,
                    Node::Literal(LiteralNode::Boolean(false)),
                )))
            }
            Token::Sub => {
                self.lexer.next()?;
                Ok(Node::BiOp(BiOpNode::new(
                    BiOpKind::Sub,
                    Node::Literal(LiteralNode::Number(0.0)),
                    self.parse_primary_expr()?,
                )))
            }
            _ => self.parse_primary_expr(),
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Node, &'static str> {
        match self.lexer.first()? {
            Token::Ident(_) => self.parse_var_func(),
            Token::ParL => self.parse_parenthesis(),
            _ => self.parse_literal(),
        }
    }

    fn parse_literal(&mut self) -> Result<Node, &'static str> {
        match self.lexer.next()? {
            Token::Number(n) => Ok(Node::Literal(LiteralNode::Number(n))),
            Token::String(s) => Ok(Node::Literal(LiteralNode::String(s))),
            Token::True => Ok(Node::Literal(LiteralNode::Boolean(true))),
            Token::False => Ok(Node::Literal(LiteralNode::Boolean(false))),
            Token::Nil => Ok(Node::Literal(LiteralNode::Nil)),
            _ => Err("literal expected in parsing expression"),
        }
    }

    fn parse_var_func(&mut self) -> Result<Node, &'static str> {
        let Token::Ident(name) = self.lexer.next()? else {
            panic!("identifier expected in parsing variable or function");
        };
        if matches!(self.lexer.first()?, Token::ParL) {
            self.lexer.next()?;
            let mut args = Vec::new();
            if matches!(self.lexer.first()?, Token::ParR) {
                self.lexer.next()?;
            } else {
                args.push(self.parse_expression()?);
                while matches!(self.lexer.first()?, Token::Comma) {
                    self.lexer.next()?;
                    args.push(self.parse_expression()?);
                }
                if !matches!(self.lexer.next()?, Token::ParR) {
                    return Err("')' expected in parsing function call");
                }
            }
            Ok(Node::FuncCall(FuncCallNode::new(name, args)))
        } else {
            Ok(Node::Variable(VariableNode::new(name)))
        }
    }

    fn parse_parenthesis(&mut self) -> Result<Node, &'static str> {
        assert!(matches!(self.lexer.next(), Ok(Token::ParL)));
        let node = self.parse_expression()?;
        if !matches!(self.lexer.next()?, Token::ParR) {
            return Err("')' expected in parsing expression");
        }
        Ok(node)
    }
}
