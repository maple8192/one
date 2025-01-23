use std::collections::VecDeque;

use crate::{scanner::Scanner, token::Token};

#[derive(Debug)]
pub struct Lexer<'a> {
    scanner: Scanner<'a>,
    buffer: VecDeque<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            scanner: Scanner::new(src),
            buffer: VecDeque::new(),
        }
    }

    pub fn next(&mut self) -> Result<Token, &'static str> {
        if let Some(next) = self.buffer.pop_front() {
            Ok(next)
        } else {
            self.scanner.next()
        }
    }

    pub fn first(&mut self) -> Result<&Token, &'static str> {
        self.nth(0)
    }

    pub fn second(&mut self) -> Result<&Token, &'static str> {
        self.nth(1)
    }

    fn nth(&mut self, n: usize) -> Result<&Token, &'static str> {
        while self.buffer.len() <= n {
            self.buffer.push_back(self.scanner.next()?);
        }
        Ok(&self.buffer[n])
    }
}
