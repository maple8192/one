use std::{iter::Peekable, str::Chars};

use crate::token::Token;

#[derive(Debug)]
pub struct Scanner<'a> {
    src: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src: src.chars().peekable(),
        }
    }

    pub fn next(&mut self) -> Result<Token, &'static str> {
        match self.src.next() {
            None => Ok(Token::Eof),
            Some(' ' | '\n' | '\r' | '\t') => self.next(),
            Some('#') => {
                self.skip_comment();
                self.next()
            }
            Some('+') => self.connected_symbol('=', Token::Add, Token::AsgAdd),
            Some('-') => self.connected_symbol('=', Token::Sub, Token::AsgSub),
            Some('*') => self.connected_symbol('=', Token::Mul, Token::AsgMul),
            Some('/') => self.connected_symbol('=', Token::Div, Token::AsgDiv),
            Some('%') => self.connected_symbol('=', Token::Rem, Token::AsgRem),
            Some('=') => self.connected_symbol('=', Token::Asg, Token::Eq),
            Some('!') => self.connected_symbol('=', Token::Not, Token::Ne),
            Some('<') => self.connected_symbol('=', Token::Lt, Token::Le),
            Some('>') => self.connected_symbol('=', Token::Gt, Token::Ge),
            Some('&') => Ok(Token::And),
            Some('|') => Ok(Token::Or),
            Some('^') => Ok(Token::Xor),
            Some(':') => {
                if self.src.next_if_eq(&'=').is_some() {
                    Ok(Token::Var)
                } else {
                    Err("invalid symbol")
                }
            }
            Some('(') => Ok(Token::ParL),
            Some(')') => Ok(Token::ParR),
            Some('{') => Ok(Token::CurL),
            Some('}') => Ok(Token::CurR),
            Some('[') => Ok(Token::SqrL),
            Some(']') => Ok(Token::SqrR),
            Some(',') => Ok(Token::Comma),
            Some(';') => Ok(Token::Semi),
            Some(ch) if ch.is_ascii_digit() => self.number(ch),
            Some('"') => self.string(),
            Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => self.word(ch),
            _ => Err("invalid character"),
        }
    }

    fn skip_comment(&mut self) {
        let _ = self
            .src
            .by_ref()
            .take_while(|&ch| ch != '\n')
            .collect::<Vec<_>>();
    }

    fn connected_symbol(
        &mut self,
        next: char,
        alone: Token,
        together: Token,
    ) -> Result<Token, &'static str> {
        if self.src.next_if_eq(&next).is_some() {
            Ok(together)
        } else {
            Ok(alone)
        }
    }

    fn number(&mut self, first: char) -> Result<Token, &'static str> {
        let mut digits = vec![first];
        while let Some(&ch) = self.src.peek() {
            if ch.is_ascii_alphabetic() {
                return Err("invalid number suffix");
            }
            if !ch.is_ascii_digit() {
                break;
            }
            digits.push(ch);
            self.src.next();
        }
        if let Some(&'.') = self.src.peek() {
            digits.push('.');
            self.src.next();
            while let Some(&ch) = self.src.peek() {
                if ch.is_ascii_alphabetic() {
                    return Err("invalid number suffix");
                }
                if !ch.is_ascii_digit() {
                    break;
                }
                digits.push(ch);
                self.src.next();
            }
        }
        let digits = digits.iter().collect::<String>();
        Ok(Token::Number(
            digits.parse().map_err(|_| "invalid number format")?,
        ))
    }

    fn string(&mut self) -> Result<Token, &'static str> {
        let mut str = Vec::new();
        for ch in self.src.by_ref() {
            if ch == '"' {
                return Ok(Token::String(str.iter().collect()));
            }
            str.push(ch);
        }
        Err("unfinished string literal")
    }

    fn word(&mut self, initial: char) -> Result<Token, &'static str> {
        let mut word = vec![initial];
        while let Some(&ch) = self.src.peek() {
            if !ch.is_ascii_alphanumeric() && ch != '_' {
                break;
            }
            word.push(ch);
            self.src.next();
        }
        let word = word.iter().collect::<String>();
        match word.as_str() {
            "fn" => Ok(Token::Fn),
            "if" => Ok(Token::If),
            "else" => Ok(Token::Else),
            "while" => Ok(Token::While),
            "for" => Ok(Token::For),
            "true" => Ok(Token::True),
            "false" => Ok(Token::False),
            "nil" => Ok(Token::Nil),
            "break" => Ok(Token::Break),
            "return" => Ok(Token::Return),
            _ => Ok(Token::Ident(word)),
        }
    }
}
