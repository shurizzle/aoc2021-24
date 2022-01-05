use std::{rc::Rc, vec};

use cranelift_module::ModuleError;
use peg::{error::ParseError, str::LineCol};

use crate::{
  ast::{parse, Statement},
  jit::{Function, Mem},
};

#[derive(Debug)]
pub enum Error {
  ParseError(ParseError<LineCol>),
  CompileError(ModuleError),
}

impl std::fmt::Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::ParseError(err) => err.fmt(f),
      Self::CompileError(err) => err.fmt(f),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::ParseError(err) => err.source(),
      Self::CompileError(err) => err.source(),
    }
  }

  fn description(&self) -> &str {
    "description() is deprecated; use Display"
  }

  fn cause(&self) -> Option<&dyn std::error::Error> {
    match self {
      Self::ParseError(err) => err.source(),
      Self::CompileError(err) => err.source(),
    }
  }
}

#[derive(Clone)]
pub struct Alu {
  internal: Rc<Function>,
}

pub trait Input {
  type Iter: Iterator<Item = i64>;

  fn input(self) -> Self::Iter;
}

impl Input for i64 {
  type Iter = vec::IntoIter<i64>;

  fn input(self) -> Self::Iter {
    self
      .to_string()
      .chars()
      .map(|c| (c as i64) - ('0' as i64))
      .collect::<Vec<_>>()
      .into_iter()
  }
}

impl Input for Vec<i64> {
  type Iter = vec::IntoIter<i64>;

  fn input(self) -> Self::Iter {
    self.into_iter()
  }
}

impl Alu {
  pub fn new(text: &str) -> Result<Self, Error> {
    Self::compile(Self::parse(text)?)
  }

  pub fn parse(text: &str) -> Result<Vec<Statement>, Error> {
    match parse(text) {
      Ok(stmts) => Ok(stmts),
      Err(err) => Err(Error::ParseError(err)),
    }
  }

  pub fn compile(stmts: Vec<Statement>) -> Result<Self, Error> {
    match Function::new(stmts) {
      Ok(internal) => Ok(Self {
        internal: Rc::new(internal),
      }),
      Err(err) => Err(Error::CompileError(err)),
    }
  }

  pub fn run<I>(&self, input: I) -> Result<Mem, ()>
  where
    I: Input,
  {
    self.internal.run(input.input())
  }
}

unsafe impl Send for Alu {}
unsafe impl Sync for Alu {}
