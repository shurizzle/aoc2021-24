mod optimize;

use peg::{error::ParseError, str::LineCol};

use self::optimize::optimize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Register {
  W,
  X,
  Y,
  Z,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegisterOrLiteral {
  Register(Register),
  Literal(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Statement {
  Inp(Register),
  Set(Register, RegisterOrLiteral),
  Add(Register, RegisterOrLiteral),
  Mul(Register, RegisterOrLiteral),
  Div(Register, RegisterOrLiteral),
  Mod(Register, RegisterOrLiteral),
  Eql(Register, RegisterOrLiteral),
  Neq(Register, RegisterOrLiteral),
}

impl Statement {
  pub fn get_first(&self) -> Register {
    match self {
      &Self::Inp(reg)
      | &Self::Set(reg, _)
      | &Self::Add(reg, _)
      | &Self::Mul(reg, _)
      | &Self::Div(reg, _)
      | &Self::Mod(reg, _)
      | &Self::Eql(reg, _)
      | &Self::Neq(reg, _) => reg,
    }
  }

  pub fn get_second(&self) -> Option<RegisterOrLiteral> {
    match self {
      &Self::Inp(_) => None,
      &Self::Set(_, rol)
      | &Self::Add(_, rol)
      | &Self::Mul(_, rol)
      | &Self::Div(_, rol)
      | &Self::Mod(_, rol)
      | &Self::Eql(_, rol)
      | &Self::Neq(_, rol) => Some(rol),
    }
  }

  pub fn set_second(&mut self, second: RegisterOrLiteral) {
    match self {
      Self::Inp(_) => (),
      Self::Set(_, ref mut sec)
      | Self::Add(_, ref mut sec)
      | Self::Mul(_, ref mut sec)
      | Self::Div(_, ref mut sec)
      | Self::Mod(_, ref mut sec)
      | Self::Eql(_, ref mut sec)
      | Self::Neq(_, ref mut sec) => {
        *sec = second;
      }
    }
  }
}

pub fn parse(text: &str) -> Result<Vec<Statement>, ParseError<LineCol>> {
  let mut stmts = Vec::new();
  for r in [Register::W, Register::X, Register::Y, Register::Z] {
    stmts.push(Statement::Set(r, RegisterOrLiteral::Literal(0)));
  }
  stmts.extend(crate::frontend::parse(text)?);
  // for s in stmts.iter() {
  //     println!("{:?}", s);
  // }
  // println!("\n\n");
  optimize(&mut stmts);
  // for s in stmts.iter() {
  //   println!("{:?}", s);
  // }
  Ok(stmts)
}
