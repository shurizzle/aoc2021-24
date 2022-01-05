use std::collections::BTreeMap;

use super::{Register, RegisterOrLiteral, Statement};

#[derive(Debug)]
struct RegisterState {
  last_defined: Option<usize>,
  last_used: Option<usize>,
  value: Option<i64>,
}

impl Default for RegisterState {
  fn default() -> Self {
    Self {
      last_defined: Default::default(),
      last_used: Default::default(),
      value: Default::default(),
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum StepResult {
  Repeat(bool),   // continue
  Restart(bool),  // break
  Continue(bool), // do nothing
}

impl StepResult {
  pub fn value(&self) -> bool {
    match self {
      Self::Repeat(res) | Self::Restart(res) | Self::Continue(res) => *res,
    }
  }
}

fn resolve_identities(
  state: &BTreeMap<Register, RegisterState>,
  stmts: &mut Vec<Statement>,
  i: usize,
) -> StepResult {
  match stmts[i] {
    Statement::Mul(_, RegisterOrLiteral::Literal(1))
    | Statement::Div(_, RegisterOrLiteral::Literal(1))
    | Statement::Add(_, RegisterOrLiteral::Literal(0)) => {
      stmts.remove(i);
      return StepResult::Repeat(true);
    }
    Statement::Add(reg, rol) => {
      if let Some(value) = state[&reg].value {
        if value == 0 {
          stmts[i] = Statement::Set(reg, rol);
          return StepResult::Continue(true);
        }
      }
    }
    Statement::Mul(reg, rol) => {
      if let Some(value) = state[&reg].value {
        if value == 1 {
          stmts[i] = Statement::Set(reg, rol);
          return StepResult::Continue(true);
        }
      }
    }
    _ => (),
  }

  StepResult::Continue(false)
}

fn resolve_absorbing_element(
  state: &BTreeMap<Register, RegisterState>,
  stmts: &mut Vec<Statement>,
  i: usize,
) -> StepResult {
  match stmts[i] {
    // right check
    Statement::Mul(a, RegisterOrLiteral::Literal(0))
    | Statement::Mod(a, RegisterOrLiteral::Literal(1)) => {
      stmts[i] = Statement::Set(a, RegisterOrLiteral::Literal(0));
      return StepResult::Continue(true);
    }
    // left check
    Statement::Mul(reg, _) => {
      if let Some(value) = state[&reg].value {
        if value == 0 {
          stmts[i] = Statement::Set(reg, RegisterOrLiteral::Literal(0));
          return StepResult::Continue(true);
        }
      }
    }
    Statement::Mod(reg, _) => {
      if let Some(value) = state[&reg].value {
        if value == 0 {
          stmts[i] = Statement::Set(reg, RegisterOrLiteral::Literal(0));
          return StepResult::Continue(true);
        }
      }
    }
    _ => (),
  }

  StepResult::Continue(false)
}

fn eval_right(
  state: &BTreeMap<Register, RegisterState>,
  stmts: &mut Vec<Statement>,
  i: usize,
) -> StepResult {
  let stmt = stmts.get_mut(i).unwrap();
  if let Some(snd) = stmt.get_second() {
    match snd {
      RegisterOrLiteral::Register(reg) => {
        if let Some(val) = state[&reg].value {
          stmt.set_second(RegisterOrLiteral::Literal(val));
          return StepResult::Continue(true);
        }
      }
      _ => (),
    }
  }

  StepResult::Continue(false)
}

fn eval_left(
  state: &BTreeMap<Register, RegisterState>,
  stmts: &mut Vec<Statement>,
  i: usize,
) -> StepResult {
  match stmts[i] {
    Statement::Add(reg, RegisterOrLiteral::Literal(b)) => {
      if let Some(a) = state[&reg].value {
        stmts[i] = Statement::Set(reg, RegisterOrLiteral::Literal(a + b));
        return StepResult::Continue(true);
      }
    }
    Statement::Mul(reg, RegisterOrLiteral::Literal(b)) => {
      if let Some(a) = state[&reg].value {
        stmts[i] = Statement::Set(reg, RegisterOrLiteral::Literal(a * b));
        return StepResult::Continue(true);
      }
    }
    Statement::Div(reg, RegisterOrLiteral::Literal(b)) => {
      if let Some(a) = state[&reg].value {
        stmts[i] = Statement::Set(reg, RegisterOrLiteral::Literal(a / b));
        return StepResult::Continue(true);
      }
    }
    Statement::Mod(reg, RegisterOrLiteral::Literal(b)) => {
      if let Some(a) = state[&reg].value {
        stmts[i] = Statement::Set(reg, RegisterOrLiteral::Literal(a % b));
        return StepResult::Continue(true);
      }
    }
    Statement::Eql(reg, RegisterOrLiteral::Literal(b)) => {
      if let Some(a) = state[&reg].value {
        stmts[i] = Statement::Set(
          reg,
          RegisterOrLiteral::Literal(if a == b { 1 } else { 0 }),
        );
        return StepResult::Continue(true);
      }
    }
    Statement::Neq(reg, RegisterOrLiteral::Literal(b)) => {
      if let Some(a) = state[&reg].value {
        stmts[i] = Statement::Set(
          reg,
          RegisterOrLiteral::Literal(if a == b { 0 } else { 1 }),
        );
        return StepResult::Continue(true);
      }
    }
    _ => (),
  }

  StepResult::Continue(false)
}

fn remove_useless_statements(
  state: &BTreeMap<Register, RegisterState>,
  stmts: &mut Vec<Statement>,
  i: usize,
) -> StepResult {
  if matches!(&stmts[i], &Statement::Set(_, _) | &Statement::Inp(_)) {
    let astate = &state[&stmts[i].get_first()];
    if astate.last_used.is_none() {
      if let Some(last) = astate.last_defined {
        stmts.remove(last);
        return StepResult::Restart(true);
      }
    }
  }

  StepResult::Continue(false)
}

fn convert_eq(
  state: &BTreeMap<Register, RegisterState>,
  stmts: &mut Vec<Statement>,
  i: usize,
) -> StepResult {
  if let &Statement::Eql(reg, RegisterOrLiteral::Literal(0)) = &stmts[i] {
    let astate = &state[&reg];
    if astate.last_used.is_none() {
      if let Some(last) = astate.last_defined {
        match stmts[last] {
          Statement::Eql(_, rol) => {
            stmts[last] = Statement::Neq(reg, rol);
            stmts.remove(i);
            return StepResult::Repeat(true);
          }
          Statement::Neq(_, rol) => {
            stmts[last] = Statement::Eql(reg, rol);
            stmts.remove(i);
            return StepResult::Repeat(true);
          }
          _ => (),
        }
      }
    }
  }

  StepResult::Continue(false)
}

fn main_loop(stmts: &mut Vec<Statement>) {
  while {
    let mut modified = false;
    let mut i = 0;

    let mut state = BTreeMap::new();
    for reg in [Register::W, Register::X, Register::Y, Register::Z] {
      state.insert(reg, RegisterState::default());
    }

    'restart: while i < stmts.len() {
      let mut cont = true;

      for f in [
        resolve_identities,
        resolve_absorbing_element,
        eval_right,
        eval_left,
        convert_eq,
        remove_useless_statements,
      ] {
        let res = f(&state, stmts, i);
        modified = modified || res.value();
        match res {
          StepResult::Repeat(_) => {
            cont = false;
            break;
          }
          StepResult::Restart(_) => break 'restart,
          StepResult::Continue(_) => (),
        }
      }

      if cont {
        // Update state
        {
          let s = stmts[i];
          let a = s.get_first();
          let b = s.get_second();

          let astate = state.get_mut(&a).unwrap();
          astate.last_used = None;
          astate.last_defined = Some(i);

          let bvalue = if let Some(b) = b {
            match b {
              RegisterOrLiteral::Literal(n) => Some(n),
              RegisterOrLiteral::Register(b) => {
                state.get_mut(&b).unwrap().last_used = Some(i);
                None
              }
            }
          } else {
            None
          };

          let avalue = if matches!(s, Statement::Set(_, _)) {
            bvalue
          } else {
            None
          };

          state.get_mut(&a).unwrap().value = avalue;
        }

        i += 1;
      }
    }

    modified
  } {}
}

pub fn optimize(stmts: &mut Vec<Statement>) {
  main_loop(stmts)
}
