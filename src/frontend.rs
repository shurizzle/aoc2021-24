use crate::ast::{Register, RegisterOrLiteral, Statement};
use peg::{error::ParseError, str::LineCol};

peg::parser! {
    grammar asm() for str {
        rule _ = [' ' | '\t']+

        rule __ = [' ' | '\t' | '\n']*

        rule newline() = quiet!{"\n" __}

        rule literal() -> i64
            = n:$(("-"? ['1'..='9'] ['0'..='9']*) / "0") {? n.parse().or(Err("literal"))}

        rule register() -> Register = precedence!{
            ['W' | 'w'] { Register::W }
            ['X' | 'x'] { Register::X }
            ['Y' | 'y'] { Register::Y }
            ['Z' | 'z'] { Register::Z }
        }

        rule register_or_literal() -> RegisterOrLiteral = precedence!{
            l:literal() { RegisterOrLiteral::Literal(l) }
            r:register() { RegisterOrLiteral::Register(r) }
        }

        rule operation() -> Statement = precedence!{
            __ (['I' | 'i']['N' | 'n']['P' | 'p']) _ reg:register() _? { Statement::Inp(reg) }
            __ (['A' | 'a']['D' | 'd']['D' | 'd']) _ a:register() _ b:register_or_literal() _? { Statement::Add(a, b) }
            __ (['M' | 'm']['U' | 'u']['L' | 'l']) _ a:register() _ b:register_or_literal() _? { Statement::Mul(a, b) }
            __ (['D' | 'd']['I' | 'i']['V' | 'v']) _ a:register() _ b:register_or_literal() _? { Statement::Div(a, b) }
            __ (['M' | 'm']['O' | 'o']['D' | 'd']) _ a:register() _ b:register_or_literal() _? { Statement::Mod(a, b) }
            __ (['E' | 'e']['Q' | 'q']['L' | 'l']) _ a:register() _ b:register_or_literal() _? { Statement::Eql(a, b) }
        }

        pub rule code() -> Vec<Statement>
            = l:operation() ** "\n" __ { l }
    }
}

pub fn parse(s: &str) -> Result<Vec<Statement>, ParseError<LineCol>> {
  asm::code(s)
}

#[test]
fn test1() {
  assert_eq!(
    parse(
      "inp x
mul x -1",
    ),
    Ok(vec![
      Statement::Inp(Register::X),
      Statement::Mul(Register::X, RegisterOrLiteral::Literal(-1))
    ])
  );
}

#[test]
fn test2() {
  assert_eq!(
    parse(
      "inp z
inp x
mul z 3
eql z x",
    ),
    Ok(vec![
      Statement::Inp(Register::Z),
      Statement::Inp(Register::X),
      Statement::Mul(Register::Z, RegisterOrLiteral::Literal(3)),
      Statement::Eql(Register::Z, RegisterOrLiteral::Register(Register::X))
    ])
  );
}

#[test]
fn test3() {
  assert_eq!(
    parse(
      "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2",
    ),
    Ok(vec![
      Statement::Inp(Register::W),
      Statement::Add(Register::Z, RegisterOrLiteral::Register(Register::W)),
      Statement::Mod(Register::Z, RegisterOrLiteral::Literal(2)),
      Statement::Div(Register::W, RegisterOrLiteral::Literal(2)),
      Statement::Add(Register::Y, RegisterOrLiteral::Register(Register::W)),
      Statement::Mod(Register::Y, RegisterOrLiteral::Literal(2)),
      Statement::Div(Register::W, RegisterOrLiteral::Literal(2)),
      Statement::Add(Register::X, RegisterOrLiteral::Register(Register::W)),
      Statement::Mod(Register::X, RegisterOrLiteral::Literal(2)),
      Statement::Div(Register::W, RegisterOrLiteral::Literal(2)),
      Statement::Mod(Register::W, RegisterOrLiteral::Literal(2)),
    ])
  );
}
