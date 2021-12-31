use peg::{error::ParseError, str::LineCol};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
pub enum Operation {
    Inp(Register),
    Add(Register, RegisterOrLiteral),
    Mul(Register, RegisterOrLiteral),
    Div(Register, RegisterOrLiteral),
    Mod(Register, RegisterOrLiteral),
    Eql(Register, RegisterOrLiteral),
}

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

        rule operation() -> Operation = precedence!{
            __ (['I' | 'i']['N' | 'n']['P' | 'p']) _ reg:register() _? { Operation::Inp(reg) }
            __ (['A' | 'a']['D' | 'd']['D' | 'd']) _ a:register() _ b:register_or_literal() _? { Operation::Add(a, b) }
            __ (['M' | 'm']['U' | 'u']['L' | 'l']) _ a:register() _ b:register_or_literal() _? { Operation::Mul(a, b) }
            __ (['D' | 'd']['I' | 'i']['V' | 'v']) _ a:register() _ b:register_or_literal() _? { Operation::Div(a, b) }
            __ (['M' | 'm']['O' | 'o']['D' | 'd']) _ a:register() _ b:register_or_literal() _? { Operation::Mod(a, b) }
            __ (['E' | 'e']['Q' | 'q']['L' | 'l']) _ a:register() _ b:register_or_literal() _? { Operation::Eql(a, b) }
        }

        pub rule code() -> Vec<Operation>
            = l:operation() ** "\n" __ { l }
    }
}

pub fn parse(s: &str) -> Result<Vec<Operation>, ParseError<LineCol>> {
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
            Operation::Inp(Register::X),
            Operation::Mul(Register::X, RegisterOrLiteral::Literal(-1))
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
            Operation::Inp(Register::Z),
            Operation::Inp(Register::X),
            Operation::Mul(Register::Z, RegisterOrLiteral::Literal(3)),
            Operation::Eql(Register::Z, RegisterOrLiteral::Register(Register::X))
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
            Operation::Inp(Register::W),
            Operation::Add(Register::Z, RegisterOrLiteral::Register(Register::W)),
            Operation::Mod(Register::Z, RegisterOrLiteral::Literal(2)),
            Operation::Div(Register::W, RegisterOrLiteral::Literal(2)),
            Operation::Add(Register::Y, RegisterOrLiteral::Register(Register::W)),
            Operation::Mod(Register::Y, RegisterOrLiteral::Literal(2)),
            Operation::Div(Register::W, RegisterOrLiteral::Literal(2)),
            Operation::Add(Register::X, RegisterOrLiteral::Register(Register::W)),
            Operation::Mod(Register::X, RegisterOrLiteral::Literal(2)),
            Operation::Div(Register::W, RegisterOrLiteral::Literal(2)),
            Operation::Mod(Register::W, RegisterOrLiteral::Literal(2)),
        ])
    );
}
