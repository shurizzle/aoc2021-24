use crate::alu::Alu;

mod alu;
mod frontend;
mod jit;

const INPUT: &str = include_str!("../inputs/24");

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let alu = Alu::new(INPUT)?;
    let mut c = 0;
    let mut one = None;
    let mut two = None;
    for i in (10000000000000..=99999999999999i64).rev() {
        let digits = i
            .to_string()
            .chars()
            .map(|c| (c as i64) - ('0' as i64))
            .collect::<Vec<_>>();
        if c > 10_000_000 {
            println!("At {}", i);
            c = 0;
        } else {
            c += 1;
        }
        let one_ok = one.is_none() && digits.iter().all(|&c| c != 0);
        let mem = alu.run(digits).unwrap();
        let ok = mem.z == 0;
        if one_ok && ok {
            one = Some(i);
        }
        if two.is_none() && ok {
            two = Some(i);
        }

        if one.is_some() && two.is_some() {
            break;
        }
    }
    println!("Solution 1: {}\nSolution 2: {}", one.unwrap(), two.unwrap());
    Ok(())
}
