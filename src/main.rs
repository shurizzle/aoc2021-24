use crate::alu::Alu;

mod alu;
mod frontend;
mod jit;

const INPUT: &str = include_str!("../inputs/24");

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let alu = Alu::new(INPUT)?;
    let mut c = 0;
    for i in (11111111111111..=99999999999999).rev() {
        if i.to_string().chars().all(|c| c != '0') {
            if c > 10_000_000 {
                println!("At {}", i);
                c = 0;
            } else {
                c += 1;
            }
            let mem = alu.run(i).unwrap();
            if mem.z == 0 {
                println!("Solution: {}", i);
                return Ok(());
            }
        }
    }
    Ok(())
}
