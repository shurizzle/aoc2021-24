use std::{
  ops::RangeInclusive,
  sync::{Arc, Mutex},
  time::Instant,
};

use alu::Input;

use crate::alu::Alu;

mod alu;
mod ast;
mod frontend;
mod jit;

const INPUT: &str = include_str!("../inputs/24");

#[derive(Clone)]
pub struct Monad {
  alu: Alu,
}

impl Monad {
  pub fn new(alu: Alu) -> Self {
    Self { alu }
  }

  pub fn run<I: Input>(&self, input: I) -> Result<bool, ()> {
    self.alu.run(input).map(|mem| mem.z == 0)
  }
}

struct ChunkGenerator {
  start: i64,
  stop: i64,
  size: i64,
}

impl Iterator for ChunkGenerator {
  type Item = RangeInclusive<i64>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.start < self.stop {
      let stop = self.stop;
      let start = (self.stop - self.size).max(self.start);
      self.stop = start - 1;
      Some(start..=stop)
    } else {
      None
    }
  }
}

fn chunk_generator(start: i64, stop: i64, chunk_size: u32) -> ChunkGenerator {
  ChunkGenerator {
    start,
    stop,
    size: chunk_size as i64,
  }
}

fn generator(
  range: RangeInclusive<i64>,
) -> impl Iterator<Item = (i64, Vec<i64>)> {
  range.rev().filter_map(|i| {
    let digits = i
      .to_string()
      .chars()
      .map(|c| (c as i64) - ('0' as i64))
      .collect::<Vec<_>>();
    if digits.iter().all(|&c| c != 0) {
      Some((i, digits))
    } else {
      None
    }
  })
}

struct State {
  generator: ChunkGenerator,
  res: Option<i64>,
}

fn next_range(state: &Arc<Mutex<State>>) -> Option<RangeInclusive<i64>> {
  let mut s = state.lock().unwrap();
  if s.res.is_none() {
    (s.generator).next()
  } else {
    None
  }
}

fn merge_result(state: &Arc<Mutex<State>>, res: Option<i64>) {
  let mut s = state.lock().unwrap();

  if let Some(b) = res {
    if let Some(a) = s.res {
      s.res = Some(a.max(b));
    } else {
      s.res = Some(b);
    }
  }
}

fn get_task(state: Arc<Mutex<State>>, monad: Monad) -> impl FnOnce() -> () {
  move || {
    while let Some(range) = next_range(&state) {
      let mut res = None;
      let last_stop = Instant::now();
      let mut last = 0;

      for (i, digits) in generator(range) {
        last = i;
        if monad.run(digits).unwrap() {
          res = Some(i);
          break;
        }
      }

      let elapsed = last_stop.elapsed() / 10_000_000;
      let speed = 1000000000u128 / elapsed.as_nanos();
      println!("At {} ({:?} each, {}e/s)", last, elapsed, speed);

      merge_result(&state, res);
    }
  }
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
  let monad = Monad::new(Alu::new(INPUT)?);
  let generator = chunk_generator(11111111111111, 99999999999999, 10_000_000);
  let state = State {
    generator,
    res: None,
  };
  let state = Arc::new(Mutex::new(state));
  let threads_no = num_cpus::get();
  let mut threads = Vec::with_capacity(threads_no);
  for _ in 0..threads_no {
    threads.push(std::thread::spawn(get_task(state.clone(), monad.clone())));
  }

  for t in threads.into_iter() {
    t.join().unwrap();
  }

  let s = state.lock().unwrap();
  println!("Solution 1: {}", s.res.unwrap());

  Ok(())
}
