#[derive(Debug, Clone, Copy)]
struct Light {
    brightness: u64
}

#[derive(Debug)]
enum Cmd {
    IncBrightness,
    DecBrightness,
    IncBrightness2,
}

#[derive(Debug)]
struct Instruction {
    cmd: Cmd,
    start: (usize, usize),
    end: (usize, usize)
}

impl Light {
    fn exec(&mut self, cmd: &Cmd) {
        match cmd {
            IncBrightness => {
                self.brightness += 1;
            }
            DecBrightness => {
                if self.brightness > 0 {
                    self.brightness -= 1;
                }
            }
            IncBrightness2 => {
                self.brightness += 2;
            },
        }
    }
}

impl Instruction {
    fn run(self, matrix: &mut [[Light; 1000]; 1000]) {
        for i in self.start.0..(self.end.0 + 1) {
            for j in self.start.1..(self.end.1 + 1) {
                matrix[i][j].exec(&self.cmd);
            }
        }
    }
}

use std::fs;
use Cmd::*;

fn main() {
    let f = fs::read_to_string("inputs/day6.txt").unwrap();
    let lines: Vec<&str> = f.lines().collect();

    let mut matrix = [[Light { brightness: 0 }; 1000]; 1000];

    for instruction in lines {
        parse(instruction).run(&mut matrix);
    }

    let mut total = 0;

    for i in 0..999 {
        for j in 0..999 {
            total += matrix[i][j].brightness;
        }
    }

    println!("{}", total);
}

fn parse(s: &str) -> Instruction {
    let split_string: Vec<&str> = s.split(" through ").collect();

    let mut iter = split_string[0].split_whitespace();

    let cmd = match iter.next().unwrap() {
        "toggle" => IncBrightness2,
        "turn" => {
            match iter.next().unwrap() {
                "on" => IncBrightness,
                _ => DecBrightness
            }
        }
        _ => panic!("invalid format")
    };

    let start: Vec<usize> = iter
        .next()
        .unwrap()
        .split(',')
        .map(|i| i.parse::<usize>().unwrap())
        .collect();

    let end: Vec<usize> = split_string[1]
        .split(',')
        .map(|i| i.parse::<usize>().unwrap())
        .collect();

    Instruction {
        cmd: cmd,
        start: (start[0], start[1]),
        end: (end[0], end[1])
    }
}
