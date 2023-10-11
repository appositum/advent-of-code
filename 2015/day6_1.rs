#[derive(Debug, Clone, Copy)]
enum Light {
    On,
    Off,
}

#[derive(Debug)]
enum Cmd {
    TurnOn,
    TurnOff,
    Toggle,
}

#[derive(Debug)]
struct Instruction {
    cmd: Cmd,
    start: (usize, usize),
    end: (usize, usize),
}

impl Light {
    fn exec(&mut self, cmd: &Cmd) {
        match cmd {
            TurnOn => {
                *self = On;
            }
            TurnOff => {
                *self = Off;
            }
            Toggle => match self {
                On => {
                    *self = Off;
                }
                Off => {
                    *self = On;
                }
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
use Light::*;

fn main() {
    let f = fs::read_to_string("inputs/day6.txt").unwrap();
    let lines: Vec<&str> = f.lines().collect();
    let mut matrix = [[Off; 1000]; 1000];

    for instruction in lines {
        parse(instruction).run(&mut matrix);
    }

    let mut lit = 0;

    for i in 0..999 {
        for j in 0..999 {
            match matrix[i][j] {
                On => {
                    lit += 1;
                }
                Off => (),
            }
        }
    }

    println!("{}", lit);
}

fn parse(s: &str) -> Instruction {
    let split_string: Vec<&str> = s.split(" through ").collect();

    let mut iter = split_string[0].split_whitespace();

    let cmd = match iter.next().unwrap() {
        "toggle" => Toggle,
        "turn" => match iter.next().unwrap() {
            "on" => TurnOn,
            _ => TurnOff,
        },
        _ => panic!("invalid format"),
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
        end: (end[0], end[1]),
    }
}
