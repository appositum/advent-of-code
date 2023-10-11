#[derive(Debug, Clone, Copy)]
enum LightPt1 {
    On,
    Off,
}

#[derive(Debug, Clone, Copy)]
struct LightPt2 {
    brightness: u64,
}

#[derive(Debug)]
enum Cmd {
    TurnOn, // pt2: increase brightness
    TurnOff, // pt2: decrease brightness
    Toggle, // pt2: increase brightness (+2)
}

#[derive(Debug)]
struct Instruction {
    cmd: Cmd,
    start: (usize, usize),
    end: (usize, usize),
}

impl LightPt1 {
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

impl LightPt2 {
    fn exec(&mut self, cmd: &Cmd) {
        match cmd {
            TurnOn => {
                self.brightness += 1;
            }
            TurnOff => {
                if self.brightness > 0 {
                    self.brightness -= 1;
                }
            }
            Toggle => {
                self.brightness += 2;
            }
        }
    }
}

impl Instruction {
    fn run1(self, matrix: &mut Vec<Vec<LightPt1>>) {
        for i in self.start.0..(self.end.0 + 1) {
            for j in self.start.1..(self.end.1 + 1) {
                matrix[i][j].exec(&self.cmd);
            }
        }
    }

    fn run2(self, matrix: &mut Vec<Vec<LightPt2>>) {
        for i in self.start.0..(self.end.0 + 1) {
            for j in self.start.1..(self.end.1 + 1) {
                matrix[i][j].exec(&self.cmd);
            }
        }
    }
}

use std::fs;
use Cmd::*;
use LightPt1::*;

fn main() {
    let f = fs::read_to_string("inputs/day6.txt").unwrap();
    let lines: Vec<&str> = f.lines().collect();

    let mut matrix1 = vec![vec![Off; 1000]; 1000];
    let mut matrix2 = vec![vec![LightPt2 { brightness: 0 }; 1000]; 1000];

    for instruction in lines {
        parse(instruction).run1(&mut matrix1);
        parse(instruction).run2(&mut matrix2);
    }

    let mut lit_lamps = 0;

    for i in 0..999 {
        for j in 0..999 {
            match matrix1[i][j] {
                On => {
                    lit_lamps += 1;
                }
                Off => (),
            }
        }
    }

    let mut total_brightness = 0;

    for i in 0..999 {
        for j in 0..999 {
            total_brightness += matrix2[i][j].brightness;
        }
    }

    println!("part 1: {}\npart 2: {}", lit_lamps, total_brightness);
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
