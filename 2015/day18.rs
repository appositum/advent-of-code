use std::fs;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Light {
    On,
    Off,
}

use Light::*;

fn main() {
    let matrix: Vec<Vec<char>> = vec![
        vec!['.', '#', '.', '#', '.', '#'],
        vec!['.', '.', '.', '#', '#', '.'],
        vec!['#', '.', '.', '.', '.', '#'],
        vec!['.', '.', '#', '.', '.', '.'],
        vec!['#', '.', '#', '.', '.', '#'],
        vec!['#', '#', '#', '#', '.', '.'],
    ];


    let f = fs::read_to_string("inputs/day18.txt").unwrap();
    let input: Vec<Vec<char>> = f.lines().map(|s| s.chars().collect()).collect();

    println!("{}", solve(input, 100));

}

fn pretty_print(matrix: &Vec<Vec<char>>) {
    for i in matrix {
        for j in i {
            print!("{}", j);
        }
        print!("\n");
    }
}

fn light_new_state(matrix: &Vec<Vec<char>>, coord: (usize, usize)) -> char {
    let (mut x, mut y) = coord;
    let mut neighbors = vec![];

    let startx = if x > 0 { x-1 } else { x };
    let endx = if x+1 >= matrix.len() { x+1 } else { x+2 };

    let starty = if y > 0 { y-1 } else { y };
    let endy = if y+1 >= matrix.len() { y+1 } else { y+2 };

    for i in startx..endx {
        for j in starty..endy {
            if !(i == x && j == y) {
                neighbors.push(matrix[i][j]);
            }
        }
    }

    neighbors = neighbors
        .into_iter()
        .filter(|&light| light == '#')
        .collect();

    if matrix[x][y] == '#' {
        if neighbors.len() == 2 || neighbors.len() == 3 {
            '#'
        } else {
            '.'
        }
    } else {
        if neighbors.len() == 3 {
            '#'
        } else {
            '.'
        }
    }
}

fn update_matrix(matrix: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut new_matrix: Vec<Vec<char>> = matrix.clone();

    for i in 0..matrix.len() {
        for j in 0..matrix.len() {
            new_matrix[i][j] = light_new_state(&matrix, (i, j));
        }
    }

    new_matrix
}

fn solve(matrix: Vec<Vec<char>>, mut step: usize) -> usize {
    let mut new_matrix = matrix.clone();

    loop {
        if step == 0 {
            break;
        }

        new_matrix = update_matrix(new_matrix);
        step -= 1;
    }

    let mut lit = 0;

    for row in new_matrix {
        for col in row {
            if col == '#' {
                lit += 1;
            }
        }
    }

    lit
}
