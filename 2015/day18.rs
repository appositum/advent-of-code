use std::fs;

fn main() {
    let f = fs::read_to_string("inputs/day18.txt").unwrap();
    let mut input: Vec<Vec<char>> = f.lines().map(|s| s.chars().collect()).collect();

    println!("part 1: {}", solve(input.clone(), 100, false));
    let i = input.len()-1;
    input[0][0] = '#';
    input[0][i] = '#';
    input[i][0] = '#';
    input[i][i] = '#';
    println!("part 2: {}", solve(input, 100, true));
}

fn light_new_state(matrix: &Vec<Vec<char>>, coord: (usize, usize), stuck_lights: bool) -> char {
    let (x, y) = coord;
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

    if stuck_lights {
        match (x, y) {
            (0, 0) => {
                return '#';
            },
            (x, 0) => {
                if x == matrix.len()-1 {
                    return '#';
                }
            },
            (0, y) => {
                if y == matrix.len()-1 {
                    return '#';
                }
            },
            (x, y) => {
                if x == matrix.len()-1 && y == matrix.len()-1 {
                    return '#';
                }
            },
        }
    }

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

fn update_matrix(matrix: Vec<Vec<char>>, stuck_lights: bool) -> Vec<Vec<char>> {
    let mut new_matrix: Vec<Vec<char>> = matrix.clone();

    for i in 0..matrix.len() {
        for j in 0..matrix.len() {
            new_matrix[i][j] = light_new_state(&matrix, (i, j), stuck_lights);
        }
    }

    new_matrix
}

fn solve(matrix: Vec<Vec<char>>, mut step: usize, stuck_lights: bool) -> usize {
    let mut new_matrix = matrix.clone();

    loop {
        if step == 0 {
            break;
        }

        new_matrix = update_matrix(new_matrix, stuck_lights);
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
