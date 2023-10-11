use std::collections::HashMap;
use std::fs;

fn main() {
    let f = fs::read_to_string("inputs/day7.txt").unwrap();
    let mut input: Vec<Vec<&str>> = f
        .lines()
        .map(|c| c.split(" -> ").collect::<Vec<&str>>())
        .collect();

    let mut result = populate(&input, HashMap::new());

    while input.len() > result.len() {
        result = populate(&input, result);
    }

    let wire_a = result.get("a").unwrap().to_string();
    let pos = input.iter().position(|x| x[1] == "b").unwrap();
    input[pos][0] = &wire_a;

    result = populate(&input, HashMap::new());

    while input.len() > result.len() {
        result = populate(&input, result);
    }

    println!("wire a: {:?}", result.get("a").unwrap());
}

fn populate<'a>(
    input: &Vec<Vec<&'a str>>,
    mut values: HashMap<&'a str, u16>,
) -> HashMap<&'a str, u16> {
    for i in input {
        match i[0].split(' ').collect::<Vec<&str>>()[..] {
            [var] => {
                if let Ok(number) = var.parse::<u16>() {
                    values.insert(i[1], number);
                } else {
                    if let Some(val) = values.get(&var) {
                        values.insert(i[1], *val);
                    }
                }
            }
            ["NOT", x] => {
                if let Ok(number) = x.parse::<u16>() {
                    values.insert(i[1], !number);
                } else {
                    if let Some(val) = values.get(&x) {
                        values.insert(i[1], !val);
                    }
                }
            }
            [x, "AND", y] => {
                if let Some((valx, valy)) = eval(x, y, &values) {
                    values.insert(i[1], valx & valy);
                }
            }
            [x, "OR", y] => {
                if let Some((valx, valy)) = eval(x, y, &values) {
                    values.insert(i[1], valx | valy);
                }
            }
            [x, "LSHIFT", y] => {
                if let Some((valx, valy)) = eval(x, y, &values) {
                    values.insert(i[1], valx << valy);
                }
            }
            [x, "RSHIFT", y] => {
                if let Some((valx, valy)) = eval(x, y, &values) {
                    values.insert(i[1], valx >> valy);
                }
            }
            _ => {
                panic!("opora");
            }
        }
    }

    values
}

fn eval(x: &str, y: &str, values: &HashMap<&str, u16>) -> Option<(u16, u16)> {
    match (x.parse::<u16>(), y.parse::<u16>()) {
        (Ok(valx), Ok(valy)) => {
            return Some((valx, valy));
        }
        (Ok(valx), Err(_)) => {
            if let Some(valy) = values.get(&y) {
                return Some((valx, *valy));
            }
        }
        (Err(_), Ok(valy)) => {
            if let Some(valx) = values.get(&x) {
                return Some((*valx, valy));
            }
        }
        _ => {
            if let (Some(valx), Some(valy)) = (values.get(&x), values.get(&y)) {
                return Some((*valx, *valy));
            }
        }
    }

    None
}
