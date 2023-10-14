use std::fs;

fn main() {
    let f = fs::read_to_string("inputs/day09.txt").unwrap();
    let v: Vec<(&str, &str, usize)> = f
        .lines()
        .map(|line| {
            let split_to: Vec<&str> = line.split(" to ").collect();
            let origin: &str = split_to[0];
            let split_eq: Vec<&str> = split_to[1].split(" = ").collect();
            let destination = split_eq[0];
            let distance: usize = split_eq[1].parse().unwrap();
            (origin, destination, distance)
        })
        .collect();

    let mut matrix: Vec<Vec<usize>> = vec![vec![0; 8]; 8];

    for (orig, dest, dist) in &v {
        matrix[loc_to_int(orig)][loc_to_int(dest)] = *dist;
        matrix[loc_to_int(dest)][loc_to_int(orig)] = *dist;
    }

    let mut weight_sums_pt1: Vec<usize> = Vec::new();
    let mut weight_sums_pt2: Vec<usize> = Vec::new();

    // part 1
    for i in 0..8 {
        let mut unvisited = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let mut excluded_indices = Vec::new();
        let mut weight_sum = 0;
        let mut bitmask: Vec<usize> = Vec::new();

        let (mut shortest_edge_index, mut shortest_edge) = smallest(&matrix[i], &excluded_indices);
        weight_sum += shortest_edge;

        bitmask.push(i);
        bitmask.push(shortest_edge_index);
        unvisited.remove(i);
        excluded_indices.push(i);

        loop {
            if unvisited.is_empty() {
                break;
            }

            unvisited.retain(|&i| i != shortest_edge_index);
            excluded_indices.push(shortest_edge_index);
            bitmask.push(shortest_edge_index);

            (shortest_edge_index, shortest_edge) =
                smallest(&matrix[shortest_edge_index], &excluded_indices);

            weight_sum += shortest_edge;
            bitmask.push(shortest_edge_index);

            unvisited.retain(|&i| i != shortest_edge_index);
            excluded_indices.push(shortest_edge_index);
        }
        weight_sums_pt1.push(weight_sum);
    }

    // part 2
    for i in 0..8 {
        let mut unvisited = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let mut excluded_indices = Vec::new();
        let mut weight_sum = 0;
        let mut bitmask: Vec<usize> = Vec::new();

        let (mut longest_edge_index, mut longest_edge) = biggest(&matrix[i], &excluded_indices);
        weight_sum += longest_edge;

        bitmask.push(i);
        bitmask.push(longest_edge_index);
        unvisited.remove(i);
        excluded_indices.push(i);

        loop {
            if unvisited.is_empty() {
                break;
            }

            unvisited.retain(|&i| i != longest_edge_index);
            excluded_indices.push(longest_edge_index);
            bitmask.push(longest_edge_index);

            (longest_edge_index, longest_edge) =
                biggest(&matrix[longest_edge_index], &excluded_indices);

            weight_sum += longest_edge;
            bitmask.push(longest_edge_index);

            unvisited.retain(|&i| i != longest_edge_index);
            excluded_indices.push(longest_edge_index);
        }
        weight_sums_pt2.push(weight_sum);
    }
    println!("part 1: {:?}", weight_sums_pt1.iter().min().unwrap());
    println!("part 2: {:?}", weight_sums_pt2.iter().max().unwrap());
}

fn loc_to_int(loc: &str) -> usize {
    match loc {
        "London" => 0,
        "Dublin" => 1,
        "Belfast" => 2,
        "Faerun" => 0,
        "Tristram" => 1,
        "Tambi" => 2,
        "Norrath" => 3,
        "Snowdin" => 4,
        "Straylight" => 5,
        "AlphaCentauri" => 6,
        "Arbre" => 7,
        &_ => panic!("Invalid location"),
    }
}

fn smallest(array: &Vec<usize>, excluded: &Vec<usize>) -> (usize, usize) {
    let min = array.iter().min().unwrap();
    let mut en: Vec<(usize, usize)> = array.clone().into_iter().enumerate().collect();
    en.retain(|(i, el)| !excluded.contains(i) && el != min);
    *en.iter().min_by(|(i1, a), (i2, b)| a.cmp(b)).unwrap()
}

fn biggest(array: &Vec<usize>, excluded: &Vec<usize>) -> (usize, usize) {
    let mut en: Vec<(usize, usize)> = array.clone().into_iter().enumerate().collect();
    en.retain(|(i, el)| !excluded.contains(i));
    *en.iter().max_by(|(i1, a), (i2, b)| a.cmp(b)).unwrap()
}
