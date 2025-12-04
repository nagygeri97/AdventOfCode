use std::io::stdin;

fn main() {
    let grid: Vec<Vec<bool>> = stdin().lines().map(|line_opt| {
        let line = line_opt.unwrap();
        line.chars().map(|c| {
            match c {
                '.' => false,
                '@' => true,
                _ => panic!()
            }
        }).collect()
    }).collect();

    let n = grid.len() as i32;
    let m = grid[0].len() as i32;
    let offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

    let mut result = 0;
    for i in 0..n {
        for j in 0..m {
            if !grid[i as usize][j as usize] {
                continue;
            }
            let count: i32 = offsets.map(|(x, y)| {
                let nx = x + i;
                let ny = y + j;
                if nx >= 0 && nx < n && ny >= 0 && ny < m {
                    return if grid[nx as usize][ny as usize] {1} else {0};   
                }
                0
            }).iter().sum();
            if count < 4 {
                result += 1;
            }
        }
    }
    println!("{result}");

    let mut grid_mut = grid.clone();
    let mut removed = 1;
    let mut result = 0;
    while removed > 0 {
        removed = 0;
        let mut new_grid = grid_mut.clone();
        for i in 0..n {
            for j in 0..m {
                if !grid_mut[i as usize][j as usize] {
                    continue;
                }
                let count: i32 = offsets.map(|(x, y)| {
                    let nx = x + i;
                    let ny = y + j;
                    if nx >= 0 && nx < n && ny >= 0 && ny < m {
                        return if grid_mut[nx as usize][ny as usize] {1} else {0};   
                    }
                    0
                }).iter().sum();
                if count < 4 {
                    new_grid[i as usize][j as usize] = false;
                    removed += 1;
                }
            }
        }
        result += removed;
        grid_mut = new_grid;
    }
    println!("{result}");
}
