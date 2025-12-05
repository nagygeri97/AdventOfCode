use std::io::stdin;

fn main() {
    let mut ranges: Vec<(i64, i64)> = Vec::new();
    let mut ids: Vec<i64> = Vec::new();

    let mut seen_blank = false;
    stdin().lines().for_each(|line_opt| {
        let line = line_opt.unwrap();
        if line.is_empty() {
            seen_blank = true;
            return;
        }
        
        if !seen_blank {
            match line.split('-').map(|x| x.parse::<i64>().unwrap()).collect::<Vec<_>>().as_slice() {
                [x, y] => ranges.push((*x, *y)),
                _ => panic!(),
            }
        } else {
            ids.push(line.parse::<i64>().unwrap());
        }

    });

    let mut result = 0;
    ids.iter().for_each(|id| {
        if ranges.iter().any(|(x, y)| {
            x <= id && id <= y
        }) {
            result += 1;
        }
    });

    println!("{result}");

    let mut starts = ranges.iter().map(|(x, _)| x).collect::<Vec<_>>();
    starts.sort();

    let mut ends = ranges.iter().map(|(_, y)| y).collect::<Vec<_>>();
    ends.sort();

    let mut si = 0 as usize;
    let mut ei= 0 as usize;

    let mut active = 0;
    let mut last_start = starts[si];

    let mut result = 0;
    while si < starts.len() && ei < ends.len() {
        if starts[si] <= ends[ei] {
            last_start = last_start.min(starts[si]);
            active += 1;
            si += 1;
        } else {
            active -= 1;

            if active == 0 {
                result += ends[ei] - last_start + 1;
                last_start = starts[si];
            }
            ei += 1;
        }
    }

    if si < starts.len() {
        panic!()
    }

    result += *ends.last().unwrap() - last_start + 1;
    println!("{result}");
    
}
