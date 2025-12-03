use std::io::stdin;
use std::collections::HashMap;

fn solve(
    digits: &[u8],
    pos: usize,
    remaining: usize,
    memo: &mut HashMap<(usize, usize), i64>
) -> i64 {
    if remaining == 0 {
        return 0;
    }
    if pos + remaining - 1 >= digits.len() {
        return 0;
    }

    if let Some(&res) = memo.get(&(pos, remaining)) {
        return res;
    }

    let tens = 10_i64.pow((remaining - 1) as u32);

    let mut best = 0;
    for i in pos..digits.len() - remaining + 1 {
        let val = tens * digits[i] as i64;
        best = best.max(val + solve(digits, i + 1, remaining - 1, memo));
    }

    memo.insert((pos, remaining), best);
    best
}
fn main() {
    let mut result = 0;
    let mut result2 = 0;
    stdin().lines().for_each(|line_opt| {
        let digits: Vec<_> = line_opt.unwrap().chars().map(|c| c.to_digit(10).unwrap() as u8).collect();
        let mut memo = HashMap::new();
        result += solve(&digits, 0, 2, &mut memo);
        result2 += solve(&digits, 0, 12, &mut memo);
    });

    println!("{result}");
    println!("{result2}");
}
