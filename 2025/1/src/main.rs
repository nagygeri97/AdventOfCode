use std::io::stdin;

fn main() {
    let mut turns = Vec::new();
    
    for line in stdin().lines() {
        let unwrapped = line.unwrap();
        let value = unwrapped[1..].parse::<i32>().unwrap();
        if unwrapped.starts_with('R') {
            turns.push(value);
        } else {
            turns.push(-1 * value);
        }
    }

    {
        let mut current= 50;
        let mut result = 0;

        for turn in &turns {
            current += turn;
            current %= 100;
            if current == 0 {
                result += 1;
            } 
        }

        println!("{result}");
    }

    {
        let mut current = 50;
        let mut result = 0;
        let mut prev_zero;

        for turn in &turns {
            prev_zero = current == 0;
            current += turn;
            if current > 0 {
                result += current / 100;
            } else if current == 0 {
                result += 1;
            } else {
                result += -(current / 100) + (if prev_zero {0} else {1});
            }
            current %= 100;
            if current < 0 {
                current += 100;
            }
        }

        println!("{result}")
    }
}
