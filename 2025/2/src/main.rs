use std::{collections::HashSet, io::stdin};

struct Range {
    start_half: i64,
    end_half: i64,
    start: i64,
    end: i64,
}

impl Range {
    fn add_invalids(&self) -> i64 {
        let mut result = 0;
        for value in self.start_half..=self.end_half {
            let value_str = value.to_string();
            let doubled_value = (value_str.clone() + &value_str).parse::<i64>().unwrap();
            if doubled_value >= self.start && doubled_value <= self.end {
                result += doubled_value;
            }
        }
        result
    }

    fn add_invalids_part2(&self) -> i64 {
        let mut result = 0;
        let start = self.start;
        let mut end = self.end;
        if start.to_string().len() != end.to_string().len() {
            end = (0..start.to_string().len()).map(|_| "9").collect::<String>().parse::<i64>().unwrap();
            result += Range { 
                start_half: 0,
                end_half: 0,
                start: end + 1,
                end: self.end,
            }.add_invalids_part2();
        }

        let mut seen = HashSet::new();
        let start_str = start.to_string();
        let len = start_str.len();
        for rep_len in 1..=len / 2 {
            let rep_start = start_str[0..rep_len].to_owned();
            if start_str.len() % rep_len != 0 {
                continue;
            }
            let rep_count = len / rep_len;

            let mut rep_int = rep_start.parse::<i64>().unwrap();
            let mut current = repeat_times(rep_int.to_string(), rep_count as i64);
            while current <= end {
                if current >= start && !seen.contains(&current) {
                    result += current;
                    seen.insert(current);
                }

                rep_int += 1;
                current = repeat_times(rep_int.to_string(), rep_count as i64);
            }
        }
        result
    }
}

fn repeat_times(s: String, times: i64) -> i64 {
    let mut result_str = String::from("");
    for _ in 0..times {
        result_str = result_str + &s;
    }

    result_str.parse::<i64>().unwrap()
}

fn halve_num(num: i64, is_start: bool) -> i64 {
    let num_str = num.to_string();
    let num_half = &num_str[0..(num_str.len() + if is_start {0} else {1}) / 2];
    if num_half.is_empty() {
        return 1;
    }
    num_half.parse::<i64>().unwrap()
}

fn main() {
    let input = stdin().lines().next().unwrap().unwrap();

    let ranges = input.split(",")
        .map(|range_str| {
            match range_str.split("-")
                .map(|elem| {
                    elem.parse::<i64>().unwrap()
                })
                .collect::<Vec<_>>()
                .as_slice() 
            {
                [start, end] => 
                    Range {
                        start_half: halve_num(*start, true),
                        end_half: halve_num(*end, false),
                        start: *start,
                        end: *end},
                _ => panic!(),
            }
        })
        .collect::<Vec<_>>();

    // Part 1
    let mut result = 0;

    for range in &ranges {
        result += range.add_invalids();
    }

    println!("{result}");

    // Part 2
    result = 0;

    for range in &ranges {
        result += range.add_invalids_part2();
    }

    println!("{result}");
}
