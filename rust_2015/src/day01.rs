use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub fn main() {
    let input = read_file("input/day01.txt");
    let mut index = 0;
    let mut final_index = 0;
    let count = input.chars().fold(0, |count, char| {
        if count == -1 && final_index == 0 {
            final_index = index;
        }
        index += 1;
        return if char == '(' {count + 1 } else {count - 1 }
    });
    println!("{}", final_index);
    println!("{}", count);
}

fn read_file (path: &str) -> String {
    let mut file = File::open(Path::new(path)).unwrap();
    let mut str = String::new();
    file.read_to_string(&mut str).unwrap();
    return str
}
