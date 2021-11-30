use crate::utils::read_file;

pub fn main() {
    let input = read_file("day01");
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
