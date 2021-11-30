use crate::utils::read_file;
use std::cmp::min;

struct Box {
    l: i32,
    w: i32,
    h: i32
}

pub fn main() {
    println!("Day 2!");
    let input = read_file("day02");
    let boxes: Vec<Box> = input.split("\n")
        .map(|s| {
            let b: Vec<i32> = s.split("x").map(|s| s.parse::<i32>().unwrap()).collect();
            let (l, w, h) = (b[0], b[1], b[2]);
            Box {l, w, h}
        }).collect();
    let boxes_paper: Vec<i32> = boxes.iter().map(
        |Box {l, w, h}| {
            // println!("{}, {}, {}", l, w, h);
            2*l*w + 2*w*h + 2*h*l + min(min(l*w, w*h), h*l)
        }
    ).collect();
    let total_paper: i32 = boxes_paper.iter().sum();
    println!("Total paper required: {}", total_paper);
    let ribbon: i32 = boxes.iter().map(|b| box_ribbon(b)).sum();
    println!("Total ribbon required: {}", ribbon);
}

fn box_ribbon (&Box {w, l, h}: &Box) -> i32 {
    let mut vec: Vec<i32> = vec![w, l, h];
    vec.sort();
    let dist = vec[0] * 2 + vec[1] * 2;
    let bow = w * l * h;
    dist + bow
}