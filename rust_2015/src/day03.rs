use crate::utils::read_file;
use std::collections::HashMap;

enum Direction {
    North,
    West,
    South,
    East
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Position {x: i32, y: i32}

impl Position {
    pub fn new() -> Position {
        Position {x: 0, y: 0}
    }
}

pub fn main () {
    println!("Day 3");
    let input = read_file("day03");
    let mut map: HashMap<Position, u32> = HashMap::new();
    map.insert(Position::new(), 0);
    let walk: Vec<Direction> = input.chars().map(char_to_dir).collect();

    walk.iter().fold(Position::new(), |pos, dir| doo(&mut map, pos, dir));

    println!("{}", map.len());
    main2();
}

pub fn main2 () {
    println!("Day 3, part 2");
    let input = read_file("day03");
    let mut map: HashMap<Position, u32> = HashMap::new();
    map.insert(Position::new(), 0);
    let walk: Vec<Direction> = input.chars().map(char_to_dir).collect();
    let mut index = 0;
    let (even, odd): (Vec<Direction>, Vec<Direction>) = walk.into_iter()
        .partition(|_| {index += 1; index % 2 == 0});

    even.iter().fold(Position::new(), |pos, dir| doo(&mut map, pos, dir));
    odd.iter().fold(Position::new(), |pos, dir| doo(&mut map, pos, dir));

    println!("{}", map.len());
    println!("{}", even.len());
    println!("{}", odd.len());
}

fn doo (map: &mut HashMap<Position, u32>, Position {x, y}: Position, dir: &Direction ) -> Position {
        let new_pos = match dir {
            Direction::North => Position {x, y: y + 1 },
            Direction::West => Position { x: x - 1, y },
            Direction::South => Position { x, y: y - 1 },
            Direction::East => Position { x: x + 1, y }
        };
        visit_post(map, &new_pos);
        return new_pos
}

fn visit_post (map: &mut HashMap<Position, u32>, pos: &Position) {
    let op_count = map.get_mut(pos);
    if let Some(count) = op_count {
        *count += 1;
    } else {
        map.insert(*pos, 1);
    }
}

fn char_to_dir (char: char) -> Direction {
    match char {
        '^' => Direction::North,
        'v' => Direction::South,
        '>' => Direction::East,
        '<' => Direction::West,
        _   => panic!("Invalid character")
    }
}