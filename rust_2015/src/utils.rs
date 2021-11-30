use std::fs::File;
use std::path::Path;
use std::io::Read;

pub fn read_file (file: &str) -> String {
    let s = format!("input/{}.txt", file);
    let path = Path::new( &s);
    let mut file = File::open(path).unwrap();
    let mut str = String::new();
    file.read_to_string(&mut str).unwrap();
    return str
}
