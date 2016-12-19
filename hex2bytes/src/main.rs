fn hex2bytes(s: &str) -> Vec<u8> {
    let mut v: Vec<u8> = vec![0; s.len() / 2];
    for i in 0..s.len() / 2 {
        let sub = &s[i * 2..i * 2 + 2];
        v[i] = u8::from_str_radix(sub, 16).unwrap();
    }
    v
}

fn bytes2file(fname: &str, v: &[u8]) -> std::io::Result<usize> {
    use std::io::prelude::*;
    use std::fs::File;

    let mut buffer = try!(File::create(fname));
    buffer.write(v)
}

fn main() {
    println!("Hello, world!");
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    bytes2file("crust.bin", hex2bytes(s).as_slice()).unwrap();
}
