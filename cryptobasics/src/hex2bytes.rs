use std::str;

fn unhex(a: u8) -> u8 {
    match a as char {
        '0'...'9' => a - ('0' as u8),
        'A'...'F' => a - ('A' as u8) + 10,
        'a'...'f' => a - ('a' as u8) + 10,
        _ => panic!("Invalid hex!"),
    }
}

fn parse_hex_hex(a: u8, b: u8) -> u8 {
    unhex(a) * 16 + unhex(b)
}

pub fn hex2bytes(s: &str) -> Vec<u8> {
    s.as_bytes()[..s.len() - s.len() % 2]
        .chunks(2)
        .map(|v: &[u8]| parse_hex_hex(v[0], v[1]))
        .collect()
}

pub fn bytes2file(fname: &str, v: &[u8]) -> ::std::io::Result<usize> {
    use std::io::prelude::*;
    use std::fs::File;

    let mut buffer = try!(File::create(fname));
    buffer.write(v)
}

pub fn demo() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    assert_eq!(String::from_utf8_lossy(&hex2bytes(s)),
               "I'm killing your brain like a poisonous mushroom");
    bytes2file("crust.bin", &hex2bytes(s)).unwrap();
    println!("hex2bytes demo passed! Check crust.bin.");
    ()
}
