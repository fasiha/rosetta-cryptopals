#![feature(advanced_slice_patterns, slice_patterns)]

use std::str;
use std::cmp;

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

// FIXME: Not ideal that this returns a heap-allocated vector.
fn triplet2quad(a0: u8, b0: u8, c0: u8) -> Vec<u8> {
    let lut = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".as_bytes();
    let a = a0 >> 2;
    let b = ((a0 & 0b_0000_0011) << 4) + (b0 >> 4);
    let c = ((b0 & 0b_0000_1111) << 2) + (c0 >> 6);
    let d = c0 & 0b_0011_1111;
    vec![lut[a as usize], lut[b as usize], lut[c as usize], lut[d as usize]]
}

fn encode(bytes: Vec<u8>) -> String {
    let mut out: Vec<u8> = vec!['=' as u8; 4 * ((2 + bytes.len()) / 3)];
    for i in 0..out.len() / 4 {
        let v = &bytes[i * 3..cmp::min(bytes.len(), i * 3 + 3)];
        match v {
            &[x, y, z] => {
                let quad = triplet2quad(x, y, z);
                out[i * 4] = quad[0];
                out[i * 4 + 1] = quad[1];
                out[i * 4 + 2] = quad[2];
                out[i * 4 + 3] = quad[3];
            }
            &[x, y] => {
                let quad = triplet2quad(x, y, 0);
                out[i * 4] = quad[0];
                out[i * 4 + 1] = quad[1];
                out[i * 4 + 2] = quad[2];
            }
            &[x] => {
                let quad = triplet2quad(x, 0, 0);
                out[i * 4] = quad[0];
                out[i * 4 + 1] = quad[1];
            }
            _ => {}
        }
    }
    str::from_utf8(out.as_slice()).unwrap().to_string()
}

fn main() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    bytes2file("crust.bin", hex2bytes(s).as_slice()).unwrap();

    println!("{}", encode(hex2bytes(s)));
    println!("{}", encode(vec![77u8]));
    println!("{}", encode(vec![77u8, 97]));
    println!("{}", encode(vec![77u8, 97, 110]));
}
