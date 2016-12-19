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

fn encode(bytes: Vec<u8>) -> String {
    let lut = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".as_bytes();
    let mut out: Vec<u8> = vec!['=' as u8; 4 * ((2 + bytes.len()) / 3)];
    println!("{} bytes, {} outputs", bytes.len(), out.len());
    for i in 0..out.len() / 4 {
        let v = &bytes[i * 3..cmp::min(bytes.len(), i * 3 + 3)];
        let mut a0 = 0;
        let mut b0 = 0;
        let mut c0 = 0;
        let mut nequals = 0;
        match v {
            &[x, y, z] => {
                a0 = x;
                b0 = y;
                c0 = z;
            }
            &[x, y] => {
                a0 = x;
                b0 = y;
                c0 = 0;
                nequals = 1;
            }
            &[x] => {
                a0 = x;
                b0 = 0;
                c0 = 0;
                nequals = 2;
            }
            _ => {}
        }
        let a = a0 >> 2;
        let b = ((a0 & 0b_0000_0011) << 4) + (b0 >> 4);
        let c = ((b0 & 0b_0000_1111) << 2) + (c0 >> 6);
        let d = c0 & 0b_0011_1111;
        out[i * 4] = lut[a as usize];
        out[i * 4 + 1] = lut[b as usize];
        out[i * 4 + 2] = if nequals >= 2 {
            '=' as u8
        } else {
            lut[c as usize]
        };
        out[i * 4 + 3] = if nequals >= 1 {
            '=' as u8
        } else {
            lut[d as usize]
        };
    }
    println!("{:?}", str::from_utf8(out.as_slice()).unwrap());
    str::from_utf8(out.as_slice()).unwrap().to_string()
}

fn main() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    bytes2file("crust.bin", hex2bytes(s).as_slice()).unwrap();


    let bytes2 = hex2bytes(s);
    let bytes: Vec<u8> = vec![77u8, 97];// , 110u8
    encode(bytes2);
    encode(bytes);

}
