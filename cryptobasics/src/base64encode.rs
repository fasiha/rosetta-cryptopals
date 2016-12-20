use std::cmp;
use std::str;

// FIXME: Not ideal that this returns a heap-allocated vector.
// Consider https://docs.rs/arrayvec/*/arrayvec/struct.ArrayVec.html
fn triplet2quad(a0: u8, b0: u8, c0: u8) -> Vec<u8> {
    let lut = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".as_bytes();
    let a = a0 >> 2;
    let b = ((a0 & 0b_0000_0011) << 4) + (b0 >> 4);
    let c = ((b0 & 0b_0000_1111) << 2) + (c0 >> 6);
    let d = c0 & 0b_0011_1111;
    vec![lut[a as usize], lut[b as usize], lut[c as usize], lut[d as usize]]
}

pub fn encode(bytes: Vec<u8>) -> String {
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

pub fn demo() {
    use hex2bytes;

    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    assert_eq!(encode(vec![77u8, 97, 110]), "TWFu");
    assert_eq!(encode(vec![77u8, 97]), "TWE=");
    assert_eq!(encode(vec![77u8]), "TQ==");
    assert_eq!(encode(hex2bytes::hex2bytes(s)),
               "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t");
    println!("base64encode demo passed!");
    ()
}
