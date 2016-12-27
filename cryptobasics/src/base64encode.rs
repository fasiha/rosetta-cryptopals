use std::str;

fn triplet2quad(a0: u8, b0: u8, c0: u8) -> [u8; 4] {
    let lut = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".as_bytes();
    let a = a0 >> 2;
    let b = ((a0 & 0b_0000_0011) << 4) + (b0 >> 4);
    let c = ((b0 & 0b_0000_1111) << 2) + (c0 >> 6);
    let d = c0 & 0b_0011_1111;
    [lut[a as usize], lut[b as usize], lut[c as usize], lut[d as usize]]
}

pub fn encode(bytes: &[u8]) -> String {
    // out has 4 * ceil(bytes.len() / 3.0) elements:
    let mut out: Vec<u8> = vec![b'=' ; 4 * ((2 + bytes.len()) / 3)];
    let initer = bytes.chunks(3);
    {
        let outiter = out.as_mut_slice().chunks_mut(4); // &out is NOT out[..]!
        // TODO: can/should we avoid a match until the last iteration?
        for (v, vo) in initer.zip(outiter) {
            match *v { // [👒]
                [x, y, z] => {
                    vo.copy_from_slice(&triplet2quad(x, y, z));
                }
                [x, y] => {
                    vo[..3].copy_from_slice(&triplet2quad(x, y, 0)[..3]);
                }
                [x] => {
                    vo[..2].copy_from_slice(&triplet2quad(x, 0, 0)[..2]);
                }
                _ => {}
            }
        }
    }
    str::from_utf8(out.as_slice()).unwrap().to_string()
}


pub fn demo() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    let base64example = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";

    use hex2bytes;
    assert_eq!(encode(&hex2bytes::hex2bytes(s)), base64example);
    assert_eq!(encode(&vec![77u8, 97, 110]), "TWFu");
    assert_eq!(encode(&vec![77u8, 97]), "TWE=");
    assert_eq!(encode(&vec![77u8]), "TQ==");

    println!("base64encode demo passed!");
}
