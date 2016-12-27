fn base64byte_to_idx(a: u8) -> u8 {
    match a as char {
        'A'...'Z' => a - ('A' as u8),
        'a'...'z' => a - ('a' as u8) + 26,
        '0'...'9' => a - ('0' as u8) + 52,
        '+' => 62,
        '/' => 63,
        _ => panic!("Invalid base64 character!"),
    }
}

fn quad2triplet(a0: u8, b0: u8, c0: u8, d0: u8) -> [u8; 3] {
    let a = base64byte_to_idx(a0);
    let b = base64byte_to_idx(b0);
    let c = base64byte_to_idx(c0);
    let d = base64byte_to_idx(d0);
    let x = (a << 2) + (b >> 4); // all 6 bits of a, first 2 of b
    let y = (b << 4) + (c >> 2); // last 4 bits of b, first 4 of c
    let z = (c << 6) + d; // last 2 of c, all 6 of d
    [x, y, z]
}

pub fn decode(s: &[u8]) -> Vec<u8> {
    // `s.len()` is guaranteed to be multiple of 4.
    let mut out: Vec<u8> = vec![0; s.len() / 4 * 3];
    for (v, vo) in s.chunks(4).zip(out.as_mut_slice().chunks_mut(3)) {
        // println!("v: {:?}", v);
        vo.copy_from_slice(&quad2triplet(v[0],
                                         v[1],
                                         if v[2] == b'=' { b'A' } else { v[2] },
                                         if v[3] == b'=' { b'A' } else { v[3] }))
    }
    if s.len() > 0 {
        if s[s.len() - 1] == b'=' {
            out.pop();
        }
        if s[s.len() - 2] == b'=' {
            out.pop();
        }
    }
    out
}

pub fn demo() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    let base64example = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";

    use hex2bytes;

    assert_eq!(hex2bytes::hex2bytes(s), decode(base64example.as_bytes()));
    assert_eq!(vec![77u8, 97, 110], decode("TWFu".as_bytes()));
    assert_eq!(vec![77u8, 97], decode("TWE=".as_bytes()));
    assert_eq!(vec![77u8], decode("TQ==".as_bytes()));

    println!("base64decode demo passed!");
}
