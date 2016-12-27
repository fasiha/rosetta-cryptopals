pub fn onbits(x: u8) -> usize {
    (0..8).map(|n| ((x >> n) & 1u8) as usize).sum()
}

pub fn hamming_distance(s1: &[u8], s2: &[u8]) -> usize {
    s1.iter()
        .zip(s2.iter())
        .map(|(&x, &y)| onbits(x ^ y))
        .sum()
}

pub fn normalized_edit_distance(s: &[u8], keysize: usize) -> f32 {
    (hamming_distance(&s[..keysize], &s[keysize..2 * keysize]) as f32) / (keysize as f32)
}

pub fn normalized_edit_distance2(s: &[u8], keysize: usize, ntimes: usize) -> f32 {
    let num: f32 = s.chunks(2 * keysize)
        .take(ntimes)
        .map(|v| normalized_edit_distance(&v, keysize))
        .sum();
    num / (ntimes as f32)
}

pub fn crack(s: &[u8], keysize: usize) -> String {
    use crack_byte_xor::crack as crack1;

    let nchunks = s.len() / keysize;
    let mut sub = vec![0; nchunks];
    let mut best_keys = vec![0; keysize];
    for keyidx in 0..keysize {
        for chunk in 0..nchunks {
            sub[chunk] = s[chunk * keysize + keyidx];
        }
        best_keys[keyidx] = crack1(&sub);
    }
    use std::str;
    str::from_utf8(best_keys.as_slice()).unwrap().to_string()
}

pub fn demo() {
    use base64decode;
    use repeat_key_xor::codec as xor_encode;
    use std::str;

    let s1: &[u8] = "this is a test".as_bytes();
    let s2: &[u8] = "wokka wokka!!!".as_bytes();
    assert_eq!(hamming_distance(s1, s2), 37);

    // Load data
    let raw: Vec<u8> = include_str!("../resources/6.txt")
        .as_bytes()
        .iter()
        .map(|&x| x)
        .filter(|&x| x != b'\n')
        .collect();
    let message = base64decode::decode(&raw);

    // Find the key
    for keysize in 2..41 {
        println!("{:02}: {:.2} or {:.2}: {}",
                 keysize,
                 normalized_edit_distance(&message, keysize),
                 normalized_edit_distance2(&message, keysize, 2),
                 crack(&message, keysize));
    }

    // Decode the message
    let decoded = str::from_utf8(xor_encode(&message, crack(&message, 29).as_bytes()).as_slice())
        .unwrap()
        .to_string();
    println!("{}", decoded);

    println!("Passed crack_repeat_key_xor demo!");
}
