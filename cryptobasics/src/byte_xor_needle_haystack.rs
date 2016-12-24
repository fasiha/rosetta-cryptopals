use crack_byte_xor::{decode, score, crack};

pub fn search(messages: &[Vec<u8>]) -> (usize, u8) {
    (0..messages.len())
        .map(|idx| (idx, crack(&messages[idx])))
        .max_by_key(|&(idx, key)| score(&decode(&messages[idx], key)))
        .unwrap()
}

pub fn demo() {
    use std::str;
    use hex2bytes::hex2bytes;
    let messages: Vec<_> =
        include_str!("../resources/4.txt").split('\n').map(|x| hex2bytes(x)).collect();

    let (lino, key) = search(&messages);

    assert_eq!((lino, key), (170, 53));
    println!("byte_xor_needle_haystack demo passed!");
    // println!("Line #{}, key {} -> {:?}",
    //          lino,
    //          key,
    //          str::from_utf8(&decode(&messages[lino], key)));
}
