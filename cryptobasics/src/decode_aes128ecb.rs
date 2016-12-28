use openssl::symm::{Cipher, Crypter, Mode};

pub fn demo() {
    use base64decode;

    let raw: Vec<u8> = include_str!("../resources/7.txt")
        .as_bytes()
        .iter()
        .map(|&x| x)
        .filter(|&x| x != b'\n')
        .collect();
    let message = base64decode::decode(&raw);

    let key = "YELLOW SUBMARINE".as_bytes();

    let decrypter = Crypter::new(Cipher::aes_128_ecb(), Mode::Decrypt, key, None);
    let mut decrypted = vec![0u8; message.len() + key.len()];
    decrypter.unwrap().update(&message, decrypted.as_mut_slice()).unwrap();

    use std::str;
    let printable = str::from_utf8(&decrypted)
        .unwrap()
        .to_string();

    assert!(printable.starts_with("I'm back and I'm ringin' the bell"));
    // println!("Decoded: {}", printable);

    println!("decode_aes128ecb demo passed!")
}
