fn eng_core(c: u8) -> f32 {
    let space_freq = 13f32;
    let letter_freqs = vec![8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153,
                            0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056,
                            2.758, 0.978, 2.360, 0.150, 1.974, 0.074f32];
    match c as char {
        'A'...'Z' => letter_freqs[(c as usize) - ('A' as usize)],
        'a'...'z' => letter_freqs[(c as usize) - ('a' as usize)],
        ' ' => space_freq,
        _ => 0.0,
    }
}

pub fn crack(message: &[u8]) -> (u8, String) {
    use std::str;

    let bestkey: u8 = (0..128u8)
        .max_by_key(|key| {
            message.iter().map(|c| c ^ key).fold(0f32, |sum, c| sum + 100f32 * eng_core(c)) as isize
        })
        .unwrap();
    let res: Vec<u8> = message.iter().map(|c| c ^ bestkey).collect();
    let decoded = str::from_utf8(res.as_slice()).unwrap().to_string();
    (bestkey, decoded)
}

pub fn demo() {
    use hex2bytes;
    let message = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
    assert_eq!((88u8, "Cooking MC's like a pound of bacon".to_string()),
               crack(&hex2bytes::hex2bytes(message)));
    println!("crack_byte_xor demo passed!");
}
