// fn eng_relative_freq_score(s: ::std::slice::Iter<u8>) -> f32 {
// let space_freq = 13f32;
// let letter_freqs = vec![8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153,
// 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056,
// 2.758, 0.978, 2.360, 0.150, 1.974, 0.074f32];
// s.fold(0f32, |sum, &c| {
// sum +
// match c as char {
// 'A'...'Z' => letter_freqs[(c as usize) - ('A' as usize)],
// 'a'...'z' => letter_freqs[(c as usize) - ('a' as usize)],
// ' ' => space_freq,
// _ => 0.0,
// }
// })
// }

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

pub fn crack(message: &[u8]) -> Vec<f32> {
    (0..128u8)
        .map(|key| message.iter().map(|c| c ^ key).fold(0f32, |sum, c| sum + eng_core(c)))
        .collect()
}

pub fn eng_relative_freq_score2(s: &str) -> f32 {
    let space_freq = 13f32;
    let letter_freqs = vec![8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153,
                            0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056,
                            2.758, 0.978, 2.360, 0.150, 1.974, 0.074f32];
    s.as_bytes().iter().fold(0f32, |sum, &c| {
        sum +
        match c as char {
            'A'...'Z' => letter_freqs[(c as usize) - ('A' as usize)],
            'a'...'z' => letter_freqs[(c as usize) - ('a' as usize)],
            ' ' => space_freq,
            _ => 0.0,
        }
    })
}
