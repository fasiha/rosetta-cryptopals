extern "C" {
    fn hex2bytes(s: *const u8, N: i32, out: *mut u8);
}

pub fn demo() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    let mut v: Vec<u8> = vec![0; s.len()/2];
    unsafe {
        hex2bytes(s.as_ptr(), s.len() as i32, v.as_mut_ptr());
    }
    assert_eq!(String::from_utf8_lossy(&v),
               "I'm killing your brain like a poisonous mushroom");

    println!("c_hex2bytes demo passed!");
}
