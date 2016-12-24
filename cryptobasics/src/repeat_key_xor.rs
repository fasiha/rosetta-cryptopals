pub fn encode(message: &[u8], key: &[u8]) -> Vec<u8> {
    message.iter().enumerate().map(|(i, c)| c ^ key[i % key.len()]).collect()
}

pub fn bytestohex(v: &[u8]) -> String {
    v.iter().map(|x| format!("{:02x}", x)).collect::<Vec<String>>().concat()
}

pub fn demo() {
    let text = "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal";
    let key = "ICE";

    let encoded = encode(text.as_bytes(), key.as_bytes());

    assert_eq!(bytestohex(&encoded),
               "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272\
a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f");

    // Just go the other way too!
    use hex2bytes::hex2bytes;
    assert_eq!(encoded,
               hex2bytes("0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632\
4272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"));
    println!("repeat_key_xor demo passed!")
}
