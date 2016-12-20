pub fn combine(x: &[u8], y: &[u8]) -> Vec<u8> {
    x.iter().zip(y.iter()).map(|(&l, &r)| l ^ r).collect()
}

pub fn demo() {
    let this = "1c0111001f010100061a024b53535009181c";
    let that = "686974207468652062756c6c277320657965";
    let expected = "746865206b696420646f6e277420706c6179";

    use hex2bytes;
    assert_eq!(String::from_utf8_lossy(&combine(&hex2bytes::hex2bytes(this),
                                                &hex2bytes::hex2bytes(that))),
               String::from_utf8_lossy(&hex2bytes::hex2bytes(expected)));
    println!("fixedxor demo passed!");
    ()
}
