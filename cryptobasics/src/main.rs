extern crate cryptobasics;
use std::str;

fn main() {
    cryptobasics::hex2bytes::demo();
    cryptobasics::base64encode::demo();
    cryptobasics::fixedxor::demo();

    // Extra testing code.
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
    let bytes = cryptobasics::hex2bytes::hex2bytes(s);
    println!("Got: {}", str::from_utf8(&bytes).unwrap());
    println!("{}", cryptobasics::base64encode::encode(bytes));
}
