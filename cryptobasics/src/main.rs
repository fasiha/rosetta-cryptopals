extern crate cryptobasics;

fn main() {
    cryptobasics::hex2bytes::demo();
    cryptobasics::base64encode::demo();
    cryptobasics::fixedxor::demo();
    cryptobasics::crack_byte_xor::demo();
    cryptobasics::byte_xor_needle_haystack::demo();
    cryptobasics::repeat_key_xor::demo();
    cryptobasics::base64decode::demo();
    cryptobasics::crack_repeat_key_xor::demo();
}
