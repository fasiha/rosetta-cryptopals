#![feature(slice_patterns)]
extern crate openssl;

pub mod hex2bytes;
pub mod base64encode;
pub mod fixedxor;
pub mod crack_byte_xor;
pub mod byte_xor_needle_haystack;
pub mod repeat_key_xor;
pub mod crack_repeat_key_xor;
pub mod base64decode;
pub mod decode_aes128ecb;

pub mod c_hex2bytes;
