extern crate gcc;

fn main() {
    gcc::compile_library("libhex2bytes.a", &["src/hex2bytes.c"]);
}
