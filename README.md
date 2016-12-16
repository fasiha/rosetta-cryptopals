# Cryptopals

## Prelude: stringy hex to raw binary file

### Octave/Matlab
Using Octave, so I can define functions at the command-line instead of saving to a file (Matlab has an archaic one-function-one-file requirement).
~~~octave
function raw = hex2bytes(s)
  if mod(length(s), 2) ~= 0
    error('hex2bytes:nonEvenInput', 'String must be even-length');
  end

  pairs = ext.partition(s, [1 2]); % [👜]
  raw = uint8(cellfun(@hex2dec, pairs));
end

function bytes2file(bytes, fname)
  fid = fopen(fname, 'wb');
  if fid > 0
    fwrite(fid, bytes, 'uint8');
    fclose(fid);
  else
    error('bytes2file:fileError', 'Cannot create file %s', fname);
  end
end

s = '49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d';
bytes2file(hex2bytes(s), 'slood.bin');
~~~

Note [👜]: `partition` is inside the `+ext` directory. This is how Matlab and now Octave do namespaces. (`partition` is [public domain](https://github.com/fasiha/personal-matlab-namespace/blob/master/%2Barf/partition.m).) The exact same thing could be affected with built-in `mat2cell`.

Now check the file with `xxd`, which is a hex dumper that seems to be affiliated with Vim on Homebrew/macOS:
```
$ xxd slood.bin
00000000: 4927 6d20 6b69 6c6c 696e 6720 796f 7572  I'm killing your
00000010: 2062 7261 696e 206c 696b 6520 6120 706f   brain like a po
00000020: 6973 6f6e 6f75 7320 6d75 7368 726f 6f6d  isonous mushroom
```
Looks good!

> Personal sidenote: it took me a long time to appreciate this, despite being married to an embedded engineer—one byte = eight bits = 0 to 255 (unsigned) = *two* hex digits, 0x0 to 0xFF.

That’s why the implementation above throws an exception for odd-sized hex strings: I couldn’t decide what to do with the last single hex digit. It could be padded with zero to the right, or to the left, or it could be omitted, or… Recently I’ve been leaning towards writing functions with very narrow expectations, and throwing errors when those are broken, instead of trying to be “smart”, since my expectation of “smart” is fickle.

### Haskell

~~~haskell
import Data.Word
import qualified Data.ByteString as B

hexStringToInts s = case s of
  x:y:zs -> (read ['0', 'x', x, y] :: Word8) : hexStringToInts zs
  _ -> [] -- [👟]

hexStringToFile str filename = B.writeFile filename . B.pack . hexStringToInts $ str

s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
hexStringToFile s "water.bin"
~~~
Note [👟]: this implementation doesn’t error if given a trailing character. Thanks to this 👟 line, any input that doesn’t have two leading elements is treated as the same, including the empty string and single-character strings.

Interestingly enough, Haskell (IHaskell in Atom via Hydrogen and Jupyter) prints out `ByteString`s as ASCII, so when you do `B.pack . hexStringToInts $ s`, you see `"I'm killing your brain like a poisonous mushroom"` 😂, same as above.

(Sidenote: the above Haskell and Octave implementations were written entirely in [Atom](https://atom.io) with the [Hydrogen](https://atom.io/packages/hydrogen) plugin talking to [Octave](https://github.com/Calysto/octave_kernel) and [IHaskell](https://github.com/gibiansky/IHaskell) Jupyter plugins. No REPLs or interpreters were harmed in the making of these two code snippets!)

### Rust
My first non-trivial Rust adventure!
~~~rust
// included: hex2bytes/src/main.rs

fn hex2bytes(s: &str) -> Vec<u8> {
    let mut v: Vec<u8> = vec![0; s.len() / 2];
    for i in 0..s.len() / 2 {
        let sub = &s[i * 2..i * 2 + 2];
        v[i] = u8::from_str_radix(sub, 16).unwrap();
    }
    v
}

fn bytes2file(fname: &str, v: &[u8]) -> std::io::Result<usize> {
    use std::io::prelude::*;
    use std::fs::File;

    let mut buffer = try!(File::create(fname));
    buffer.write(v)
}

fn main() {
    println!("Hello, world!");
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    bytes2file("crust.bin", hex2bytes(s).as_slice()).unwrap();
}
~~~
With `cargo build && cargo run`, `crust.bin` is created in the `hex2bytes` directory, with the same contents as the above Octave and Haskell implementations. I am sure I’m not doing error handing with `Result` properly (`try!()` and the two `unwrap()`s)—please enlighten me!
