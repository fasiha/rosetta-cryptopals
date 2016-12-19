# Cryptopals Crypto Challenge

My literate-programming-style document of solving the [Cryptopals Crypto Challenges](https://cryptopals.com/).

(See also my [Rosetta Euler](https://github.com/fasiha/rosetta-euler), which is in a similar format, solving Project Euler problems.)

<!-- TOC START min:1 max:3 link:true update:true -->
- [Cryptopals Crypto Challenge](#cryptopals-crypto-challenge)
  - [Prelude: stringy hex⟹bytes⟹raw binary file](#prelude-stringy-hexbytesraw-binary-file)
    - [Octave/Matlab](#octavematlab)
    - [Haskell](#haskell)
    - [Rust](#rust)
  - [Bytes⟹Base64](#bytesbase64)
    - [Octave](#octave)
    - [Haskell](#haskell-1)
    - [Rust](#rust-1)
  - [Set 1, Challenge 2](#set-1-challenge-2)
    - [Haskell](#haskell-2)
  - [Set 1, Challenge 3](#set-1-challenge-3)
    - [Haskell](#haskell-3)

<!-- TOC END -->

## Prelude: stringy hex⟹bytes⟹raw binary file

A prelude to [set 1→challenge 1](https://cryptopals.com/sets/1/challenges/1).

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
~~~

Test this:
~~~octave
s = '49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d';
bytes2file(hex2bytes(s), 'slood.bin');
~~~

Note [👜]: `partition` is inside the `+ext` directory. This is how Matlab and now Octave do namespaces. (`partition` is [public domain](https://github.com/fasiha/personal-matlab-namespace/blob/master/%2Barf/partition.m).) With more logic you can do the same thing with built-in `mat2cell`.

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
~~~
First, confirm it works:
~~~haskell
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

## Bytes⟹Base64

Building on that, we can complete [set 1→challenge 1](https://cryptopals.com/sets/1/challenges/1).

### Octave
~~~octave
function c = base64Lookup(n)
  c = char((n <= 25) * ('A' + n) + ... % [🌂]
           (n <= 51 && n > 25) * ('a' + n - 26) + ...
           (n <= 61 && n > 51) * ('0' + n - 52) + ...
           (n == 62) * ('+') + (n == 63) * ('/'));
end

function encoded = bytes2base64(bytes)
% BYTES2BASE64 encodes uint8 array in Base64
  triplets = ext.partition(bytes, [1 3]);
  Npadding = 3 - numel(triplets{end});
  if Npadding > 0
    triplets{end} = [triplets{end} zeros(1, Npadding)];
  end
  % Each triplet of uint8 -> quad of "uint6" -> 2^6=64 element ASCII table
  triplet2quad = @(v) bin2dec(reshape(dec2bin(v, 8)', 6, [])');
  encodeTriplet = @(v) arrayfun(@base64Lookup, triplet2quad(v));

  encoded = cell2mat(cellfun(encodeTriplet, triplets, 'un', 0)')';
  if Npadding > 0
    encoded = strcat(encoded(1 : end - Npadding), ('=') * ones(1, Npadding));
  end
end
~~~
With these core functions, run some tests:
~~~octave
% First, check base64 lookup table:
assert(strcmp('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/', ...
              arrayfun(@base64Lookup, 0:63)), 'error in base64Lookup')

% Then, a few complete Base64 tests, via Wikipedia
assert(strcmp(bytes2base64(uint8([77])), 'TQ=='));
assert(strcmp(bytes2base64(uint8([77 97])), 'TWE='));
assert(strcmp(bytes2base64(uint8([77 97 110])), 'TWFu'));

~~~
Finally, evaluate the solution:
~~~octave
% Solution
assert(strcmp(bytes2base64(hex2bytes(s)), ...
              'SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t'));
disp(bytes2base64(hex2bytes(s)));
~~~
This implementation makes *heavy* use of row-vs-column nuances (see all those `'` transposes), array `reshape` magic, and `dec2bin`/`bin2dec` specifics. If you’re familiar with Octave/Matlab, I think this is a pretty elegant implementation, but someone with even an intermediate familiarity with the language may struggle to get this right if they tried it.

Note [🌂]: the couple of implementations I saw on [Rosetta Code](http://rosettacode.org/wiki/Base64_encode_data#Manual_implementation) used string indexing, and that is probably both more efficient and more straightforward than this overengineered approach. The slight pedagogical advantage here is it shows how Matlab/Octave handles “arithmetic” on `char`s—they work but the result is numeric, and has to be applied to `char()` to get a string back.

If you’re using actual Matlab, you can use this [Base64.m](https://github.com/fasiha/personal-matlab-namespace/blob/master/%2Barf/Base64m.m) wrapper to a Java method, `org.apache.commons.codec.binary.Base64.encodeBase64()` to test this.

### Haskell
~~~haskell
import Data.Bits

lutBase64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

tripletToQuad :: Word8 -> Word8 -> Word8 -> [Char]
tripletToQuad a b c =
  let
    a' = shiftR a 2 .&. 0x3f -- take first 6 bits of a
    b' = shiftL a 4 .&. 0x3f + (shiftR b 4 .&. 0xf) -- last 2 of a + first 4 of b
    c' = shiftL b 2 .&. 0x3f + (shiftR c 6 .&. 0x3) -- last 4 of b + first 2 of c
    d' = c .&. 0x3f -- last 6 of c
  in
    map ((!!) lutBase64 . fromIntegral) [a', b', c', d']

intsToBase64 :: [Word8] -> [Char]
intsToBase64 bs = case bs of
  a:b:c:rest -> tripletToQuad a b c ++ intsToBase64 rest
  a:b:[] -> (init $ tripletToQuad a b 0) ++ "="
  a:[] -> (init . init $ tripletToQuad a 0 0) ++ "=="
  _ -> []
~~~
Problem solution and a handful of tests to confirm correctness for non-multiple-of-3 length inputs:
~~~haskell
intsToBase64 . hexStringToInts $ s
intsToBase64 ([77] :: [Word8])
intsToBase64 ([77, 97] :: [Word8])
intsToBase64 ([77, 97, 110] :: [Word8])
~~~
So. It works. Ain’t pretty. But I 🐷 it.

### Rust
I’ve switched to [`rustup`](https://rustup.rs/). First, run `rustup install nightly`, then use `nightly` to build and run: `rustup run nightly cargo build && rustup run nightly cargo run`. This is so that I can use [slice patterns](https://doc.rust-lang.org/beta/book/slice-patterns.html).

~~~rust
// included: hex2base64/src/main.rs
// NEEDED! Commented for Markdown TOC // #![feature(slice_patterns)]

use std::str;
use std::cmp;

fn hex2bytes(s: &str) -> Vec<u8> {
    let mut v: Vec<u8> = vec![0; s.len() / 2];
    for i in 0..s.len() / 2 {
        let sub = &s[i * 2..i * 2 + 2];
        v[i] = u8::from_str_radix(sub, 16).unwrap();
    }
    v
}

// FIXME: Not ideal that this returns a heap-allocated vector.
fn triplet2quad(a0: u8, b0: u8, c0: u8) -> Vec<u8> {
    let lut = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".as_bytes();
    let a = a0 >> 2;
    let b = ((a0 & 0b_0000_0011) << 4) + (b0 >> 4);
    let c = ((b0 & 0b_0000_1111) << 2) + (c0 >> 6);
    let d = c0 & 0b_0011_1111;
    vec![lut[a as usize], lut[b as usize], lut[c as usize], lut[d as usize]]
}

fn encode(bytes: Vec<u8>) -> String {
    let mut out: Vec<u8> = vec!['=' as u8; 4 * ((2 + bytes.len()) / 3)];
    for i in 0..out.len() / 4 {
        let v = &bytes[i * 3..cmp::min(bytes.len(), i * 3 + 3)];
        match v {
            &[x, y, z] => {
                let quad = triplet2quad(x, y, z);
                out[i * 4] = quad[0];
                out[i * 4 + 1] = quad[1];
                out[i * 4 + 2] = quad[2];
                out[i * 4 + 3] = quad[3];
            }
            &[x, y] => {
                let quad = triplet2quad(x, y, 0);
                out[i * 4] = quad[0];
                out[i * 4 + 1] = quad[1];
                out[i * 4 + 2] = quad[2];
            }
            &[x] => {
                let quad = triplet2quad(x, 0, 0);
                out[i * 4] = quad[0];
                out[i * 4 + 1] = quad[1];
            }
            _ => {}
        }
    }
    str::from_utf8(out.as_slice()).unwrap().to_string()
}

fn main() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    println!("{}", encode(hex2bytes(s)));
    println!("{}", encode(vec![77u8]));
    println!("{}", encode(vec![77u8, 97]));
    println!("{}", encode(vec![77u8, 97, 110]));
}
~~~

For sure this is suboptimal in many ways, but I wanted to record here that this nicely balances the functional/expressions-only style of programming with the “dirty” imperative style, per [“Mixing matching, mutation, and moves in Rust”](https://blog.rust-lang.org/2015/04/17/Enums-match-mutation-and-moves.html):

> `match` embraces both imperative and functional styles of programming: you can continue using `break` statements, assignments, et cetera, rather than being forced to adopt an expression-oriented mindset.

(I say this notwithstanding John Carmack’s warning against “multi-paradigm”—if there’s an escape hatch allowed by the language, it will be used and abused in your codebase.)

## Set 1, Challenge 2

Onto [set 1, challenge 2](https://cryptopals.com/sets/1/challenges/2)!

> Write a function that takes two equal-length buffers and produces their XOR combination.

### Haskell

~~~haskell
import Data.List
import Data.Bits
import qualified Data.ByteString as B

xorWord8s = zipWith xor
~~~
Behold, the shortest implementation. Thanks to pair programming 💚. And type hints/linter 💖. It works:
~~~haskell
string1 = "1c0111001f010100061a024b53535009181c"
string2 = "686974207468652062756c6c277320657965"
expected = "746865206b696420646f6e277420706c6179"
hexStringToInts expected == xorWord8s
  (hexStringToInts string1)
  (hexStringToInts string2)
B.pack . hexStringToInts $ expected
~~~

## Set 1, Challenge 3

### Haskell
~~~haskell
import Text.Regex.Posix

message = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

xorAll key = B.map (xor key)
decode key = xorAll key . B.pack . hexStringToInts $ message
score key = (decode key =~ "[a-zA-Z0-9 ]" :: Int) - (decode key =~ "[^a-zA-Z0-9 ]" :: Int)
~~~
I’m noticing a trend here: in Haskell, the core implementations are about as long as the testing code 😙:
~~~haskell
take 5 . sortOn (\(_,x,_) -> -1*x) $ zip3 [0..127] (map score [0..127]) (map decode [0..127])
~~~
A little bit messy, but that was fun!

Note: for the above to work, I needed to do the following:

- Register IHaskell with Jupyter, to be aware of Stack: `ihaskell install --stack`.
- Install `regex-posix`: `stack install regex-posix`.
- Finally, edit `~/.stack/global-project/stack.yaml` to list the following: `resolver: lts-6.2` instead of another `resolver`. There are newer resolvers, but this is the one from IHaskell. Without this, Atom/Hydrogen will instantiate a IHaskell Jupyter session using whatever `resolver` is listed in `~/.stack/global-project/stack.yaml` which did *not* work if I registered IHaskell with Jupyter *with* Stack.
