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
  - [Set 1, Challenge 2: XOR two sequences](#set-1-challenge-2-xor-two-sequences)
    - [Haskell](#haskell-2)
    - [Rust](#rust-2)
  - [Set 1, Challenge 3: crack a one-byte XOR cipher](#set-1-challenge-3-crack-a-one-byte-xor-cipher)
    - [Haskell](#haskell-3)
    - [Rust](#rust-3)
  - [Challenge 4: Detect single-byte-XOR-ciphered in a library](#challenge-4-detect-single-byte-xor-ciphered-in-a-library)
    - [Haskell](#haskell-4)
    - [Rust](#rust-4)
  - [Challenge 5–6: ciphering and breaking repeating-key XOR](#challenge-56-ciphering-and-breaking-repeating-key-xor)
    - [Rust](#rust-5)
  - [Problem 7: decoding AES-128-ECB with key](#problem-7-decoding-aes-128-ecb-with-key)
    - [Rust](#rust-6)
  - [Challenge 8: detect AES in ECB](#challenge-8-detect-aes-in-ecb)
    - [Shell script](#shell-script)

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
// included: cryptobasics/src/hex2bytes.rs
use std::str;

fn unhex(a: u8) -> u8 {
    match a as char {
        '0'...'9' => a - ('0' as u8),
        'A'...'F' => a - ('A' as u8) + 10,
        'a'...'f' => a - ('a' as u8) + 10,
        _ => panic!("Invalid hex!"),
    }
}

fn parse_hex_hex(a: u8, b: u8) -> u8 {
    unhex(a) * 16 + unhex(b)
}

pub fn hex2bytes(s: &str) -> Vec<u8> {
    s.as_bytes()[..s.len() - s.len() % 2]
        .chunks(2)
        .map(|v: &[u8]| parse_hex_hex(v[0], v[1]))
        .collect()
}

pub fn bytes2file(fname: &str, v: &[u8]) -> ::std::io::Result<usize> {
    use std::io::prelude::*;
    use std::fs::File;

    let mut buffer = try!(File::create(fname));
    buffer.write(v)
}

pub fn demo() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    assert_eq!(String::from_utf8_lossy(&hex2bytes(s)),
               "I'm killing your brain like a poisonous mushroom");
    bytes2file("crust.bin", &hex2bytes(s)).unwrap();
    println!("hex2bytes demo passed! Check crust.bin.");
    ()
}
~~~
This `hex2bytes.rs` is a free-standing Rust module, inside the `cryptobasics` crate (included in this repo). With an appropriate `lib.rs`:
~~~rust
// included: cryptobasics/src/lib.rs
pub mod hex2bytes;
~~~
and a `main.rs` like this:
~~~rust
// included: cryptobasics/src/main.rs
extern crate cryptobasics;

fn main() {
    cryptobasics::hex2bytes::demo();
}
~~~
you can run `cargo build && cargo run` inside the `cryptobasics` directory. `hex2bytes::demo()` will create `crust.bin`, with the same contents as the above Octave and Haskell implementations. I am sure I’m not doing error handing with `Result` properly (`try!()` and `unwrap()`)—please enlighten me!

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
I’ve switched to [`rustup`](https://rustup.rs/). First, install `rustup` and run `rustup install nightly`, then use `nightly` to build and run: `rustup run nightly cargo build && rustup run nightly cargo run`. This is so that I can use [slice patterns](https://doc.rust-lang.org/beta/book/slice-patterns.html).

I created a new Rust module for this challenge: it lives at `cryptobasics/src/base64encode.rs` and its `demo()` function at the bottom uses the `hex2bytes` module introduced previously.

~~~rust
// included: cryptobasics/src/base64encode.rs
use std::cmp;
use std::str;

fn triplet2quad(a0: u8, b0: u8, c0: u8) -> [u8; 4] {
    let lut = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".as_bytes();
    let a = a0 >> 2;
    let b = ((a0 & 0b_0000_0011) << 4) + (b0 >> 4);
    let c = ((b0 & 0b_0000_1111) << 2) + (c0 >> 6);
    let d = c0 & 0b_0011_1111;
    [lut[a as usize], lut[b as usize], lut[c as usize], lut[d as usize]]
}

pub fn encode(bytes: Vec<u8>) -> String {
    let mut out: Vec<u8> = vec!['=' as u8; 4 * ((2 + bytes.len()) / 3)];
    for i in 0..out.len() / 4 {
        let v = &bytes[i * 3..cmp::min(bytes.len(), i * 3 + 3)];
        match v { // [👒]
            &[x, y, z] => {
                &out[i * 4..i * 4 + 4].copy_from_slice(&triplet2quad(x, y, z));
            }
            &[x, y] => {
                &out[i * 4..i * 4 + 3].copy_from_slice(&triplet2quad(x, y, 0)[..3]);
            }
            &[x] => {
                &out[i * 4..i * 4 + 2].copy_from_slice(&triplet2quad(x, 0, 0)[..2]);
            }
            _ => {}
        }
    }
    str::from_utf8(out.as_slice()).unwrap().to_string()
}

pub fn demo() {
    use hex2bytes;

    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    assert_eq!(encode(vec![77u8, 97, 110]), "TWFu");
    assert_eq!(encode(vec![77u8, 97]), "TWE=");
    assert_eq!(encode(vec![77u8]), "TQ==");
    assert_eq!(encode(hex2bytes::hex2bytes(s)),
               "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t");
    println!("base64encode demo passed!");
    ()
}
~~~
Note [👒]: observe the similarity between Rust’s pattern match in `encode()` and Haskell’s in `intsToBase64` above—that is *really* special!

To use this, `cryptobasics/src/lib.rs` needs to be:
~~~rust
// included: cryptobasics/src/lib.rs
#![feature(slice_patterns)]
pub mod hex2bytes;
pub mod base64encode;
~~~
With this in place, `main.rs` can be:
~~~rust
// included: cryptobasics/src/main.rs
extern crate cryptobasics;
fn main() {
    cryptobasics::hex2bytes::demo();
    cryptobasics::base64encode::demo();
}
~~~
For sure this is suboptimal in many ways, but I wanted to record here that this nicely balances the functional/expressions-only style of programming with the “dirty” imperative style, per [“Mixing matching, mutation, and moves in Rust”](https://blog.rust-lang.org/2015/04/17/Enums-match-mutation-and-moves.html):

> `match` embraces both imperative and functional styles of programming: you can continue using `break` statements, assignments, et cetera, rather than being forced to adopt an expression-oriented mindset.

(I say this notwithstanding John Carmack’s warning against “multi-paradigm”—if there’s an escape hatch allowed by the language, it will be used and abused in your codebase.)

## Set 1, Challenge 2: XOR two sequences

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

### Rust
The Rust implementation is also quite short! Just the first function above, `combine()`. (The demo/test function `demo()` is below that.) This is a new module, `cryptobasics/src/fixedxor.rs`:
~~~rust
// included: cryptobasics/src/fixedxor.rs
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
~~~
Append `pub mod fixedxor;` to `cryptobasics/src/lib.rs` so the crate knows about this new module, and add `cryptobasics::fixedxor::demo();` to the `main()` function in `cryptobasics/src/main.rs` to exercise the demo. (I’m omitting the details because they follow the two Rust examples above very closely. This Rust crate is included in this repository, so refer to that for complete details!)

So, the Haskell implementation above is magically short thanks to `zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]`. I actually wrote something very similar to the Rust code for Haskell, before helpful Haskell told me to use `zipWith`… Rust could readily have something as flexible as `zipWith` since it clearly can express the same ideas. It’s increasingly surprising how I can achieve highly functional code with a systems language intended to displace C/C++.

## Set 1, Challenge 3: crack a one-byte XOR cipher

### Haskell
~~~haskell
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix
import Data.Char

message = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

xorAll key = B.map (xor key)
decode key = xorAll key . B.pack . hexStringToInts $ message

-- All alphabets
score :: B.ByteString->Int
score s = (s =~ "[a-zA-Z ]" :: Int)

-- Use frequencies!
matchUpperLower lowUp item =
  elem item . sequence [toUpper, toLower] $ lowUp

histogram :: [Char] -> B.ByteString -> [(Char, Int)]
histogram charRange input =
  [ (x,c) | x <- charRange,
            let c = length . filter (matchUpperLower x) . C.unpack $ input]

-- Via http://stackoverflow.com/a/16192050/500207
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

englishLetters = " ETAONRISHDLFCMUGYPWBVKJXQZ"
allRelativeScore = sum . mapInd (\(_, freq) ind -> freq * (26 - ind)) . histogram englishLetters
~~~
Wikipedia has a nice table of relative frequencies of letters in English, including space [(Wikipedia.org)](https://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language) and we can improve our English scorer by using it. Note that the gap between this `allRelativeScore` and the naive any-alphabet scorer `score` is much higher.
~~~haskell
allRelativeScore $ C.pack "qwzxy" -- 19 😪
allRelativeScore $ C.pack "aeiou" -- 100 😮

evaluateScorer score = take 5 . sortOn (\(x,_,_) -> negate x) . map (\n -> let
    str = decode n
    sco = score str
  in (sco, str, n)) $ [0..127]

evaluateScorer allRelativeScore
evaluateScorer score

import Text.Printf
import Data.Char ( isPrint )
tupleToCSV (sco, str, num) =
  printf "| %d | %d | ```%s``` |" num sco (map (\c->if isPrint c then c else '.') . C.unpack $ str)
putStrLn . intercalate "\n" . map tupleToCSV $ evaluateScorer allRelativeScore
putStrLn . intercalate "\n" . map tupleToCSV $ evaluateScorer score
~~~
Here’re the five most English-like results using the frequency-based scoring, `allRelativeScore`:

| key | score | decoded string |
|-----|-------|----------------|
| 88 | 595 | ```Cooking MC's like a pound of bacon``` |
| 95 | 441 | ```Dhhlni`'JD t'knlb'f'whric'ha'efdhi``` |
| 120 | 439 | ```cOOKING.mc.S.LIKE.A.POUND.OF.BACON``` |
| 82 | 430 | ```Ieeacdm*GI-y*fcao*k*ze.dn*el*hkied``` |
| 114 | 430 | ```iEEACDM.gi.Y.FCAO.K.ZE_DN.EL.HKIED``` |

Compare this to the alphabetic `score`:

| key | score | decoded string |
|-----|-------|----------------|
| 88 | 33 | ```Cooking MC's like a pound of bacon``` |
| 95 | 27 | ```Dhhlni`'JD t'knlb'f'whric'ha'efdhi``` |
| 82 | 26 | ```Ieeacdm*GI-y*fcao*k*ze.dn*el*hkied``` |
| 90 | 26 | ```Ammikle"OA%q"nkig"c"rmwlf"md"`caml``` |
| 92 | 26 | ```Gkkomjc$IG#w$hmoa$e$tkqj`$kb$fegkj``` |

I’m noticing a trend here: in Haskell, the core implementations are very, very short 😙!

Note: for the above to work, I needed to do the following:

- Register IHaskell with Jupyter, to be aware of Stack: `ihaskell install --stack`.
- Install `regex-posix`: `stack install regex-posix`.
- Finally, edit `~/.stack/global-project/stack.yaml` to list the following: `resolver: lts-6.2` instead of another `resolver`. There are newer resolvers, but this is the one from IHaskell. Without this, Atom/Hydrogen will instantiate a IHaskell Jupyter session using whatever `resolver` is listed in `~/.stack/global-project/stack.yaml` which did *not* work if I registered IHaskell with Jupyter *with* Stack.

### Rust
~~~rust
// included: cryptobasics/src/crack_byte_xor.rs
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

pub fn xor_score(message: &[u8], key: u8) -> isize {
    message.iter().fold(0f32, |sum, &c| sum + 100f32 * eng_core(c ^ key)) as isize
}

pub fn score(message: &[u8]) -> isize {
    message.iter().fold(0f32, |sum, &c| sum + 100f32 * eng_core(c)) as isize
}

pub fn decode(message: &[u8], key: u8) -> Vec<u8> {
    message.iter().map(|c| c ^ key).collect()
}

pub fn crack(message: &[u8]) -> u8 {
    (0..128u8)
        .max_by_key(|&key| xor_score(message, key))
        .unwrap()
}

pub fn demo() {
    use std::str;
    use hex2bytes;
    let message = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
    let bytes = hex2bytes::hex2bytes(message);
    let best_key = crack(&bytes);
    let best_result = decode(&bytes, best_key);

    assert_eq!((88u8, "Cooking MC's like a pound of bacon"),
               (best_key, str::from_utf8(&best_result).unwrap()));

    println!("crack_byte_xor demo passed!");
}
}
~~~
This shows a better way to do letter relative scoring: use each character’s frequency as its score. Notice how I’ve split the problem into `score` a string, `decode` a string with a given key, and finally, `crack` an input string by finding the key that maximizes the score—this organization is a result of the next challenge. Originally, I had only `crack` which returned the score-maximizing key and the decoded message as a `String` (so, `str::from_utf8` every invocation of `crack`).

## Challenge 4: Detect single-byte-XOR-ciphered in a library
There’s this file, `4.txt`, at page for [set 1, challenge 4](https://cryptopals.com/sets/1/challenges/4).

### Haskell
~~~haskell
import Data.String

decodeString :: String -> Word8 -> B.ByteString
decodeString message key = xorAll key . B.pack . hexStringToInts $ message

mostEnglishy = head . sortOn (negate . allRelativeScore . snd) . zip [0..]

mostEnglishy2 = head . sortOn (negate . allRelativeScore . snd . snd) . zip [0..]

bestMessage = mostEnglishy2 . map (\msg -> mostEnglishy $ map (decodeString msg) [0..127])
~~~
A big to-do: how to combine `mostEnglishy` and `mostEnglishy2`?

With this in place, it’s relatively straightforward to find the answer:
~~~haskell
do
  src <- readFile "4.txt"
  let strings = lines src
  print (bestMessage strings)
-- (170,(53,"Now that the party is jumping\n"))
-- I.e., string 170 (0-indexed) with key 53:
decodeString "7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f" 53
~~~
It’s actually a bit slow in IHaskell/Jupyter/Hydrogen/Atom, so I should try this in its own compiled binary.

### Rust
~~~rust
// included: cryptobasics/src/byte_xor_needle_haystack.rs
use crack_byte_xor::{decode, score, crack};

pub fn search(messages: &[Vec<u8>]) -> (usize, u8) {
    (0..messages.len())
        .map(|idx| (idx, crack(&messages[idx])))
        .max_by_key(|&(idx, key)| score(&decode(&messages[idx], key)))
        .unwrap()
}

pub fn demo() {
    use std::str;
    use hex2bytes::hex2bytes;
    let messages: Vec<_> =
        include_str!("../resources/4.txt").split('\n').map(|x| hex2bytes(x)).collect();

    let (lino, key) = search(&messages);

    assert_eq!((lino, key), (170, 53));
    println!("byte_xor_needle_haystack demo passed!");
    // println!("Line #{}, key {} -> {:?}",
    //          lino,
    //          key,
    //          str::from_utf8(&decode(&messages[lino], key)));
}
~~~
I haven’t been talking about it much because I’ve been having so much fun with Rust, but I have to talk about it—I’m having a ton of fun with Rust. Especially with Atom and `linter-rust` highlighting and displaying compiler errors instantly on every save, I’m getting a sense of the few circumstances a type annotation is needed, and of course using “type holes” ([Kris Jenkins](http://blog.jenkster.com/2016/11/type-bombs-in-elm.html)). This problem and the last I was struggling with iterators, but that concept is coming together. (I still don’t know how to write a function that consumes a slice or an iterator. @stephaneyfx helped with this: https://is.gd/nZOWvh but that’s far from ideal—StackOverflow fodder.) Also, I’m getting better about understanding that iterator functions like `map` or `max_by_key` can take references vs values, and that sometimes Rust is smart enough to hide the difference.

Now, observe the unfortunate loss of symmetry between the inner and outer maximizations that was so evident in the Haskell version above (with `mostEnglishy` and `mostEnglishy2`). I think both languages could benefit from writing a generic `argmax` function?

It is, needless to say, instantaneous, even in debug mode (0.33 s). (0.7 s release mode.)

## Challenge 5–6: ciphering and breaking repeating-key XOR

### Rust
First, given a message and a fixed secret key, encode the message by repeatedly XORing it with the key ([challenge 5](https://cryptopals.com/sets/1/challenges/5)):
~~~rust
// included: cryptobasics/src/repeat_key_xor.rs
pub fn codec(message: &[u8], key: &[u8]) -> Vec<u8> {
    message.iter().enumerate().map(|(i, c)| c ^ key[i % key.len()]).collect()
}

pub fn bytestohex(v: &[u8]) -> String {
    v.iter().map(|x| format!("{:02x}", x)).collect::<Vec<String>>().concat()
}

pub fn demo() {
    let text = "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal";
    let key = "ICE";

    let encoded = codec(text.as_bytes(), key.as_bytes());

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
~~~
Just the first function, `codec()` is needed. This cipher is symmetric so the same function encodes and decodes. The second function `bytestohex()` is for the demo: for some reason, although the first challenge was base64-encoding, this problem gives the answer in hex, instead of base64.

And then, for [challenge 6](https://cryptopals.com/sets/1/challenges/6), Cryptopals gives us a base64-encoded input! They never asked us to write a base64 *decoder*, so I spent most of my time doing that. Having read a lot more of the documentation on [iterators](https://doc.rust-lang.org/std/iter/trait.Iterator.html) and [slices](https://doc.rust-lang.org/std/primitive.slice.html) since I first wrote the `base64encode.rs` module, I was really able to improve first the encoder and then write a short decoder.

Here’s the encoder, rewritten:
~~~rust
// included: cryptobasics/src/base64encode.rs
use std::str;

fn triplet2quad(a0: u8, b0: u8, c0: u8) -> [u8; 4] {
    let lut = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".as_bytes();
    let a = a0 >> 2;
    let b = ((a0 & 0b_0000_0011) << 4) + (b0 >> 4);
    let c = ((b0 & 0b_0000_1111) << 2) + (c0 >> 6);
    let d = c0 & 0b_0011_1111;
    [lut[a as usize], lut[b as usize], lut[c as usize], lut[d as usize]]
}

pub fn encode(bytes: &[u8]) -> String {
    // out has 4 * ceil(bytes.len() / 3.0) elements:
    let mut out: Vec<u8> = vec![b'=' ; 4 * ((2 + bytes.len()) / 3)];
    let initer = bytes.chunks(3);
    {
        let outiter = out.as_mut_slice().chunks_mut(4); // &out is NOT out[..]!
        // TODO: can/should we avoid a match until the last iteration?
        for (v, vo) in initer.zip(outiter) {
            match *v { // [👒]
                [x, y, z] => {
                    vo.copy_from_slice(&triplet2quad(x, y, z));
                }
                [x, y] => {
                    vo[..3].copy_from_slice(&triplet2quad(x, y, 0)[..3]);
                }
                [x] => {
                    vo[..2].copy_from_slice(&triplet2quad(x, 0, 0)[..2]);
                }
                _ => {}
            }
        }
    }
    str::from_utf8(out.as_slice()).unwrap().to_string()
}

pub fn demo() {
    let s = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757\
             368726f6f6d";
    let base64example = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";

    use hex2bytes;
    assert_eq!(encode(&hex2bytes::hex2bytes(s)), base64example);
    assert_eq!(encode(&vec![77u8, 97, 110]), "TWFu");
    assert_eq!(encode(&vec![77u8, 97]), "TWE=");
    assert_eq!(encode(&vec![77u8]), "TQ==");

    println!("base64encode demo passed!");
}
~~~
Thanks to using two iterators, one for the input and one for the output, I no longer have any complicated indexing arithmetic—**hooray** 🙌! This is a huge win for me because this was kind of a litmus test for Rust: in functional languages, from Clojure to Haskell, I love mapping and folding over collections and avoiding hard-to-write, hard-to-believe, and hard-to-check indexing arithmetic. Rust just showed me that it can obviate that also. I will have to analyze the codegen (by diving into x64 assembly) to make sure these magical iterator abstractionas are indeed zero-cost, but I am *very* pleased.

I won’t put the decoder here. It’s in `cryptobasics/src/base64decode.rs` and follows the encoder pretty closely. (It’s actually less complicated than the encoder because we can assume the encoded text has multiple-of-four length.)

With this sorted, finally I was able to test my attack code.
~~~rust
// included: cryptobasics/src/crack_repeat_key_xor.rs
pub fn onbits(x: u8) -> usize {
    (0..8).map(|n| ((x >> n) & 1u8) as usize).sum()
}

pub fn hamming_distance(s1: &[u8], s2: &[u8]) -> usize {
    s1.iter()
        .zip(s2.iter())
        .map(|(&x, &y)| onbits(x ^ y))
        .sum()
}

pub fn normalized_edit_distance(s: &[u8], keysize: usize) -> f32 {
    (hamming_distance(&s[..keysize], &s[keysize..2 * keysize]) as f32) / (keysize as f32)
}

pub fn normalized_edit_distance2(s: &[u8], keysize: usize, ntimes: usize) -> f32 {
    let num: f32 = s.chunks(2 * keysize)
        .take(ntimes)
        .map(|v| normalized_edit_distance(&v, keysize))
        .sum();
    num / (ntimes as f32)
}

pub fn crack(s: &[u8], keysize: usize) -> String {
    // [🌂]
    use crack_byte_xor::crack as crack1;

    let nchunks = s.len() / keysize;
    let mut sub = vec![0; nchunks];
    let mut best_keys = vec![0; keysize];
    for keyidx in 0..keysize {
        for chunk in 0..nchunks {
            sub[chunk] = s[chunk * keysize + keyidx];
        }
        best_keys[keyidx] = crack1(&sub);
    }
    use std::str;
    str::from_utf8(best_keys.as_slice()).unwrap().to_string()
}

pub fn demo() {
    use base64decode;
    use repeat_key_xor::codec as xor_encode;
    use std::str;

    let s1: &[u8] = "this is a test".as_bytes();
    let s2: &[u8] = "wokka wokka!!!".as_bytes();
    assert_eq!(hamming_distance(s1, s2), 37);

    // Load data
    let raw: Vec<u8> = include_str!("../resources/6.txt")
        .as_bytes()
        .iter()
        .map(|&x| x)
        .filter(|&x| x != b'\n')
        .collect();
    let message = base64decode::decode(&raw);

    // Decode the message
    assert_eq!("Terminator X: Bring the noise", crack(&message, 29));
    let printable = str::from_utf8(xor_encode(&message, crack(&message, 29).as_bytes()).as_slice())
        .unwrap()
        .to_string();
    assert!(printable.starts_with("I'm back and I'm ringin' the bell"));
    assert!(printable.ends_with("Play that funky music \n"));

    // Display runtime information?
    if !true {
        // Find the key
        for keysize in 2..41 {
            println!("{:02}: {:.2} or {:.2}: {}",
                     keysize,
                     normalized_edit_distance(&message, keysize),
                     normalized_edit_distance2(&message, keysize, 2),
                     crack(&message, keysize));
        }
        println!("{}", printable);
    }

    println!("Passed crack_repeat_key_xor demo!");
}
~~~
The [problem statement](https://cryptopals.com/sets/1/challenges/6) insisted I use normalized edit distances as some kind of entropy-quantifying metric to decide the key length. Although I got the unit test to work straightforwardly (“wokka wokka!!!”), I settled on the “correct” key length just by inspecting them all. Either I’m implementing the distance calculations incorrectly, or… well, I suppose that’s the only possibility since they assure me their recipe is correct.

(For the record, I don’t really see why the first key-length chunk and the second key-length chunk should have a small edit distance for the correct key length, unless this encodes some kind of ASCII condition?)

I’m mentioning this not to complain but to point out that without the edit distance code, the cracking code, at [🌂], is ~15 lines long. The demo (setting up the data, printing the recovered keys for various key lengths, and finally printing the decoded message with the correct key) is much longer. That is most gratifying.

> Just a quick reminder, to run the code, check it out, and in the `cryptobasics` directory, run `rustup run nightly cargo build --release && rustup run nightly cargo run --release`.

## Problem 7: decoding AES-128-ECB with key
[Problem statement](https://cryptopals.com/sets/1/challenges/7).

### Rust
Using the [`rust-openssl` bindings](https://docs.rs/openssl/0.9.4/openssl/) (see my [example](https://github.com/sfackler/rust-openssl/issues/40#issuecomment-269417798)).

~~~rust
// included: cryptobasics/src/decode_aes128ecb.rs
use openssl::symm::{Cipher, Crypter, Mode};

pub fn decode(message: &[u8], key: &[u8]) -> Vec<u8> {
    let decrypter = Crypter::new(Cipher::aes_128_ecb(), Mode::Decrypt, key, None);
    let mut decrypted = vec![0u8; message.len() + key.len()];
    decrypter.unwrap().update(&message, decrypted.as_mut_slice()).unwrap();
    decrypted
}

pub fn demo() {
    use base64decode;

    let raw: Vec<u8> = include_str!("../resources/7.txt")
        .as_bytes()
        .iter()
        .map(|&x| x)
        .filter(|&x| x != b'\n')
        .collect();
    let message = base64decode::decode(&raw);
    let key = "YELLOW SUBMARINE".as_bytes();
    let decrypted = decode(&message, key);

    use std::str;
    let printable = str::from_utf8(&decrypted)
        .unwrap()
        .to_string();

    assert!(printable.starts_with("I'm back and I'm ringin' the bell"));

    println!("decode_aes128ecb demo passed!")
}
~~~
I didn’t appreciate it before, but crypto is an ugly, ugly world. Are all evolutionary arms races this Hobbesian?

## Challenge 8: detect AES in ECB
Given a bunch of ciphertexts (of same length), find the one that’s been encrypted by AES in ECB mode, with an unknown key. [Problem statement.](https://cryptopals.com/sets/1/challenges/8)

### Shell script
~~~sh
✗ cat 8.txt | sed 's/\(.\{32\}\)/\1\n/g' | sort | uniq -c | sort | tail
1 fe6913515a1e27978827dfd2b44b08d0
1 fe78202740befc9638063f02558db37d
1 fe8566cac240c3bf6628c758feeb458b
1 fec7cb9a60020ce11e2c59323752c542
1 ff05860332fece4a9fe1b49188e7c82f
1 ff43b2ab045a712372ce8f8a229e8452
1 ffb555283e4c07914f82dc6dae5e8f3b
1 ffb8cb0e1b4c4b00e8bb3652fb80bd6b
4 08649af70dc06f4fd5d2d69c744cd283
204

✗ grep -n 08649af70dc06f4fd5d2d69c744cd283 8.txt
133:d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a
~~~
So in this one ciphertext, the same 16-byte (32-hexit) sequence was repeated four times, which gave it away: line 133, with the repeats highlighted is:

d880619740a8a19b7840a8a31c810a3d **08649af70dc06f4fd5d2d69c744cd283** e2dd052f6b641dbf9d11b0348542bb57 **08649af70dc06f4fd5d2d69c744cd283** 9475c9dfdbc1d46597949d9c7e82bf5a **08649af70dc06f4fd5d2d69c744cd283** 97a93eab8d6aecd566489154789a6b03 **08649af70dc06f4fd5d2d69c744cd283** d403180c98c8f6db1f2a3f9c4040deb0 ab51b29933f2c123c58386b06fba186a
