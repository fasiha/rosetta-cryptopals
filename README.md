# Cryptopals

## Prelude: stringy hex to raw binary file

### Octave/Matlab
Using Octave, so I can define functions at the command-line instead of saving to a file (Matlab has an archaic one-function-one-file requirement).
~~~octave
function hex2binfile(s, fname)
  if mod(length(s), 2) ~= 0
    error('hex2binfile:nonEvenInput', 'String must be even-length');
  end

  pairs = ext.partition(s, [1 2]); % [👜]
  raw = cellfun(@(d) uint8(hex2dec(d)), pairs);
  fid = fopen(fname, 'wb');

  if fid > 0
    fwrite(fid, raw, 'uint8');
    fclose(fid);
  else
    error('hex2binfile:fileError', 'Cannot create file %s', fname);
  end
end

s = '49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d';
hex2binfile(s, 'slood.bin');
~~~

Note 👜: `partition` is inside the `+ext` directory. This is how Matlab and now Octave do namespaces. (`partition` is [public domain](https://github.com/fasiha/personal-matlab-namespace/blob/master/%2Barf/partition.m).) The exact same thing could be affected with built-in `mat2cell`.

Now check the file with `xxd`, which is a hex dumper that seems to be affiliated with Vim on Homebrew/macOS:
```
$ xxd slood.bin
00000000: 4927 6d20 6b69 6c6c 696e 6720 796f 7572  I'm killing your
00000010: 2062 7261 696e 206c 696b 6520 6120 706f   brain like a po
00000020: 6973 6f6e 6f75 7320 6d75 7368 726f 6f6d  isonous mushroom
```
Looks good!

Personal sidenote: it took me a long time to appreciate this, despite being married to an embedded engineer—one byte = eight bits = 0 to 255 (unsigned) = *two* hex digits, 0x0 to 0xFF. (That’s why the implementation above throws an exception for odd-sized hex strings: I couldn’t decide what to do with the last single hex digit. It could be padded with zero to the right, or to the left, or it could be omitted, or… Recently I’ve been leaning towards writing functions with very narrow expectations, and throwing errors when those are broken, instead of trying to be “smart”, since my expectation of “smart” is fickle.)
