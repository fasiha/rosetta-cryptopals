char unhex(char a) {
  if (a >= '0' && a <= '9') {
    return a - '0';
  } else if (a >= 'A' && a <= 'F') {
    return a - 'A' + 10;
  } else if (a >= 'a' && a <= 'f') {
    return a - 'a' + 10;
  }
  return 0xff;
}

char parse_hex_hex(char a, char b) { return 16 * unhex(a) + unhex(b); }

void hex2bytes(const char *const s, int N, char *const out) {
  for (int i = 0; i < N; i += 2) {
    out[i / 2] = parse_hex_hex(s[i], s[i + 1]);
  }
}
