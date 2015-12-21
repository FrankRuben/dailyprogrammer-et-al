#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// === Decoding

/*
 * The whole algorithm is based around that array of arrays:
 * We're decoding any gG like a binary number, where 'g' == 0 and 'G' == 1.
 * And since we also need to differentiate e.g. 'g' from 'gg', we additionaly need the length
 * of the encoding to be able to define unique encodings.
 * Happily using the length will also greatly simplify the decoding.
 * Note: for simplicity we're not using the len == 0 array.
 */

// We support only encodings up to that Gg length.
#define MAX_LEN 10
#define MAX_IDX (1 << MAX_LEN)

char map_to_decoded_char[MAX_LEN][MAX_IDX];

void print_decoding_map() {
  for (unsigned l = 0; l < MAX_LEN; l++) {
    for (unsigned i = 0; i < MAX_IDX; i++) {
      const char decoded = map_to_decoded_char[l][i];
      if (decoded != '\0') {
        printf("%u, %u -> %c; ", l, i, decoded);
      }
    }
  }
  puts("");
}

const char* parse_encoded(const char* enc_msg, unsigned* pidx, char* opt_decoded_char) {
  unsigned idx = 0;
  const char* pos_in_msg = enc_msg;
  for ( ; *pos_in_msg; ) {
    assert(("Key too long", pos_in_msg - enc_msg < MAX_LEN));
    if (*pos_in_msg == 'g' || *pos_in_msg == 'G') {
      if (*pos_in_msg == 'G') {         // 'G' is our bit 1, whereas 'g' is 0
        idx += 1 << (pos_in_msg - enc_msg);
      }
      pos_in_msg++;
      if (opt_decoded_char != NULL) {   // caller wants to lookup an encoded gG
        const char match = map_to_decoded_char[pos_in_msg - enc_msg][idx];
        if (match) {
          *opt_decoded_char = match;
          break;
        }
      } // else: caller wants to walk over that gG and get its length and index
    } else {
      break;
    }
  }
  *pidx = idx;
  return pos_in_msg;                    // return 1st pos *after* gG
}

const char* parse_key(const char* key, unsigned* pidx) {
  return parse_encoded(key, pidx, NULL);
}

void parse_key_line(const char* key_line) {
  char state = 'K', last_state = '\0', encoded_char = '\0';
  unsigned spaces = 0;
  for (const char* pos_in_key = key_line; *pos_in_key; ) {
    switch (state) {
    case 'K':                           // (K)ey
      encoded_char = *pos_in_key++;
      last_state = state; state = 'S';
      break;
    case 'S':                           // (S)pace
      if isspace(*pos_in_key) {
          spaces++;
          pos_in_key++;
        } else {
          assert(("Missing space", spaces));
          spaces = 0;
          const int last_was_key = last_state == 'K';
          last_state = state;
          state = last_was_key
            ? 'E'                       // before the spaces we had a key, so now parse an encoding
            : 'K';                      // otherwise we'll find a key again
      }
      break;
    case 'E': {                         // (E)ncoded gG
      unsigned idx;
      const char* p_after = parse_key(pos_in_key, &idx);
      map_to_decoded_char[p_after - pos_in_key][idx] = encoded_char;
      pos_in_key = p_after;
      last_state = state; state = 'S';
      break;
    }
    default:
      assert(("Unexpected state", 1 == 0));
    }
  }
}

void decode(const char* enc_msg) {
  for (const char* pos_in_msg = enc_msg; *pos_in_msg; ) {
    unsigned idx;
    char decoded_char = '\0';
    const char* p_after = parse_encoded(pos_in_msg, &idx, &decoded_char);
    if (p_after == pos_in_msg) {        // no gG encoded character
      putchar(*pos_in_msg++);           // print that one literally
    } else if (decoded_char != '\0') {  // found decoding
      putchar(decoded_char);
      pos_in_msg = p_after;
    } else {                            // found gG w/ unknown encoding
      putchar('?');
      pos_in_msg++;
    }
  }
}

// === Part 2 - Encoding, incl. Bonus - Compressed encoding

char map_to_encoded_char[UCHAR_MAX][MAX_LEN];

struct char_freq {
  unsigned cnt;
  char ch;
};

void print_encoding_map() {
  for (char ch = 'A'; ch <= 'z'; ch++) {
    const char* enc = map_to_encoded_char[ch];
    if (enc[0] != '\0') {
      printf("%c -> %s; ", ch, enc);
    }
  }
  puts("");
}

const char* encoding_map_as_key_line(char* key_line_buf, const size_t buf_len) {
  char* pos_in_buf = key_line_buf;
  for (char ch = 'A'; ch <= 'z'; ch++) {
    const char* enc = map_to_encoded_char[ch];
    if (enc[0] != '\0') {
      const size_t len_enc = strlen(enc);
      const size_t bytes_needed = 1 /*next char*/ + 1 /*space*/ + len_enc + 1 /*space*/;
      if (pos_in_buf - key_line_buf + bytes_needed + 1 /* \0 */ < buf_len) {
        *pos_in_buf++ = ch;
        *pos_in_buf++ = ' ';
        memcpy(pos_in_buf, enc, len_enc);
        pos_in_buf+= len_enc;
        *pos_in_buf++ = ' ';
      } else {
        break;
      }
    }
  }
  *pos_in_buf++ = '\0';
  return key_line_buf;
}

int cmp_char_freq(const void* p1, const void* p2) {
  const struct char_freq* pf1 = p1;
  const struct char_freq* pf2 = p2;
  return pf1->cnt < pf2->cnt;           // sort descending
}

const char* calc_char_freq(const char* orig_msg) {
  static struct char_freq cnt_as_struct[UCHAR_MAX]; // make this static to have it 0-initialized
  for (const char* pos_in_msg = orig_msg; *pos_in_msg; pos_in_msg++) {
    const char ch = *pos_in_msg;
    if (isalpha(ch)) {
      cnt_as_struct[ch].ch = ch;                    // during insert we just use the char code as index
      cnt_as_struct[ch].cnt++;
    }
  }

  qsort(cnt_as_struct, UCHAR_MAX, sizeof (struct char_freq), cmp_char_freq);

  static char char_by_freq[UCHAR_MAX];              // chars found more often will have lower indices here
  unsigned i;
  const struct char_freq* pos_in_freq;
  for (i = 0, pos_in_freq = cnt_as_struct; i < UCHAR_MAX && pos_in_freq->cnt > 0; i++, pos_in_freq++) {
    char_by_freq[i] = pos_in_freq->ch;
  }
  return char_by_freq;
}

const char* encode_single(unsigned len, unsigned idx, char* buf, const size_t buf_len) {
  assert(("Bad len", len > 0 && len < MAX_LEN));
  assert(("Idx too large", idx < (1 << len)));
  assert(("buf_len too small", buf_len >= MAX_LEN));

  for (unsigned l = 0; l < len; l++) {
    const unsigned m = 1 << l;
    if (m & idx) {
      buf[l] = 'G';
    } else {
      buf[l] = 'g';
    }
  }
  buf[len] = '\0';
  return buf;
}

size_t calc_encoding(const char* orig_msg) {
  const char* char_by_freq = calc_char_freq(orig_msg);
  const char* pos_in_freq = char_by_freq;

  // how many different chars do we have in the message
  for (pos_in_freq = char_by_freq; *pos_in_freq != '\0'; pos_in_freq++) ;
  const size_t num_diff_chars_in_msg = pos_in_freq - char_by_freq;

  // how many chars do we need to encode them - uncompressed constant size encoding assumed
  size_t num_chars_in_encoded = 0, pow_of_2 = 1;
  while (pow_of_2 < num_diff_chars_in_msg) {
    pow_of_2 <<= 1;
    num_chars_in_encoded++;
  }

  // for each unique character compute its encoding, simply based on its index in the frequency array
  for (pos_in_freq = char_by_freq; *pos_in_freq != '\0'; pos_in_freq++) {
    encode_single(num_chars_in_encoded, pos_in_freq - char_by_freq, map_to_encoded_char[*pos_in_freq], MAX_LEN);
  }
  return num_chars_in_encoded;
}

size_t encode(const char* orig_msg, char* buf, const size_t buf_len) {
  const size_t num_chars_in_encoded = calc_encoding(orig_msg);
  print_encoding_map();

  const char* pos_in_msg = orig_msg;
  char* pos_in_buf = buf;
  for ( ; *pos_in_msg; pos_in_msg++) {
    if (pos_in_buf - buf + num_chars_in_encoded + 1 < buf_len) {
      const char ch = *pos_in_msg;
      if (isalpha(ch)) {
        const char* encoded_single = map_to_encoded_char[ch];
        if (encoded_single[0] != '\0') {
          memcpy(pos_in_buf, encoded_single, num_chars_in_encoded);
          pos_in_buf += num_chars_in_encoded;
        } else {
          *pos_in_buf++ = '?';
        }
      } else {
        *pos_in_buf++ = ch;
      }
    } else {
      break;
    }
  }
  *pos_in_buf++ = '\0';
  return pos_in_msg - orig_msg;
}

// === main

int main () {

#if defined TEST_ENCODE_SINGLE

  char buf[MAX_LEN];
  assert(0 == strcmp(encode_single(1, 0, buf, MAX_LEN), "g"));  // 1 char  -> idx: 0, 1
  assert(0 == strcmp(encode_single(1, 1, buf, MAX_LEN), "G"));
  assert(0 == strcmp(encode_single(2, 0, buf, MAX_LEN), "gg")); // 2 chars -> idx: 0 .. 1|2 -> 0..3
  assert(0 == strcmp(encode_single(2, 1, buf, MAX_LEN), "Gg"));
  assert(0 == strcmp(encode_single(2, 2, buf, MAX_LEN), "gG"));
  assert(0 == strcmp(encode_single(2, 3, buf, MAX_LEN), "GG"));
  assert(0 == strcmp(encode_single(MAX_LEN - 1, 0, buf, MAX_LEN), "ggggggggg"));
  assert(0 == strcmp(encode_single(MAX_LEN - 1, 1, buf, MAX_LEN), "Ggggggggg"));

#elif defined TEST_DECODE_CHALLENGE_1

  const char* key_line = "H GgG d gGg e ggG l GGg o gGG r Ggg w ggg";
  const char* enc_msg = "GgGggGGGgGGggGG, ggggGGGggGGggGg!";

  parse_key_line(key_line);
  print_decoding_map();
  decode(enc_msg);                      // should print "Hello, world!"

#elif defined TEST_DECODE_CHALLENGE_2

  const char* key_line = "a GgG d GggGg e GggGG g GGGgg h GGGgG i GGGGg l GGGGG m ggg o GGg p Gggg r gG y ggG";
  const char* enc_msg = "GGGgGGGgGGggGGgGggG /gG/GggGgGgGGGGGgGGGGGggGGggggGGGgGGGgggGGgGggggggGggGGgG!";

  parse_key_line(key_line);
  print_decoding_map();
  decode(enc_msg);                      // should print "hooray /r/dailyprogrammer!"

#elif defined TEST_ENCODE_DECODE_HELLO

  char enc_buf[10 * 1024];
  char key_line_buf[10 * 1024];
  const char* orig_msg = "Hello, world!";

  const size_t num_encoded = encode(orig_msg, enc_buf, sizeof enc_buf);
  assert(num_encoded == strlen(orig_msg));
  parse_key_line(encoding_map_as_key_line(key_line_buf, sizeof key_line_buf));
  print_decoding_map();
  decode(enc_buf);                      // should print "Hello, world!"

#elif defined TEST_ENCODE_DECODE_SELF

  char orig_msg_buf[500 * 120];
  char enc_msg_buf[MAX_LEN * 500 * 120];
  char key_line_buf[10 * 1024];

  FILE* fp = fopen("main.c", "rb");
  assert(("Cannot fopen", fp));
  const size_t file_len = fread(orig_msg_buf, sizeof(char), sizeof orig_msg_buf, fp);
  assert(("Cannot fread", file_len));

  const size_t num_encoded = encode(orig_msg_buf, enc_msg_buf, sizeof enc_msg_buf);
  assert(("Encoding incomplete", num_encoded == file_len));
  // We don't need the key line here, but we call encoding_map_as_key_line() for its side effect
  parse_key_line(encoding_map_as_key_line(key_line_buf, sizeof key_line_buf));
  print_decoding_map();
  decode(enc_msg_buf);                  // should print content of this source file

  fclose(fp);

#endif

  return 0;
}

// See here: https://www.reddit.com/r/dailyprogrammer/comments/3x3hqa/20151216_challenge_245_intermediate_ggggggg_gggg/
// Results: Just prints this file, when using '-DTEST_ENCODE_DECODE_SELF'.

/*
 * Local Variables:
 * compile-command: "gcc -std=c99 -DTEST_ENCODE_DECODE_SELF -O2 main.c -o main && ./main"
 * End:
 */
