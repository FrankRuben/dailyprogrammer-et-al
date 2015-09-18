#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_WORD_LEN 31u                // must be odd to compute average word length as (N + 1) / 2
#define NUM_WORDS_PER_LEN 30000         // little cheat: found out word distribution in advance
#define NUM_BUF_CHARS (MAX_WORD_LEN * ((MAX_WORD_LEN + 1) / 2) * NUM_WORDS_PER_LEN)

char word_len_bufs[NUM_BUF_CHARS];
unsigned int word_buf_offset[MAX_WORD_LEN];
unsigned int word_len_idx[MAX_WORD_LEN] = { 0u };

void set_buf_offset(void) {
  size_t i, len_buf_offset = 0;
  word_buf_offset[0] = 0;
  for (i = 1; i < MAX_WORD_LEN; i++) {
    len_buf_offset += i * NUM_WORDS_PER_LEN;
    word_buf_offset[i] = len_buf_offset;
  }
}

char* word_start_pos(const size_t len, const unsigned int idx) {
  const size_t buf_offset = word_buf_offset[len - 1]; // buffer for words of length len starts here
  const size_t word_offset = buf_offset + len * idx;  // word starts here
  return word_len_bufs + word_offset;
}

char* set_word(const size_t len, const unsigned int idx, const char* word) {
  char* target_pos = word_start_pos(len, idx);
  char* const save_pos = target_pos;
  unsigned int i;
  for (i = 0; i < len; i++) {
    assert(*word != '\0');
    *target_pos++ = tolower(*word++);
  }
  assert(*word == '\0');
  return save_pos;
}

char* add_word(char* const word) {
  char safe[MAX_WORD_LEN + 1];          // this one needs to be \0-terminated
  size_t i, len = 0;
  for (i = 0; i <= MAX_WORD_LEN; i++) {
    const char c = word[i];
    if (c == '\0') {
      safe[len] = '\0';
      break;
    }
    if (isalpha(c)) {
      safe[len++] = c;
    }
  }
  assert(len >= 1 && len <= MAX_WORD_LEN && safe[len] == '\0');
  const unsigned int idx = word_len_idx[len - 1]++;
  return set_word(len, idx, safe);
}

int cmp_words(const size_t len1, const unsigned int idx1, const size_t len2, const unsigned int idx2) {
  char* const startPos1 = word_start_pos(len1, idx1);
  char* const startPos2 = word_start_pos(len2, idx2);
  char* const endPos1 = word_start_pos(len1, idx1) + len1 - 1;
  char* const endPos2 = word_start_pos(len2, idx2) + len2 - 1;

  size_t i;
  for (i = 0; i < len1 + len2; i++) {
    const char c1 = (i < len1) ? *(startPos1 + i) : *(startPos2 + i - len1);
    const char c2 = (i < len2) ? *(endPos2 - i) : *(endPos1 - (i - len2));
    if (c1 != c2) {
      return c1 - c2;
    }
  }
  return 0;
}

void print_word_distribution(void) {
  size_t i;
  for (i = 1; i <= MAX_WORD_LEN; i++) {
    printf("%02lu -> %u\n", i, word_len_idx[i-1]);
  }
}

void print_word(const size_t len, const unsigned int idx) {
  assert(len >= 1 && len <= MAX_WORD_LEN);
  assert(idx >= 0 && idx < NUM_WORDS_PER_LEN);
  char* word = word_start_pos(len, idx);
  char* const end = word + len;
  for (; word < end; word++) {
    putchar(*word);
  }
}

int main() {
  set_buf_offset();
  assert(word_len_bufs == word_start_pos(1, 0));

#ifdef TEST
  set_word(strlen("Yell"), 0, "Yell");
  set_word(strlen("alley"), 0, "alley");
  assert(cmp_words(strlen("Yell"), 0, strlen("alley"), 0) == 0);

  set_word(strlen("sex"), NUM_WORDS_PER_LEN-1, "sex");
  set_word(strlen("axes"), NUM_WORDS_PER_LEN-1, "axes");
  assert(cmp_words(strlen("sex"), NUM_WORDS_PER_LEN-1, strlen("axes"), NUM_WORDS_PER_LEN-1) == 0);
#endif

  char buf[1024];
  unsigned int words = 0;
  FILE* const in = fopen("enable1.txt", "r");
  if (in == NULL) {
    perror("File opening failed");
    return EXIT_FAILURE;
  } else {
    while (fgets(buf, sizeof buf, in) != NULL) {
      assert(strlen(buf) <= MAX_WORD_LEN);
      add_word(buf);                    // this assumes we don't have duplicates in enable1.txt
      words++;
    }
    const int read_ok = (feof(in) && !ferror(in));
    fclose(in);
    if (read_ok) {
      const time_t start = time(NULL);
      unsigned long tests = 0ul, hits = 0ul;
      size_t l1, l2;
      unsigned int i1, i2;
      for (l1 = 1; l1 <= MAX_WORD_LEN; l1++) {
        for (i1 = 0; i1 < word_len_idx[l1 - 1]; i1++) {
          for (l2 = 1; l2 <= MAX_WORD_LEN; l2++) {
            for (i2 = 0; i2 < word_len_idx[l2 - 1]; i2++) {
              if ((l1 == l2) && (i2 <= i1)) continue;
              if ((i1 == i2) && (l2 <= l1)) continue;
              tests++;
              if (0 == cmp_words(l1, i1, l2, i2)) {
                hits++;
              }
            }
          }
        }
      }
      printf("[%4.0f:%lu/%lu/%u]\n", difftime(time(NULL), start), hits, tests, words);
    } else {
      perror("File read failed");
      return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}

/*
Result:

gcc -O3 main.c -o main
time ./main
[ 178:6053/28282066971/172820]
177.60user 0.00system 2:57.61elapsed 99%CPU (0avgtext+0avgdata 2228maxresident)k
0inputs+0outputs (0major+598minor)pagefaults 0swaps
*/
