#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static inline int is_male(char c) {
  return islower(c);
}

static inline int is_female(char c) {
  return isupper(c);
}

static inline int couple_idx(char c) {
  if (is_male(c)) return (c - 'a');
  return (c - 'A');
}

static bool is_allowed(char c, const int nb_taken, const int nb_chairs, char *const chairs_taken) {
  if (nb_taken > 0) {
    const char left = chairs_taken[nb_taken - 1];
    if (couple_idx(left) == couple_idx(c)) return false;
  } else {
    const char left = chairs_taken[nb_chairs - 1];
    if (isalpha(left) && couple_idx(left) == couple_idx(c)) return false;
  }
  if (nb_taken == nb_chairs - 1) {
    const char right = chairs_taken[0];
    if (couple_idx(c) == couple_idx(right)) return false;
  } else {
    const char right = chairs_taken[nb_taken + 1];
    if (isalpha(right) && couple_idx(c) == couple_idx(right)) return false;
  }
  return true;
}

static int find_chair(const int nb_taken, const int nb_couples, const int nb_chairs, char *const chairs_taken,
                      int count) {
  if (nb_taken == nb_chairs) {
    return count + 1;
  }
  const char taken_char = chairs_taken[nb_taken];
  if (isalpha(taken_char)) {
    if (is_allowed(taken_char, nb_taken, nb_chairs, chairs_taken)) {
      count = find_chair(nb_taken+1, nb_couples, nb_chairs, chairs_taken, count);
    }
  } else {
    for (char couple_char = 'a'; couple_char < 'a' + nb_couples; couple_char++) {
      bool knows_allowed = false, couple_is_allowed = false;
      if (strchr(chairs_taken, couple_char) == NULL) {
        if (is_allowed(couple_char, nb_taken, nb_chairs, chairs_taken)) {
          chairs_taken[nb_taken] = couple_char;
          count = find_chair(nb_taken+1, nb_couples, nb_chairs, chairs_taken, count);
          chairs_taken[nb_taken] = '\0';
          couple_is_allowed = true;
        } else {
          couple_is_allowed = false;
        }
        knows_allowed = true;
      }
      const char spouse_char = toupper(couple_char);
      if (strchr(chairs_taken, spouse_char) == NULL) {
        if (knows_allowed && !couple_is_allowed) continue;
        if (is_allowed(spouse_char, nb_taken, nb_chairs, chairs_taken)) {
          chairs_taken[nb_taken] = spouse_char;
          count = find_chair(nb_taken+1, nb_couples, nb_chairs, chairs_taken, count);
          chairs_taken[nb_taken] = '\0';
        }
      }
    }
  }
  return count;
}

int main(int argc, char *argv[]) {
  int nb_couples = 5;
  if (argc > 1) {
    nb_couples = atoi(argv[1]);
  }
  if (nb_couples > 0) {
    int nb_chairs = nb_couples * 2;
    char *chairs_taken = calloc(nb_chairs + 1, 1);
    char *save_chairs_taken = malloc(nb_chairs + 1);
    if (argc > 2 && strlen(argv[2]) <= (size_t) nb_chairs) {
      strncpy(chairs_taken, argv[2], nb_chairs);
    } else {
      chairs_taken[0] = 'a';
    }
    for (char *p = chairs_taken; p < chairs_taken + nb_chairs; p++) {
      if (*p == '\0' || !isalpha(*p) || (couple_idx(*p) > nb_couples) || strchr(chairs_taken, *p) < p) {
        *p = '_';
      }
    }
    strncpy(save_chairs_taken, chairs_taken, nb_chairs);

    const int count = find_chair(/*nb_taken*/ 0, nb_couples, nb_chairs, chairs_taken, /*count*/ 0);
    printf("%d couples with initial seats taken as %s will yield %d permutation(s)\n",
           nb_couples, save_chairs_taken, count);
    free(save_chairs_taken);
    free(chairs_taken);
  }

  return EXIT_SUCCESS;
}

// See: https://www.reddit.com/r/dailyprogrammer/comments/5ijb4z/20161215_challenge_295_intermediate_seperated/

/*

$ time ./main 5
5 couples with initial seats taken as a_________ will yield 112512 permutation(s)

real    0m0.080s
user    0m0.076s
sys 0m0.000s

$ time ./main 6
6 couples with initial seats taken as a___________ will yield 12771840 permutation(s)

real    0m3.422s
user    0m3.412s
sys 0m0.004s

$ time ./main 6 "abc___ABC"
6 couples with initial seats taken as abc___ABC___ will yield 2872 permutation(s)

real    0m0.003s
user    0m0.000s
sys 0m0.000s

*/

/*
 * Local Variables:
 * compile-command: "gcc -Wall -Werror -Wno-unused-result -g -Og dp295_intermediate_spouses.c && time ./a.out"
 * End:
 */
