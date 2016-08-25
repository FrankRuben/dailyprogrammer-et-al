// ============================================================================
// File:            main.c
// Last changed by: NN  on 25-08-2016 02:47:50
// Purpose:         Daily Programmer #280 Easy
// ============================================================================

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int eval_finger(const char finger) {
  switch (finger) {
  case '0': return 0;
  case '1': return 1;
  default: return -1;                   // invalid finger marker
  }
}

int eval_hand(const char* hand, const size_t from, const size_t to) {
  const size_t dir = from < to ? 1 : -1;
  bool found_0 = false;
  size_t i = from;
  int n = 5 * eval_finger(hand[i]);
  do {
    i += dir;
    const int finger_val = eval_finger(hand[i]);
    switch (finger_val) {
    case 0:
      found_0 = true;
      break;
    case 1:
      if (found_0) {                    // found extented finger after curled one,
        return -1;                      // that's invalid
      }
      n += 1;
      break;
    default:
      return finger_val;
    }
  } while (i != to);
  return n;
}

int eval_both_hands(const char* both_hands) {
  const int left_hand = eval_hand(both_hands, 4, 0);   // digits 0..4: LP LR LM LI LT
  const int right_hand = eval_hand(both_hands, 5, 9);  // digits 5..8: RT RI RM RR RP
  if (left_hand < 0 || right_hand < 0) {
    return -1;
  } else {
    return 10 * left_hand + right_hand;
  }
}

int test_both_hands(const char* both_hands, const int exp_val) {
  const int hand_val = eval_both_hands(both_hands);
  if (hand_val == exp_val) {
    printf("%s -> %d OK\n", both_hands, hand_val);
    return 0;
  } else if (hand_val < 0) {
    printf("%s: INVALID\n", both_hands);
    return -1;
  } else {
    printf("%s -> %d; error, expected: %d\n", both_hands, hand_val, exp_val);
    return -1;
  }
}

int main(void) {
  int rc = test_both_hands("0111011100", 37); // Given inputs
  rc    |= test_both_hands("1010010000", -1);
  rc    |= test_both_hands("0011101110", 73);
  rc    |= test_both_hands("0000110000", 55);
  rc    |= test_both_hands("1111110001", -1); // Additional test cases
  rc    |= test_both_hands("0001001000", 11);
  rc    |= test_both_hands("0000000000",  0);
  rc    |= test_both_hands("1111111111", 99);

  return rc == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

// See: https://www.reddit.com/r/dailyprogrammer/comments/4z04vj/20160822_challenge_280_easy_0_to_100_real_quick/

/*
 * Local Variables:
 * compile-command: "gcc -Wall -Werror -O2 main.c -o main && ./main"
 * End:
 */
