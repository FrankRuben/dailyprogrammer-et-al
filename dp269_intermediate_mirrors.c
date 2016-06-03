// ============================================================================
// File:            main.c
// Last changed by: NN  on 02-06-2016 22:16:25
// Purpose:         Daily Programmer #269 Intermediate
// ============================================================================

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#define GRID_SZ 13
#define MAX_LEVELS 25
#define BS 'b'            // we'll take 'b' instead of a backslash (simplifying our life in C)
#define SL 's'            // hence we also need a replacement for slash, 's' is good enough

char mirrors[GRID_SZ][GRID_SZ];

void init_mirrors(const char *lines) {
  int cnt = 0;
  for (const char *p = lines; *p; p++, cnt++) {
    mirrors[cnt / GRID_SZ][cnt % GRID_SZ] = *p;
  }
}

char follow_trace_by_col(const int row, const int col, const bool move_down, const unsigned lvl);

char follow_trace_by_row(const int row, const int col, const bool move_right, const unsigned lvl) {
  assert(("Bad row", row >= 0 && row < GRID_SZ));
  assert(("Bad col", (lvl > 0) || (col >= 0 && col < GRID_SZ)));
  assert(("Trapped in a house of mirrors", lvl < MAX_LEVELS));
  const int dir = (move_right ? +1 : -1);
  for (int c = col; c >= 0 && c < GRID_SZ; c += dir) {
    const char m = mirrors[row][c];
    if (m == BS) {
      return follow_trace_by_col(row + dir, c, /*move_down*/ move_right, lvl + 1);
    } else if (m == SL) {
      return follow_trace_by_col(row - dir, c, /*move_down*/ !move_right, lvl + 1);
    }
  }
  return row + (move_right ? 'n' : 'A');
}

char follow_trace_by_col(const int row, const int col, const bool move_down, const unsigned lvl) {
  assert(("Bad row", (lvl > 0 ) || (row >= 0 && row < GRID_SZ)));
  assert(("Bad col", col >= 0 && col < GRID_SZ));
  assert(("Trapped in a house of mirrors", lvl < MAX_LEVELS));
  const int dir = (move_down ? +1 : -1);
  for (int r = row; r >= 0 && r < GRID_SZ; r += dir) {
    const char m = mirrors[r][col];
    if (m == BS) {
      return follow_trace_by_row(r, col + dir, /*move_right*/ move_down, lvl + 1);
    } else if (m == SL) {
      return follow_trace_by_row(r, col - dir, /*move_right*/ !move_down, lvl + 1);
    }
  }
  return col + (move_down ? 'N' : 'a');
}

char decode_char(const char ch) {
  if (ch >= 'a' && ch <= 'm') {
    return follow_trace_by_col(0, ch - 'a', /*move_down*/ true, 0);
  } else if (ch >= 'n' && ch <= 'z') {
    return follow_trace_by_row(ch - 'n', GRID_SZ-1, /*move_right*/ false, 0);
  } else if (ch >= 'A' && ch <= 'M') {
    return follow_trace_by_row(ch - 'A', 0, /*move_right*/ true, 0);
  } else if (ch >= 'N' && ch <= 'Z') {
    return follow_trace_by_col(GRID_SZ-1, ch - 'N', /*move_down*/ false, 0);
  } else {
    return ch;
  }
}

const char *decode(const char *msg, char *const buf) {
  char *buf_ptr = buf;
  for (const char *msg_ptr = msg; *msg_ptr; ) {
    *buf_ptr++ = decode_char(*msg_ptr++);
  }
  return buf;
}

// === main

int main () {
  init_mirrors("\
...bb..sb....\
............b\
...s.........\
......b.....b\
....b........\
..s......s...\
b..s......b..\
.....b.......\
bs...........\
s............\
..........b..\
....bs.......\
...s.......s.");

  const char in[] = "TpmQSjdmZdpoohd";
  char out_buf[sizeof(in)] = { '\0' };
  const char *out = decode(in, out_buf);
  printf("%s -> %s\n", in, out); / TpmQSjdmZdpoohd -> DaolyProgrammer (yes, really - see the challenge's thread)
  
  return 0;
}

// See: https://www.reddit.com/r/dailyprogrammer/comments/4m3ddb/20160601_challenge_269_intermediate_mirror/

/*
 * Local Variables:
 * compile-command: "gcc -std=c99 -O2 main.c -o main && ./main"
 * End:
 */
