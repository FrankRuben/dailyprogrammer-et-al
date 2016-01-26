input1 = """\
....*
...**
..*.*
.*..*
*****"""

input2 = """\
....**.*..
...*****..
..******..
.********.
**********
.*......*.
.*.**.*.*.
.*.**.*.*.
.*.**...*.
.********."""

input3 = """\
.....***.......
..****.**......
.******.******.
.*.****.**....*
.******.***..**
.******.*******
******.********
.*...**********
.*...**********
.*...**********
.*.*.****..****
.***.****..****
.....****..****
.....****..****
.....****..****"""

def split_rows(text):
    return map(list, text.split("\n"))

def transpose_rows(rows):
    return map(list, zip(*rows))

def count_sets(row, empty='.'):
    return map(len, [ c for c in ''.join(row).split(empty) if c ])

def draw_bonus(msg, row_counts, col_counts, sep=' ', empty='  ', fmt_non_empty="%2d"):
    max_row_sets = max(map(len, row_counts))
    max_col_sets = max(map(len, col_counts))

    print msg
    col_offset = [ empty for _ in range(max_row_sets) ]
    for l in range(max_col_sets):
        i = l - max_col_sets
        print sep.join(col_offset
                       + [ fmt_non_empty % set_count[i + len(set_count)] if i + len(set_count) >= 0 else empty
                           for set_count in col_counts ])
    for set_count in row_counts:
        col_offset = [ empty for _ in range(max_row_sets - len(set_count)) ]
        print sep.join(col_offset + [ fmt_non_empty % c for c in set_count ])

def run(msg, input):
    input_rows = split_rows(input)
    transposed_rows = transpose_rows(input_rows)
    draw_bonus(msg, map(count_sets, input_rows), map(count_sets, transposed_rows))

run("Input 1", input1)
run("\nInput 2", input2)
run("\nInput 3", input3)

# See https://www.reddit.com/r/dailyprogrammer/comments/42lhem/20160125_challenge_251_easy_create_nonogram/
# Output:
# Input 1
#              1  1   
#        1  2  1  1  5
#     1
#     2
#  1  1
#  1  1
#     5
# 
# Input 2
#                                4         
#                    3  4  5  5  2  5      
#              1  7  1  4  4  1  1  1  7  1
#        2  1
#           5
#           6
#           8
#          10
#        1  1
#  1  2  1  1
#  1  2  1  1
#     1  2  1
#           8
# 
# Input 3
#                    2           1                        
#                    3  6        4  2        1  1  1  1   
#              1 10  1  2  6 15  8  9 14  8  6 10 10 11 12
#           3
#        4  2
#        6  6
#  1  4  2  1
#     6  3  2
#        6  7
#        6  8
#        1 10
#        1 10
#        1 10
#  1  1  4  4
#     3  4  4
#        4  4
#        4  4
#        4  4
