true constant [challenge]

[challenge] [if]

    0   constant min-x
    50  constant max-x
    1   constant min-y
    10  constant max-y
    5   constant n-hist

    create hist-values
    0  , 10 , 1 ,
    10 , 20 , 3 ,
    20 , 30 , 6 ,
    30 , 40 , 4 ,
    40 , 50 , 2 ,

[else]

    140 constant min-x
    190 constant max-x
    1   constant min-y
    8   constant max-y
    5   constant n-hist

    create hist-values
    140 , 150 , 1 ,
    150 , 160 , 0 ,
    160 , 170 , 7 ,
    170 , 180 , 6 ,
    180 , 190 , 2 ,

[then]

: hist-clip ( n-idx -- n-idx'' ; return N-IDX clipped to 0..n-hist - 1 )
    n-hist 1- swap  0 max  swap  min ;

: hist-x-low ( i -- x-low ; return interval start of I-th record )
    hist-clip  3 *     cells  hist-values +  @ ;

: hist-x-upp ( i -- x-upp ; return interval end of I-th record)
    hist-clip  3 * 1+  cells  hist-values +  @ ;

: hist-y     ( i -- y ; return frequency of I-th record )
    hist-clip  3 * 2 + cells  hist-values +  @ ;

: 10-exp { n | m -- m ; return M with 10^M being the smallest 10-power >= N }
    n 0 <= if -257 throw then
    n 1- to n  1 to m
    begin
        n 10 / to n
        n 0>
    while
            m 1+ to m
    repeat  m ;

: 10-power { m | n -- where N = 10^M }
    m 0 < if -258 throw then
    1 to n
    m 0=  if n leave then
    m 0 do
        n 10 * to n
    loop  n ;

: x-line { x-off x-space -- }
    x-off spaces
    0 hist-x-low x-space .r
    n-hist 0 do
        space  i hist-x-upp x-space .r
    loop  cr ;

: hist-line { x-space hist-idx -- }
    n-hist 0 do
        x-space spaces
        hist-idx  i hist-y <=
        if [char] * emit else space then
    loop  cr ;

: hist-lines { x-off x-space -- }
    10-exp 10-power 1 swap do
        i x-off .r
        x-space i hist-line
    -1 +loop ;

max-y  1+            10-exp 1+  constant y-axis-len
n-hist 1- hist-x-upp 10-exp     constant x-axis-len

max-y y-axis-len x-axis-len  hist-lines
y-axis-len x-axis-len        x-line


\ See: https://www.reddit.com/r/dailyprogrammer/comments/5st2so/20170208_challenge_302_intermediate_ascii/
\ Run with: pforth dp302_intermediate_histogram.fs
