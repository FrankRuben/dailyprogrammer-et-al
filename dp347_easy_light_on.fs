32 constant line-sz     \ expected line length for input format: "start end"
variable curr-sum       \ bitmask, each 1-bit means light is on for 1 time unit

: curr-sum-bit! ( n -- ; set Nth-bit in CURR-SUM )
    1 swap lshift  curr-sum @ or  curr-sum ! ;

: parse-number ( -- n ; parse number from input or abort )
    bl word number? ( n flag ) 0= abort" Not a number" ;

: parse-line ( -- ; parse "start end" time from TIB and update curr-sum )
    parse-number parse-number swap do i curr-sum-bit! loop ;

: parse-stdin ( -- ; parse all lines in stdin, updating CURR-SUM )
    begin  tib line-sz stdin read-line throw
    while  #tib !  0 >in !  parse-line
    repeat #tib @    >in !  drop ;

: pop-count ( n -- n ; return number of 1-bits in N )
    0 swap
    8 cell * 0 do
        dup  1 and  if swap 1+ swap then
        1 rshift
    loop drop ;

parse-stdin
curr-sum @  pop-count . cr

\ See here: https://www.reddit.com/r/dailyprogrammer/comments/7qn07r/20180115_challenge_347_easy_how_long_has_the/

\ Run e.g. as:
\ cat /tmp/bonus_input.txt | gforth dp347_easy_light_on.fs
