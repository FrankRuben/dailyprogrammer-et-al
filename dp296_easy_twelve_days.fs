12 constant nb-days     \ our poem will have paragraphs for that many days
8 constant day-len      \ the maximum length of the ordinal number fo the first 12 days

\             123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678
: "day"    c" first   second  third   FORTH   fifth   sixth   seventh eigth   ninth   tenth   eleventhtwelfth " ;

: .%day ( n [0-based]  -- )
    day-len * "day" 1+ + day-len
    -trailing type space ;

: .day  ( n [1-based] -- )
    1-
    0 max nb-days min
    .%day ;

: .head ( n -- )
    ." On the " .day ." day of Christmas" cr
    ." my true love sent to me:" cr ;

: .gift ( n f -- )
    invert if dup 1 = if ." and" space then then
    dup .
    case
        1  of ." Partridge in a Pear Tree"      endof
        2  of ." Turtle Doves"                  endof
        3  of ." French Hens"                   endof
        4  of ." Calling Birds"                 endof
        5  of ." Golden Rings"                  endof
        6  of ." Geese a Laying"                endof
        7  of ." Swans a Swimming"              endof
        8  of ." Maids a Milking"               endof
        9  of ." Ladies Dancing"                endof
        10 of ." Lords a Leaping"               endof
        11 of ." Pipers Piping"                 endof
        12 of ." Drummers Drumming"             endof
    endcase
    cr ;

: gift-loop ( n -- )
    dup 1 =                     \ n f - flag for .gift, whether we're at the first day's paragraph
    swap                        \ f n
    dup 0 ?do                   \ f n - and now for some stack juggling to add or not add the "and"
        over                    \ f n f
        over                    \ f n f n
        i -                     \ f n f n
        swap                    \ f n n f
        .gift                   \ f n
    loop 
    cr ;

: day-loop
    nb-days 0 ?do
        i 1+ .head
        i 1+ gift-loop
    loop ;

day-loop

\ See: https://www.reddit.com/r/dailyprogrammer/comments/5j6ggm/20161219_challenge_296_easy_the_twelve_days_of/
\ Run with: gforth dp296easy.fs
