\ --- constants, values, variables

s" /tmp/challenge2.txt" r/o open-file throw
( fid ) constant infile-id
255     constant line-sz

0       value min-x
0       value max-x
0       value min-y
0       value max-y
0       value interval-sz
0       value nb-records

0       value freq[]          \ array of size interval-sz + 1, 1st cell is size
0       value bounds[]        \ array of size interval-sz + 1, 1st cell is size

\ --- helpers

: $array-addr ( start i -- addr ; return address of I-th elem of array at START )
    1+ ( skip size cell ) cells  + ;

: bounds-addr ( i -- addr ; return address of I-th elem of bounds[] array )
    bounds[] swap $array-addr ;

: freq-addr ( i -- addr ; return address of I-th elem of freq[] array )
    freq[] swap $array-addr ;

: freq+! ( i n -- ; incr I-th elem of freq[] array by N )
    swap freq-addr +! ;

: round ( n1 n2 -- q ; return rounded quotient for integer division N1 / N2 )
    dup 1 rshift >r                     \ R: n2/2
    /mod ( rem quot ) swap r> ( quot rem n2/2 ) >= if 1+ then ;

: freq-y> ( i -- n ; return height of I-th frequency interval )
    \ spec: "area of bar is total frequency of all of covered values in range"
    freq-addr @  interval-sz  round ;

: bound> ( i -- n ; return upper bound of I-th interval )
    1+ interval-sz *  min-x 1-  + ;

: nb-intervals> ( -- nb-intervals )
    max-x min-x - 1+ ( delta-x )  interval-sz  /mod
    \ force integer sized intervals, requires "1 40 0 100" in challenge 2:
    ( rem quot ) swap  abort" Bad interval size" ;

: record-freq+! { x y | x-bound -- ; increment interval of X by Y }
    bounds[] @  0  do
        i  bounds-addr @  to x-bound
        x x-bound <= if
            i y freq+!
            leave
        then
    loop ;

: allot-array ( n -- addr ; return address of new array of N cells )
    cells  dup  here >r   allot  r@ swap erase  r> ;

: allot-$array ( n -- addr ; return address of new counted array of N cells )
    dup 1+ allot-array  dup >r  !  r> ;

: make-bounds[] ( n -- initialize bounds[] for N intervals )
    dup   allot-$array to bounds[]
    ( n ) 0 do
        i bound>  i bounds-addr  !
    loop ;

: allot-freq[] ( n -- initialize freq[] for N intervals )
    allot-$array to freq[] ;

\ --- input parsing

defer parse-line

: parse-number ( -- n ; parse number from input or abort )
    bl word number?  ( n flag ) 0= abort" Not a number" ;

: parse-record ( -- )
    parse-number parse-number  record-freq+! ;

: parse-nb-records ( -- )
    parse-number to nb-records
    ['] parse-record is parse-line ;

: parse-interval-sz ( -- )
    parse-number to interval-sz
    nb-intervals>
    dup  allot-freq[]
    make-bounds[]
    ['] parse-nb-records is parse-line ;

: parse-axis ( -- )
    parse-number to min-x   parse-number to max-x
    parse-number to min-y   parse-number to max-y
    ['] parse-interval-sz is parse-line ;

' parse-axis is parse-line              \ file starts with axis definition

: parse-infile ( -- )
    begin
        \ read linewise into TIB, so that we can use standard parse words
        tib line-sz infile-id read-line throw
    while
            #tib !  0 >in !  parse-line
    repeat  #tib @    >in !  drop ;

\ --- histogram gfx

: stars ( n -- ; emit N stars )
    0 do [char] * emit loop ;

: x-line { x-off x-space -- ; draw x-axis, using a slightly different format }
    x-off spaces
    min-x x-off .r
    bounds[] @  0  do
        i bounds-addr @  x-space .r
        space
    loop  cr ;

: hist-line { x-off x-space y -- ; draw single histogram line at Y }
    x-off spaces
    freq[] @  0  do
        y  i freq-y>  <=
        if interval-sz stars else interval-sz spaces then
        space
    loop  cr ;

: hist-lines { x-off x-space -- ; draw histogram }
    ( max-y ) 1 swap
    do
        i x-off .r
        x-off x-space i hist-line
    -1 +loop ;

\ --- main

parse-infile

3     constant num-len
max-y num-len interval-sz  hist-lines
      num-len interval-sz  x-line


\ See: https://www.reddit.com/r/dailyprogrammer/comments/5st2so/20170208_challenge_302_intermediate_ascii/
\ Run with: pforth dp302_intermediate_histogram.fs
