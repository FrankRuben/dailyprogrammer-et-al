\ === test helpers

false constant [test-inline]

[test-inline] [if]
    : assert     ( i*x t/f -- | abort ) invert abort" assertion failed" ;
    : assert-not ( i*x t/f -- | abort ) abort" not-assertion failed" ;
    : assert-empty depth 0= assert ;
[then]

\ === input data

0 value data-set                        \ 0: sample, 1..4: challenges

data-set 0= [if]                        \ sample
    4 value #tuple
    24 value #steps
    create 0s[]    0   ,    0 ,    0 ,    0 ,
    create init[]  0   ,  653 , 1854 , 4063 ,
[then]

data-set 1 = [if]                       \ challenge 1
    5 value #tuple
    30 value #steps
    create 0s[]    0   ,    0 ,    0 ,    0 ,    0 ,
    create init[]  1   ,    5 ,    7 ,    9 ,    9 ,
[then]

data-set 2 = [if]                       \ challenge 2
    6 value #tuple
    30 value #steps
    create 0s[]    0   ,    0 ,    0 ,    0 ,    0 ,    0 ,
    create init[]  1   ,    2 ,    1 ,    2 ,    1 ,    0 ,
[then]

data-set 3 = [if]                       \ challenge
    6 value #tuple
    30 value #steps
    create 0s[]    0   ,    0 ,    0 ,    0 ,    0 ,    0 ,
    create init[]  10  ,   12 ,   41 ,   62 ,   31 ,   50 ,
[then]

data-set 4 = [if]                       \ challenge
    5 value #tuple
    30 value #steps
    create 0s[]    0   ,    0 ,    0 ,    0 ,    0 ,
    create init[]  10  ,   12 ,   41 ,   62 ,   31 ,
[then]

\ === handling of tuples and history of steps

0 value #history                        \ current line count in history[]
0 value history[][]                     \ matrix of size #steps / #tuple

: 1+#history
    #history 1+ to #history ;

: allot-array                 ( n -- adr ; return address of new array of N cells )
        cells  dup  here >r   allot  r@ swap erase  r> ;

: allot-history[][]           ( n -- ; initialize history matrix for N steps )
        #tuple * allot-array to history[][] ;

: i-history[]-adr             ( i -- adr ; return step address of I-th step )
        #tuple *  cells
        history[][] swap + ;

: c-history[]-line-adr        ( -- adr ; return step ADR of current step )
        #history i-history[]-adr ;

: n-history[]-line-adr        ( -- adr ; return step ADR of next step )
        #history 1+  i-history[]-adr ;

: a-history[]-line-@          ( adr i -- n ; return Ith step value at step ADR )
        cells + @ ;

: a-history[]-line-!          ( adr i n -- set Ith step value at step ADR to N )
        >r  cells +  r> swap ! ;

: .us                         ( u -- ; print U with 2 two digit and leading 0 )
        0 <# # # #> type space ;

: .a-history[]-line           ( adr -- print tuple at step ADR )
        #tuple 0 do
            dup i a-history[]-line-@  .  2 spaces
        loop drop ;

: .history[][]                ( -- ; print history matrix up to current step )
        #history 1+  0  do
            i .us
            i i-history[]-adr .a-history[]-line
            cr
        loop ;

: tuple->history[]            ( src-adr -- ; copy from source SRC-ADR to current step adr )
        #tuple 0 do
            dup i a-history[]-line-@
            c-history[]-line-adr i rot  a-history[]-line-!
        loop drop ;

: c-history[]-line-0?         ( -- t/f ; t if current step has only 0s )
        c-history[]-line-adr
        #tuple 0 do
            dup i a-history[]-line-@
            0<> if drop unloop 0 exit then
        loop drop -1 ;

: i-history[]-line-=?         ( i -- t/f ; t if step I matches current step )
        i-history[]-adr
        #tuple 0 do
            dup                  i a-history[]-line-@
            c-history[]-line-adr i a-history[]-line-@
            <> if drop unloop 0 exit then
        loop drop -1 ;

: c-history[]-line-diff       ( i j -- n ; return absolute difference of step elements i and j )
            >r  c-history[]-line-adr  swap  a-history[]-line-@
            r>  c-history[]-line-adr  swap  a-history[]-line-@
            - abs ;

: c-history[]-line-ducci      ( -- ; Ducci-y values from current step line into next step )
            #tuple 1- 0 do
                n-history[]-line-adr  i
                i  i 1+  c-history[]-line-diff
                a-history[]-line-!
            loop
            n-history[]-line-adr  #tuple 1-
            0  #tuple 1-  c-history[]-line-diff
            a-history[]-line-! ;

\ === minimal testing

#steps allot-history[][]      \ allocate history matrix for #STEPS steps

[test-inline] [if]
    history[][]  0 i-history[]-adr  = assert
    assert-empty
[then]

[test-inline] [if]
    0s[]      tuple->history[]
    c-history[]-line-0?    assert
    0 i-history[]-line-=?  assert
    assert-empty
    1+#history

    init[]    tuple->history[]
    c-history[]-line-0?    assert-not
    1 i-history[]-line-=?  assert
    0 i-history[]-line-=?  assert-not
    assert-empty
    0 to #history
[then]

\ === main processing

0 value done?                 \ stop flag for nested loops
init[]    tuple->history[]    \ copy initial values to 1st history line

: run
        #steps 0 do
            c-history[]-line-ducci
            1+#history

            c-history[]-line-0? if
                ." data set " data-set .
                ." : all elements are 0 in step " #history .
                ." : " #history i-history[]-adr .a-history[]-line cr
                leave
            then

            #history 0 do
                    i i-history[]-line-=? if
                        ." data set " data-set .
                        ." : elements are the same for " #history . ." and " i .
                        ." : " i        i-history[]-adr .a-history[]-line
                        ." ; " #history i-history[]-adr .a-history[]-line cr
                        true to done?
                        leave
                    then
            loop
            done? if leave then
        loop ;

run

[test-inline] [if]
    ." final history: " cr .history[][]
[then]

\ data set 0 : all elements are 0 in step 23 : 0   0   0   0
\ data set 1 : elements are the same for 22 and 7 : 0   0   0   2   2   ; 0   0   0   2   2
\ data set 2 : all elements are 0 in step 2 : 0   0   0   0   0   0
\ data set 3 : elements are the same for 21 and 15 : 1   0   0   1   1   1   ; 1   0   0   1   1   1
\ data set 4 : elements are the same for 29 and 14 : 0   0   0   1   1   ; 0   0   0   1   1

\ See: https://www.reddit.com/r/dailyprogrammer/comments/8sjcl0/20180620_challenge_364_intermediate_the_ducci/
