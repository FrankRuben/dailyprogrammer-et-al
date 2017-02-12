\ Note: "Cell" below means the Forth cell, not the cellular automaton cell.
\       The position of an automaton cell w/in a line is named col-idx.

: dot  ( -- ) [char] . emit ;
: star ( -- ) [char] * emit ;

: bit-mask ( n -- m ) 1 swap lshift ;

cell 8 * constant cell-bits
variable tape-width
variable generation-rule

: tape-center@ ( -- n )
    tape-width @ 2/ ;

: cell-mod ( col-idx -- quot rem )
    \ quot: offset in cell-units
    \ rem:  bit-offset within cell
    cell-bits /mod   swap ;

: tape-width-in-cells@ ( -- len-in-cells )
    tape-width @   cell-mod             \ quot rem
    \ quot: number of cells with all bits used for tape line
    \ rem:  number of bits used in last cell
    0= invert if 1+ then ;

: tape-line-addr ( line-addr col-idx -- rem cell-addr )
    cell-mod  -rot cells + ;

: tape-line-cell@ ( line-addr col-idx -- t/f )
    tape-line-addr                      \ rem cell-addr
    @ swap   bit-mask   and 0= invert ;

: tape-line-cell! ( t/f line-addr col-idx -- )
    tape-line-addr                      \ t/f rem cell-addr
    >r swap                             \ rem t/f  R: cell-addr
    if   bit-mask         r@ @ or
    else bit-mask invert  r@ @ and
    then                                \ new-cell-value  R: cell-addr
    r> ! ;

2variable lines
: alloc-2-lines ( -- )
    here >r  tape-width-in-cells@ cells allot \ 1st line
    here >r  tape-width-in-cells@ cells allot \ 2nd line
    r> r> lines 2! ;                          \ store addresses

: line-idx ( idx -- idx' )
    1 and ;

: line-addr ( idx -- addr-of-indexed-line )
    line-idx    lines swap    cells + @ ;

: init-line ( idx -- )
    line-addr
    tape-width-in-cells@ 0 do
        dup   i +   0 swap !
    loop drop ;

: save-col-idx ( col-idx -- col-idx' )
    tape-width @ mod ;                  \ works also fine for negative numbers

: tape-line-left-parent@ ( line-addr col-idx -- rule-bits )
    1- save-col-idx  tape-line-cell@  if 4 else 0 then ;

: tape-line-mid-parent@ ( line-addr col-idx -- rule-bits )
    tape-line-cell@ if 2 else 0 then ;

: tape-line-right-parent@ ( line-addr col-idx -- rule-bits )
    1+ save-col-idx  tape-line-cell@  if 1 else 0 then ;

: tape-line-parents@ ( line-addr col-idx -- rule-idx )
    2dup tape-line-left-parent@    >r
    2dup tape-line-mid-parent@     r> + >r
         tape-line-right-parent@   r> + ;

: tape-line-child? ( rule-nb rule-idx -- t/f )
    \ That's simply the heart of it: check whether next generation cell needs to
    \ be set, based on rule and current own state and neigbours' state.
    ( rule-idx ) bit-mask ( rule-nb: next state's table ) and 0= invert ;

: tape-line-child! ( t/f child-line-idx col-idx -- )
    swap line-addr swap    tape-line-cell! ;

: generate-line ( parent-line-idx -- )
    tape-width @ 0 do
        dup line-addr i         tape-line-parents@
        generation-rule @ swap  tape-line-child?
        over 1+ i               tape-line-child!
    loop drop ;

: print-line ( line-idx -- )
    tape-width @ 0 do
        dup line-addr i         tape-line-cell@
        if star else dot then
    loop drop ;

: generate-print-lines ( nb-lines -- )
    0 do
        i print-line cr         \ print current generation
        i generate-line         \ compute next generation
    loop cr ;

: generate ( width nb-lines rule -- )
    generation-rule !
    swap tape-width !
    alloc-2-lines
    0     init-line    1 init-line
    true  0 line-addr  tape-center@  tape-line-cell!
    ( nb-lines ) generate-print-lines ;

43 24 90 generate

\ See: https://www.reddit.com/r/dailyprogrammer/comments/5q9cll/20170126_challenge_300_easyintermediate_lets_make/
\ Run with: gforth dp300_intermediate_noise.fs
