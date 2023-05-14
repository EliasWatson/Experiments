\ ---- Variables ----
: ? @ . ;

\ ---- Stack ----
: dup 0 pick ;
: swap 1 roll ;
: rot 2 roll ;
: rotr rot rot ;
: nip swap drop ;
: tuck dup rot swap ;
: over swap dup rot swap ;

\ ---- Math ----
: negate -1 * ;
: mod over over / floor * - ;
: max over over < if swap then drop ;
: min over over > if swap then drop ;
: abs dup negate max ;

\ ---- Boolean Logic ----
: bool abs 1 min negate ;
: bool2 bool swap bool ;
: not bool 1 + negate ;
: or bool2 + -1 max ;
: and bool2 * negate ;
: xor bool2 over over or rotr and not and ;

\ ---- Comparison ----
: = - not ;
: > swap < ;
: <= over over < rotr = or ;
: >= swap <= ;
