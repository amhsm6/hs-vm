    pushf 1 ; e number
    pushf 0 ; (n - 1)!
    pushf 1 ; n - 1
loop:
    ; calculate n!
    swap 1
    pushf 1
    addf
    swap 1
    dup 1
    mulf

    ; calculate new e
    pushf 1
    dup 1
    divf
    dup 3
    addf

    ; clean up
    swap 3
    pop

    dup 2 print

    dup 2
    pushf 2.718281
    lef
    jz loop

    hlt
