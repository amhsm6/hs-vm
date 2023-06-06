main:
    pushf 1 ; e number
    pushf 0 ; (n - 1)!
    pushf 1 ; n - 1
loop:
    ; calculate n!
    swap 1
    pushf 1
    add
    swap 1
    dup 1
    mul

    ; calculate new e
    pushf 1
    dup 1
    div
    dup 3
    add

    ; clean up
    swap 3
    drop

    dup 2 print

    dup 2
    pushf 2.718281
    le
    jz loop

    hlt
