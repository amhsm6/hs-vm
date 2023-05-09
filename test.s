loop:
    pushf 1
    pushf 0
    divf

    dup print

    dup
    eqf
    jz loop

    hlt
