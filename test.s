    push 0
loop:
    push 1
    add

    dup print

    dup
    push 10
    lt
    jz loop

    hlt
