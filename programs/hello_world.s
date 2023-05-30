malloc :: Int -> Ptr
write :: Int -> Ptr -> Int -> Int

main:
    pushi 13
    ext malloc

    dup 0

    dup 0
    pushb 72   ; H
    stb

    pushp 1
    addp

    dup 0
    pushb 101  ; e
    stb

    pushp 1
    addp

    dup 0
    pushb 108  ; l
    stb

    pushp 1
    addp

    dup 0
    pushb 108  ; l
    stb

    pushp 1
    addp

    dup 0
    pushb 111  ; o
    stb

    pushp 1
    addp

    dup 0
    pushb 44   ; ,
    stb

    pushp 1
    addp

    dup 0
    pushb 32   ; <space>
    stb

    pushp 1
    addp

    dup 0
    pushb 87   ; W
    stb

    pushp 1
    addp

    dup 0
    pushb 111  ; o
    stb

    pushp 1
    addp

    dup 0
    pushb 114  ; r
    stb

    pushp 1
    addp

    dup 0
    pushb 108  ; l
    stb

    pushp 1
    addp

    dup 0
    pushb 100  ; d
    stb

    pushp 1
    addp

    pushb 10   ; \n
    stb

    pushi 1
    swap 1
    pushi 13
    ext write

    hlt
