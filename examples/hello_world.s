malloc :: Int -> Ptr
write :: Int -> Ptr -> Int -> Int

push_char:
    swap 2
    swap 1
    dup 1
    swap 1
    stb

    pushp 1
    add

    swap 1
    ret

main:
    pushi 13
    ext malloc

    dup 0

    pushc 'H' call push_char
    pushc 'e' call push_char
    pushc 'l' call push_char
    pushc 'l' call push_char
    pushc 'o' call push_char
    pushc ',' call push_char
    pushc ' ' call push_char
    pushc 'W' call push_char
    pushc 'o' call push_char
    pushc 'r' call push_char
    pushc 'l' call push_char
    pushc 'd' call push_char
    pushb 10  call push_char

    drop 

    pushi 1
    swap 1
    pushi 13
    ext write

    hlt
