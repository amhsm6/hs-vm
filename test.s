    push 0
loop:
    push 1
    add
    dup print
    dup push 10 eq jz end
    jmp loop
end:
    hlt
