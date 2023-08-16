%define DEBUG

[bits 16]   ;tell assembler this is 16 bit code
[org 0x7c00] ; tell assembler where code will be in memory after it was loaded (BIOS will load boot sector into memory at this address)

%ifdef DEBUG
    mov si, errStr
%endif

call print
jmp $

err: 
%ifdef DEBUG
    mov si, errStr
    call print
    jmp $

    print: ; print null-terminated string
        ; string location in si
        mov al, [si]
        and al, al
        jz print.exit
        call printC
        inc si
        jmp print
    .exit: 
        ret

    printC: ; print Character   
        ; https://stanislavs.org/helppc/int_10-e.html
        ; char in al
        mov ah, 0x0e    ; writer character in tty mode
        mov bx, 0x07    ; page nr0. + light grey on black

        int 0x10        ; (the IVT was already filled by the BIOS)
        ret

%endif

; Data
%ifdef DEBUG
    errStr db 'Ouch!', 0
%endif

TIMES 510 - ($ - $$) db 0	; fill the rest of sector with 0 ($ = pos beginning of line, $$ = beginning of current section, TIMES = assemble instruction N times)
DW 0xAA55			; magic number
