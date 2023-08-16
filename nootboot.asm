%define DEBUG

[bits 16]   ;tell assembler this is 16 bit code
[org 0x7c00] ; tell assembler where code will be in memory after it was loaded (BIOS will load boot sector into memory at this address)

%ifdef DEBUG
    call checkFuncs
%endif

call print
jmp $

diskRead:   ; read from disk, using LBA adressing
    ; .count = ax, .offset = bx, .segment = cx
    ; has to be called from unreal mode or protected mode
    
    push edx    ; save edx

    ; setup disk address packet structure
    mov edx, [curLBA]
    mov [DAPACK.count], ax
    mov [DAPACK.offset], bx
    mov [DAPACK.segment], cx
    mov [DAPACK.lba], edx
    add [curLBA], ax
    
    ; actually read
    mov si, DAPACK
    mov ah, 0x42
    mov dl, 0x80    ; first drive
    int 0x13
    jc err

    pop edx     ; restore edx 

    ret
    
%ifdef DEBUG
checkFuncs: ; functions to check if certain hardware features are available
    checkFuncs.LBA: ; check if LBA adressing works
        mov ah, 0x41
        mov bx, 0x55aa
        mov dl, 0x80
        int 0x13
        jc err
    
    ret
%endif

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

DAPACK: ; Disk Adress Packet Structure
            db 0x10     ; size of packet (16 bytes)
            db 0x0      ; always 0
.count      dw 0        ; nr of sectors to transfer
.offset     dw 0        ; transfer buffer offset
.segment    dw 0        ; transfer buffer segment
.lba        dd 0        ; lower 32-bits of 48-bit starting LBA 
            dd 0        ; upper 32-bits of 48-bit starting LBA (we won't need them)

curLBA dd 0x0           ; holds current LBA (TODO set this to kernel position)

TIMES 510 - ($ - $$) db 0	; fill the rest of sector with 0 ($ = pos beginning of line, $$ = beginning of current section, TIMES = assemble instruction N times)
DW 0xAA55			; magic number
