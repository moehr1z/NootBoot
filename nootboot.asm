%define DEBUG

[bits 16]   ;tell assembler this is 16 bit code
[org 0x7c00] ; tell assembler where code will be in memory after it was loaded (BIOS will load boot sector into memory at this address)

%ifdef DEBUG
    call checkFuncs
%endif

;;; init regs and stuff
cli
mov sp, 0x7c00  ; init stack for our stuff, BIOS interrupts, ... 
xor ax, ax       
mov ds, ax      ; in real mode segment registers refer to segment base address not segment selector
mov ss, ax

;;; prepare kernel loading
; the kernel is too large, so we need to switch to unreal mode to avoid the 64KiB segment limit
; unreal mode allows us to still use BIOS functions, but address memory above 1MiB
; also we need to activate the A20 gate

;; activate A20 via BIOS 
; (This is one of many methods, some of which may or may not be supported. Proper way would be to try all of them and see which works, we just pray this one does.)
mov     ax, 2401h    
int     15h
jc      err

;; enter unreal mode 
; so basically what we do is: prepare GDT where DS limit is 4GiB -> enter protected mode -> set ds, this will update the descriptor cache -> switch back to real mode, in real mode the cache isn't filled with GDT entries (as there's none) but processor generates entries internally and not all fields are updated on segment register loads, especially not the limit.

;enter protected mode
lgdt [GDT_DESCRIPTOR]
mov eax, cr0
or al, 1        ; set protection enable bit 
mov cr0, eax
mov bx, 0x8     ; first descriptor
mov ds, bx

; go back to real mode
and al, 0xfe
mov cr0, eax    ; so toggle protection bit again

xor ax, ax          ; restore real mode selectors
mov ds, ax
mov ax, 0x1000      ; this is segment where we want to store kernel to (so: beginning at 0x10000)
mov es, ax

sti
jmp $

diskRead:   ; read from disk, using LBA adressing
    ; .count = ax, .offset = bx, .segment = cx
    
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

GDT: ; temporary global descriptor table we need to avoid the 64KiB segment limit
    dq 0x0  ; first entry always 0
    ; data segment
    dw 0xffff           ; limit 
    dw 0x0              ; base 
    db 0x0              ; base 
    db 0b10010010       ; access byte
    db 0b11001111       ; flags | limit 
    db 0x0              ; base
GDT_END: 

GDT_DESCRIPTOR:    
    dw GDT - GDT_END - 1; size
    dd GDT              ; offset (linear address)


DAPACK: ; Disk Adress Packet Structure
            db 0x10     ; size of packet (16 bytes)
            db 0x0      ; always 0
.count      dw 0        ; nr of sectors to transfer
.offset     dw 0        ; transfer buffer offset
.segment    dw 0        ; transfer buffer segment
.lba        dd 0        ; lower 32-bits of 48-bit starting LBA 
            dd 0        ; upper 32-bits of 48-bit starting LBA (we won't need them)

curLBA dd 0x1           ; holds current LBA, MBR is at block 0, as we expect kernel to follow immediately after, we set it to 1

TIMES 510 - ($ - $$) db 0	; fill the rest of sector with 0 ($ = pos beginning of line, $$ = beginning of current section, TIMES = assemble instruction N times)
DW 0xAA55			; magic number
