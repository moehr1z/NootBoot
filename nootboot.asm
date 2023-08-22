%define DEBUG

[bits 16]   ;tell assembler this is 16 bit code
[org 0x7c00] ; tell assembler where code will be in memory after it was loaded (BIOS will load boot sector into memory at this address)

;;; prepare kernel loading

; init regs and stuff
cli
mov sp, 0x7c00  ; init stack for our stuff, BIOS interrupts, ... 
xor ax, ax       
mov ds, ax      ; in real mode segment registers refer to segment base address not segment selector
mov ss, ax

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

jmp .flushipfq    ; see https://stackoverflow.com/questions/76928196/does-the-cs-register-need-to-be-set-when-setting-up-unreal-mode
.flushipfq

xor ax, ax          ; restore real mode selectors
mov ds, ax
mov ax, 0x1000      ; this is segment where we want to store kernel to (so: beginning at 0x10000)
mov es, ax

sti

;;; handle real mode kernel

; load first sector (header)
mov ax, 0x1
xor bx, bx
mov cx, es
call diskRead

mov al, [es:0x1f1]   ; setup_sects
cmp al, 0x0
jne cont
mov ax, 0x4          ; if nr of sects = 0, actual default is 4 

cont:

; load the rest of the real mode part of the kernel
mov bx, 512     ; already read one sector
mov cx, es
call diskRead

; fill header fields
cmp word [0x206], 0x202 ; suppported boot protocol version >=2.02
jb err

mov byte [es:210], 0xff; type_of_loader 
mov byte [es:211], 0b10000001    ; loadflags
mov word [es:0x224], 0xde00      ; heap_end_ptr (0xe000 - 0x0200)
mov dword [es:0x228], 0x1e000    ; cmd_line_ptr (base_ptr + heap_end)
; ramdisk fields we'll do later

; copy cmd line
mov si, [COMMANDLINE.config]
mov di, 0xe000
mov cx, [COMMANDLINE.length]
rep movsb

;;; load the protected mode kernel
mov edx, [es:0x1f4]     ; syssize
shl edx, 4   ; "units of 16-byte paragraphs" = begins at multiple of 16 bytes
call highLoader

;;; load the initrd
mov eax, [HIGHADDR]
mov dword [es:0x218], eax        ; ramdisk_image
mov dword [es:0x21c], INITRDSIZE ; ramdisk_size (INITRDSIZE comes from the build script)

mov edx, INITRDSIZE
call highLoader


jmp $

highLoader:    ; read from disk to a high (> 1MB) address
    ; bytes to load = dx
    .loop: 
        cmp edx, 127 * 512  ; less than 64KiB (BIOS still bound by 64KiB segment limit)
        jl highLoader.rest
       
        ; load + move high
        mov ax, 127
        xor bx, bx          
        mov cx, 0x2000
        call diskRead
        call highMove

        sub edx, 127 * 512                
        jmp highLoader.loop

    .rest:
        shr edx, 9      ; divide by 512, get count
        inc edx         ; increase in case no clean divide, if there was a clean divide we load sone additional, which isn't problematic
        mov ax, dx        
        xor bx, bx
        mov cx, 0x2000
        call diskRead
        call highMove
   
        ret
       
highMove:
        mov esi, 0x20000 
        mov edi, [HIGHADDR]     
        mov ecx, 512 * 127  ; always copy this much, last copy will probably contain some junk but that's no problem
    .loop:
        ; rep movsd would be nice, but we haven't maxed the limit of es, so I do it this way...
        mov eax, [ds:esi] 
        mov [ds:edi], eax

        add esi, 4  
        add edi, 4
        sub ecx, 4

        jnz highMove.loop
        mov [HIGHADDR], edi
        ret


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

COMMANDLINE:   
.config     db "auto",0
.length     db $-COMMANDLINE.config

HIGHADDR: dd 0x100000

TIMES 510 - ($ - $$) db 0	; fill the rest of sector with 0 ($ = pos beginning of line, $$ = beginning of current section, TIMES = assemble instruction N times)
DW 0xAA55			; magic number
