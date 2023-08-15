[bits 16]   ;tell assembler this is 16 bit code
[org 0x7c00] ; tell assembler where code will be in memory after it was loaded (BIOS will load boot sector into memory at this address)
TIMES 510 - ($ - $$) db 0	; fill the rest of sector with 0 ($ = pos beginning of line, $$ = beginning of current section, TIMES = assemble instruction N times)
DW 0xAA55			; magic number
