;
; AxeOS Bootloader
; (c) AxeOS Development team, 2009
; Based on EikeBoot by Eike Dehling, 2004
; all rights reserved
;
; Compile with nasm, write to first sector of a floppy, have fun
;

[BITS 16]
[ORG 0x7c00]

; -------------------------------------
; disk description
; -------------------------------------
	jmp short start	; 3 bytes to e.g. jump
	nop
	db "AXEOS   "	; 8 byte label / OemId
	dw 512			; bytes per sector
	db 1			; sectors per cluster
	dw 1			; size of the bootloader, in sectors
	db 2			; number of copies of the FAT
	dw 224			; number of entries in Root-Dir
	dw 2880 		; 16-bit number of sectors
	db 0xf0			; media descriptor
	dw 9			; number of sectors per FAT
	dw 18			; sectors per track
	dw 2			; number of heads
	dd 0			; number of hidden sectors
	dd 0			; 32-bit number of sectors
	db 0			; bios drive number
	db 0			; reserved
	db 0x29 		; extended boot signature
	dd 0			; volume ID
	db "AXEOS BOOT "; volume label
	db "FAT12   "	; filesystem type

; -------------------------------------
; bootloader code
; -------------------------------------
print_io_error_trampoline:
	jmp print_io_error	; out of range crap :(

start:
	; set segments, bios may set 0x07c0:0x0000 or 0x0000:0x7c00
	jmp 0:start2
start2: 
	mov ax, 0
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 0x7000

	; save boot device
	mov byte [bootdev], dl
	
	; reset disks 
	mov ah, 0
	mov dl, byte [bootdev]
	int 0x13

;	mov si, hello
;	call print

	; load FAT
	mov ah, 0x02			; read from disk
	mov al, 9				; number of sectors to read
	mov bx, fatbuf			; es: bx = buffer
	mov ch, 0				; track ((1 / 18) / 2)
	mov cl, 2				; sector (1 % 18 + 1)
	mov dh, 0				; head ((1 / 18) % 2)
	mov dl, byte [bootdev]	; dl = device, but that's still correct
	int 0x13
	jc print_io_error_trampoline

	; load root directory
	mov ah, 0x02			; read from disk
	mov al, 14				; number of sectors to read
	mov bx, rootdir			; es: bx = buffer
	mov ch, 0				; track ((19 / 18) / 2)
	mov cl, 2				; sector (19 % 18 + 1)
	mov dh, 1				; head ((19 / 18) % 2)
	mov dl, byte [bootdev]	; dl = device, but that's still correct
	int 0x13
	jc print_io_error_trampoline
	
	; find file
	mov ax, rootdir			; ax = pointer to current dir-entry
	cld

find_start:
	mov si, ax				; point si to current filename
	mov di, filename		; point di to the filename we need
	mov cx, 11				; compare max 11 bytes

	repe cmpsb
	je found_file
	
	add ax, 32				; done yet?
	cmp ax, rootdir_end
	jne find_start

	; print error and halt
	mov si, file_not_found
	call print
	jmp $

found_file:
	; save cluster
	mov si, ax
	mov ax, word [si + 26]	; load cluster of file from the directory entry
	mov word [cluster], ax

	; decode FAT
	mov cx, 512*(9/3)		; number of entries in the fat.
	mov si, fatbuf
	mov di, fatdecoded

	;
	; XXX - WATCH OUT, THIS REALLY NEEDS TO USE EAX, EBX & CO!!!
	;
fat_decode_loop:		; load dword, split into two pieces of 12 bits, and discard last byte
	lodsd				; load dword
	dec si				; need only 3 bytes
	mov ebx, eax
	and eax, 0xFFF			; mask
	stosw
	mov eax, ebx
	shr eax, 12			
	and eax, 0xFFF			; shift & mask
	stosw
	loop fat_decode_loop
	
	; load file:
	;	es:bx = buffer
	;	ds:si = decoded fat buffer
	;	rest is for temporary usage only	
	
	; prepare buffer
	mov ax, 0x50
	mov es, ax
	mov bx, 0
	mov ax, word [cluster]

load_loop:
	; calculate next cluster
	mov si, ax
	shl si, 1
	mov cx, word [fatdecoded + si]
	mov word [cluster], cx
	
	; calculate track, head and sector
	add ax, 31			; ax = logical sector
	mov dx, 0
	mov di, 18
	div di
	mov cl, dl
	add cl, 1			; cl = sector = (logical % 18 + 1)
	mov dx, 0
	mov di, 2
	div di
	mov ch, al			; ch = track = ((logical / 18) / 2)
	mov dh, dl			; dh = head = ((logical / 18) % 2)

	; read data
	mov ah, 0x02
	mov al, 1
	mov dl, byte [bootdev]
	int 0x13
	jc print_io_error
	add bx, 512

	; done?
	mov ax, word [cluster]
	cmp ax, 0xFF8
	jb load_loop

	; DEBUG: print loaded file && halt
	;mov si, 0
	;mov di, 0
	;mov ax, 0x1000
	;mov ds, ax
	;mov ax, 0xb800
	;mov es, ax
	;mov cx, 0x10
	;mov al, 7
	;print_file_loop:
	;movsb
	;stosb
	;loop print_file_loop
	;jmp $

	; execute file?
	 jmp 0x50:0x0000
	 jmp $

; -------------------------------------
; procedures
; -------------------------------------

print_io_error:
	mov si, io_error
	call print
	jmp $

	; print zero-terminated string (without formatting)
	;	ds:si = string
	;	es:di = position on screen
print:
	mov ax, 0xb800
	mov es, ax
	mov di, 0
.print_loop:
	lodsw
	cmp al, 0
	jz .print_done
	stosw
	jmp .print_loop
.print_done:
	ret

; -------------------------------------
; data & variables (not in a separate section, cause that makes padding to the
; 	512 byte border easier)
; -------------------------------------

	; filename
	filename: db "KERNEL  BIN"

	; messages
	hello:			db "H E L L O   F R O M   L O A D E R", 0
	io_error:		db "I / O   e r r o r ", 0
	file_not_found:	db "F i l e   n o t   f o u n d ", 0
	executing:		db "E x e c u t i n g . . . ", 0

; -------------------------------------
; BSS data (starts at 0x8000, that leaves 32k space in the first 64k segment)
; -------------------------------------

[ABSOLUTE 0x8000]
	
	; boot device
	bootdev: resb 1
	
	; first cluster of the file we need
	cluster: resw 1
	
	; FAT buffer
	fatbuf: resb 9 * 512

	; root directory
	rootdir: resb 224 * 32
	rootdir_end:

	; buffer for decoding the FAT
	fatdecoded: resb (9 * 512 * 3) / 2

