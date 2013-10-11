; ------------------------------------------------------------------
; Test program - The most simple demo program
; ------------------------------------------------------------------
BITS 16
%INCLUDE "api.inc"
org 0x100

start:
	call scr_clear_screen
	
	lea ax, [file_name]
	mov dx, ds
	lea cx, [files]
	call disk_load_file
	jnc .print
	
	mov si, not_found
	call scr_print_string	
	jmp end

.print:
	lea si, [files]
	call scr_print_string	
	
end:	call kbd_wait_for_key

	mov ax, 0
	call os_exit
	
files	times 2048 db 0
file_name db 'test.txt',0
not_found db 'File test.txt not found',0
; ------------------------------------------------------------------

