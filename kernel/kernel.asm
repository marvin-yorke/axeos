[BITS 16]
		%DEFINE AXEOS_VER '0.9rc1'
		disk_buffer equ 24576	; Offset of buffer for disk operations
		text_buffer equ 32768	; Offset of buffer for "cat" command

vectors:
		jmp os_main			; OS main cycle - called from bootloader

		jmp scr_text_mode		; Set screen text mode 80x25
		jmp scr_move_cursor 		; Set cursor position
		jmp scr_print_string		; Print text Z-string in current cursor position
		jmp scr_clear_screen		; Clear screen
		jmp scr_print_newline		; Print newline
		jmp scr_get_cursor_pos		; Get current cursor position
		jmp scr_input_string		; Print string

		jmp kbd_wait_for_key		; Read single key with wait
		jmp kbd_check_for_key		; Read single key without waiting

		jmp disk_get_file_list		; List files in root directory
		jmp disk_load_file		; Load file in RAM
		jmp disk_file_exists		; Check if file exist on floppy	
		jmp disk_get_file_size		; Get file size	

		jmp str_string_uppercase	; Convert string to uppercase
		jmp str_string_length		; Calculate string length
		jmp str_string_compare		; Compare 2 strings
		jmp str_string_parse		; Parse string by ',' symbol
		jmp str_string_parse_ex		; Parse string by specified symbol
		jmp str_string_chomp		; Remove trailing spaces 
		jmp str_string_strincmp		; Compare n first chars of 2 strings
		jmp str_char_seek		; Seek specified symbol in string
		jmp str_int_to_string		; Convert integer value to ASCII Z-string
				
		jmp os_exit			; Exit from extern program

		

os_main:
		mov ax, cs			; Set segment registers to kernel segment
		mov ds, ax
		mov es, ax
		mov [kernel_adr], ax		; And store in variable

		cli
		mov ss, ax
		mov sp, 0xFFFF
		sti

		push cs				; Set text mode
		call scr_text_mode

		mov si, hello_msg		; Print hello message
		push cs
		call scr_print_string
		push cs
		call scr_print_newline

		xor eax, eax			; Prepare disk buffer
		mov cx, 2048
		mov di, disk_buffer
		rep  stosd

		xor eax,eax			; Prepare text buffer
		mov cx, 8192
		mov di, text_buffer
		rep stosd

		mov ax, logo			; Print AxeOS logo
		mov dx, ds
		mov cx, text_buffer
		push cs
		call disk_load_file		
		jc .next			; If logo not found, just load console

		mov si, text_buffer
		push cs
		call scr_print_string
	
		push cs
		call scr_print_newline
		push cs
		call scr_print_newline

		mov al, 0			; Clear text buffer
		mov cx, bx
		mov di, text_buffer
		rep stosb			

		mov si, pak_msg
		push cs
		call scr_print_string

.next:		push cs
		call kbd_wait_for_key
		
		call os_command_line		; Run console
		
		jmp $


; Files with system functions

%include "disk.asm"
%include "screen.asm"
%include "keyboard.asm"
%include "string.asm"
%include "misc.asm"
%include "cli.asm"

; ==================================================================

;Memory table
mem_table_length db 9
mem_table	dw 0x1000, 0x2000, 0x3000, 0x4000, 0x5000, 0x6000, 0x7000, 0x8000, 0x9000
is_enabled	db  0FFh,   01h,    01h,    01h,    01h,    01h,    01h,    01h,    01h

kernel_adr	dw 0			; Kernel segment

logo 		db 'logo.lof',0		; File with logo

hello_msg	db 'AxeOS ',AXEOS_VER,13,10,'Smirnov & Sofronov, 2009',13,10,0
pak_msg		db 'Press Any Key To Continue...',0