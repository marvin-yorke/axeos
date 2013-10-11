; ==================================================================
; COMMAND LINE INTERFACE
; ==================================================================

os_command_line:
	push cs
	call scr_clear_screen

	mov si, help_text
	push cs
	call scr_print_string

get_cmd:
		
	mov si, prompt			; Main loop; prompt for input
	push cs
	call scr_print_string

	mov ax, input			; Get command string from user
	push cs
	call scr_input_string

	push cs
	call scr_print_newline

	mov ax, input			; Remove trailing spaces
	push cs
	call str_string_chomp

	mov si, input			; If just enter pressed, prompt again
	cmp byte [si], 0
	je get_cmd

	mov si, input			; Convert to uppercase for comparison
	push cs
	call str_string_uppercase


	mov si, input			; 'EXIT' entered?
	mov di, exit_string
	push cs
	call str_string_compare
	jc near exit

	mov si, input			; 'HELP' entered?
	mov di, help_string
	push cs
	call str_string_compare
	jc near print_help

	mov si, input			; 'CLS' entered?
	mov di, cls_string
	mov cl, 3
	push cs
	call str_string_strincmp
	jc near clear_screen

	mov si, input			; 'DIR' entered?
	mov di, dir_string
	mov cl, 3
	push cs
	call str_string_strincmp
	jc near list_directory

	mov si, input			; 'VER' entered?
	mov di, ver_string
	mov cl, 3
	push cs
	call str_string_strincmp
	jc near print_ver

	mov si, input			; 'TIME' entered?
	mov di, time_string
	mov cl, 4
	push cs
	call str_string_strincmp
	jc near print_time

	mov si, input			; 'DATE' entered?
	mov di, date_string
	mov cl, 4
	push cs
	call str_string_strincmp
	jc near print_date

	mov si, input			; 'CAT' entered?
	mov di, cat_string
	mov cl, 3
	push cs
	call str_string_strincmp
	jc near cat_file
	
	mov si, input
	mov di, lion_string
	mov cl, 3
	push cs
	call str_string_strincmp
	jc near its_lion


	; If the user hasn't entered any of the above commands, then we
	; need to check if an executable filename (.BIN) was entered...


	mov si, input			; User entered dot in filename?
	mov al, '.'
	push cs
	call str_char_seek
	cmp ax, 0
	je suffix

	jmp full_name

suffix:
	mov ax, input
	push cs
	call str_string_length

	mov si, input
	add si, ax			; Move to end of input string

	mov byte [ds:si], '.'
	mov byte [ds:si+1], 'B'
	mov byte [ds:si+2], 'I'
	mov byte [ds:si+3], 'N'
	mov byte [ds:si+4], 0		; Zero-terminate string


full_name:
		
	mov si, input			; User tried to execute kernel?
	mov di, kern_file_string
	push cs
	call str_string_compare
	jc near kern_warning

	mov ax, input
	push cs
	call str_string_length

	mov si, input			; check for .bin suffix
	add si,ax
	sub si,3
	mov di, extention
	push cs
	call str_string_compare
	jnc not_bin

	mov ax, input
	push cs	
	call disk_file_exists		; Check if file exists
	jc fail				; if not - fail
	
	push cs				; Allocate segment in RAM for program
	call os_alloc_mem
	jc no_free_mem			; if no available memory
	mov word [load_addres+2], dx	; save load address
	mov word [load_addres], 0100h
		
	mov ax, input			; Try to load specified program in RAM
	mov cx, 0100h
	push cs
	call disk_load_file
	
	jc fail				; Skip if error

	mov cx,word[load_addres] 
	mov bx,word[load_addres+2]
	
	mov word [ds:sp_value],sp	; save kernel stack address

	mov ax, bx			; Set segment registers to program segment
	mov ds, ax
	mov es, ax
	cli
	mov ss, ax
	mov sp, 0xFFFF
	push 0000h
	sti
	
	mov word [ds:0],cx		; save load adress in program PSP
	mov word [ds:2],bx
	
	xor ax,ax			; Clear all registers
	xor bx,bx
	xor cx,cx
	xor dx,dx
	xor di,di
	xor si,si

	jmp far [ds:0]			; Call the external program

fail:
	mov si, invalid_msg
	push cs
	call scr_print_string

	jmp end

no_free_mem:
	mov si, no_free_mem_msg
	push cs
	call scr_print_string

	jmp end

not_bin:
	mov si, not_bin_file_msg
	push cs
	call scr_print_string
	
	jmp end

end:					; When program has finished, start again
	push cs
	call scr_print_newline
	jmp get_cmd

; ------------------------------------------------------------------

print_help:
	mov si, help_text
	push cs
	call scr_print_string
	jmp get_cmd


; ------------------------------------------------------------------

clear_screen:
	mov si, input
	push cs
	call str_string_parse
	cmp bx, 0			; Was an argument provided?
	jne .check_argument

.clear_it:
	push cs
	call scr_clear_screen
	jmp get_cmd

.check_argument:
	mov si,bx			; It was '-h' ?
	mov di,help_arg
	push cs
	call str_string_compare
	jne .clear_it
	
	mov si, help_cls		; show help message
	push cs
	call scr_print_string
	jmp get_cmd

; ------------------------------------------------------------------

print_time:
	mov si, not_realised_yet
	push cs
	call scr_print_string
	jmp get_cmd


; ------------------------------------------------------------------

print_date:
	mov si, not_realised_yet
	push cs
	call scr_print_string
	jmp get_cmd


; ------------------------------------------------------------------

print_ver:
	mov si, input
	push cs
	call str_string_parse
	cmp bx, 0			; Was an argument provided?
	jne .check_argument

.print_it:
	mov si, version_msg
	push cs
	call scr_print_string
	jmp get_cmd

.check_argument:
	
	mov si,bx			; It was '-h' ?
	mov di,help_arg
	push cs
	call str_string_compare
	jne .print_it	

	mov si, help_ver		; show help message
	push cs
	call scr_print_string
	jmp get_cmd


; ------------------------------------------------------------------

kern_warning:
	mov si, kern_warn_msg
	push cs
	call scr_print_string
	jmp get_cmd


; ------------------------------------------------------------------

list_directory:
	mov si, input
	push cs
	call str_string_parse
	cmp bx, 0			; Was an argument provided?
	jne .check_argument

.get_it:
	mov ax, dirlist			; Get list of files on disk
	push cs
	call disk_get_file_list

.loop:
	mov al, ','
	mov si, dirlist
	mov di, dir_buf
	push cs
	call str_string_parse_ex
	cmp bx,0
	je .last
	
	mov ax, dir_buf
	push cs
	call disk_get_file_size
	
	mov ax, bx
	mov si, dir_size
	push cs
	call str_int_to_string
	
	mov si, dir_buf
	push cs
	call scr_print_string
	
	push cs
	call scr_get_cursor_pos
	mov dl, 20
	push cs
	call scr_move_cursor
	
	mov si, dir_size
	push cs
	call scr_print_string
	
	push cs
	call scr_print_newline

	jmp .loop

.last:
	mov si, dirlist
	push cs
	call scr_print_string

	mov ax,dirlist
	push cs
	call disk_get_file_size
	
	mov ax, bx
	mov si, dir_size
	push cs
	call str_int_to_string

	push cs
	call scr_get_cursor_pos
	mov dl, 20
	push cs
	call scr_move_cursor

	mov si, dir_size
	push cs
	call scr_print_string

	push cs
	call scr_print_newline

	jmp get_cmd

.check_argument:
	
	mov si,bx			; It was '-h' ?
	mov di,help_arg
	push cs
	call str_string_compare
	jne .get_it	
	
	mov si, help_dir		;  show help message
	push cs
	call scr_print_string
	jmp get_cmd

; ------------------------------------------------------------------

exit:
	ret


; ------------------------------------------------------------------

cat_file:
	mov si, input
	push cs
	call str_string_parse
	cmp bx, 0			; Was a filename provided?
	jne .check_argument

	mov si, nofilename_msg		; If not, show error message
	push cs
	call scr_print_string
	jmp get_cmd

.check_argument:
	mov si,bx			; It was '-h' ?
	mov di,help_arg
	push cs
	call str_string_compare
	jne .filename_provided
	
	mov si, help_cat		; show help message
	push cs
	call scr_print_string
	jmp get_cmd


.filename_provided:
	mov ax, bx

	push cs	
	call disk_file_exists		; Check if file exists
	jc .not_found

	mov dx, word [kernel_adr]
	mov cx, text_buffer		; Load file into second 32K
	push cs
	call disk_load_file

	push bx

	mov si, text_buffer
	mov ah, 0Eh			; int 10h teletype function
.loop:
	lodsb				; Get byte from loaded file

	cmp al, 0Ah			; Move to start of line if we get a newline char
	jne .not_newline

	push cs
	call scr_get_cursor_pos
	mov dl,0
	push cs
	call scr_move_cursor

.not_newline:
	int 10h				; Display it
	dec bx				; Count down file size
	cmp bx, 0			; End of file?
	jne .loop

	push cs
	call scr_print_newline	

	pop bx

	mov al, 0			; clear text buffer
	mov cx, bx
	mov di, text_buffer
	rep stosb

	jmp get_cmd

.not_found:
	mov si, notfound_msg
	push cs
	call scr_print_string
	jmp get_cmd



; ------------------------------------------------------------------

its_lion:
	mov si, lion_msg
	push cs
	call scr_print_string
	push cs
	call scr_print_newline	
	jmp get_cmd


; ------------------------------------------------------------------

	input			times 80 db 0
	dirlist			times 255 db 0
	dir_buf			times 13 db 0
	dir_size		times 7 db 0
	tmp_string		times 15 db 0

	prompt			db '> ', 0
	not_realised_yet	db 'This command is not realised yet :(',13,10,0
	help_text		db 'Inbuilt commands: DIR, CAT, CLS, HELP, TIME, DATE, VER, EXIT', 13, 10,'If you need more information, type %COMMAND% -h',13,10,0
	help_cat		db 'CAT %FILENAME% prints content of %FILENAME% (as text)',13,10,'CAT -h prints this help message',13,10,13,10,0
	help_cls		db 'CLS clears screen',13,10,'CLS -h prints this help message',13,10,13,10,0
	help_dir		db 'DIR display list of files on floppy',13,10,'DIR -h prints this help message',13,10,13,10,0
	help_ver		db 'VER prints current version of AxeOS',13,10,'VER -h prints this help message',13,10,13,10,0
	invalid_msg		db 'No such command or program', 13, 10, 0
	nofilename_msg		db 'No filename specified', 13, 10, 0
	notfound_msg		db 'File not found', 13, 10, 0
	version_msg		db 'AxeOS ', AXEOS_VER, 13, 10, 0
	extention		db 'BIN',0
	
	exit_string		db 'EXIT', 0
	help_string		db 'HELP', 0
	cls_string		db 'CLS', 0
	dir_string		db 'DIR', 0
	time_string		db 'TIME', 0
	date_string		db 'DATE', 0
	ver_string		db 'VER', 0
	cat_string		db 'CAT', 0
	lion_string		db '>:3',0
	help_arg		db '-H', 0
	
	sp_value		dw 0
	kern_file_string	db 'KERNEL.BIN', 0
	kern_warn_msg		db 'Cannot execute kernel file!', 13, 10, 0
	not_bin_file_msg	db 'This is not a program',13,10,0
	no_free_mem_msg		db 'There is no free memory to load this program',13,10,0	

	lion_msg		db "RAWR I'M A LION!",13,10,"JESUS CHRIST IT'S A LION GET IN VL-85!",13,10,0

	load_addres		dw 0,0
	
; ==================================================================

