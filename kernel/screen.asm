; ------------------------------------------------------------------
; scr_text_mode - Sets 80x25 text mode terminal
; IN/OUT: Nothing (registers preserved)

scr_text_mode:
		pusha
		
		mov ax, 3
		int 10h

		popa
		retf


; ------------------------------------------------------------------
; scr_clear_screen - Clears the screen to background
; IN/OUT: Nothing (registers preserved)

scr_clear_screen:
		pusha

		mov dx, 0			; Position cursor at top-left
		
		push cs		
		call scr_move_cursor

		mov ah, 6			; Scroll full-screen
		mov al, 0			; Normal white on black
		mov bh, 7			;
		mov cx, 0			; Top-left
		mov dh, 24			; Bottom-right
		mov dl, 79
		int 10h

		popa
		retf	


; ------------------------------------------------------------------
; scr_move_cursor - Moves cursor in text mode
; IN: DH, DL = row, column
; OUT: Nothing (registers preserved)

scr_move_cursor:
		pusha

		mov bh, 0
		mov ah, 2
		int 10h				; BIOS interrupt to move cursor
	
		popa
		retf


; ------------------------------------------------------------------
; scr_get_cursor_pos - Return position of text cursor
; IN: Nothing
; OUT: DH, DL = row, column

scr_get_cursor_pos:
	push ax
	push bx
	push cx

	mov bh, 0
	mov ah, 3
	int 10h				; BIOS interrupt to get cursor position

	pop cx	
	pop bx
	pop ax
	
	retf



; ------------------------------------------------------------------
; scr_print_string - Displays text
; IN: DS:SI = message location (zero-terminated string)
; OUT: Nothing (registers preserved)

scr_print_string:
	pusha

	mov ah, 0Eh			; int 10h teletype function

.repeat:
	lodsb				; Get char from string
	cmp al, 0
	je .done			; If char is zero, end of string

	int 10h				; Otherwise, print it
	jmp .repeat			; And move on to next char

.done:
	popa
	retf

; ------------------------------------------------------------------
; scr_print_newline - Reset cursor to start of next line
; IN/OUT: Nothing (registers preserved)

scr_print_newline:
	pusha

	mov ah, 0Eh			; BIOS output char code

	mov al, 13
	int 10h
	mov al, 10
	int 10h

	popa
	retf


; ------------------------------------------------------------------
; scr_input_string - Take string from keyboard entry
; IN/OUT: DS:AX = location of string, other regs preserved
; (Location will contain up to 255 characters, zero-terminated)

scr_input_string:
	pusha
	
	push ds
	pop es
	
	mov di, ax			; DI is where we'll store input (buffer)
	mov cx, 0			; Character received counter for backspace


.more:					; Now onto string getting
	push cs
	call kbd_wait_for_key

	cmp al, 13			; If Enter key pressed, finish
	je .done

	cmp al, 8			; Backspace pressed?
	je .backspace			; If not, skip following checks

	cmp al, ' '			; In ASCII range (32 - 126)?
	jb .more			; Ignore most non-printing characters

	cmp al, '~'
	ja .more

	jmp .nobackspace


.backspace:
	cmp cx, 0			; Backspace at start of string?
	je .more			; Ignore it if so

	
	push cs
	call scr_get_cursor_pos		; Backspace at start of screen line?
	cmp dl, 0
	je .backspace_linestart

	pusha
	mov cx,1
	mov ah, 0Eh			; If not, write space and move cursor back
	mov al, 8
	int 10h				; Backspace twice, to clear space
	mov al, 32
	int 10h
	mov al, 8
	int 10h
	popa

	dec di				; Character position will be overwritten by new
					; character or terminator at end

	dec cx				; Step back counter

	jmp .more


.backspace_linestart:
	dec dh				; Jump back to end of previous line
	mov dl, 79
	push cs
	call scr_move_cursor

	mov al, ' '			; Print space there
	mov ah, 0Eh
	int 10h

	mov dl, 79			; And jump back before the space
	push cs
	call scr_move_cursor

	dec di				; Step back position in string
	dec cx				; Step back counter

	jmp .more


.nobackspace:
	pusha
	mov cx,1
	mov ah, 0Eh			; Output entered, printable character
	int 10h
	popa

	stosb				; Store character in designated buffer
	inc cx				; Characters processed += 1
	cmp cx, 254			; Make sure we don't exhaust buffer
	jae near .done

	jmp near .more			; Still room for more


.done:
	mov ax, 0
	stosb

	popa
	retf


; ==================================================================