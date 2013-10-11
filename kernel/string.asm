; ------------------------------------------------------------------
; str_string_uppercase - Convert zero-terminated string to upper case
; IN/OUT: DS:AX = string location

str_string_uppercase:
	pusha
	
	mov si, ax			; Use SI to access string

.more:
	cmp byte [ds:si], 0		; Zero-termination of string?
	je .done			; If so, quit

	cmp byte [ds:si], 'a'		; In the lower case A to Z range?
	jb .noatoz
	cmp byte [ds:si], 'z'
	ja .noatoz

	sub byte [ds:si], 20h		; If so, convert input char to upper case

	inc si
	jmp .more

.noatoz:
	inc si
	jmp .more

.done:
	popa
	retf

; ------------------------------------------------------------------
; str_string_length - Return length of a string
; IN: DS:AX = string location
; OUT AX = length (other regs preserved)

str_string_length:
	push cx
	push bx
	
	mov bx, ax			; Move location of string to BX

	mov cx, 0			; Counter

.more:
	cmp byte [ds:bx], 0		; Zero (end of string) yet?
	je .done
	inc bx				; If not, keep adding
	inc cx
	jmp .more


.done:
	mov ax, cx
	pop bx
	pop cx
	
	retf


; ------------------------------------------------------------------
; str_string_compare - See if two strings match
; IN: DS:SI = string one, ES:DI = string two
; OUT: carry set if same, clear if different

str_string_compare:
	pusha

.more:
	mov al, [ds:si]			; Retrieve string contents
	mov bl, [es:di]

	cmp al, bl			; Compare characters at current location
	jne .not_same

	cmp al, 0			; End of first string? Must also be end of second
	je .terminated

	inc si
	inc di
	jmp .more


.not_same:				; If unequal lengths with same beginning, the byte
	popa				; comparison fails at shortest string terminator
	clc				; Clear carry flag
	retf


.terminated:				; Both strings terminated at the same position
	popa
	stc				; Set carry flag
	retf


; ------------------------------------------------------------------
; str_string_chomp - Strip leading and trailing spaces from a string
; IN: DS:AX = string location

str_string_chomp:
	pusha

	mov dx, ax			; Save string location

	mov di, ax			; Put location into DI
	mov cx, 0			; Space counter

.keepcounting:				; Get number of leading spaces into BX
	cmp byte [ds:di], ' '
	jne .counted
	inc cx
	inc di
	jmp .keepcounting

.counted:
	cmp cx, 0			; No leading spaces?
	je .finished_copy

	mov si, di			; Address of first non-space character
	mov di, dx			; DI = original string start

.keep_copying:
	mov al, [ds:si]			; Copy SI into DI
	mov [ds:di], al			; Including terminator
	cmp al, 0
	je .finished_copy
	inc si
	inc di
	jmp .keep_copying

.finished_copy:
	mov ax, dx			; AX = original string start
	
	push cs
	call str_string_length
	cmp ax, 0			; If empty or all blank, done, return 'null'
	je .done

	mov si, dx
	add si, ax			; Move to end of string

.more:
	dec si
	cmp byte [ds:si], ' '
	jne .done
	mov byte [ds:si], 0		; Fill end spaces with 0s
	jmp .more			; (First 0 will be the string terminator)

.done:
	popa
	retf

; ------------------------------------------------------------------
; str_char_seek - Find location of character in a string
; IN: DS:SI = string location, AL = character to find
; OUT: AX = location in string, or 0 if char not present

str_char_seek:
	push si
	push cx
	mov cx, 1			; Counter -- start at first char (we count
					; from 1 in chars here, so that we can
					; return 0 if the source char isn't found)

.more:
	cmp byte [ds:si], al
	je .done
	cmp byte [ds:si], 0
	je .notfound
	inc si
	inc cx
	jmp .more

.done:
	mov ax,cx
	pop cx
	pop si
	retf

.notfound:
	pop cx
	pop si
	mov ax, 0
	retf

; ------------------------------------------------------------------
; str_string_parse - Take string (eg "run foo bar baz") and return
; pointers to zero-terminated strings (eg AX = "run", BX = "foo" etc.)
; IN: SI = string; OUT: AX, BX, CX, DX = individual strings

str_string_parse:
	push si

	mov ax, si			; AX = start of first string

	mov bx, 0			; By default, other strings start empty
	mov cx, 0
	mov dx, 0

	push ax				; Save to retrieve at end

.loop1:
	lodsb				; Get a byte
	cmp al, 0			; End of string?
	je .finish
	cmp al, ' '			; A space?
	jne .loop1
	dec si
	mov byte [ds:si], 0		; If so, zero-terminate this bit of the string

	inc si				; Store start of next string in BX
	mov bx, si

.loop2:					; Repeat the above for CX and DX...
	lodsb
	cmp al, 0
	je .finish
	cmp al, ' '
	jne .loop2
	dec si
	mov byte [ds:si], 0

	inc si
	mov cx, si

.loop3:
	lodsb
	cmp al, 0
	je .finish
	cmp al, ' '
	jne .loop3
	dec si
	mov byte [ds:si], 0

	inc si
	mov dx, si

.finish:
	pop ax

	pop si
	retf

; ------------------------------------------------------------------
; str_string_strincmp - See if two strings match up to set number of chars
; IN: SI = string one, DI = string two, CL = chars to check
; OUT: carry set if same, clear if different

str_string_strincmp:
	pusha

.more:
	mov al, [ds:si]			; Retrieve string contents
	mov bl, [es:di]

	cmp al, bl			; Compare characters at current location
	jne .not_same

	cmp al, 0			; End of first string? Must also be end of second
	je .terminated

	inc si
	inc di

	dec cl				; If we've lasted through our char count
	cmp cl, 0			; Then the bits of the string match!
	je .terminated

	jmp .more


.not_same:				; If unequal lengths with same beginning, the byte
	popa				; comparison fails at shortest string terminator
	clc				; Clear carry flag
	retf


.terminated:				; Both strings terminated at the same position
	popa
	stc				; Set carry flag
	retf


; ------------------------------------------------------------------
; str_string_parse_ex - Parse string by specified delimiter
; IN: AL = delimiter, DS:SI = points to string to parse
; 	  ES:DI = out buffer address
; OUT: BX = substring length,  DS:SI = points to parsed string
;	 ES:DI = substring address
; if string can't be parsed by this delimiter, 0 is returned in BX

str_string_parse_ex:
		push si					; save SI for future use		

		push cs
		call str_char_seek	; search for delim first occurence in str
		cmp ax,0		; if none, return with error
		je .cant_parse
		
		mov cx,ax		; copy result string to out buffer
		mov bx,ax
		dec cx
		repne movsb
		mov al,0
		stosb

		pop si			; restore si

		push es
		push ds
		pop es

		mov di,si		; shift initial string left				
		add si,bx	
.cp:
		lodsb
		cmp al,0
		je .end
		stosb
		jmp .cp

.end:					; set trailing zero
		mov al,0
		stosb
		pop es
		retf

.cant_parse:
		pop si
		mov bx,0
		retf


; ------------------------------------------------------------------
; str_int_to_string - Convert unsigned integer to string
; IN: AX = signed int
;     SI = pointer to output string	
; OUT: Nothing

str_int_to_string:
	pusha

	mov cx, 0
	mov bx, 10			; Set BX 10, for division and mod
	
.push:
	mov dx, 0
	div bx				; Remainder in DX, quotient in AX
	inc cx                          ; Increase pop loop counter
	push dx				; Push remainder, so as to reverse order when popping
	test ax, ax			; Is quotient zero?
	jnz .push			; If not, loop again
.pop:
	pop dx				; Pop off values in reverse order, and add 48 to make them digits
	add dl, '0'			; And save them in the string, increasing the pointer each time
	mov [ds:si], dl
	inc si
	dec cx
	jnz .pop

	mov byte [ds:si], 0		; Zero-terminate string

	popa
	retf