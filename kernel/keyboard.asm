; ------------------------------------------------------------------
; kbd_wait_for_key - Waits for keypress and returns key
; IN: Nothing
; OUT: AX = key pressed, other regs preserved

kbd_wait_for_key:
		
	xor ax, ax
	mov ah, 10h			; BIOS call to wait for key
	int 16h

	retf

; ------------------------------------------------------------------
; kbd_check_for_key - Scans keyboard for input, but doesn't wait
; IN: Nothing
; OUT: AX = 0 if no key pressed, otherwise scan code

kbd_check_for_key:
	
	mov ax, 0
	mov ah, 1			; BIOS call to check for key
	int 16h

	jz .nokey			; If no key, skip to end

	mov ax, 0			; Otherwise get it from buffer
	int 16h

	retf

.nokey:
	mov al, 0			; Zero result if no key pressed
	retf

; ==================================================================

