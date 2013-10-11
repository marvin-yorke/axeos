; ==================================================================
; MISCELLANEOUS ROUTINES
; ==================================================================


; ------------------------------------------------------------------
; os_fatal_error - Display error message and halt execution
; IN: AX = error message string location

os_fatal_error:
	mov bx, ax			; Store string location for now

	mov dh, 0
	mov dl, 0
	push cs
	call scr_move_cursor

	pusha
	mov ah, 09h			; Draw red bar at top
	mov bh, 0
	mov cx, 160
	mov bl, 01001111b
	mov al, ' '
	int 10h
	popa

	mov dh, 0
	mov dl, 0
	push cs
	call scr_move_cursor

	lea si, [.msg_inform]		; Inform of fatal error
	push cs
	call scr_print_string

	mov si, bx			; Program-supplied error message
	push cs
	call scr_print_string

	jmp $				; Halt execution

	
	.msg_inform		db '>>> KERNEL PANIC! Something goes wrong:', 13, 10, 0


; ==================================================================

;os_alloc_mem - Allocate 64K block in RAM
;IN: nothing
;OUT: DX = allocated segment of RAM, carry set if no free memory

os_alloc_mem:
	push ds
	push ax
	push cx
	push si
	
	mov ax,0x50		; Set to kernel segment
	mov ds,ax

	mov cl, byte [ds:mem_table_length]	;load length of Memory Table
	xor si,si

.cycl:	cmp byte [ds:is_enabled+si], 01h	; Find for free segment in RAM
	je .ok
	inc si
	loop .cycl
	
	pop si
	pop cx
	pop ax
	pop ds
	stc
	retf

.ok:	mov byte [ds:is_enabled+si], 0FFh	; If found, set disable byte 0FFh
	shl si,1
	mov dx, word[ds:mem_table+si]		; Return address of allocated segment
	pop si
	pop cx
	pop ax
	pop ds
	clc
	retf

; ==================================================================

;os_exit - End program and unload it from memory
;IN/OUT: AX = exit code

os_exit:
	push ax
	mov dx, word [ds:2]	; Get program segment address
	mov cx, 0FFFFh		; Clear segment
	xor di,di
	xor ax,ax
	rep stosb

	mov bx,0x50		; Set to kernel
	mov es,bx
	mov ds,bx	

	mov cl, byte [ds:mem_table_length]	; load length of memory table
	xor si,si
.cycl:	cmp word [ds:mem_table+si], dx		; find this segment in table
	je .ok
	inc si
	inc si
	loop .cycl

.ok:	shr si,1
	mov byte [ds:is_enabled+si], 01h	; enable this segment as "free" (byte 01h)
	
	pop ax

	cli
	mov ss, bx				; set back kernel stack
	mov sp, word [ds:sp_value]
	sti	

	xor bx,bx
	xor cx,cx
	xor dx,dx
	xor si,si
	xor di,di

	jmp end					; return to console