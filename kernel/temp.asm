int09h:	
		push es
		push bx
		push ax
		mov ax, 0x40
		mov es, ax
		mov bl, [es:018h]
		test bl, 00000001b
		jz .back
		in al, 60h
		cmp al, 30h
		jne .back
		mov byte [es:018h],0
		mov al,0
		out 60h,al
		pop ax
		pop bx
		pop es 
		mov ax, 1
		call 0x50:0048h
		

.back:		pop ax
		pop bx
		pop es
		
		jmp far [cs:int09_offset]

os_main:
		mov ax, cs
		mov ds, ax
		mov es, ax
		
		cli
		mov ss, ax
		mov sp, 0xFFFF
		sti

set_int09h:
		push es
		xor ax, ax
		mov es, ax
		mov ax, word [es:0024h]
		mov word [int09_offset], ax
		mov ax, word [es:0026h]
		mov word [int09_segment], ax

		lea ax, [int09h]
		mov word [es:0024h], ax
		mov word [es:0026h], cs
		pop es

		xor eax, eax
		mov cx, 2048
		mov di, disk_buffer
		rep  stosd