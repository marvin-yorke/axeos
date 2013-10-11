; ------------------------------------------------------------------
; disk_get_file_list - Generate comma-separated string of files on floppy
; IN/OUT: DS:AX = location to store zero-terminated filename string

disk_get_file_list:
	pusha
	
	push 0050h
	pop es
	
	mov word [es:.file_list_tmp], ax

	mov eax, 0			; Needed for some older BIOSes

	call disk_reset_floppy		; Just in case disk was changed

	mov ax, 19			; Root dir starts at logical sector 19
	call disk_convert_l2hts

	mov si, disk_buffer		; ES:BX should point to our buffer
	mov bx, si

	mov ah, 2			; Params for int 13h: read floppy sectors
	mov al, 14			; And read 14 of them

	pusha				; Prepare to enter loop


.read_root_dir:
	popa
	pusha

	stc
	int 13h				; Read sectors
	call disk_reset_floppy		; Check we've read them OK
	jnc .show_dir_init		; No errors, continue

	call disk_reset_floppy		; Error = reset controller and try again
	jnc .read_root_dir
	jmp .done			; Double error, exit 'dir' routine

.show_dir_init:
	popa

	mov ax, 0
	mov si, disk_buffer		; Data reader from start of filenames

	mov word di, [es:.file_list_tmp]	; Name destination buffer


.start_entry:
	mov al, [es:si+11]			; File attributes for entry
	cmp al, 0Fh			; Windows marker, skip it
	je .skip

	test al, 10h			; Is this a directory entry?
	jnz .skip			; Yes, ignore it

	mov al, [es:si]
	cmp al, 229			; If we read 229 = deleted filename
	je .skip

	cmp al, 0			; 1st byte = entry never used
	je .done


	mov cx, 1			; Set char counter
	mov dx, si			; Beginning of possible entry

.testdirentry:
	inc si
	mov al, [es:si]			; Test for most unusable characters
	cmp al, ' '			; Windows sometimes puts 0 (UTF-8) or 0FFh
	jl .nxtdirentry
	cmp al, '~'
	ja .nxtdirentry

	inc cx
	cmp cx, 11			; Done 11 char filename?
	je .gotfilename
	jmp .testdirentry


.gotfilename:				; Got a filename that passes testing
	mov si, dx			; DX = where getting string

	mov cx, 0
.loopy:
	mov byte al, [es:si]
	cmp al, ' '
	je .ignore_space
	mov byte [ds:di], al
	inc si
	inc di
	inc cx
	cmp cx, 8
	je .add_dot
	cmp cx, 11
	je .done_copy
	jmp .loopy

.ignore_space:
	inc si
	inc cx
	cmp cx, 8
	je .add_dot
	jmp .loopy

.add_dot:
	mov byte [ds:di], '.'
	inc di
	jmp .loopy

.done_copy:
	mov byte [ds:di], ','		; Use comma to separate filenames
	inc di

.nxtdirentry:
	mov si, dx			; Start of entry, pretend to skip to next

.skip:
	add si, 32			; Shift to next 32 bytes (next filename)
	jmp .start_entry


.done:
	dec di
	mov byte [ds:di], 0		; Zero-terminate string (gets rid of final comma)

	popa
	retf


	.file_list_tmp		dw 0

; --------------------------------------------------------------------------
; disk_file_exists - Check for presence of file on the floppy
; IN: AX = filename location
; OUT: carry clear if found, set if not

disk_file_exists:
	push es	
	push bx
	mov bx,0x50
	mov es,bx	

	push cs
	call str_string_uppercase
	
	call int_filename_convert	; Make FAT12-style filename
	
	push ax
	
	call disk_read_root_dir

	pop ax				; Restore filename

	mov di, disk_buffer

	call disk_get_root_entry	; Set or clear carry flag

	pop bx
	pop es
	retf

; --------------------------------------------------------------------------
; disk_get_file_size - Get file size information for specified file
; IN: AX = filename
; OUT: BX = file size in bytes (up to 64K) or carry set if file not found

disk_get_file_size:
	pusha

	mov bx,0x50
	mov es,bx	

	push cs
	call str_string_uppercase
	call int_filename_convert

	clc

	push ax

	call disk_read_root_dir
	jc .failure

	pop ax

	mov di, disk_buffer

	call disk_get_root_entry
	jc .failure

	mov word bx, [es:di+28]

	mov word [es:.tmp], bx

	popa

	mov word bx, [es:.tmp]

	retf

.failure:
	popa
	stc
	retf


	.tmp	dw 0


; ------------------------------------------------------------------
; disk_load_file - Load file into RAM
; IN: DS:AX = location of filename, DX:CX = location in RAM to load file
; OUT: BX = file size (in bytes), carry set if file not found

disk_load_file:
	push es	
	mov bx,0x50
	mov es,bx

	push cs
	call str_string_uppercase
	
	call int_filename_convert

	mov [es:.filename_loc], ax		; Store filename location
	mov [es:.load_position], cx	; And where to load the file!
	mov [es:.load_seg], dx		;
	
	mov eax, 0			; Needed for some older BIOSes

	call disk_reset_floppy		; In case floppy has been changed
	jnc .floppy_ok			; Did the floppy reset OK?

	mov ax, .err_msg_floppy_reset	; If not, bail out
	jmp os_fatal_error


.floppy_ok:				; Ready to read first block of data
	mov ax, 19			; Root dir starts at logical sector 19
	call disk_convert_l2hts

	mov si, disk_buffer		; ES:BX should point to our buffer
	mov bx, si

	mov ah, 2			; Params for int 13h: read floppy sectors
	mov al, 14			; 14 root directory sectors

	pusha				; Prepare to enter loop


.read_root_dir:
	popa
	pusha

	stc				; A few BIOSes clear, but don't set properly
	int 13h				; Read sectors
	jnc .search_root_dir		; No errors = continue

	call disk_reset_floppy		; Problem = reset controller and try again
	jnc .read_root_dir

	popa
	jmp .root_problem		; Double error = exit

.search_root_dir:
	popa

	mov cx, word 224		; Search all entries in root dir
	mov bx, -32			; Begin searching at offset 0 in root dir

.next_root_entry:
	add bx, 32			; Bump searched entries by 1 (offset + 32 bytes)
	mov di, disk_buffer		; Point root dir at next entry
	add di, bx

	mov al, [es:di]			; First character of name

	cmp al, 0			; Last file name already checked?
	je .root_problem

	cmp al, 229			; Was this file deleted?
	je .next_root_entry		; If yes, skip it

	mov al, [es:di+11]			; Get the attribute byte

	cmp al, 0Fh			; Is this a special Windows entry?
	je .next_root_entry

	test al, 10h			; Is this a directory entry?
	jnz .next_root_entry

	mov byte [es:di+11], 0		; Add a terminator to directory name entry

	mov ax, di			; Convert root buffer name to upper case
	
	push ds
	push es
	pop ds
	
	push cs
	call str_string_uppercase
	
	mov si, [es:.filename_loc]		; ES:SI = location of filename to load

	push cs
	call str_string_compare		; Current entry same as requested?

	pop ds

	jc .found_file_to_load

	loop .next_root_entry

.root_problem:
	pop es
	mov bx, 0			; If file not found or major disk error,
	stc				; return with size = 0 and carry set
	retf


.found_file_to_load:			; Now fetch cluster and load FAT into RAM
	mov ax, [es:di+28]			; Store file size to return to calling routine
	mov word [es:.file_size], ax

	mov ax, [es:di+26]			; Now fetch cluster and load FAT into RAM
	mov word [es:.cluster], ax

	mov ax, 1			; Sector 1 = first sector of first FAT
	call disk_convert_l2hts

	mov di, disk_buffer		; ES:BX points to our buffer
	mov bx, di

	mov ah, 2			; int 13h params: read sectors
	mov al, 9			; And read 9 of them

	pusha

.read_fat:
	popa				; In case registers altered by int 13h
	pusha

	stc
	int 13h
	jnc .read_fat_ok

	call disk_reset_floppy
	jnc .read_fat

	popa
	jmp .root_problem


.read_fat_ok:
	popa


.load_file_sector:
	
	mov ax, word [es:.cluster]		; Convert sector to logical
	add ax, 31

	call disk_convert_l2hts		; Make appropriate params for int 13h

	push es
	
	mov bx, [es:.load_position]
	push bx
	mov bx, [es:.load_seg]
	mov es,bx	
	pop bx
	

	mov ah, 02			; AH = read sectors, AL = just read 1
	mov al, 01

	stc
	int 13h
	
	pop es
	jnc .calculate_next_cluster	; If there's no error...

	call disk_reset_floppy		; Otherwise, reset floppy and retry
	jnc .load_file_sector

	mov ax, .err_msg_floppy_reset	; Reset failed, bail out
	jmp os_fatal_error


.calculate_next_cluster:
	
	mov ax, [es:.cluster]
	mov bx, 3
	mul bx
	mov bx, 2
	div bx				; DX = [CLUSTER] mod 2
	mov si, disk_buffer		; AX = word in FAT for the 12 bits
	add si, ax
	mov ax, word [es:si]

	or dx, dx			; If DX = 0 [CLUSTER] = even, if DX = 1 then odd

	jz .even			; If [CLUSTER] = even, drop last 4 bits of word
					; with next cluster; if odd, drop first 4 bits

.odd:
	shr ax, 4			; Shift out first 4 bits (belong to another entry)
	jmp .calculate_cluster_cont	; Onto next sector!

.even:
	and ax, 0FFFh			; Mask out top (last) 4 bits

.calculate_cluster_cont:
	mov word [es:.cluster], ax		; Store cluster

	cmp ax, 0FF8h
	jae .end

	add word [es:.load_position], 512
	jmp .load_file_sector		; Onto next sector!


.end:
	mov bx, [es:.file_size]		; Get file size to pass back in BX
	clc				; Carry clear = good load
	pop es
	retf


	.bootd		db 0 		; Boot device number
	.cluster	dw 0 		; Cluster of the file we want to load
	.pointer	dw 0 		; Pointer into disk_buffer, for loading 'file2load'

	.filename_loc	dw 0		; Temporary store of filename location
	.load_position	dw 0		; Where we'll load the file
	.load_seg	dw 0		; Segment
	.file_size	dw 0		; Size of the file

	.string_buff	times 12 db 0	; For size (integer) printing

	.err_msg_floppy_reset	db 'disk_load_file: Floppy failed to reset', 0



; ==================================================================
; INTERNAL OS ROUTINES - Not accessible to user programs

; ------------------------------------------------------------------
; int_filename_convert - Change 'TEST.BIN' into 'TEST    BIN' as per FAT12
; IN: DS:AX = filename string
; OUT: ES:AX = location of converted string (carry set if invalid)

int_filename_convert:
	pusha

	mov si, ax

	push cs
	call str_string_length
	cmp ax, 12			; Bigger than name + dot + extension?
	jg .failure			; Fail if so

	cmp ax, 0
	je .failure			; Similarly, fail if zero-char string

	mov dx, ax			; Store string length for now

	mov di, .dest_string

	mov cx, 0
.copy_loop:
	lodsb
	cmp al, '.'
	je .extension_found
	stosb
	inc cx
	cmp cx, dx
	jg .failure			; No extension found = wrong
	jmp .copy_loop

.extension_found:
	cmp cx, 0
	je .failure			; Fail if extension dot is first char

	cmp cx, 8
	je .do_extension		; Skip spaces if first bit is 8 chars

	; Now it's time to pad out the rest of the first part of the filename
	; with spaces, if necessary

.add_spaces:
	mov byte [es:di], ' '
	inc di
	inc cx
	cmp cx, 8
	jl .add_spaces

	; Finally, copy over the extension
.do_extension:
	lodsb				; 3 characters
	stosb
	lodsb
	stosb
	lodsb
	stosb

	mov byte [es:di], 0		; Zero-terminate filename

	popa
	mov ax, .dest_string
	clc				; Clear carry for success
	ret


.failure:
	popa
	stc				; Set carry for failure
	ret


	.dest_string	times 13 db 0


; --------------------------------------------------------------------------
; disk_get_root_entry - Search RAM copy of root dir for file entry
; IN: AX = filename
; OUT: DI = location in disk_buffer of root dir entry, or carry set if file not found

disk_get_root_entry:
	pusha

	mov word [es:.filename], ax

	mov cx, 224			; Search all (224) entries
	mov ax, 0			; Searching at offset 0

.to_next_root_entry:
	xchg cx, dx			; We use CX in the inner loop...

	mov word si, [es:.filename]	; Start searching for filename
	mov cx, 11
	rep cmpsb
	je .found_file			; Pointer DI will be at offset 11, if file found

	add ax, 32			; Bump searched entries by 1 (32 bytes/entry)

	mov di, disk_buffer		; Point to next root dir entry
	add di, ax

	xchg dx, cx			; Get the original CX back
	loop .to_next_root_entry

	popa

	stc				; Set carry if entry not found
	ret


.found_file:
	sub di, 11			; Move back to start of this root dir entry

	mov word [es:.tmp], di		; Restore all registers except for DI

	popa

	mov word di, [es:.tmp]

	clc
	ret


	.filename	dw 0
	.tmp		dw 0


; --------------------------------------------------------------------------
; disk_read_fat - Read FAT entry from floppy into disk_buffer
; IN: Nothing
; OUT: carry set if failure

disk_read_fat:
	pusha

	mov ax, 1			; FAT starts at logical sector 1 (after boot sector)
	call disk_convert_l2hts

	mov si, disk_buffer		; Set ES:BX to point to 8K OS buffer
	mov bx, 0050h
	mov es, bx
	mov bx, si

	mov ah, 2			; Params for int 13h: read floppy sectors
	mov al, 9			; And read 9 of them for first FAT

	pusha				; Prepare to enter loop


.read_fat_loop:
	popa
	pusha

	stc				; A few BIOSes do not set properly on error
	int 13h				; Read sectors

	jnc .fat_done
	call disk_reset_floppy		; Reset controller and try again
	jnc .read_fat_loop		; Floppy reset OK?

	popa
	jmp .read_failure		; Fatal double error

.fat_done:
	popa				; Restore registers from main loop

	popa				; And restore registers from start of system call
	clc
	ret

.read_failure:
	popa
	stc				; Set carry flag (for failure)
	ret


; --------------------------------------------------------------------------
; disk_read_root_dir - Get the root directory contents
; IN: Nothing
; OUT: root directory contents in disk_buffer, carry set if error

disk_read_root_dir:
	pusha

	mov ax, 19			; Root dir starts at logical sector 19
	call disk_convert_l2hts

	mov si, disk_buffer		; Set ES:BX to point to OS buffer
	mov bx, 0050h
	mov es, bx
	mov bx, si

	mov ah, 2			; Params for int 13h: read floppy sectors
	mov al, 14			; And read 14 of them (from 19 onwards)

	pusha				; Prepare to enter loop


.read_root_dir_loop:
	popa
	pusha

	stc				; A few BIOSes do not set properly on error
	int 13h				; Read sectors

	jnc .root_dir_finished
	call disk_reset_floppy		; Reset controller and try again
	jnc .read_root_dir_loop		; Floppy reset OK?

	popa
	jmp .read_failure		; Fatal double error


.root_dir_finished:
	popa				; Restore registers from main loop

	popa				; And restore from start of this system call
	clc				; Clear carry (for success)
	ret

.read_failure:
	popa
	stc				; Set carry flag (for failure)
	ret


; --------------------------------------------------------------------------
; Reset floppy disk

disk_reset_floppy:
	push ax
	push dx
	mov ax, 0
	mov dl, 0
	stc
	int 13h
	pop dx
	pop ax
	ret


; --------------------------------------------------------------------------
; disk_convert_l2hts - Calculate head, track and sector for int 13h
; IN: logical sector in AX
; OUT: correct registers for int 13h

disk_convert_l2hts:
	push bx
	push ax

	mov bx, ax			; Save logical sector

	mov dx, 0			; First the sector
	div word [es:SecsPerTrack]		; Sectors per track
	add dl, 01h			; Physical sectors start at 1
	mov cl, dl			; Sectors belong in CL for int 13h
	mov ax, bx

	mov dx, 0			; Now calculate the head
	div word [es:SecsPerTrack]		; Sectors per track
	mov dx, 0
	div word [es:Sides]		; Floppy sides
	mov dh, dl			; Head/side
	mov ch, al			; Track

	pop ax
	pop bx

	mov dl, byte 0			; Set correct device

	ret


	Sides dw 2
	SecsPerTrack dw 18


