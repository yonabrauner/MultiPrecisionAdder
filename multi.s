section .rodata
    str_format: db "%02hhx",0
    str_endline: db 10,0
	str_default: db "handling defaults:",10,0
	str_random: db "handling randoms:",10,0
	str_input: db "handling input:",10,0
	str_num1: db "num1:",10,0
    str_num2: db "num2:",10,0
    str_result: db "num1 + num2:",10,0
	str_error: db "invalid argument. exiting.",10,0
	x_struct: db 5
	x_num: db 0xaa, 1,2,0x44,0x4f
	y_struct: db 6
	y_num: db 0xaa, 1,2,3,0x44,0x4f
	buffer: times 600 db 0
	R: db "-R",0
	I: db "-I",0

section .data
    STATE: dd 0x1210 
    MASK: dd 0xfedc

segment .text
global main
extern printf
extern fgets
extern stdin
extern malloc

PRmulti:
	push ebp				; function entry sequence
	mov ebp, esp

	generate_num:			; generate first PRN to determine size of new struct
		call rand_num			; new PRN saved in eax
		movzx edi, al			; save LS byte from eax to edi, extended with zeros
		cmp edi, 0				; make sure that the first byte isn't 0 ->
		jz generate_num			; if it is, loop back and generate again

	inc edi					; adjustment to memory space for malloc
	push edi				; preparing argument for malloc
	call malloc				; returned pointer to new struct is in eax
	
	dec edi					; restoring edi to correct size
	mov dword [eax], edi	; setting edi (size of struct) to first byte of new struct

	push eax				; saving pointer to start of struct in stack
	inc eax					; moving eax to point to next byte - first byte of data of new struct
	mov ebx, eax			; copying eax to ebx as new PRNs will be located in eax

	PRmulti_loop:
		cmp edi, 0				; if counter got to 0,
		jz PRmulti_finish		; go to end function

		push ebx				; ebx is altered in rand_num - save its value beforehand
		call rand_num			; generate new PRN, located in eax
		pop ebx					; restore ebx

		cmp edi, 1				; if arrived at last byte (struct size was odd)->
		je PRmulti_last_byte	; handle last byte

		; if not at last byte of new struct:

		mov [ebx], ax			; insert 2 bytes from new PRN into current bytes pointed by ebx
		add ebx, 2				
		sub edi, 2					
		jmp PRmulti_loop

		PRmulti_last_byte:
			mov [ebx], al			; insert LS byte from PRN into last byte of new struct
			dec edi
			jmp PRmulti_loop

	PRmulti_finish:
		pop eax					; restore eax to point at first byte of new struct

		leave
		ret	

rand_num:
	push ebp				; function entry sequence
	mov ebp, esp

	mov ax, [STATE]			; saving the state to first 16 bits of eax
	mov bX, [MASK]			; saving the mask to first 16 bits of ebx

	xor bx, ax				; checking if both state and mask are 1's or 0's
	jp even_ones			; if result is 1, theyre even
	
	odds:
		STC 					; set carry flag to 1
		RCR ax,1				; rotate right with carry once
		jmp rand_finish

	even_ones:
		shr ax,1				; rotate right without carry
		jmp rand_finish

	rand_finish:   
		mov [STATE] ,ax			; update STATE value for different results
		mov  eax, [STATE]		; returned PRN saved at eax

		leave
		ret

add_multi:
	push ebp				; function entry sequence
	mov ebp, esp	

	mov eax, [ebp+8] 		; saving pointer of struct1 to eax
    mov ebx, [ebp+12] 		; saving pointer of struct2 to ebx
    call get_max_min
    mov ecx, ebx 			; clearing eax since malloc writes to eax
    mov ebx, eax 			; now eax can be written over
    movzx edi, byte [ebx]	; saving size of bigger struct to edi (extended with 0's)
    inc edi					; adding 1 byte of space

	push dword ebx			; ebx and ecx registers are altered in malloc, save them beforehand
    push dword ecx
    push edi				; preparing call to malloc 
    call malloc 			; takes input from edi, returns pointer to allocated memory in eax
    
    pop edi
    pop dword ecx
    pop dword ebx

	push dword eax 			; store pointer to beginning of allocated memory (new struct) in stack
    dec edi 				; returning edi to size of bigger struct
    mov dword [eax], edi 	; insert size of new struct to its first byte

    movzx esi, byte [ebx] 	; esi will be the difference in size:
    movzx edi, byte [ecx] 	; edi = min size
    sub esi, edi			; esi = max size - min size

	; update pointers s.t they point to first byte of data (after size)
    inc eax
    inc ebx
    inc ecx 				

    CLC 					; clear carry flag
    inc edi					
    inc esi

	add_loop:
		dec edi					; if edi (remaining size of smaller struct) = 0 ->
		jz only_max_left		; handle rest of bigger struct

		mov dl,byte [ebx]		; save first byte of struct1 to lower byte of edx
		adc dl,byte [ecx]		; add the first byte of struct2 + carry to lower byte of edx
		mov byte [eax], dl		; save the result of addition to the first byte of newly allocated memory

		inc eax					; increment all pointers to next byte:
		inc ebx	
		inc ecx
		jmp add_loop

    only_max_left:	
		dec esi					; if esi (remaining difference of sizes) = 0
		jz add_finish			; go to end function

		mov dl,byte [ebx]		; save next byte from bigger struct to lower byte of edx
		adc dl, 0				; add the carry (either 1 or 0) to lower byte of edx
		mov byte[eax], dl 		; save result of addition to current byte of allocated memory pointed by eax
		inc ebx					; inc ebx s.t it points to the next byte of bigger struct
		inc eax					; inc eax s.t it points to next free byte allocated

		jmp only_max_left

	add_finish:
		pop eax					; restore eax to point to start of new struct

		leave
		ret  

get_max_min:
	push ebp
	mov ebp, esp

	movzx ecx, byte [eax]	; saving size of struct in eax to ecx, while extending remaining bits with 0's.
	movzx edx, byte [ebx]	; saving size of struct in ebx to edx, while extending remaining bits with 0's.

	cmp ecx, edx			; comparing struct sizes
	jge max_min_end			; if ecx > edx, end function

	; switching the pointers 
	mov ecx, ebx			; ecx = ebx
	mov ebx, eax			; ebx = eax
	mov eax, ecx			; eax = ecx

	max_min_end:
	leave
	ret


;get_multi:
;	push ebp				; function entry sequence
 ;   mov ebp, esp
;
;	; passing input from stdin to buffer
;	push dword[stdin]		; arguments for fgets
;	push 600				; buffer size
;	push buffer	
;	call fgets				; eax now points to start of buffer filled with input
;	add esp, 12

	; computing size of received input
;	mov ecx, 0				; size-counter

;	size_count:
;		cmp byte [eax], 0		; if current byte is null character ->
;		je count_finished
;
	;	inc ecx 				; inc size-counter
	;	inc eax					; inc buffer pointer
	;	jmp size_count
	
	; now eax points to (end-of-string) null char in buffer
	; and size of data is in ecx
	;count_finished:
	;	mov ebx, eax			; save EOBuffer from eax to ebx because malloc will return pointer in eax
	;	inc ecx 				; need one more byte for malloc
	;	push ebx				; ebx might be altered in malloc, save its content
	;	push ecx				; prepare call to malloc
	;	call malloc
	;	pop ecx					
	;	pop ebx				; restore ebx to pointer of end of buffer
	;	dec ecx					
	;	mov [eax], ecx			; save size of new struct to its first byte
	;	inc  eax				; move eax to point to first byte of actual data of new struct
	
	; eax = start of data of new struct
	; ebx = end of input in buffer
	; ecx = size of data left to copy from buffer to struct

	;convert_bytes:
	
	; in a loop: while size of received input isn't 0
	;			 a. take last byte from buffer
	;			 b. transform it from ascii to hex
	;			 c. insert to start of 
	;			 d. inc struct pointer, dec buffer pointer

print_multi:
    push ebp				; function entry sequence
    mov ebp, esp  

    mov edi, [ebp+8] 		; saving argument (p - pointer to struct) to edi
    movzx esi, byte [edi]  	; reading first byte (extended with zeros) which is size of struct into esi
    add edi, esi			; s.t edi points to end of struct

	print_loop:
		cmp esi, 0				; comparing size of struct with 0
		jz print_finish			; if struct size indeed is 0, go to end function
		
		movzx ecx, byte [edi]	; read a single byte from struct into ecx
		push ecx				; pushing data into stack (second argument of printf)
		push str_format   		
		call printf		
		add esp, 8

		sub edi, 1				; go to next byte of struct
		sub esi, 1				; decrement size by a byte
		jmp print_loop

    print_finish:
		push dword str_endline
		call printf				; printing newline
		add esp, 4

		leave					; function end sequence
		ret


main:
	push ebp                ; saving base pointer
    mov ebp, esp            ; saving stack pointer

	mov edi, [ebp+8]        ; saving argc to edi
	cmp edi, 1				; checking if argc is 1 (no arguments - handle defalut nums)
	je default_nums

	mov eax, [ebp+12]		; saving arg to eax

	cmp dword [eax], R		; checking if argv[1] is "-R"
	je random_nums

	cmp dword [eax], I		; checking if argv[1] is "-I"
	je input_nums

	push str_error			; if got here, invalid arg
	call printf
	jmp main_finish			; go to quit program

	input_nums:
		push str_input
		call printf
		add esp, 4
		call get_multi
		mov ebx, eax
		push ebx
		call get_multi
		pop ebx
		mov ecx, eax
		jmp handle_nums

	random_nums:
		push str_random
		call printf
		add esp, 4
		call PRmulti
		mov ebx, eax
		push ebx
		call PRmulti
		pop ebx
		mov ecx, eax
		jmp handle_nums

	default_nums:
		push str_default
		call printf
		add esp, 4
		mov ebx, x_struct
		mov ecx, y_struct
		jmp handle_nums

	handle_nums:
		; printing first num
		push ecx
		push dword str_num1
		call printf
		add esp, 4
		pop ecx

		push ecx
		push dword ebx
		call print_multi
		pop ebx
		pop ecx

		; printing second num
		push ecx
		push dword str_num2
		call printf
		add esp, 4
		pop ecx
		
		push dword ecx
		call print_multi

		; printing result
		push dword str_result
		call printf
		add esp, 4
		push dword ebx
		call add_multi
		push dword eax
		call print_multi
		jmp main_finish
	
	main_finish:
		mov ebx, 0
		mov eax, 1
		int 0x80