[org 0x0100]
	jmp start
	brick1ax:dw 1
	brick1cx:dw 2
	brick2ax:dw 2
	brick2cx:dw 2
	brick3ax:dw 3
	brick3cx:dw 2
	brick4ax:dw 2
	brick4cx:dw 3
	oldisr:dd 0
	timisr: dd 0
	oldTisr:dd 0
	bandacx:dw 2
	bandaax:dw 1
	bandaFlag:dw 0
	bandarow:dw 23
	bandabrickflag:dw 0
	randomNum: dw 0
	randomNum1: dw 0
	randomNum2:dw 0
	tickcount:dw 0
	brick3timer:dw 0
	coinFlag:dw 0
	initialFlag:dw 0
	row19:dw 0
	row25:dw 0
	row19ax: dw 0
	row25ax: dw 0
	row25cx:dw 0
	coinax:dw 0
	coincx:dw 0
	score:dw 0
	scorestr:db 'Score: '
	nameinput:	db "Enter your name: "
	rollno1:db 'Azan Saeed 22L-6749'
	rollno2:db 'Muzamil Ahmed 22I-0970'
	message1:db 'Brick 1'
	message2:db 'Brick 2'
	message3:db 'Brick 3'
	message4:db 'Brick 4 vanishes after 7 seconds'
	message5:db 'Press Enter key to continue'
	welcomestr:db 'Welcome to Jumping Rabbit'
	pausestr:db 'Do you want to exit?'
	yesstr:db 'Press y to exit'
	nostr:db 'Press n to continue'
	escape:dw 0
	yes:dw 0
	no:dw 0

	username: times 20 db " "
	gameoverstr:db 'Game Over'
	screenbuffer:times 2000 dw 0



printEndScr:
push bp
push ax
push bx
push dx

push es
			call clrscr
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x041c         ; row 10 column 3 
			mov  cx, 9             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,gameoverstr			; offset of string    
            int  0x10
			
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x051c         ; row 10 column 3 
			mov  cx, 7             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,scorestr			; offset of string    
            int  0x10
			
			push word[score]
			call printnum1		

pop es
pop dx
pop bx
pop ax
pop bp
ret

printFrontScr:

push bp
push ax
push bx
push dx

push es
			call clrscr
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x0814         ; row 10 column 3 
			mov  cx, 25             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,welcomestr			; offset of string    
            int  0x10

			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x003C         ; row 10 column 3 
			mov  cx, 19             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,rollno1         ; offset of string    
            int  0x10
			
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x013A         ; row 10 column 3 
			mov  cx, 22             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,rollno2        ; offset of string    
            int  0x10
			
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x0a1e         ; row 10 column 3 
			mov  cx, 7             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,message1        ; offset of string    
            int  0x10
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x0b1e         ; row 10 column 3 
			mov  cx, 7             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,message2        ; offset of string    
            int  0x10

			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x0c1e         ; row 10 column 3 
			mov  cx, 7             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,message3        ; offset of string    
            int  0x10
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x0d1e         ; row 10 column 3 
			mov  cx, 32             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,message4        ; offset of string    
            int  0x10
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x0e1e         ; row 10 column 3 
			mov  cx, 17             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,nameinput        ; offset of string    
            int  0x10
			
			
		
		    mov si , 0
		
input1:	
        mov ah , 0x01
		int 0x21
		cmp al , 13
		je set1
		mov [username + si] , al
		add si , 1
		jmp input1
set1:
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x0f14         ; row 10 column 3 
			mov  cx, 27             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,message5        ; offset of string    
            int  0x10
			
			mov ah , 0x01
		    int 0x21
		    cmp al , 13
			je exitFrontscr
			
exitFrontscr:			

pop es
pop dx
pop bx
pop ax
pop bp

ret

printPauseScr:
push bp
push ax
push bx
push dx

push es
	mov word[escape],0
		pauseloop:
			call clrscr
			
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x041e         ; row 10 column 3 
			mov  cx, 20             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,pausestr			; offset of string    
            int  0x10
			
			
			
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x061c         ; row 10 column 3 
			mov  cx, 15             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,yesstr			; offset of string    
            int  0x10
			
			mov  ah, 0x13           ; service 13 - print string   
            mov  al, 1              ; subservice 01 – update cursor 
			mov  bh, 0              ; output on page 0 
			mov  bl, 7              ; normal attrib 
			mov  dx, 0x071c         ; row 10 column 3 
			mov  cx, 19             ; length of string 
			push cs             
			pop  es                 ; segment of string
			mov  bp,nostr			; offset of string    
            int  0x10
			
			mov ah,0
			int 0x16
			cmp al,121
			jne nxtchk
			jmp gameexit
			nxtchk:
			cmp al,110
			je countinue
			
			jmp pauseloop
			
			
			
countinue:	
			call printMainScreen

pop es
pop dx
pop bx
pop ax
pop bp

ret


	
	
	
	Delay:              
    push cx
    mov cx, 0xFFFF
    del:
    loop del
    pop cx
    ret
	
GenRandNum:
	push bp
	mov bp,sp
	push cx
	push ax
	push dx
	
	;mov ah,00h
	;int 1ah
	;mov ax,dx
	;xor dx,dx
	;mov cx,3
	;div cx
	;add dl,1
	
;	mov word[randomNum],dx
	mov ax,[tickcount]
	mov cx,3
	div cx
	add dx,1
	
	mov word[randomNum],dx
	


	
	pop dx
	pop ax
	pop cx
	pop bp
	
	ret
	
GenRandNum1:
	push bp
	mov bp,sp
	push cx
	push ax
	push dx
	mov ax,[tickcount]
	mov cx,4
	div cx
	add dx,1
	mov word[randomNum1],dx
	pop dx
	pop ax
	pop cx
	pop bp
	ret

GenRandNum2:
	push bp
	mov bp,sp
	push cx
	push ax
	push dx
	
	;mov ah,00h
	;int 1ah
	;mov ax,dx
	;xor dx,dx
	;mov cx,3
	;div cx
	;add dl,1
	
;	mov word[randomNum],dx
	mov ax,[tickcount]
	mov cx,6
	div cx
	add dx,1
	
	mov word[randomNum2],dx
	


	
	pop dx
	pop ax
	pop cx
	pop bp
	
	ret



phase2:
	push ax
	push es
	push di
	push si
	push cx
	push bx 
	push dx
	push ds
		mov ax, 0xb800 					
		mov es, ax 
		mov ds,ax
		mov di ,0
		mov si ,2
		mov dl ,7
		
		cmp di,0
		jne adition
		Phase2jmp:
		cmp dl, 0;
		je Phase2exit
		sub dl,1
		mov cx,72
		mov word bx,[es:di] ;Saved the charcter of 0 position
		cld
		rep movsw 
		
		mov word [es:di],bx;
		
		
		adition:
		;mov dh, dl
		mov al,7
		sub al, dl;
		mov di ,0
		mov si,0;
		phase2mul:
		add si,160;
		add di,160;
		sub al,1
		cmp al,0
		jne phase2mul
		add si,2
		;add di,14
		;add si,14
		jmp Phase2jmp
		Phase2exit:
		pop ds
		pop dx
		pop bx
		pop cx
		pop si
		pop di
		pop es
		pop ax
		ret 
		
phase2road:
	push ax
	push es
	push di
	push si
	push cx
	push bx 
	push dx
	push ds
	push bp

		mov ax, 0xb800 					; load video base in ax
		mov es, ax 
		mov ds,ax
		mov di ,1280
		mov bp, di
		mov si ,1278
		mov dl ,7
		;cmp di,1280
		;jne roadadition
		Phase2roadjmp:
		cmp dl, 0;
		je Phase2roadexit
		sub dl,1
		mov cx,80
		
		mov word bx,[es:di]
		std
		rep movsw 
		add di,2 
		mov word [es:di],bx;
		;sub di,2;
		
		roadadition:
		add bp, 160
		mov di,bp 
		mov si,di
		sub si,2
		jmp Phase2roadjmp
		Phase2roadexit:
		pop bp
		pop ds
		pop dx
		pop bx
		pop cx
		pop si
		pop di
		pop es
		pop ax
		ret 
		
printnum: 
 push bp
 mov bp, sp
 push es
 push ax
 push bx
 push cx
 push dx
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov ax, [bp+4] ; load number in ax
 mov bx, 10 ; use base 10 for division
 mov cx, 0 ; initialize count of digits
nextdigit: 
 mov dx, 0 ; zero upper half of dividend
 div bx ; divide by 10
 add dl, 0x30 ; convert digit into ascii value
 push dx ; save ascii value on stack
 inc cx ; increment count of values
 cmp ax, 0 ; is the quotient zero
 jnz nextdigit ; if no divide it again
 mov di, 2542 ; point di to 70th column
nextpos: 
 pop dx ; remove a digit from the stack
 mov dh, 0x07 ; use normal attribute
 mov [es:di], dx ; print char on screen
 add di, 2 ; move to next screen location
 loop nextpos ; repeat for all digits on stack
 pop di
 pop dx
 pop cx
 pop bx
 pop ax
 pop es
 pop bp
 ret 2
 
 printnum1: 
 push bp
 mov bp, sp
 push es
 push ax
 push bx
 push cx
 push dx
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov ax, [bp+4] ; load number in ax
 mov bx, 10 ; use base 10 for division
 mov cx, 0 ; initialize count of digits
nextdigit1: 
 mov dx, 0 ; zero upper half of dividend
 div bx ; divide by 10
 add dl, 0x30 ; convert digit into ascii value
 push dx ; save ascii value on stack
 inc cx ; increment count of values
 cmp ax, 0 ; is the quotient zero
 jnz nextdigit1 ; if no divide it again
 mov di, 870 ; point di to 70th column
nextpos1: 
 pop dx ; remove a digit from the stack
 mov dh, 0x07 ; use normal attribute
 mov [es:di], dx ; print char on screen
 add di, 2 ; move to next screen location
 loop nextpos1 ; repeat for all digits on stack
 pop di
 pop dx
 pop cx
 pop bx
 pop ax
 pop es
 pop bp
 
ret 2
 
 
		
timer:

push ax

inc word[cs:tickcount]



mov al,0x20
out 0x20,al

pop ax

iret
		
kbisr:		push ax
			push es

			mov ax, 0xb800
			mov es, ax						; point es to video memory

			in al, 0x60						; read a char from keyboard port

			cmp al, 0x39					; is the key left shift
			jne nomatch						; no, try next comparison
			cmp word[bandaFlag],0
			jne nomatch
			mov word[bandaFlag],1
						
nomatch:
			cmp al ,0x01
			jne nomatch1
			mov word [escape] , 1
			
nomatch1:						; DONT send EOI to PIC
			
			pop es
			pop ax
			jmp far [cs:oldisr]	

			mov al, 0x20
			out 0x20, al
			 
			pop es
			pop ax
			
			iret 
clrscr:	
push ax
push es
push di
			mov ax, 0xb800 					; load video base in ax
			mov es, ax 					; point es to video base
			mov di, 0 					; point di to top left column
									; es:di pointint to --> 0xB800:0000 (B8000)

nextchar: 	
		mov word [es:di], 0x0720 				; clear next char on screen
		add di, 2 						; move to next screen location
		cmp di, 4000 						; has the whole screen cleared
		jne nextchar
pop di
pop es
pop ax	
ret	; if no clear next position
sky:	
push ax
push es
push di
			mov ax, 0xb800 					; load video base in ax
			mov es, ax 					; point es to video base
			mov di, 0 					; point di to top left column
									; es:di pointint to --> 0xB800:0000 (B8000)

skychar: 	
		mov word [es:di], 0x3D20 				; clear next char on screen
		add di, 2 						; move to next screen location
		cmp di, 1120					; has the whole screen cleared
		jne skychar
pop di
pop es
pop ax	
ret	;

sun:	
push bp
mov bp,sp

push ax
push bx
push es
push di
push cx

			mov ax,[bp+4]
			mov cx,80
			mul cx
			add ax,[bp+6]
			mov cx,2
			mul cx
			mov bx, 0xb800 					; load video base in ax
			mov es, bx 					; point es to video base
			mov di, 150					; point di to top left column
									; es:di pointint to --> 0xB800:0000 (B8000)

 	
		mov word [es:di-2], 0x3E5C 
		mov word [es:di+2],0x3E7C
		mov word [es:di+6],0x3E2F
		add di,160
		mov word [es:di-2],0x3E2D
		mov word [es:di+2],0x3e2A
		mov word [es:di+6],0x3e2D
		add di,160
		
		
		mov word [es:di+6],0x3e5C
		mov word [es:di+2],0x3e7C
		mov word [es:di-2], 0x3e2F 
pop cx		
pop di
pop es
pop bx
pop ax
pop bp	
ret	; if no clear next position

mountain:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
			mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov si,[bp+4]

			mov di, 0 					; point di to top left column
			Loop1:
			add di,160
			dec si
			cmp si,0
			jne Loop1
			add di,[bp+6]
			add di,[bp+6]
			
			mov cx,5
			mov ax,di
mountainchar: 	
		mov word [es:di], 0x6A2F
		cmp si,0
		jz postspace
		mov dx,si
		spaces:
		add di,2
		mov word [es:di], 0x6020
		dec dx
		cmp dx,0
		jnz spaces
		postspace:
		add si,2
		add di,2
		mov word [es:di], 0x6A5C
		add ax,160
		sub ax,2
		mov di,ax
		loop mountainchar
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	
ret	;

building:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
			mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov si,[bp+4]
			mov di,[bp+6]
			add di,[bp+6]
			bmulti:
			add di,160
			dec si
			cmp si,0
			jnz bmulti
			sub si,2
			mov cl,3
			mov bl,3
			mov bh,3
	printBuilding:
			mov cl,bl
			cmp bh,0
			je bexit 
			mov ax,di
			mov word[es:di],0x0720
			walls:
				add di,160
				mov word[es:di],0x0f7C
				add di,2 
				mov word[es:di],0x0f7C
				add di,2 
				mov word[es:di],0x0f7C
				sub di,4
				sub cl,1
				cmp cl,0
				jnz walls
				mov di,ax
				add di,2
				mov word[es:di],0x0f5F
				add di,2
				mov word[es:di],0x0f5F
				add di,2
				sub di,160
				add bl,1
				sub bh,1
				jmp printBuilding
					
bexit:
mov cl,bl
sub cl,1
mov di,ax
add di,6
ewalls:
				add di,160
				mov word[es:di],0x0f7C
				sub cl,1
				cmp cl,0
				jnz ewalls
					
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	
ret	4;


grass:	
push bp
mov bp,sp
push ax
push es
push di
push dx

			mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax					; point di to top left column
			add ax,160	
			
			;mov ax, 0xb800 					; load video base in ax
			;mov es, ax 					; point es to video base
			;mov di, 1280					; point di to top left column
									; es:di pointint to --> 0xB800:0000 (B8000)

grasschar: 	
		mov word [es:di], 0x2820 				; clear next char on screen
		add di, 2 						; move to next screen location
		cmp di, ax						; has the whole screen cleared
		jne grasschar
pop dx
pop di
pop es
pop ax	
pop bp	
ret	

road:	
push bp
mov bp,sp
push ax
push es
push di
push dx
				
			mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax					; point di to top left column
			add ax,160						; es:di pointint to --> 0xB800:0000 (B8000)

roadchar: 	
		mov word [es:di], 0x0F5F 				; clear next char on screen
		add di, 2 						; move to next screen location
		cmp di, ax						; has the whole screen cleared
		jne roadchar
pop dx
pop di
pop es
pop ax	
pop bp
ret	

barrier:	
push bp
mov bp,sp
push ax
push es
push di
push dx
				
			mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax					; point di to top left column
			add ax,160						; es:di pointint to --> 0xB800:0000 (B8000)

barrierchar: 	
		mov word [es:di], 0x0F5F 				; clear next char on screen
		add di, 4 						; move to next screen location
		cmp di, ax						; has the whole screen cleared
		jl barrierchar
pop dx
pop di
pop es
pop ax	
pop bp	
ret	

water:	
push bp
mov bp,sp
push ax
push es
push di
push dx
				
			mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax					; point di to top left column
			add ax,160						; es:di pointint to --> 0xB800:0000 (B8000)

waterchar: 	
		mov word [es:di], 0x1f20 				; clear next char on screen
		add di, 2 						; move to next screen location
		cmp di, ax						; has the whole screen cleared
		jne waterchar
pop dx
pop di
pop es
pop ax	
pop bp
ret	

car:
push bp
mov bp,sp
push ax
push es
push di
push dx

		
		    mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov di,[bp+4]
			
			mov word[es:di],0x4F5F
			sub di,160
			mov word[es:di],0x0f5F
			add di, 162
			mov word[es:di],0x4F5F
			sub di,160
			mov word[es:di],0x0f5F
			
			add di,162
			
			mov word[es:di],0x4F5F
			
			mov word[es:di],0x4F5F
			sub di,160
			mov word[es:di],0x0f5F
			
			add di,162
			
			mov word[es:di],0x4F5F
			sub di,160
			mov word[es:di],0x0f5F
			add di,162
			mov word[es:di],0x4F5F
			sub di,160
			mov word[es:di],0x0f5F
			add di,322
			mov word[es:di],0x4f5C
			sub di,160
			mov word[es:di],0x0f5C
			add di,2
			mov word[es:di],0x0f5f
			add di,2
			mov word[es:di],0x0f5f
			add di,2
			;mov word[es:di],0x0f5f
			add di,160
			mov word[es:di],0x0f5c
			
			sub di,2
			mov word[es:di],0x4f5f
			sub di,2 
			mov word[es:di],0x4f5f
			add di,164
			mov word[es:di],0x4f7C
			mov cx,12
			carloop:
			sub di,2
			mov word[es:di],0x4f5f
			loop carloop
			;add di,164
			mov word[es:di],0x4f7C
			sub di,160
			mov word[es:di],0x0f2f
			add di,2
			mov word[es:di],0x4f5f
			add di,2
			mov word[es:di],0x4f5f
			add di,2
			mov word[es:di],0x4f2f
			add di,2
			mov word[es:di],0x0f7c
			add di,2
			mov word[es:di],0x0f5f
			add di,2
			mov word[es:di],0x0f7c
			add di,2
			mov word[es:di],0x0f5f
			add di,2
			mov word[es:di],0x0f7c
			sub di,170
			mov word[es:di],0x0f2f
			sub di,2 
			mov word[es:di],0x0f5f
			sub di,2 
			mov word[es:di],0x0f5f
			add di,480
			mov word[es:di],0x0f4f
			add di, 20
			mov word[es:di],0x0f4f
			
			


pop dx
pop di
pop es
pop ax	
pop bp
ret	
printMainScreen:
push ax
push cx

call clrscr
call sky
push 1
push 76
call sun
push 4
push 2
call mountain
push 12
push 3
call mountain

push 18
push 3
call building

push 31
push 3
call building

push 44
push 3
call building

push 59
push 3
call mountain
push 67
push 2
call mountain



push 7
call road

push 10
call barrier

push 13
call road



push 1460
call car

push 1674
call car


call PlayAnimation


mov ah,0x1
int 0x21


pop cx
pop ax

ret 


Phase31:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
		
		mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax					; point di to top left column
			add ax,160	
			
			;mov ax, 0xb800 					; load video base in ax
			;mov es, ax 					; point es to video base
			;mov di, 1280					; point di to top left column
									; es:di pointint to --> 0xB800:0000 (B8000)

phase31char: 	
		mov word [es:di], 0x1020 				; clear next char on screen
		add di, 2 						; move to next screen location
		cmp di, ax						; has the whole screen cleared
		jne phase31char	
			
			
			
			
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	



ret 2




brick1:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
		    mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax	
			; point di to top left column
			add di,[bp+6]
			add di,[bp+6]
			mov cx ,12
			
brick1char: 	
		mov word [es:di], 0x4020 				; clear next char on screen
		add di, 2 
		sub cx ,1; move to next screen location
		cmp cx, 0						; has the whole screen cleared
		jne brick1char
			
			
		
			
			
			
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	



ret 4

brick2:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
		    mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax	
			; point di to top left column
			add di,[bp+6]
			add di,[bp+6]
			mov cx ,16
			
brick2char: 	
		mov word [es:di], 0x4020 				; clear next char on screen
		add di, 2 
		sub cx ,1; move to next screen location
		cmp cx, 0						; has the whole screen cleared
		jne brick2char
			
			
		
			
			
			
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	



ret 4


brick3:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
		    mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax	
			; point di to top left column
			add di,[bp+6]
			add di,[bp+6]
			mov cx ,18
			
brick3char: 	
		mov word [es:di], 0x3E20 				; clear next char on screen
		add di, 2 
		sub cx ,1; move to next screen location
		cmp cx, 0						; has the whole screen cleared
		jne brick3char
			
			
		
			
			
			
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	



ret 4


brick4:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
		    mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax	
			; point di to top left column
			add di,[bp+6]
			add di,[bp+6]
			mov cx ,18
			
brick4char: 	
		mov word [es:di], 0x2020 				; clear next char on screen
		add di, 2 
		sub cx ,1; move to next screen location
		cmp cx, 0						; has the whole screen cleared
		jne brick4char
			
			
		
			
			
			
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	



ret 4



bluebackground:
push 15
	call Phase31
	push 16
	call Phase31
	push 17
	call Phase31
	push 18
	call Phase31
	push 19
	call Phase31
	push 20
	call Phase31
	push 21
	call Phase31
	push 22
	call Phase31
	push 23
	call Phase31
	push 24
	call Phase31
	push 25
	call Phase31

ret 

movebrick1:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx

    mov cx,[brick1cx]
	mov ax,[brick1ax]
	cmp cx,60     ;Range of brick on rightside
	jne farward1
	mov ax ,-4    ;Speed of brick
	mov [brick1ax],ax
	farward1:
	cmp cx,4	  ;Range of brick on leftside
	
	jne backward1
	mov ax ,2      ;speed of brick
	mov [brick1ax],ax
	 backward1:
	 
	 add cx,ax
	 mov [brick1cx],cx
	 push cx
	 push word[bp+4]
	call brick1
	
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp
ret 2



movebrick2:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx

    mov cx,[brick2cx]
	mov ax,[brick2ax]
	cmp cx,60
	jne farward2
	mov ax ,-1;
	mov [brick2ax],ax
	farward2:
	cmp cx,4
	
	jne backward2
	mov ax ,2;
	mov [brick2ax],ax
	 backward2:
	 
	 add cx,ax
	 mov [brick2cx],cx
	 push cx
	 push word[bp+4]
	call brick2
	
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp
ret 2


movebrick3:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx

    mov cx,[brick3cx]
	mov ax,[brick3ax]
	cmp cx,60
	jle farward3
	mov ax ,-3;
	mov [brick3ax],ax
	farward3:
	cmp cx,20
	
	jge backward3
	mov ax ,2 ;
	mov [brick3ax],ax
	 backward3:
	 
	 add cx,ax
	 mov [brick3cx],cx
	 push cx
	 push word[bp+4]
	call brick3
	
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp
ret 2


movebrick4:



push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx

    mov cx,[brick4cx]
	mov ax,[brick4ax]
	cmp cx,60
	jl farward4
	mov ax ,-3;
	mov [brick4ax],ax
	farward4:
	cmp cx,4
	
	jg backward4
	mov ax ,3 ;
	mov [brick4ax],ax
	 backward4:
	 
	 add cx,ax
	 mov [brick4cx],cx
	 push cx
	 push word[bp+4]
	call brick4
	
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp




ret 2

banda:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
		 
		mov ax, 0xb800 					; load video base in ax
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax	
			
			add di,[bp+6]
			add di,[bp+6]
	
			mov word[es:di],0x1e2F
			add di,2
			mov word[es:di],0x1e20
			add di,2
			mov word[es:di],0x1e5c
			sub di,162
			mov word[es:di],0x1E7C
			sub di,2
			mov word[es:di],0x1E2f
			add di,4
			mov word[es:di],0x1E5C
			sub di,162
			mov word[es:di],0x1E4f



pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp

ret 4

movebanda:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
    
	cmp word[bandaFlag],2  ;space is entered
	jne checkflag
	cmp word[bandarow],17
	jnge bandarowchk 
	sub word[bandarow],2; char coming downward 
	bandarowchk:
	;mov dx,0
	mov word[bandaFlag],0
	cmp word[bandabrickflag],2    ;This flag is set when successful jmp
	jne checkflag
	MOV word[bandabrickflag],1

	
	
	checkflag: 
	;mov dx,word[bandaFlag]
	cmp word[bandaFlag],1
	jne checkflag2
	;mov dx,word[bandarow]
	
	cmp word[bandarow],17
	jnge bandarowchk1 
	sub word[bandarow],2
	bandarowchk1:
	;mov word[bandarow],dx
	;mov dx,2
	mov word[bandaFlag],2
	mov cx,[bandacx]
	cmp word[row19],1
	jne rowchk
	mov dx,[brick1cx]
	rowchk:
	cmp word[row19],2
	jne rowchk1
	mov dx,[brick2cx]
	rowchk1:
	cmp word[row19],3
	jne rowchk2
	mov dx,[brick4cx]
	rowchk2:
	cmp word[row19],4
	jne rowchk6
	mov dx,[brick3cx]
	rowchk6:
	
	add cx,1
	cmp cx,dx
	jnge checkflag3
	cmp word[row19],1
	jne rowchk3
	add dx,7
	rowchk3:
	cmp word[row19],2
	jne rowchk4
	add dx,9
	rowchk4:
	cmp word[row19],3
	jne rowchk5
	add dx,10
	rowchk5:
	cmp word[row19],4
	jne rowchk7
	add dx,10
	rowchk7:
	
	add dx,4
	cmp cx,dx
	jnle checkflag3
	mov word[bandabrickflag],2
	jmp checkflag2
	checkflag3:
	jmp gameexit
	
	
	checkflag2:
	
	mov cx,[bandacx]
	
	 add cx,word[bandaax]
	 mov [bandacx],cx
	 push cx
	 push word[bandarow]
	 
	call banda
	
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp
ret 




initial:

push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx

	cmp word[initialFlag],0 ;InitialFlag is 0 only when 1st iteration
	jne iini3
	
	;Below part is only for 1st jmp
	mov word[initialFlag],1
	call GenRandNum
	mov ax,word[randomNum]
	mov word[row19],ax
	cmp word[row19],1
	jne ini1
	mov ax,word[brick1ax] ;brick1ax is the jump i.e speed of brick1
	mov word[row19ax],ax
	ini1:
	cmp word[row19],2
	jne ini2
	mov ax,word[brick2ax]
	mov word[row19ax],ax
	ini2:
	cmp word[row19],3
	jne ini3
	mov ax,word[brick4ax]
	mov word[row19ax],ax
	ini3:
	
	
	cmp word[row19],1
	jne randomjmp
	mov word[row25],2
	randomjmp:
	cmp word[row19],2
	jne randomjmp1
	mov word[row25],3
	randomjmp1:
	cmp word[row19],3
	jne randomjmp2
	mov word[row25],1
	randomjmp2:
	
	
	
	cmp word[row25],1
	jne iini1
	mov ax,word[brick1cx] ;matching the position of char with the brick
	mov word[bandacx],ax
	mov ax,word[brick1ax] ;matching speed of char with brick
	mov word[row25ax],ax
	jmp iini3
	iini1:
	
	cmp word[row25],2
	jne iini2
	mov ax,word[brick2cx]
	mov word[bandacx],ax
	mov ax,word[brick2ax]
	mov word[row25ax],ax
	jmp iini3
	iini2:
	cmp word[row25],3
	jne iini3
	mov ax,word[brick4cx]
	mov word[bandacx],ax
	mov ax,word[brick4ax]
	mov word[row25ax],ax
	;above is only for 1st jmp
	
	iini3:
	cmp word[bandabrickflag],1 ;bandabrickflag is 1 when jump successful
	jne initialchk10
	mov word[bandabrickflag],0
	
	cmp word[coinFlag],1 ;calculating the score 
	jne scorechk
	add word[score],30
	jmp scorechk1
	scorechk:
	add word[score],10
	
	scorechk1:
	call GenRandNum1
	add word[bandarow],4 ;moving char downward
	
	cmp word[row19],4 ;comparing brick
	jne row19chk
	mov ax,[tickcount]
	mov word[brick3timer],ax
	add word[brick3timer],130 ; adding 7 seconds 
	
	row19chk:
	mov ax,word[row19]
	mov word[row25],ax
	cmp ax,word[randomNum1] ;if row19 is same as row25
	jne diffrow
	cmp word[row25],1
	jne diffrow1
	mov word[row19],2
	jmp initialchk
	diffrow1:
	cmp word[row25],2
	jne diffrow2
	mov word[row19],1
	jmp initialchk
	diffrow2:
	cmp word[row25],3
	jne diffrow3
	mov word[row19],1
	jmp initialchk
	diffrow3:
	cmp word[row25],4
	jne initialchk
	mov word[row19],2
	jmp initialchk
	diffrow:	
	mov ax,word[randomNum1]
	mov word[row19],ax
	
	
	initialchk:
	call GenRandNum2
	cmp word[randomNum2],1
	jne randomcoinchk
	mov word[coinFlag],1
	jmp initialchk10
	randomcoinchk:
	cmp word[randomNum2],2
	jne randomcoinchk1
	mov word[coinFlag],0
	jmp initialchk10
	randomcoinchk1:
	cmp word[randomNum2],3
	jne randomcoinchk2
	mov word[coinFlag],1
	jmp initialchk10
	randomcoinchk2:
	cmp word[randomNum2],4
	jne randomcoinchk3
	mov word[coinFlag],0
	jmp initialchk10
	randomcoinchk3:
	cmp word[randomNum2],5
	jne randomcoinchk4
	mov word[coinFlag],1
	jmp initialchk10
	randomcoinchk4:
	mov word[coinFlag],0
	
	
	
	
	initialchk10:
	
	cmp word[coinFlag],1
	jne initialchk11
	push 18
	call movecoin
	
	
	initialchk11:
	
	push 20
	cmp word[row19],1
	jne initialchk2
	call movebrick1
	initialchk2:
	cmp word[row19],2
	jne initialchk3
	call movebrick2
	initialchk3:
	cmp word[row19],3
	jne initialchk4
	call movebrick4
	
	initialchk4:
	cmp word[row19],4
	jne initialchk9
	call movebrick3
	
	initialchk9:
	
	push 24
	cmp word[row25],1
	jne initialchk5	
	call movebrick1
	mov ax, [brick1ax]
	mov [bandaax],ax
	
	initialchk5:
	cmp word[row25],2
	jne initialchk6
	
	call movebrick2
	mov ax, [brick2ax]
	mov [bandaax],ax
	
	initialchk6:
	cmp word[row25],3
	jne initialchk7
	
	call movebrick4
	mov ax, [brick4ax]
	mov [bandaax],ax
	
	initialchk7:
	
	cmp word[row25],4
	jne iniexit
	mov ax,word[tickcount]
	cmp ax,word[brick3timer]
	jnle initialchk8
	
	call movebrick3
	mov ax, [brick3ax]
	mov [bandaax],ax
	
	jmp iniexit
	initialchk8:
	jmp gameexit
	
	
	

iniexit:

pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp


ret

coin:

push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx
		    mov ax, 0xb800 					
			mov es, ax
			mov ax,[bp+4]
			mov dx,160
			mul dx
			mov di, ax	
			
			add di,[bp+6]
			add di,[bp+6]
			
		    mov word [es:di], 0x6020
            add di,2
			mov word [es:di], 0x6020
							
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	

ret 4


movecoin:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx

    ;mov cx,[coincx]
	;mov ax,[coinax]
	
	cmp word[row19],1
	jne coinchk1
	mov cx,[brick1cx]
	add cx,4
	mov ax,[brick1ax]
	jmp coinchk
	coinchk1:
	cmp word[row19],2
	jne coinchk2
	mov cx,[brick2cx]
	add cx,6
	mov ax,[brick2ax]
	jmp coinchk
	coinchk2:
	cmp word[row19],3
	jne coinchk3
	mov cx,[brick4cx]
	add cx,7
	mov ax,[brick4ax]
	jmp coinchk
	coinchk3:
	
	mov cx,[brick3cx]
	add cx,7
	mov ax,[brick3ax]
	
	
coinchk: 
	 add cx,ax
	 mov [coincx],cx
	 push cx
	 push word[bp+4]
	call coin
	
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp
ret 2

printstr: 
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov di, 2530 ; point di to top left column 
 mov si, [bp+6] ; point si to string 
 mov cx, [bp+4] ; load length of string in cx 
 mov ah, 0x71 ; normal attribute fixed in al 
strchar: 
 mov al, [si] ; load next char of string 
 mov [es:di], ax ; show this char on screen 
 add di, 2 ; move to next screen location 
 add si, 1 ; move to next char in string 
 loop strchar ; repeat the operation cx times 
 pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 4



PlayAnimation:
push bp
mov bp,sp	
push ax
push dx
push es
push si
push di
push cx


	AniLoop:
	
	call phase2
	call phase2road
	call bluebackground
	
	mov ax,scorestr
	push ax
	push 7
	call printstr
	
	push word[score]
	call printnum
	
	call movebanda

	call initial
	
	
	
	ee:
	call Delay
	call Delay
	
	call Delay
	call Delay
	call Delay
	
	cmp word[escape],1
	jne aniloopnext
	call printPauseScr
	aniloopnext:
	
	jmp AniLoop
	
pop cx	
pop di
pop si
pop es
pop dx
pop ax
pop bp	
	ret





start:
			;call  ScreenBuffer

			call printFrontScr

			xor ax, ax
			mov es, ax


			mov ax, [es:8*4]
			mov [timisr], ax

			mov ax, [es:8*4+2]
			mov [timisr+2], ax
			
			mov ax, [es:9*4]
			mov [oldisr], ax

			mov ax, [es:9*4+2]
			mov [oldisr+2], ax
			
			

			cli
			mov word[es:8*4],timer
			mov [es:8*4+2],cs
			
			mov word [es:9*4], kbisr
			mov [es:9*4+2], cs
			sti

			call printMainScreen

gameexit:
			call printEndScr
			
			
			
			cli			
			mov ax,[oldisr]
			mov  [es:9*4],ax
			mov ax,[oldisr+2]
			mov  [es:9*4+2],ax
			mov ax,[timisr]
			mov  [es:8*4],ax
			mov ax,[timisr+2]
			mov  [es:8*4+2],ax
			sti 
			
			
			
			;call ScreenBuffer2

mov ax, 0x4c00 
int 0x21

