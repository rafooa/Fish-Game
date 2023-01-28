[org 0x0100]
jmp start
oldisr: dd 0 ; space for saving old isr 
oldisr2: dd 0 ; for timer isr
data1: db '_' 
data2: db '|'
data3: db ' '
data4: db '='
length: dw 1
fish: dw 3280
endflag: dw 0
tickcount: dd 0
coin1: dw 3240
coin2: dw 3690
coin1life: dw 180
coin2life: dw 90
score: dw 0
message: db 'Score: ' ; string to be printed
length2: dw 7 ; length of string
etrname: dw 'enter name: $'
inst: dw 'Instructions for the game are as follows$ '
rightarr: dw 'Press the right arrow key (->) to move right$ '
leftarr: dw 'Press the left arrow key (<-) to move left$ ' 
uparr: dw 'Press the up arrow key (^) to move up$ ' 
downarr: dw 'Press the down arrow key (v) to move down$ ' 
points: dw 'Points will be scored by collecting the gems$'
greetings: dw 'Hello! $'
gamename: dw 'Welcome to FISH MANIA$ '
greetings2: dw'We sincerely hope you have a GOOD TIME!!!!$'
conti: dw 'Press enter to continue, or press Esc to exit the game$ '
develop: dw 'Developed by:$'
name1:dw 'Hadi Ali 21L-5426$'
name2:dw 'Rafay Junaid 21L-7607$'
quit1:dw 'Are you sure you want to quit? Press (Y) for yes and (N) for no $'
wrongentry: dw 'wrong entry please try again$ '
buffer: times 4000 db 0
name: db 80
db 0
times 80 db 0

decision: db 80
db 0
times 80 db 0

escFlag: dw 0


;--------------------------------------------------------------------
; subroutine to clear the screen phase 5
;--------------------------------------------------------------------
clrscrphase5:		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push di

		mov ax, 0xb800
		mov es, ax ; point es to video base
		xor di, di ; point di to top left column ... es:di-->b800:0000

		mov ax, word[bp+4] ; space char in normal attribute
		mov cx, 2000 ; number of screen locations
	
		cld ; auto increment mode
		rep stosw ; clear the whole screen

		pop di 
		pop cx
		pop ax
		pop es
		pop bp
		ret 2
;---------------------------------------------------------------------
;-----------------------------------------------------------------
; subroutine to restore the screen
;-----------------------------------------------------------------
restoreScreen:		pusha
			push ds
			push es	


			mov cx, 4000 ; number of screen locations

					

			mov ax, 0xb800
			mov es, ax ; ds = 0xb800

			push cs
			pop ds
		
			mov si, buffer
			mov di, 0

			cld ; set auto increment mode
			rep movsb ; save screen

			;[es:di] = [ds:si]
			pop es
			pop ds

popa
			ret	
;-----------------------------------------------------------------
 ;-----------------------------------------------------------------
; subroutine to save the screen
;-----------------------------------------------------------------
saveScreen:	pusha	
		push ds
		push es

			mov cx, 4000 ; number of screen locations

					

			mov ax, 0xb800
			mov ds, ax ; ds = 0xb800

			push cs
			pop es
		
			mov si, 0
			mov di, buffer
 
			cld ; set auto increment mode
			rep movsb ; save screen

			;[es:di] = [ds:si]
			
			pop es
			pop ds
			popa
			ret
;-----------------------------------------------------------------
clrscrphase52:		push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column

nextloc:	mov word [es:di], 0x0720	; clear next char on screen
			add di, 2					; move to next screen location
			cmp di, 4000				; has the whole screen cleared
			jne nextloc					; if no clear next position

			pop di
			pop ax
			pop es
			ret
;-----------------------------------------------------------------------------

;-------------------------------------------------------------------
; move first 2/3rd of screen left and right
;------------------------------------------------------------------
moving:			push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			push ds

			mov si, 8
			mov bp, [bp+4]
			cmp bp, 1
			je left
			jne right
			
left:			mov di, 160
			jmp st
right:			mov di, 2718
			mov si, 9
			jmp st

st:			add bp, bp
			mov dx, bp
			add dx, dx
			mov ax, 0xb800
			mov es, ax
			
nextrow:		mov cx, 78
			mov bx, [es:di]
			add di, bp

shift:			mov ax, [es:di]
			sub di, bp
			mov [es:di], ax
			add di, dx
			loop shift
			mov ax, [es:di]
			sub di, bp
			mov [es:di], ax
			add di, bp
			mov [es:di], bx
			add di, bp
			sub si, 1
			cmp si, 0
			jnz nextrow

			pop ds
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 2
;-------------------------------------------------------------------
; print score
;-------------------------------------------------------------------

printscore:	push bp
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push di

		mov ax, 0
		push ax 		; push row number
		mov ax, 70
		push ax 		; push col number
		
		mov ax, 0x70 		; blue on black attribute
		push ax 		; push attribute
		mov ax, message
		push ax 		; push address of message
		push word [length2] 	; push message length
		call printstr 		; call the printstr subroutine

			mov ax, 0xb800
			mov es, ax ; point es to video base
			mov ax, [bp+4] ; load number in ax
			mov bx, 10 ; use base 10 for division
			mov cx, 0 ; initialize count of digits

nextdigit: 		mov dx, 0 ; zero upper half of dividend
			div bx ; divide by 10
			add dl, 0x30 ; convert digit into ascii value
			push dx ; save ascii value on stack
			inc cx ; increment count of values
			cmp ax, 0 ; is the quotient zero
			jnz nextdigit ; if no divide it again
			mov di, 152 ; point di to 78th column

nextpos: 		pop dx ; remove a digit from the stack
			mov dh, 0x70 ; use normal attribute
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
;-------------------------------------------------------------------
; boundary sound notification
;-------------------------------------------------------------------
playsound:		push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

mov cx, 5
looped:         mov al, 0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay

;load the counter 2 value for a3
mov ax, 152fh
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay
	
;load the counter 2 value for a4
mov ax, 0A97h
out 42h, al
mov al, ah
out 42h, al
	
;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay
loop looped
			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 
;-------------------------------------------------------------------
; keyboard interrupt service routine
;-------------------------------------------------------------------

kbisr:			push ax
			push es
			push di 

			mov ax, 0xb800
			mov es, ax ; point es to video memory
			mov di, [cs:fish]

			in al, 0x60 ; read a char from keyboard port

			cmp al, 0x4b ; has the left key pressed
			jne nextcmp ; no, try next comparison

			mov ax, 0x1720
			mov [es:di], ax
			
			cmp di, 3040
			je overlapleft
			cmp di, 3200
			je overlapleft
			cmp di, 3360
			je overlapleft
			cmp di, 3520
			je overlapleft
			cmp di, 3680
			je overlapleft
			cmp di, 3840
			je overlapleft

			sub di, 2
			mov ax, 0x141B
			mov [es:di], ax
			mov [cs:fish], di
			jmp exit ; leave interrupt routine

overlapleft:		add di, 158
			mov ax, 0x141B
			mov [es:di], ax
			mov [cs:fish], di
			jmp exit ; leave interrupt routine
		
nextcmp:		cmp al, 0x4d ; has the right key pressed
			jne nextcmp2 ; no, try next comparison

			mov ax, 0x1720
			mov [es:di], ax

			cmp di, 3198
			je overlapright
			cmp di, 3358
			je overlapright
			cmp di, 3518
			je overlapright
			cmp di, 3678
			je overlapright
			cmp di, 3838
			je overlapright
			cmp di, 3998
			je overlapright

			add di, 2
			mov ax, 0x141B
			mov [es:di], ax
			mov [cs:fish], di
			jmp exit; leave interrupt routine

overlapright:		sub di, 158
			mov ax, 0x141B
			mov [es:di], ax
			mov [cs:fish], di
			jmp exit ; leave interrupt routine
		
nextcmp2:		cmp al, 0x48 ; has the up key pressed
			jne nextcmp3 ; no, try next comparison

			mov ax, di
			sub ax, 160
			cmp ax, 3040
			jb hitboundary1
			
			mov ax, 0x1720
			mov [es:di], ax
			sub di, 160
			mov ax, 0x141B
			mov [es:di], ax
			mov [cs:fish], di

			jmp exit ; leave interrupt routine 

nextcmp3:		cmp al, 0x50 ; has the down key pressed
			jne nextcmp4; no, leave interrupt routine

			mov ax, di
			add ax, 160
			cmp ax, 4000
			jae hitboundary

			mov ax, 0x1720
			mov [es:di], ax
			add di, 160
			mov ax, 0x141B
			mov [es:di], ax
			mov [cs:fish], di

			jmp exit; leave interrupt routine

nextcmp4:		cmp al, 0x01 ; has the escape key been pressed
			je escapepress2
			jmp comeback; no, leave interrupt routine


hitboundary1:
jmp hitboundary

escapepress2:
call saveScreen

mov ax,0x3020
push ax
call clrscrphase5

exitscreen2:
mov ah, 0x02          ; point cursor with dx
mov bh, 0
mov dx, 0x0A0A
int 0x10

mov dx, quit1
mov ah , 0x09          ;string printing
int 0x21




escLoop:
in al, 0x60


cmp al,0x31
jz restore2

cmp al,0x15
jnz skipYes
mov word[cs:escFlag], 1
skipYes:
cmp al,0x15
jnz escLoop
jz comeback


restore2:
call restoreScreen
jmp  comeback




hitboundary:		call playsound

exit:			mov ax, [cs:fish]
			cmp ax, [cs:coin1]
			je greenfound
			cmp ax, [cs:coin2]
			je redfound
						

comeback:		mov al, 0x20
			out 0x20, al ; send EOI to PIC

			pop di
			pop es
			pop ax
			iret ; return from interrupt

;--------------------------------------------------------------------
; subroutine for when green coin found
;--------------------------------------------------------------------

greenfound:		push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

			add word[cs:score], 10
			mov ax, [cs:score]
			push ax
			call printscore

			mov word[cs:coin1life], 0	
			call printcoin

			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			jmp comeback
;--------------------------------------------------------------------
; subroutine for when red coin found
;--------------------------------------------------------------------

redfound:		push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

			add word[cs:score], 50
			mov ax, [cs:score]
			push ax
			call printscore
		
			mov word[cs:coin2life], 0
			call printcoin2


			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			jmp comeback

;--------------------------------------------------------------------
; subroutine to print the sea and sky
;--------------------------------------------------------------------

printseasky:		push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

		mov ax, 0xb800
		mov es, ax
		xor di, di
		
		mov cx, 640
		mov ax, 0x3020	; space in cyan background

		cld
		rep stosw

		mov ax, 0xb800
		mov es, ax ; point es to start of grid
		mov di, 1280 ; point di to top left of middle section 

		mov ax, 0x1720 ; space in blue background
		mov cx, 800 ; number of screen locations
	
		cld 
		rep stosw 
		
		mov cx, 80
		mov ax, 0x105F

		cld 
		rep stosw

		mov ax, 0x1720 ; space in blue background
		mov cx, 480 ; number of screen locations
	
		cld 
		rep stosw 

			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 

;--------------------------------------------------------------------
; subroutine to print fish
;--------------------------------------------------------------------

printfish:		push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

		mov ax, 0xb800
		mov es, ax
		mov di, [fish]

		mov ax, 0x141B	; blue background red escape key
		mov [es:di], ax

			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 


;--------------------------------------------------------------------
; subroutine to print a building
;--------------------------------------------------------------------

Building:	
			push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

		mov bx, 7
		mov dx, 0
		add dx, [bp+4]	; parameter passed for offset
		mov cx, [bp+6]	; parameter passed for height of building
		
		mov si, 8
color1:				
		mov ax, bx
		push ax					
		mov ax, dx
		push ax					
		mov ax, 0x7E			
		push ax					
		mov ax, data4
		push ax					
		push word [length]

		call printstr
		
		inc dx
		dec si
		cmp si, 0
		jnz color1

		mov si, 8
		sub dx, 8
		dec bx
		loop color1

			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 4
;--------------------------------------------------------------------
; subroutine to print a boat
;--------------------------------------------------------------------

boat:			push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

		mov bx, 9		;starting row of boats
		add bx, [bp+4]
		add dx, 5		;space between boats
		mov cx, [bp+6]
	
boat0:		mov ax, bx
		push ax					
		mov ax, dx
		push ax					
		mov ax, 0x17			
		push ax					
		mov ax, data1
		push ax					
		push word [length]			
		
		call printstr	

		add dx, 1
		loop boat0
		
		mov cx, [bp+6]
		sub dx, cx
		add bx, 1

boat1:		mov ax, bx
		push ax					
		mov ax, dx
		push ax					
		mov ax, 0x40			
		push ax					
		mov ax, data3
		push ax					
		push word [length]			
	
		call printstr
	
		add dx, 1
		loop boat1

		add cx, [bp+6]
		sub cx, 2
		add bx, 1
		sub dx, cx
		sub dx, 1

boat2:		mov ax, bx
		push ax					
		mov ax, dx
		push ax					
		mov ax, 0x40			
		push ax					
		mov ax, data3
		push ax					
		push word [length]			
	
		call printstr
	
		add dx, 1
		loop boat2

		add cx, [bp+6]
		sub cx, 4
		add bx, 1
		sub dx, cx
		sub dx, 1

boat3:		mov ax, bx
		push ax					
		mov ax, dx
		push ax					
		mov ax, 0x40			
		push ax					
		mov ax, data3
		push ax					
		push word [length]			
	
		call printstr
	
		add dx, 1
		loop boat3


			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 4
;--------------------------------------------------------------------
; subroutine to clear the screen
;--------------------------------------------------------------------
clrscr:			push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

		mov ax, 0xb800
		mov es, ax ; point es to video base
		xor di, di ; point di to top left column ... es:di-->b800:0000

		mov ax, 0x0720 ; space char in normal attribute
		mov cx, 2000 ; number of screen locations
	
		cld ; auto increment mode
		rep stosw ; clear the whole screen

			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 
;-------------------------------------------------------------------
;  delay subroutine
;-------------------------------------------------------------------

delay:    		push cx
			mov cx, 0x5FFF
loop1:			loop loop1

			pop cx
			ret

;--------------------------------------------------------------------
; subroutine to print a string at top left of screen
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
printstr:	push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di

		mov ax, 0xb800
		mov es, ax 			; point es to video base

		mov al, 80 			; load al with columns per row
		mul byte [bp+12] 		; multiply with row number
		add ax, [bp+10] 		; add col
		shl ax, 1 			; turn into byte offset

		mov di, ax 			; point di to required location
		mov si, [bp+6] 			; point si to string
		mov cx, [bp+4] 			; load length of string in cx
		mov ah, [bp+8] 			; load attribute in ah

		cld 				; auto increment mode

nextchar:	lodsb 				; load next char in al -> mov al,[ds:si] -> add si,1
		stosw 				; print char/attribute pair -> mov [es:di], ax -> add di,2
		loop nextchar 			; repeat for the whole string
		
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10

;------------------------------------------------------
; print green coin at random location every 10 seconds
;------------------------------------------------------
printcoin: 		push di
			push dx
			push ax
			push es
			push bx
			

; initialize			
			xor di, di
			mov dx, 0
			mov ax, 0xb800
			mov es, ax ; point es to video base


; calculate time intervals 

			mov ax, [cs:coin1life]
			cmp ax, 0
			jne terminate

		
; clear previous coin
			mov di, [cs:coin1]

			cmp di, [cs:fish]
			jne notfish
			mov ax, 0x141B
			jmp replace

notfish:		mov ax, 0x1720
replace:		mov [es:di], ax
			

; randomly generate position of new coin
			mov ax, [cs:tickcount]
			mov bx, 25173
			mul bx
			add ax, 13849
			mov bx, 960
			div bx
			mov di, dx
			inc di
			add di, 3040


; place new coin in random position			
			mov ax, 0x2020
			mov [es:di], ax
			mov [cs:coin1], di
			mov word[cs:coin1life], 180
	

terminate:		

			pop bx
			pop es
			pop ax
			pop dx
			pop di
			ret 

;------------------------------------------------------
; print redcoin at random location every 5 seconds
;------------------------------------------------------
printcoin2: 		push di
			push dx
			push ax
			push es
			push bx
			

; initialize			
			xor di, di
			mov dx, 0
			mov ax, 0xb800
			mov es, ax ; point es to video base


; calculate time intervals 

			mov ax, [cs:coin2life]
			cmp ax, 0
			jne terminate1

		
; clear previous coin
			mov di, [cs:coin2]

			cmp di, [cs:fish]
			jne notfish1
			mov ax, 0x141B
			jmp replace1

notfish1:		mov ax, 0x1720
replace1:		mov [es:di], ax
			

; randomly generate position of new coin
			mov ax, [cs:tickcount]
			mov bx, 25177
			mul bx
			add ax, 13843
			mov bx, 960
			div bx
			mov di, dx
			inc di
			add di, 3040


; place new coin in random position			
			mov ax, 0x4020
			mov [es:di], ax
			mov [cs:coin2], di
			mov word[cs:coin2life], 90		

terminate1:		

			pop bx
			pop es
			pop ax
			pop dx
			pop di
			ret 

;------------------------------------------------------
; timer interrupt service routine
;------------------------------------------------------
timer:			push ax
			inc word [cs:tickcount]; increment tick count
			dec word[cs:coin1life]
			dec word[cs:coin2life]

			call printcoin
			call printcoin2

			mov ax, 1
			push ax
			call moving
			mov ax, -1
			push ax
			call moving
			
			mov al, 0x20
			out 0x20, al ; end of interrupt

			pop ax
			iret ; return from interrupt

;------------------------------------------------------
; 2 coins at start
;------------------------------------------------------
printcoins:		push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di

		

		mov ax, 0xb800
		mov es, ax

		mov di, [coin1]
		mov ax, 0x2020
		mov [es:di], ax

		mov di, [coin2]
		mov ax, 0x4020
		mov [es:di], ax

			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 
;------------------------------------------------------

start:		mov ax,0x3020
		push ax
		call clrscrphase5

mov ah, 0x02          ; point cursor with dx
mov bh, 0
mov dx, 0x0A0B
int 0x10

mov dx, etrname
mov ah , 0x09          ;string printing
int 0x21

mov dx, name
mov ah, 0x0A          ; input name
int 0x21

mov bh, 0
mov bl,[name + 1]       ;place dollar
mov byte[name + 2+ bx], '$'

		mov ax,0x3020
		push ax
call clrscrphase5

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0019
int 0x10

mov dx, gamename
mov ah , 0x09         ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0200
int 0x10

mov dx, greetings
mov ah , 0x09         ;string printing
int 0x21

mov dx, name + 2
mov ah , 0x09           ;print name
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0400
int 0x10

mov dx, greetings2
mov ah , 0x09         ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0600
int 0x10

mov dx, inst
mov ah , 0x09          ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x080F
int 0x10

mov dx, rightarr
mov ah , 0x09          ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x090F
int 0x10

mov dx, leftarr
mov ah , 0x09          ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0A0F
int 0x10

mov dx, uparr
mov ah , 0x09          ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0B0F
int 0x10

mov dx, downarr
mov ah , 0x09         ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0D0F
int 0x10

mov dx, points
mov ah , 0x09         ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x0F00
int 0x10

mov dx, conti
mov ah , 0x09         ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x1443
int 0x10

mov dx, develop
mov ah , 0x09         ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x153F
int 0x10

mov dx, name1
mov ah , 0x09         ;string printing
int 0x21

mov ah, 0x02           ; point cursor with dx
mov bh, 0
mov dx, 0x163B
int 0x10

mov dx, name2
mov ah , 0x09         ;string printing
int 0x21

decideagain:
mov ah, 0								; service 0 – get keystroke
int 0x16

cmp ah,0x01
je escapepress
cmp ah,0x1C
je continue
jne decideagain

escapepress:
call saveScreen

mov ax,0x3020
push ax
call clrscrphase5

exitscreen:
mov ah, 0x02          ; point cursor with dx
mov bh, 0
mov dx, 0x0A0A
int 0x10

mov dx, quit1
mov ah , 0x09          ;string printing
int 0x21

mov ah,0
int 0x16

cmp al,121
jz exitt
cmp al,110
je restore
jmp enteragain

restore:
call restoreScreen
jmp  decideagain


enteragain:
mov ah, 0x02          ; point cursor with dx

mov bh, 0
mov dx, 0x0C17
int 0x10

mov dx, wrongentry
mov ah , 0x09          ;string printing
int 0x21
jmp exitscreen

exitt:
jmp exittt

continue:
		xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:8*4]			
		mov [oldisr2], ax ; save offset of old routine
		mov ax, [es:8*4+2]
		mov [oldisr2+2], ax ; save segment of old routine

		cli ; disable interrupts
		mov word [es:8*4], timer; store offset at n*4
		mov [es:8*4+2], cs ; store segment at n*4+2
		sti ; enable interrupts

		mov dx, start ; end of resident portion
		add dx, 15 ; round up to next para
		mov cl, 4
		shr dx, cl ; number of paras

		xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:9*4]
		mov [oldisr], ax ; save offset of old routine
		mov ax, [es:9*4+2]
		mov [oldisr+2], ax ; save segment of old routine

		cli ; disable interrupts
		mov word [es:9*4], kbisr ; store offset at n*4
		mov [es:9*4+2], cs ; store segment at n*4+2
		sti ; enable interrupts

		mov dx, start ; end of resident portion
		add dx, 15 ; round up to next para
		mov cl, 4
		shr dx, cl ; number of paras..../2^4

		call clrscr
		call printseasky
		call printfish
		call printcoins
		mov ax, [score]
		push ax
		call printscore
		
build:		mov ax, 7		; height
		push ax
		mov ax, 10		; offset
		push ax
		call Building

		mov ax, 4		; height
		push ax
		mov ax, 19		; offset
		push ax
		call Building

		mov ax, 7		; height
		push ax
		mov ax, 30		; offset
		push ax
		call Building

		mov ax, 5		; height
		push ax
		mov ax, 36		; offset
		push ax
		call Building

		mov ax, 6		; height
		push ax
		mov ax, 45		; offset
		push ax
		call Building

		mov ax, 2		; height
		push ax
		mov ax, 54		; offset
		push ax
		call Building

		mov ax, 5		; height
		push ax
		mov ax, 63		; offset
		push ax
		call Building

		mov ax, 0
		mov bx, 0
		mov cx, 0
		mov dx, 0
		
ship:		mov dx, 0

		mov ax, 10		; size of first boat
		push ax
		mov ax, 1		; depth of first boat
		push ax
		call boat

		mov ax, 20		; size of second boat
		push ax
		mov ax, 2		; depth of second boat
		push ax
		call boat
	
		mov ax, 15		; size of third boat
		push ax
		mov ax, 1		; depth of third boat
		push ax
		call boat

		mov ax, 18		; size of fourth boat
		push ax
		mov ax, 3		; depth of fourth boat
		push ax
		call boat

		mov ax, 0
		mov bx, 0
		mov cx, 0
		mov dx, 0

move:		
		mov cx, [escFlag]
		cmp cx, 0
		je move		

		

		call clrscr
		

		mov ax, [oldisr2]								; read old offset in ax
		mov bx, [oldisr2+2]								; read old segment in bx
			
		cli										; disable interrupts
		mov [es:8*4], ax								; restore old offset from ax
		mov [es:8*4+2], bx								; restore old segment from bx
		sti										; enable interrupts 	


		mov ax, [oldisr]								; read old offset in ax
		mov bx, [oldisr+2]								; read old segment in bx
			
		cli										; disable interrupts
		mov [es:9*4], ax								; restore old offset from ax
		mov [es:9*4+2], bx								; restore old segment from bx
		sti										; enable interrupts 			
		
exittt:
call clrscr
		mov ax, 0x4c00 ; terminate program
		int 0x21