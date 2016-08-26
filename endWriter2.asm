; prepare animation of witch's eyes
; optimisation required
;	+ Lock ASIC when needed
;	+ call vanishMsgBg (2 VBLs)
;	+ call burnCharXY (2 VBLs)
; 	+ call setEWPositionInit / initStringSprite should be separated (2 VBLs)

;MONOFONTE	EQU 1
;ZOOM_EW		EQU %1101
;NB_LINE		EQU 16
;NB_COLUMNS	EQU 9
;NB_MASK		EQU 7
;INTER_CHAR	EQU 8
;INTER_LINE	EQU 10
;EW_X_POS	EQU 110+4
;EW_Y_POS	EQU 4
;VANISH_TIME	EQU 150		; # frames for the msg to disapear

nolist

; ******************
; * Init EndWriter *
; ******************
.initEndWriter
	;di
	push de

	call initFonte		; init fonte zoom/colors
	ld l,EW_Y_POS

	; * WARNING
	; * THOSE TWO CALLS are strange as they jump right in the processing loop below
	; * But this seem to work well anyway so ... nah don't care !
	call setEWPositionInit	; set Position for next line
	call initStringSprite	; set sprites in asic memory

	pop de
	;ei
	ret

.moveEndWriter
	call moveDisplayString
	ret

; IMPORTED CODE

; *******************
; * moveDisplayString
; *******************
.moveDisplayString

	; **********
	; * SPLITE *
	; **********
	.spliteFunctions
	nop:nop:nop

	.pauseBeforeVanish
	ld a,0
	or a
	jr z,nextStringCounter 		; no vanish in progress
	dec a
	ld (pauseBeforeVanish+1),a
	ret nz				; vanish in progress wait

	; clear screen
	;call vanishMsgBg
	jp vanishMsgBg
	.vanishEnd

	; normal scroll
	.nextStringCounter 
	ld a,INTER_CHAR*NB_COLUMNS + EW_X_POS - 1	; minus 1 because of jr nc, instead of jr nz,
	sub EW_SCROLL_SPEED
	jr nc,contineString

	; ***********************************************
	; Here, we have to burn the string on screen
	; And prepare the next sprites
	; ***********************************************

	; 1, Burn the String on screen
	.nextLine
	ld l,EW_Y_POS
	ld c,EW_X_POS
	.msgOffset
	ld de,ewMsg1
	;call burnCharXY	; don't affect L
	jp burnCharXY
	.burnCharXYEnd

	.set_offset_next_string
	ld (msgOffset+1),de

	; 2, Test if we reached end of screen (end of message)
	.nextString
	ld a,INTER_LINE
	add l
	cp NB_LINE*INTER_LINE+EW_Y_POS
	jr nz,contineNextLine
	
	; If we reached the message end
	; then force next scenario to be played
	.nbMsgPages
	ld a,EW_PAGES
	dec a
	jp z,forceNextScenario
	ld (nbMsgPages+1),a

	; End of message/screen
	; Reset the line position to the first displayed line
	ld a,VANISH_TIME
	ld (pauseBeforeVanish+1),a
	ld a,EW_Y_POS
	
	; **************
	; Next Line part
	; **************
	.contineNextLine
	ld (nextLine+1),a	; save Next line to be displayed

	ld l,a			; set Y Offset in L
	
	ld a,(de)
	or a
	jr nz,noEmptyLine

	; Here We have an empty line, so
	; - inc de
	; - jump to .set_offset_next_string
	inc de
	jp set_offset_next_string

	.noEmptyLine
	jp setEWPositionInit	; set Position for next line
	;call initStringSprite	; next string
	.initStringSpriteEnd
	
	ld a,INTER_CHAR*NB_COLUMNS + EW_X_POS - 1	; reset counter
	
	; *************
	; X Scroll part
	; *************
	.contineString
	ld (nextStringCounter+1),a	;save counter

	; get X position
	ld h,translationXTable/256
	ld l,a
	ld a,(hl)

	call setEWPositionX

	ret

; Reset Original BackGround
; Corrupted BC,HL
; IMPORTANT, DE should keep its original value
.vanishMsgBg
	;push de
	ld (vanishMsgBg_Splite2+1),de	; save DE to be restored at the end of the split

	; set memory bank to  b0;b1;b2;03
	ld bc,&7FC1	
	out (c),c

	let orig = &E037+2
	let dest = &6037+2
	let count = 0
	
	repeat NB_LINE*8/2
		ld hl,orig		; set initial bg offset
		ld de,dest
		call vanishMsgRaster

		let orig = orig + &800
		if orig/4096 = 0
			let orig = orig - &4000 + 96
		endif

		let dest = dest + &800
		if dest/4096 = 8
			let dest = dest - &4000 + 96
		endif

		; line counter
		let count = count + 1
		if count mod 8 = 0
			repeat INTER_LINE-8
				let orig = orig + &800
				if orig/4096 = 0
					let orig = orig - &4000 + 96
				endif

				let dest = dest + &800
				if dest/4096 = 8
					let dest = dest - &4000 + 96
				endif
			rend
		endif
	rend

	; reset memory bank to  b0;b1;b2;b3	
	ld bc,&7FC0	
	out (c),c

	; set SPLITE Function
	ld a,&C3		; JP xxxx
	ld (spliteFunctions),a	
	ld hl,vanishMsgBg_Splite2
	ld (spliteFunctions+1),hl	

;	pop de
	ret

; vanishMsgBg_Splite2
; *******************
.vanishMsgBg_Splite2
	ld de,0
	push de

	; set memory bank to  b0;b1;b2;03
	ld bc,&7FC1	
	out (c),c

	;let orig = &E037+2
	;let dest = &6037+2
	let count = 0
	
	repeat NB_LINE*8/2
		ld hl,orig		; set initial bg offset
		ld de,dest
		call vanishMsgRaster

		let orig = orig + &800
		if orig/4096 = 0
			let orig = orig - &4000 + 96
		endif

		let dest = dest + &800
		if dest/4096 = 8
			let dest = dest - &4000 + 96
		endif

		; line counter
		let count = count + 1
		if count mod 8 = 0
			repeat INTER_LINE-8
				let orig = orig + &800
				if orig/4096 = 0
					let orig = orig - &4000 + 96
				endif

				let dest = dest + &800
				if dest/4096 = 8
					let dest = dest - &4000 + 96
				endif
			rend
		endif
	rend

	; reset memory bank to  b0;b1;b2;b3	
	ld bc,&7FC0	
	out (c),c

	; Reset SPLITE function JMP
	xor a
	ld (spliteFunctions),a
	ld (spliteFunctions+1),a
	ld (spliteFunctions+2),a

	pop de	
	jp vanishEnd
	;ret

.vanishMsgRaster
	repeat 4*NB_COLUMNS
		ldi
	rend
	ret

; **********
; END WRITER
; **********
.initFonte
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; lock asic
 	ld a,&C9
 	ld (deco_asic),a

	; Set zoom
	ld hl,&6004
	ld de,8
	ld b,16
	.setZoomEW
		ld (hl),ZOOM_EW
 		add hl,de
	djnz setZoomEW

	; Change Sprite inks
	; Use Screenlike inks
	ld a,15;4
	ld hl,pal2+2
	ld de,&6422
	.copy_EW_pal
		ldi:ldi
		dec a
		jp nz,copy_EW_pal

	; Ugly Hack - change all to white but the first color
;if HACK1
;	let aux = 2
;	repeat 14
;		ld de,&FFF
;		ld (&6422+aux),de
;		let aux = aux + 2
;	rend
;endif

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a
	ret

.initStringSprite
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; lock asic
 	ld a,&C9
 	ld (deco_asic),a

	; Hide all sprites (easier)
	let aux=0
	xor a
	repeat NB_COLUMNS
		ld (NB_MASK + aux * 8 + &6004),a	; set zoom hide
		let aux = aux + 1
	rend

	.nextStringOffset
	ld ix,(msgOffset+1)
	let aux = 0

	repeat NB_COLUMNS
		ld de,&40 + NB_MASK + aux *&100	; sprite offset in asic's memory
		ld a,(ix+aux)		; ascii code
		or a
		jp z,endSpriteString
		cp ' '
		jr z,$+2+3+2+3
		call getPrintCharSprite
		ld a,ZOOM_EW		; show sprite
		ld (NB_MASK + aux * 8 + &6004),a	; set zoom hide/show
		let aux = aux + 1
	rend
	.endSpriteString

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a

	; set SPLITE Function
	xor a
	ld (spliteFunctions),a	
	ld (spliteFunctions+1),a	
	ld (spliteFunctions+2),a
	
	jp initStringSpriteEnd
	;ret

; IN	C = POSX (0 - 160)
;	L = POSY (0 - 168)
;	DE = String ASCII
; CORRUPT
;	All
.burnCharXY
	ld b,NB_COLUMNS mod 2 + NB_COLUMNS/2
	.loopBurnCharXY
		ld a,(de)

		inc de		; next char

		or a
		;ret z		; String end
		jp z, burnCharXYEnd

		cp ' '		; space (don't display)
		jr z,ignoreChar
		
		push de
		push hl
		push bc
		call burnChar
		pop bc
		pop hl
		pop de

		.ignoreChar
		ld a,INTER_CHAR
		add c
		ld c,a		; next pos
		
		djnz loopBurnCharXY

	; Need to save C,L,DE for Splite function
	ld a,c
	ld (burnCharXY_Splite2+1),a
	ld a,l
	ld (burnCharXY_Splite2+3),a
	ld (burnCharXY_Splite2+5),de

	; set SPLITE Function
	ld a,&C3		; JP xxxx
	ld (spliteFunctions),a	
	ld hl,burnCharXY_Splite2
	ld (spliteFunctions+1),hl	

	ret

; CORRUPT
;	All
.burnCharXY_Splite2
	; restore register values
	ld c,0
	ld l,0
	ld de,0

	ld b,NB_COLUMNS/2
	.loopBurnCharXY_Splite2
		ld a,(de)

		inc de		; next char

		;or a
		;ret z		; String end
		;jr z,endLoopBurnCharXY_Splite2

		cp ' '		; space (don't display)
		jr z,ignoreChar_Splite2
		
		push de
		push hl
		push bc
		call burnChar
		pop bc
		pop hl
		pop de

		.ignoreChar_Splite2
		ld a,INTER_CHAR
		add c
		ld c,a		; next pos
		
		djnz loopBurnCharXY_Splite2

	; Need to save L,DE for Splite function
	ld a,l
	ld (burnCharXY_Splite3+1),a
	ld (burnCharXY_Splite3+3),de

	; set SPLITE Function
	ld hl,burnCharXY_Splite3
	ld (spliteFunctions+1),hl	
	
	ret

; Do nothing but wait for next frame
; to retsrat where it stoped
.burnCharXY_Splite3

	; need to recover de and l before we go
	ld l,0
	ld de,0

	; Reset SPLITE function JMP
	xor a
	ld (spliteFunctions),a
	ld (spliteFunctions+1),a
	ld (spliteFunctions+2),a
	
	jp burnCharXYEnd

; IN	C = POSX (0 - 160)
;	L = POSY (0 - 168)
; OUT
;	HL = Sreen Offset
; Corrupted
;	DE
.getScreenOffsetFromL
	ld h,0
	add hl,hl		; HL = 2*POSY

	ld de,posYTable
	add hl,de
	ld e,(hl)
	inc l			; instead of inc hl cause table address is even !
	ld d,(hl)
	ex de,hl		; HL = base + Y

	ld d,0
	ld e,c
	srl e			; de = POSX/2 (odness in carry)
	add hl,de
	ret

; IN	C = POSX (0 - 160)
;	L = POSY (0 - 168)
;	A = Char ASCII
; Corrupt = Everything
.burnChar
	call getScreenOffsetFromL
	ex de,hl			; DE = Destination
	call getLetterOffsetScreen	; HL = Source

	ld b,8
	.loopLineChar
		push de
		repeat 4
		call burn2Pixels
		rend
		pop de		

		ld a,8
		add d
		jp P,contNextRaster
		ld a,96
		add e
		ld e,a
		ld a,&8-&40
		adc d		
		.contNextRaster
		and %01111111
		ld d,a

		djnz loopLineChar
	ret

.burn2Pixels
	ld c,(hl)
;if HACK2
;	; ugly hack convert all colors to white except black
;	ld a,%10101010
;	and c
;	jr z,dontChangeA
;	cp %10000000
;	jr z,dontChangeA
;
;	ld a,%01010101
;	and c
;	or %10101000
;	ld c,a
;
;	.dontChangeA
;	ld a,%01010101
;	and c
;	jr z,dontChangeB
;	cp %01000000
;	jr z,dontChangeB
;
;	ld a,%10101010
;	and c
;	or %01010100
;	ld c,a
;endif
	;.dontChangeB
	ld a,%10101010
	and c
	jr nz,burnPixA

	; get screen pixelA back
	ld a,(de)
	and %10101010
	;ld a,%10001000	; Hack bg=white
	or c
	ld c,a

	.burnPixA
	ld a,%01010101
	and c
	jr nz,burnPixAB

	; get screen pixelA back
	ld a,(de)
	and %01010101
	;ld a,%01000100	; Hack bg=white
	or c
	ld c,a

	.burnPixAB
	ld a,c
	ld (de),a

	;.inc_offsets
	inc hl
	inc de
	ret




; IN	A=CHAR Ascii code
; OUT	HL=Char Address
; Corrupted = a
.getLetterOffsetSprite
	push de
	sub '.'		; fetch letter a = 76543210 
	rrca		; a = 07654321 (0)
	rrca		; a = 10765432 (1)
	ld e,a
	and %00111111
	ld d,a
	ld a,%11000000
	and e
	ld e,a  	; de = a*&40
	
	ld hl,fonteSprite
	add hl,de
	pop de
	ret

; IN	A=CHAR Ascii code
; OUT	HL=Char Address
; Corrupted = a
.getLetterOffsetScreen
	push de
	sub '.'		; fetch letter a = 76543210 
	rrca		; a = 07654321 (0)
	rrca		; a = 10765432 (1)
	rrca		; a = 21076543 (2)
	ld e,a
	and %00011111
	ld d,a
	ld a,%11100000
	and e
	ld e,a  	; de = a*&20
	ld hl,fonteScreen
	add hl,de
	pop de
	ret

; IN	A=CHAR Ascii code
;	HL=Source
;	DE=Dest
; Corrupted A,BC,HL,DE
.getPrintCharSprite
	call getLetterOffsetSprite
	ld bc,8*256+64
	.loopCharSprite
		ldi:ldi:ldi:ldi
		ldi:ldi:ldi:ldi
		ld a,8
		add e
		ld e,a
		djnz loopCharSprite
	ret

; IN	L = POSY
.setEWPositionInit
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; lock asic
 	ld a,&C9
 	ld (deco_asic),a

.setEWPositionInitAsicON

	; Hide all sprites
	ld de,&300
	let aux=0;NB_MASK
	repeat 16;NB_COLUMNS
		ld (8*aux + &6000),de	; out of screen (unvisible)
		let aux = aux + 1
	rend

	; Set Y position
	ld h,0

	; Patched to match real position
	push hl
	ld de,RINT_SCREEN_ON+1
	add hl,de

	let aux=0
	repeat 16
		ld (8*0 + &6002 + aux),hl	; line in L
		let aux = aux + 8
	rend

	pop hl

	; Get compressed sprite offset
	; Sprites are 16bytes wide 
	; So offset = base + 8*l = base + l<<3
;	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,spriteWitchMask
	add hl,de

	; Here we have got the compressed sprite offset in HL
	; and we would like to display Sprites
	ld de,&4000		; first sprite offset
	ld c,NB_MASK			; 64 compressed bytes = 3*128 uncompressed bytes
	.coverSpriteLoop
		ld b,64
		.decrunchSpriteLoop2
			call decrunch2Pix
		djnz decrunchSpriteLoop2
		
		; to get next compressed sprite offset
		; we need HL = HL + 20*8*8
		push de
		ld de,20*8*8
		add hl,de
		pop de 

		; next sprite in a row
		ld e,0
		inc d

		dec c
		jr nz,coverSpriteLoop

	; Set Mask X offsets (show)
	let aux = 0
	let SX = 0
	repeat NB_MASK
		ld de,SX
		ld (&6000 + aux),de
		let aux = aux + 8
		let SX = SX + &40
	rend

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a

	; set SPLITE Function
	ld a,&C3		; JP xxxx
	ld (spliteFunctions),a	
	ld hl,initStringSprite
	ld (spliteFunctions+1),hl	

	ret

; This little helper for the code above
; Decrunch two pixels from the sprite compressed
; in memory (young witch's mask)
; IN 	= 	HL	(sprite source)
;		DE	(sprite destination in asic's memory)
; Corrupted A,C,DE,HL
.decrunch2Pix
	ld a,(hl)
	;ld a,&FF	; test (to be removed)
	inc hl

	ld (de),a	; only the 4 last bits are taken into acount in asic's memory
	inc de		
		
	rrca
	rrca
	rrca
	rrca
	ld (de),a	; only the 4 last bits are taken into acount in asic's memory
	inc de

	ret

; IN		A = POSX
; Corrupted	A,C,DE,HL
setEWPositionX
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; lock asic
 	ld hl,deco_asic
 	ld (hl),&C9

	or a
	jp P,setEWPositionXPositive
	;ret

; IN		A = POSX
; Corrupted	A,C,DE,HL
setEWPositionXMinus
	; We set positions of sprite 0-9
	ld c,-INTER_CHAR
	ld de,INTER_CHAR*4

	; prepare HL
	A_TO_HL
	ld a,l		; restore A
	add hl,hl	; * 4 to get sprite X offset		
	add hl,hl	; * 4 to get sprite X offset		

	let aux=0
	repeat NB_COLUMNS
		; Jump if a < -INTER_CHAR
		cp c
		jp M,$+6
		ld (8*NB_MASK + &6000 + aux),hl
		add hl,de
		sub c
		let aux = aux + 8
	rend
	
	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a

	ret

; IN		A = POSX
; Corrupted	A,C,DE,HL
setEWPositionXHide
	; We set positions of sprite 0-9
	ld de,INTER_CHAR*4
	ld c,&C0

	; prepare HL
	A_TO_HL
	ld a,l		; restore A
	add hl,hl	; * 4 to get sprite X offset		
	add hl,hl	; * 4 to get sprite X offset		

	let aux=0
	repeat NB_COLUMNS
		; Jump if a > &A0
		cp c
		jp P,$+6
		ld (8*NB_MASK + &6000 + aux),hl
		add hl,de
		add INTER_CHAR
		let aux = aux + 8
	rend

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a

	ret

; IN		A = POSX
; Corrupted	A,C,DE,HL
setEWPositionXPositive
	; We set positions of sprite 0-9
	ld de,INTER_CHAR*4

	; prepare HL
	A_TO_HL
	ld a,l		; restore A
	add hl,hl	; * 4 to get sprite X offset		
	add hl,hl	; * 4 to get sprite X offset		

	let aux=0
	repeat NB_COLUMNS
		ld (8*NB_MASK + &6000 + aux),hl
		add hl,de
		let aux = aux + 8
	rend
	
	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a
	
	ret