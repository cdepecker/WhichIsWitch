; Roto Zoom test
; F-Key 2010

; *****************
; * Init Rotozoom *
; *****************
.setRotoZoom
	di
	push de
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; hide sprites
	call hideAllSprites

	; clear rotozoom
	ld (saveSPclearRTZ+1),sp
	
	ld sp,8*&100+&4000-2
	ld hl,0
	ld b,8*16
	.loopClearRTZ
		push hl:push hl:push hl:push hl
		push hl:push hl:push hl:push hl
		djnz loopClearRTZ

	.saveSPclearRTZ
	ld sp,0

	call setRTZPosition00
	call setRTZPosition04

	; Set zoom
	ld hl,&6004
	ld de,8
	ld b,8
	.setZoomRoto
		ld (hl),ZOOM_ROTO
 		add hl,de
	djnz setZoomRoto

	; Change Sprite inks
	ld a,16
	ld hl,tile1pal;ball1pal
	ld de,&6422
	.copy_RZ_pal
		ldi:ldi
		dec a
		jp nz,copy_RZ_pal

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; first rotation
	;call rotationZoom

	pop de
	ei
	ret

.dispDemiLine
	repeat 16
	; get next vector in a	
	ld a,(bc)
	inc c
	
	; add vector to current position
	add e
	ld e,a

	; copy pixel from (de) to (hl) and prepare for nex pixel
	ld a,(de)
	ld (hl),a
	inc l
	rend
	ret

.displayRotoZoom
	push de

	 ; Connect Asic
 	ld bc,&7FB8		; [3]
 	out (c),c		; [4]

	; lock asic
 	ld hl,deco_asic		; [3]
 	ld (hl),&C9		; [3]

	; swap sprites 0-3 and 4-7
	call swapSprites

	; try to copy/paste in spritre memory
	; *****
	; MACRO ; 1/3 line
	; *****
	macro	line_1_2
		call dispDemiLine
		;repeat 16

		; get next vector in a	
		;ld a,(bc)
		;inc c
	
		; add vector to current position
		;add e
		;ld e,a

		; copy pixel from (de) to (hl) and prepare for nex pixel
		;ld a,(de)
		;ld (hl),a
		;inc l

		;rend
	mend

	macro line t
		ld l,t
		line_1_2
		inc h
		ld l,t
		line_1_2
	mend

	.sprite_offset
	ld de,tile1;ball1
	ld a,e
	sub &10
	ld (sprite_offset+1),a

	exx
	ld hl,Yoffsets
	exx

	SPRITE_0__OFFSET	EQU &40
	SPRITE_4__OFFSET	EQU &44

	.sprite_init_offset
	ld h,SPRITE_0__OFFSET+1

	let val = 0
	repeat 16

		; test 
		dec h

		; save initial position
		ld ixh,e

		; display an horyzontal line
		ld bc,Xoffsets
		line val
		let val = val + &10

		; add Y vector to current position
		exx
		ld a,(hl)
		inc hl
		exx
		add ixh
		ld e,a
	rend

	;test (here h is &41)
	;ld h,&42+1
	inc h
	inc h

	let val = 0
	repeat 16
		; test
		dec h

		; save initial position
		ld ixh,e

		; display an horyzontal line
		ld bc,Xoffsets
		line val
		let val = val + &10	

		; add Y vector to current position
		exx
		ld a,(hl)
		inc hl
		exx
		add ixh
		ld e,a
	rend

.rotationZoom
	;;;;;;;;;;;;;;;;;
	; ZOOM ROTATION
	;;;;;;;;;;;;;;;;;
	; MATRIX ROTATION 2D
	;         | cos; sin|
	;       * |-sin; cos|
	; | x; y| | x*cos-y*sin; x*sin+y*cos|
	; | 1; 0|=| cos; sin|
	; | 0; 1| |-sin; cos|

	; resets decimals
	xor a
	ld (xDecimal_i+1),a
	ld (xDecimal_j+1),a
	ld (yDecimal_i+1),a
	ld (yDecimal_j+1),a

	; Create Y Offsets
	; test &20*sin0
	.deg
	ld a,0
	inc a
	ld (deg+1),a
	ld (deg2+1),a
	ld l,a
	ld h,sin/256
	ld a,(hl)		; get sinus value

	; test sign
	or a
	jp P,sinYPositive
	; Sinus is <0
	; - convert to a positive value
	; - convert integer part of result to a negative value
	; - change to add -&10 in loop for I vector (decimal adjust)
	; - change to inc ixh in loop for J vector (decimal adjust)
	neg	
	ld hl,0: org $-2: neg
	ld (shouldnegY),hl
	ld hl,noYdecimal_i-1
	ld (hl),0 : org $-1 : db -&10	; second part of add -&10
	ld hl,noXdecimal_j-1
	ld (hl),0 : org $-1 : inc h	; second part of inc ixh
	jp sinYMult

	.sinYPositive
	; Sinus is >=0
	; - do not change result
	; - change to add &10 in loop for I vector (decimal adjust)
	; - change to dec ixh in loop for J vector (decimal adjust)
	ld hl,0: org $-2: nop: nop
	ld (shouldnegY),hl
	ld hl,noYdecimal_i-1
	ld (hl),0 : org $-1 : db &10	; second part of add &10
	ld hl,noXdecimal_j-1
	ld (hl),0 : org $-1 : dec h	; second part of dec ixh

	.sinYMult
	ld de,&21
	call _16mult8cust

	; div by 16 (rather than by 32 because sin is <128 and not <256)
	ld a,h
	rra:rr l
	rra:rr l
	rra:rr l
	rra:rr l
	and &03
	; finally, put Y Step in bc
	; mult a by 16 so that we are ready to fill offsets in the array
	add a
	add a
	add a
	add a
	.shouldnegY
	nop:nop
	ld (get_16_sin_a+1),a
	ld b,a
	ld c,l	

	; Create X Offsets
	; test &20*cos0
	ld a,64+20	; cos
	;inc a
	;ld ($-2),a
	.deg2
	add 0
	ld l,a
	ld h,sin/256
	ld a,(hl)

	; test sign
	or a
	jp P,cosXPositive
	; Cosinus is <0
	; - convert to a positive value
	; - convert integer part of result to a negative value
	; - change to dec ixh in loop for I vector (decimal adjust)
	; - change to add -&10 in loop for J vector (decimal adjust)
	neg	
	ld hl,0: org $-2: neg
	ld (shouldnegX),hl
	ld hl,noXdecimal_i-1
	ld (hl),0 : org $-1 : dec h	; second part of dec ixh
	ld hl,noYdecimal_j-1
	ld (hl),0 : org $-1 : db -&10	; second part of add -&10
	jp cosXMult

	.cosXPositive
	; Cosinus is >=0
	; - do not change result	
	; - change to inc ixh in loop for I vector (decimal adjust)
	; - change to add &10 in loop for J vector (decimal adjust)
	ld hl,0: org $-2: nop: nop
	ld (shouldnegX),hl
	ld hl,noXdecimal_i-1
	ld (hl),0 : org $-1 : inc h	; second part of inc ixh
	ld hl,noYdecimal_j-1
	ld (hl),0 : org $-1 : db &10	; second part of add &10

	.cosXMult
	ld de,&21*3/2-4
	call _16mult8cust

	; div by 16 (rather than by 32 because sin is <128 and not <256)
	ld a,h
	rra:rr l
	rra:rr l
	rra:rr l
	rra:rr l
	and &03
	; finally, put X Step in de
	.shouldnegX
	nop:nop
	ld (get_cos_a+1),a
	add b	; !!!! add Y value 
	ld d,a
	ld e,l

	; So here we have got
	; - d that contains the integer values of X offset AND Y offset (with correct sign) (16*sin(a) + cos(a))
	; - e that contains the absolute decimal value of X offset (cos a)
	; - c that contains the absolute decimal value of Y offset (sin a)
	; - b free to be used ;) but still contain integer part of 16*sin a
	; - a contains integer part of cos a

	; IMPORTANT
	; Here we construct the (X,Y) values of the i vector
	; and we store them so them in one byte like &YX so that it can be easily added to current position during display
	ld hl,Xoffsets
	ld b,&20
	.fillXOffsets
	
		; save integer value of X+Y vector
		; and save in ixh
		ld ixh,d

		; X decimal part
		; keep decimal value and inc integer value if needed
		.xDecimal_i
		ld a,0
		add e
		ld (xDecimal_i+1),a
		jr nc,noXdecimal_i
		inc ixh
		.noXdecimal_i

		; Y decimal part
		.yDecimal_i
		ld a,0
		add c
		ld (yDecimal_i+1),a
		ld a,ixh		; !!! get ixh in a
		jr nc,noYdecimal_i
		add &10
		.noYdecimal_i

		; here we have got the result in a, save it and continue loop
		ld (hl),a
		inc hl

	djnz fillXOffsets

	; IMPORTANT
	; Previously we had d = x*cos(a)+16*x*sin(a)
	; Now we need d = -x*sin(a)+16*x*cos(a)
	.get_16_sin_a
	ld a,0
	sra a
	sra a
	sra a
	sra a
	neg	; -x*sin(a)
	ld b,a	; save in b
	.get_cos_a
	ld a,0
	add a
	add a
	add a
	add a	; 16*x*cos(a)
	add b
	ld d,a	; d = -x*sin(a)+16*x*cos(a)

	; Here we construct the (X,Y) values of the j vector
	; and we store them so them in one byte like &YX so that it can be easily added to current position during display
	ld hl,Yoffsets
	ld b,&20
	.fillYOffsets
	
		; save integer value of X+Y vector
		; and save in ixh
		ld ixh,d	

		; X decimal part
		; keep decimal value and inc integer value if needed
		.xDecimal_j
		ld a,0
		add c
		ld (xDecimal_j+1),a
		jr nc,noXdecimal_j
		inc ixh
		.noXdecimal_j

		; Y decimal part
		.yDecimal_j
		ld a,0
		add e
		ld (yDecimal_j+1),a
		ld a,ixh		; !!! get ixh in a
		jr nc,noYdecimal_j
		add &10
		.noYdecimal_j

		; here we have got the result in a, save it and continue loop
		ld (hl),a
		inc hl

	djnz fillYOffsets

	; next tile ?
	.tile_counter
	ld a,ROTO_COUNT
	dec a
	jr nz,continue_tile

	; process next tile
	.proc_tile
	ld hl,tiles
	ld e,(hl):inc hl
	ld d,(hl):inc hl
	ld (sprite_offset+1),de
	ld e,(hl):inc hl
	ld d,(hl):inc hl
	ld (proc_tile+1),hl

	; Change Sprite inks
	ld a,16
	ex hl,de
	ld de,&6422
	.copy_RZ_pal2
		ldi:ldi
		dec a
		jp nz,copy_RZ_pal2

	
	ld a,ROTO_COUNT	; reset counter

	.continue_tile
	ld (tile_counter+1),a

	; Deconnect Asic
	ld bc,&7FA0		; [3]
 	out (c),c		; [4]

	; unlock asic
	ld hl,deco_asic
	ld (hl),0
	
	pop de
ret

.tiles
dw tile2:dw tile2pal
dw tile3:dw tile3pal
dw tile4:dw tile4pal
;dw tile5:;dw tile5pal 	; bullet
dw tile6:dw tile6pal
;dw tile7:;dw tile7pal	; write
dw tile8:dw tile8pal
dw tile9:dw tile9pal

.swapSprites
	ld a,ZOOM_ROTO
	ld (0*8+&6004),a
	ld (1*8+&6004),a
	ld (2*8+&6004),a
	ld (3*8+&6004),a
	xor ZOOM_ROTO
	ld (swapSprites+1),a
	ld (4*8+&6004),a
	ld (5*8+&6004),a
	ld (6*8+&6004),a
	ld (7*8+&6004),a

	ld a,(sprite_init_offset+1)
	xor &04
	ld (sprite_init_offset+1),a

	; Set/Change Position
	and &04
	jp nz,setRTZPosition04 	; jump if going to draw sprite 4-7
;ret				; otherwise keep going on ... for sprite 0-3

setRTZPosition00
	; Move X
	; ******
	.sinRTZTransX
	ld a,(sin)
	ld hl,sinRTZTransX+1
	inc (hl)

	; X (Beware we move 2pix at a time)
	A_TO_HL
	add hl,hl		; mult2
	ld de,96*8/2-64		; middle screen mode 2 minus ~ sprite width
	add hl,de
	
	ld (SpriteOffsetX+1),hl

	; Move Y
	; ******
	.sinRTZTransY
	ld a,(sin)
	;sra a		; div2
	;add 128		; Relative to middle screen
	or a
	jp P,posRTZOffY
	neg
	.posRTZOffY
	ld b,a
	ld a,127+32
	sub b

	;.sinRTZTransYDecimal
	;ld a,0
	;add &DD
	;ld (sinRTZTransYDecimal+1),a
	;jr nc,nosinRTZTransYinc
	ld hl,sinRTZTransY+1
	inc (hl)
	inc (hl)
	inc (hl)
	;.nosinRTZTransYinc
	
	;ld a,b
	ld (SpriteOffsetY+1),a

	; We set positions of sprite 0-3
	; 1st
	ld bc,&20	

	; first line
	.SpriteOffsetY
	ld hl,ROTO_YPOS
	ld (&6002),hl
	ld (&600A),hl

	; 2nd line
	add hl,bc
	ld (&6012),hl
	ld (&601A),hl

	; fisrt column
	.SpriteOffsetX
	ld hl,ROTO_XPOS
	ld (&6000),hl
	ld (&6010),hl

	; 2nd col
	add hl,bc
	add hl,bc
	ld (&6008),hl
	ld (&6018),hl

	ret

setRTZPosition04
	; We set positions of sprite 4-7 to the same place (buffering)
	; 1st
	ld bc,&20	

	; first line
	ld hl,(SpriteOffsetY+1)
	ld (&6022),hl
	ld (&602A),hl

	; 2nd line
	add hl,bc
	ld (&6032),hl
	ld (&603A),hl

	; fisrt column
	ld hl,(SpriteOffsetX+1)
	ld (&6020),hl
	ld (&6030),hl

	; 2nd col
	add hl,bc
	add hl,bc
	ld (&6028),hl
	ld (&6038),hl
	ret

; *******
; Offsets
; *******
align 256
.Xoffsets
repeat 32
	;db &01
	db &00
rend

.Yoffsets
repeat 32
	;db &10
	db &00
rend
