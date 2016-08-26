; We use a sound DMA to generate interrupts 
align 2
.aylSprite0
	dw &1000					; Asjust (ScrollY)
	dw &1000 + 14					; Adjust (VBL)
	dw &1000 + SPRITE_LINE-RST_BFORE_LINE+15	; pause SPRITE_LINE-1 before first interrupt
	dw &2000 + NBL - 1				; repeat
	dw &4010					; call interrupt vector
	dw &1000 + YWIDTH-2				; pause YWIDTH-1 (before next interrupt)
	dw &4001					; loop
	dw &4010					; last interrupt
	dw &4020					; stop

; ****************************
; * Init Sprite Multiplexing *
; ****************************
.setMultiSprite1
	di
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; hide sprites
	call hideAllSprites

	; copy NB_BALLS balls sprite in asic memory
	ld a,6
	ld de,&4000
	call copyNSprite2

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ei
	ret

.setMultiSprite2
	di
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; copy NB_BALLS balls sprite in asic memory
	ld a,5
	ld de,&4600 
	call copyNSprite2

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ei
	ret

.setMultiSprite3

	di
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; copy NB_BALLS balls sprite in asic memory
	ld a,5
	ld de,&4B00 
	call copyNSprite2

	; Set zoom
	;ld hl,&6004
	;ld de,8
	;ld b,NB_BALLS_TOTAL
	;.setZoom3D
	;	ld (hl),ZOOM_MULTI
 	;	add hl,de
	;djnz setZoom3D

	; Change Sprite inks
	ld a,15
	ld hl,ball1pal
	ld de,&6422
	.copy_multi_pal
		ldi:ldi
		dec a
		jp nz,copy_multi_pal

	; set PRI to generate an interrupt at VBL
	;ld hl,&6800
	;ld (hl),SPRITE_LINE-RST_BFORE_LINE

	; set interrupt vector
	;ld hl,intLine
	;ld (intvect+4),hl
	;ld (intvect+6),hl

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; enable interrupts
	ei

	; Init balls to hide
	; Compute number of balls to hide next to the text def
	ld hl,(txtSpriteOffset+1)
	ld (txtAddress+1),hl		; so that it's generated during first bgCompute function call
	ld de,NBL-1*5*-1
	add hl,de

	ld d,0
	ld b,NBL
	.hideBalls
 		ld a,16
 		ld c,(hl):inc hl
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d
 		ld c,(hl):inc hl
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d 
 		ld c,(hl):inc hl
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d
 		ld c,(hl):inc hl
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d
 		rr c:sbc d:rr c:sbc d:rr c:sbc d:rr c:sbc d
 		ld (hl),a
 		inc hl
	djnz hideBalls

	; reset SPs (generate)
	ld hl,32+2+2*NBL + ballposfast
	ld (genSPOffset+1),hl
	ld hl,32+2+2*NBL + ballposfast2
	ld (nextSwapSP+1),hl

	; reset SPs (display)
	ld hl,ballposfast
	ld (nextPosSP+1),hl
	ld (swapSPRead2+1),hl
	ld hl,ballposfast2
	ld (swapSPRead1+1),hl

	ret

.setMultiSprite4
	di

	; Cheating
	; add YWIDTH to all Y positions (unvisible for the first frame)
	;ld hl,(nextPosSP+1)
	;ld de,32
	;add hl,de	; hl points on Y pos
	;ld de,32+2+30
	;ld b,NBL
	;.loopMakeMultiUnvisible
	;	ld a,YWIDTH*4
	;	add (hl)
	;	ld (hl),a
	;	add hl,de
	;	djnz loopMakeMultiUnvisible
	;

	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; set PRI to generate an interrupt just before first sprite line
	ld hl,&6800
	ld (hl),SPRITE_LINE-RST_BFORE_LINE

	; set interrupt vector
	ld hl,intLine
	ld (intvect+4),hl
	ld (intvect+6),hl

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; enable interrupts
	ei
	ret

; ************
; * Routines *
; ************
; input HL=data pointer
; corrupt BC, DE, HL
; Currently not interruptible because of
; - ASIC Ram mapping
; - Using Stack for memory transferts
.intLine
 push bc 		; [4]
 push hl 		; [4]
 
 ; Connect Asic
 ld bc,&7FB8		; [3]
 out (c),c		; [4]

 ; lock asic
 ld hl,deco_asic	; [3]
 ld (hl),&C9		; [3]

 ; hack, we set the next Raster line to &FF (~disabled)
 ; when we re-enable the interrupts
 ld hl,&6800
 ld (hl),&FF

 ; no fear
 ei			; [1]

 push af ; [4]
 push de ; [4]

 ld (saveSPt+1),sp      ; [6]
.nextPosSP
 ld sp,ballposfast	; [3]

 ; copy X ( 128 NOPs)
 pop de:ld (&6000),de  ; [3+5]
 pop de:ld (&6008),de  ; [3+5]
 pop de:ld (&6010),de  ; [3+5]
 pop de:ld (&6018),de  ; [3+5]
 pop de:ld (&6020),de  ; [3+5]
 pop de:ld (&6028),de  ; [3+5]
 pop de:ld (&6030),de  ; [3+5]
 pop de:ld (&6038),de  ; [3+5]
 pop de:ld (&6040),de  ; [3+5]
 pop de:ld (&6048),de  ; [3+5]
 pop de:ld (&6050),de  ; [3+5]
 pop de:ld (&6058),de  ; [3+5]
 pop de:ld (&6060),de  ; [3+5]
 pop de:ld (&6068),de  ; [3+5]
 pop de:ld (&6070),de  ; [3+5]
 pop de:ld (&6078),de  ; [3+5]

 ; Y (89 NOPs low nibble only)
 ; balls 0,1 
 pop de         ; [3] d = sprite_line, e = next raster interrupt
 ld hl,&6002    ; [3]
 ld (hl),e      ; [2]
 ld l,&0a       ; [2]
 ld (hl),e      ; [2]
 
 ; then balls 2,3
 ld l,&12       ; [2]
 ld (hl),e      ; [2]
 ld l,&1a       ; [2]
 ld (hl),e      ; [2]
 
 ; then balls 4,5
 ld l,&22       ; [2]
 ld (hl),e      ; [2]
 ld l,&2a       ; [2]
 ld (hl),e      ; [2]

 ; then balls 6,7
 ld l,&32       ; [2]
 ld (hl),e      ; [2]
 ld l,&3a       ; [2]
 ld (hl),e      ; [2]
 
 ; then balls 8,9
 ld l,&42       ; [2]
 ld (hl),e      ; [2]
 ld l,&4a       ; [2]
 ld (hl),e      ; [2]

 ; then balls 10,11
 ld l,&52       ; [2]
 ld (hl),e      ; [2]
 ld l,&5a       ; [2]
 ld (hl),e      ; [2]

 ; then balls 12,13
 ld l,&62       ; [2]
 ld (hl),e      ; [2]
 ld l,&6a       ; [2]
 ld (hl),e      ; [2]

 ; then balls 14,15
 ld l,&72       ; [2]
 ld (hl),e      ; [2]
 ld l,&7a       ; [2]
 ld (hl),e      ; [2]

 ; next interrupt raster
 ld a,d
 ld (&6800),a

 ; Change Pal (?)
 pop hl
 jp (hl)
.ret_setRasterPal
	
; save sp for next call
ld (nextPosSP+1),sp

 ; Deconnect Asic
 ld bc,&7FA0		; [3]
 out (c),c		; [4]

 ; unlock asic
 ld hl,deco_asic
 ld (hl),0

; ********
; END TEST
; ********
 ; if next line to display is the last one
 ; Then we reset 
 ; - SP Offset, 
 ; - NB Lines left, 
 .nblLeft
 ld a,NBL
 dec a
 jr nz,contlines
 
 ; reset sp (buffered)
.swapSPRead1
 ld hl,ballposfast2
 ld (nextPosSP+1),hl
.swapSPRead2
 ld de,ballposfast
 ld (swapSPRead1+1),de
 ld (swapSPRead2+1),hl

 ; reset NBLines to draw
 ld a,NBL

 .contlines
 ld (nblLeft+1),a

 .saveSPt
 ld sp,0   ; [3]

 pop de
 pop af
 pop hl
 pop bc
 ret


; *********************************
; Background compute position
; *********************************
.bg_compute
	ld ixl,NBL				; line counter
	ld (saveSPGen+1),sp
.genSPOffset
	ld sp,32+2+2*NBL + ballposfast		; last palette

	; swap genSPOffset
	.nextSwapSP
	ld hl,32+2+2*NBL + ballposfast2		; prepare 2nd
	ld (genSPOffset+1),hl			; buffer
	ld (nextSwapSP+1),sp			; next buffer swap

	; Because the stak may be corrupted we should regenerate all data
	; every FRAME !!!!

.bg_compute_loop	

	.sinTableOffset
	ld hl,(sinPalTable)
	push hl
	ld hl,(sinTableOffset+1)
	inc hl
	inc hl
	ld (sinTableOffset+1),hl

	; compute Y Pos (inverted from bottom to top)
	.nextGenRaster
	ld a,SPRITE_LINE-RST_BFORE_LINE
	ld h,a				; next raster interrupt in h
	cp SPRITE_LINE-RST_BFORE_LINE
	jr nz,$+2+2 :ld a,NBL*YWIDTH+SPRITE_LINE-RST_BFORE_LINE
	sub YWIDTH
	ld (nextGenRaster+1),a
	add RST_BFORE_LINE
	ld l,a				; next sprite line in l
	push hl

	; + Scroll X
	; + Hide unused balls
	ld hl,sinScrollX+1
	ld a,SINX_DISTO
 	add (hl)
 	ld (hl),a
 	.sinScrollX 
 	ld a,(sin+192) 

 	add 128
 	ld h,0
 	ld l,a

	 ; hl holds the posX
 	; sp holds the X address
 	ld de,XWIDTH 

 	.txtAddress
 	ld bc,txtSprite_end-1-4
 	ld a,(bc)
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	inc bc
 	ld a,(bc)
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	inc bc
 	ld a,(bc)
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	inc bc
 	ld a,(bc)
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de
 	rla:jr nc,$+3:push hl:add hl,de	

	inc bc
	ld a,(bc)

 	; Hide unused balls
 	; *****************
 	or a
 	jr z,endSaveUnsudedBalls	

 	ld de,&300
 	.loopUnusedBalls
 	push de
 	dec a
 	jr nz,loopUnusedBalls
	.endSaveUnsudedBalls

	; next text line
	;inc bc
	ld hl,4+1+4*-1	; go previous line
	add hl,bc
	ld (txtAddress+1),hl;bc

	dec ixl
	jp nz,bg_compute_loop

 	; reset text offset
	.txtSpriteOffset
	ld hl,txtSprite_end-1-4
 	ld (txtAddress+1),hl

 	; reset sinus address to -SINX_SPEED (smooth)
 	ld hl,sinScrollX+1
 	ld a,-SINX_SPEED
 	add (hl)
 	ld (hl),a 

	; reset palette for next frame
	.sinPal
	ld hl,sin
	ld a,(hl)
	inc l
	ld (sinPal+1),hl
	sra a
	sra a
	sra a
	sra a	; a [-7;7]
	add a	; a [-7;7]*2
	ld hl,sinPalTable
	add l
	ld l,a
	ld (sinTableOffset+1),hl

	.saveSPGen
	ld sp,0
	ret

COLOR1_GREEN	EQU	&F
COLOR1_RED	EQU	&F
COLOR1_BLUE	EQU	&F

COLOR2_GREEN	EQU	&0
COLOR2_RED	EQU	&0
COLOR2_BLUE	EQU	&7

COLOR3_GREEN	EQU	&8
COLOR3_RED	EQU	&8
COLOR3_BLUE	EQU	&F

COLOR4_GREEN	EQU	&9
COLOR4_RED	EQU	&9
COLOR4_BLUE	EQU	&F

COLOR5_GREEN	EQU	&E
COLOR5_RED	EQU	&E
COLOR5_BLUE	EQU	&F

COLOR6_GREEN	EQU	&0
COLOR6_RED	EQU	&0
COLOR6_BLUE	EQU	&0

COLOR7_GREEN	EQU	&A
COLOR7_RED	EQU	&A
COLOR7_BLUE	EQU	&F

COLOR8_GREEN	EQU	&5
COLOR8_RED	EQU	&5
COLOR8_BLUE	EQU	&E

COLOR9_GREEN	EQU	&B
COLOR9_RED	EQU	&B
COLOR9_BLUE	EQU	&F

COLORA_GREEN	EQU	&C
COLORA_RED	EQU	&D
COLORA_BLUE	EQU	&F

COLORB_GREEN	EQU	&6
COLORB_RED	EQU	&6
COLORB_BLUE	EQU	&E

COLORC_GREEN	EQU	&0
COLORC_RED	EQU	&0
COLORC_BLUE	EQU	&C

COLORD_GREEN	EQU	&2
COLORD_RED	EQU	&1
COLORD_BLUE	EQU	&E

COLORE_GREEN	EQU	&A
COLORE_RED	EQU	&A
COLORE_BLUE	EQU	&E

COLORF_GREEN	EQU	&D
COLORF_RED	EQU	&E
COLORF_BLUE	EQU	&F


macro genPalColor w
	ifnot w-1
		ld hl,blue*16+green*16+red
	else
		ifnot w-2
			ld hl,red*16+blue*16+green
		else
			ld hl,green*16+red*16+blue
		endif
	endif  
mend

macro genPal1 w,x,y
	let cmult = x
	let cdiv = y
	let green = COLORF_BLUE - COLORF_GREEN * cmult/cdiv + COLORF_GREEN mod 16
	let red = COLORF_GREEN - COLORF_RED * cmult/cdiv + COLORF_RED mod 16
	let blue = COLORF_RED - COLORF_BLUE * cmult/cdiv + COLORF_BLUE mod 16
	genPalColor w
	ld (14*2 + &6422),hl
	
	let green = COLORE_BLUE - COLORE_GREEN * cmult/cdiv + COLORE_GREEN mod 16
	let red = COLORE_GREEN - COLORE_RED * cmult/cdiv + COLORE_RED mod 16
	let blue = COLORE_RED - COLORE_BLUE * cmult/cdiv + COLORE_BLUE mod 16
	genPalColor w
	ld (13*2 + &6422),hl
	
	let green = COLORD_BLUE - COLORD_GREEN * cmult/cdiv + COLORD_GREEN mod 16
	let red = COLORD_GREEN - COLORD_RED * cmult/cdiv + COLORD_RED mod 16
	let blue = COLORD_RED - COLORD_BLUE * cmult/cdiv + COLORD_BLUE mod 16
	genPalColor w
	ld (12*2 + &6422),hl
	
	let green = COLORC_BLUE - COLORC_GREEN * cmult/cdiv + COLORC_GREEN mod 16
	let red = COLORC_GREEN - COLORC_RED * cmult/cdiv + COLORC_RED mod 16
	let blue = COLORC_RED - COLORC_BLUE * cmult/cdiv + COLORC_BLUE mod 16
	genPalColor w
	ld (11*2 + &6422),hl
	
	let green = COLORB_BLUE - COLORB_GREEN * cmult/cdiv + COLORB_GREEN mod 16
	let red = COLORB_GREEN - COLORB_RED * cmult/cdiv + COLORB_RED mod 16
	let blue = COLORB_RED - COLORB_BLUE * cmult/cdiv + COLORB_BLUE mod 16
	genPalColor w
	ld (10*2 + &6422),hl
	
	let green = COLORA_BLUE - COLORA_GREEN * cmult/cdiv + COLORA_GREEN mod 16
	let red = COLORA_GREEN - COLORA_RED * cmult/cdiv + COLORA_RED mod 16
	let blue = COLORA_RED - COLORA_BLUE * cmult/cdiv + COLORA_BLUE mod 16
	genPalColor w
	ld (9*2 + &6422),hl
	
	let green = COLOR9_BLUE - COLOR9_GREEN * cmult/cdiv + COLOR9_GREEN mod 16
	let red = COLOR9_GREEN - COLOR9_RED * cmult/cdiv + COLOR9_RED mod 16
	let blue = COLOR9_RED - COLOR9_BLUE * cmult/cdiv + COLOR9_BLUE mod 16
	genPalColor w
	ld (8*2 + &6422),hl
	
	let green = COLOR8_BLUE - COLOR8_GREEN * cmult/cdiv + COLOR8_GREEN mod 16
	let red = COLOR8_GREEN - COLOR8_RED * cmult/cdiv + COLOR8_RED mod 16
	let blue = COLOR8_RED - COLOR8_BLUE * cmult/cdiv + COLOR8_BLUE mod 16
	genPalColor w
	ld (7*2 + &6422),hl
	
	let green = COLOR7_BLUE - COLOR7_GREEN * cmult/cdiv + COLOR7_GREEN mod 16
	let red = COLOR7_GREEN - COLOR7_RED * cmult/cdiv + COLOR7_RED mod 16
	let blue = COLOR7_RED - COLOR7_BLUE * cmult/cdiv + COLOR7_BLUE mod 16
	genPalColor w
	ld (6*2 + &6422),hl
	
	let green = COLOR6_BLUE - COLOR6_GREEN * cmult/cdiv + COLOR6_GREEN mod 16
	let red = COLOR6_GREEN - COLOR6_RED * cmult/cdiv + COLOR6_RED mod 16
	let blue = COLOR6_RED - COLOR6_BLUE * cmult/cdiv + COLOR6_BLUE mod 16
	genPalColor w
	ld (5*2 + &6422),hl
	
	let green = COLOR5_BLUE - COLOR5_GREEN * cmult/cdiv + COLOR5_GREEN mod 16
	let red = COLOR5_GREEN - COLOR5_RED * cmult/cdiv + COLOR5_RED mod 16
	let blue = COLOR5_RED - COLOR5_BLUE * cmult/cdiv + COLOR5_BLUE mod 16
	genPalColor w
	ld (4*2 + &6422),hl
	
	let green = COLOR4_BLUE - COLOR4_GREEN * cmult/cdiv + COLOR4_GREEN mod 16
	let red = COLOR4_GREEN - COLOR4_RED * cmult/cdiv + COLOR4_RED mod 16
	let blue = COLOR4_RED - COLOR4_BLUE * cmult/cdiv + COLOR4_BLUE mod 16
	genPalColor w
	ld (3*2 + &6422),hl
	
	let green = COLOR3_BLUE - COLOR3_GREEN * cmult/cdiv + COLOR3_GREEN mod 16
	let red = COLOR3_GREEN - COLOR3_RED * cmult/cdiv + COLOR3_RED mod 16
	let blue = COLOR3_RED - COLOR3_BLUE * cmult/cdiv + COLOR3_BLUE mod 16
	genPalColor w
	ld (2*2 + &6422),hl
	
	let green = COLOR2_BLUE - COLOR2_GREEN * cmult/cdiv + COLOR2_GREEN mod 16
	let red = COLOR2_GREEN - COLOR2_RED * cmult/cdiv + COLOR2_RED mod 16
	let blue = COLOR2_RED - COLOR2_BLUE * cmult/cdiv + COLOR2_BLUE mod 16
	genPalColor w
	ld (1*2 + &6422),hl

	let green = COLOR1_BLUE - COLOR1_GREEN * cmult/cdiv + COLOR1_GREEN mod 16
	let red = COLOR1_GREEN - COLOR1_RED * cmult/cdiv + COLOR1_RED mod 16
	let blue = COLOR1_RED - COLOR1_BLUE * cmult/cdiv + COLOR1_BLUE mod 16
	genPalColor w
	ld (0*2 + &6422),hl
mend

; called within the interrupt
; should jump back to int code
; Condition=ASIC ON
.setRasterPal0				; NORMAL
	genPal1 0,0,4
	jp ret_setRasterPal

.setRasterPal1
	genPal1 0,1,4
	jp ret_setRasterPal

.setRasterPal2
	genPal1 0,2,4
	jp ret_setRasterPal

.setRasterPal3
	genPal1 0,3,4
	jp ret_setRasterPal

.setRasterPal4
	genPal1 1,0,4
	jp ret_setRasterPal

.setRasterPal5
	genPal1 1,1,4
	jp ret_setRasterPal

.setRasterPal6
	genPal1 1,2,4
	jp ret_setRasterPal

.setRasterPal7
	genPal1 1,3,4
	jp ret_setRasterPal

.setRasterPal8
	genPal1 2,0,4
	jp ret_setRasterPal

.setRasterPal9
	genPal1 2,1,4
	jp ret_setRasterPal

.setRasterPalA
	genPal1 2,2,4
	jp ret_setRasterPal

.setRasterPalB
	genPal1 2,3,4
	jp ret_setRasterPal

; need a size of [-7;7] + NBL -1 = 14 + NBL
	dw setRasterPal0
	dw setRasterPal1
	dw setRasterPal2
	dw setRasterPal3
	dw setRasterPal4
	dw setRasterPal5
	dw setRasterPal6
.sinPalTable
	dw setRasterPal7
	dw setRasterPal8
	dw setRasterPal9
	dw setRasterPalA
	dw setRasterPalB
	dw setRasterPal0
	dw setRasterPal1
	dw setRasterPal2
; NBL-1 ?
	dw setRasterPal3
	dw setRasterPal4
	dw setRasterPal5
	dw setRasterPal6
	dw setRasterPal7
	dw setRasterPal8
	dw setRasterPal9
	dw setRasterPalA
	dw setRasterPalB
	dw setRasterPal0
	dw setRasterPal1
	dw setRasterPal2
	dw setRasterPal3
	dw setRasterPal4
	dw setRasterPal5
	dw setRasterPal6
