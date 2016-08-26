; Bump Map test
; F-Key 2011

; *****************
; * Init Bump Map *
; *****************
.setBumpMap0
	;if DEBUG
	;GA_COLOR 0,11		; BLANC
	;endif

	halt
	di
	; clear sprite 5
	ld hl,&45FF+1
	call clearSpriteWithSP
	; clear sprite 10
	ld hl,&4AFF+1
	call clearSpriteWithSP
	; clear sprite 15
	ld hl,&4FFF+1
	call clearSpriteWithSP
	ei
	
	; make sure that YPos is coded with 8bits only
	xor a
	let aux = 15
	while aux
		ld (aux*8 + &6003),a
		let aux = aux -1
	rend
	ld (&6003),a

	; Set zoom (hide)
	ld hl,&6004
	ld de,8
	ld b,16
	.setZoomBmp
		ld (hl),a
 		add hl,de
	djnz setZoomBmp

	; Change Sprite inks
	ld a,15
	ld hl,skull_wings_pal;tile1pal
	ld de,&6422
	.copy_BMP_pal
		ldi:ldi
		dec a
		jp nz,copy_BMP_pal

	; set first X position (avoid sprite to be aznywhere at startup)
	ld ixl,BMP_YPOS_BASE
	ld ixh,2*&20 + BMP_YPOS_BASE
	ld iyl,2*&20 + BMP_YPOS_BASE - 1
	;jp BMP_SCROLL_DOWN
	;jp setBMPPosition00
	jp scrollXBase
	;ret

.setBumpMap1
	;if DEBUG
	;GA_COLOR 0,28		; ROUGE
	;endif

	; stick tiles in every sprites (4-9)
	; TODO = too time consuming ...
	ld hl,skull_wings
	ld de,&4100
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite:inc d:inc d
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite
	
	ret

.setBumpMap2
	;if DEBUG
	;GA_COLOR 0,22		; VERT
	;endif

	; stick tiles in every sprites (10-15)
	; TODO = too time consuming ...
	ld hl,128*6+skull_wings
	ld de,&4800
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite:inc d:inc d
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite:inc d
	call decrunchSkullSprite

di
	; set DMA2 to call skull interrupt code
	;ld hl,intDLRasters
	;ld (intCallAfterScreenOn+1),hl

	; enable interrupts for skull display
	;ld a,&10
	;ld (BMP_INT),a

	; set PRI interrupt to be triggered before remaining rasters to display
	ld hl,&6800
	ld (hl),0*&20 + BMP_YPOS_BASE - 1 - 3

	; set interrupt vector
	ld hl,intDLRasters
	ld (intvect+4),hl
	ld (intvect+6),hl	
ei
	ret

; IN 	HL = sprite last offset+1 (i.e. &5000 for sprite 15)
; Corrupted A,B,HL 
.clearSpriteWithSP
	ld (saveSPCLear1+1),sp
	ld sp,hl
	xor a
	ld h,a:ld l,a
	ld b,128/4-1	; keep 4 (interrupts call)
	.loopClearSprite2
		push hl:push hl:push hl:push hl
		djnz loopClearSprite2

	add hl,sp	; save offset

	.saveSPCLear1
	ld sp,0

	; finish 8 last sprite bytes
	repeat 8
		dec l:ld (hl),a
	rend
	ret

; IN = 	HL (source)
; 	DE (destination)
; Corrupted (A,B)
; ASIC Should be enabled
.decrunchSkullSprite
	ld b,128
	.loopSkullSprite
		ld a,(hl)
		inc hl
		ld (de),a	; only the 4 last bits are taken into acount in asic's memory
		inc e		
		rrca:rrca:rrca:rrca
		ld (de),a	; only the 4 last bits are taken into acount in asic's memory
		inc e
		djnz loopSkullSprite
	ret

; IN	D = Source / 256
;	E = Destination / 256
;	L = Offset
;	C = 0	(remove sprite)
; Corrupted A,B,HL
.moveSpriteColumn
	ld b,16
	.loopMoveColumn
		ld h,d		; get value
		ld a,(hl)	; from source
		ld (hl),c	; and remove it

		ld h,e		; move value
		ld (hl),a	; to dest

		ld a,&10	; move 1 line 
		add l
		ld l,a
	djnz loopMoveColumn
	ret

.displayBumpMap
	 ; Connect Asic
 	ld bc,&7FB8		; [3]
 	out (c),c		; [4]

	; lock asic
 	ld hl,deco_asic		; [3]
 	ld (hl),&C9		; [3]

	call setBMPPosition00

	; Deconnect Asic
	ld bc,&7FA0		; [3]
 	out (c),c		; [4]

	; unlock asic
	ld hl,deco_asic
	ld (hl),0

	ret

; 3x3 sprites (keep first sprite for light)
; INPUT e=dX
;	d=dY
setBMPPosition00
	jr endScrollX
	
	; DEBUG TODO remove after debug
	;.debug_scrollX
	;ld hl,0
	;ld (setBMPPosition00),hl

;if DEBUG
	;GA_COLOR 0,14		; ORANGE
	;endif

	; PREPARE X SCROLL
	; ****************
	.BMP_X_Sroll
	ld a,16
	dec a
	jp P,continueXScroll

	; - swap sprites
	; - reset X scroll
	; - reset table X delay
	call swapSpritesX
	ld a,15

	.continueXScroll
	; here we should
	; - scroll 1 pixel left (whole X table)
	; - remove a pixel column from the left sprite
	; - add a pixel column to the right sprite
	ld (BMP_X_Sroll+1),a
	ld (scrollXBase+1),a

	; a = 15(0),14(1),13 ... 0(15)

	add -15
	neg
	and &F
	ld l,a		; set Offset to l

	; consts
	xor a
	ld c,a

	.moveSprite1
	ld de,&4145		; source / dest
	call moveSpriteColumn
	.moveSprite2
	ld de,&464A		; source / dest
	call moveSpriteColumn
	.moveSprite3
	ld de,&4B4F		; source / dest
	call moveSpriteColumn

	.endScrollX
	.sinBmpY
	ld a,&2F
	;ld a,(sin)
	;ld hl,sinBmpY+1
	;inc (hl)

	;add 127
	; a between 0;+254
	; should make it 0;16*3-1
	;srl a	; <128

.BMP_SCROLL_DOWN
	nop;inc a
	; (position between 0;16*3-1)
	cp 16*3:jp M,$+3+2:sub 16*3
	;cp 16*3:;jp M,$+3+2:;sub 16*3

	ld (sinBmpY+1),a

	; ****************
	; Positions
	; ****************
	; center
	add a		; because of zoomY * 2 (0 <= a <= 16*6-2)
	;add BMP_YPOS_BASE
	add ixl

	; set Y offsets
	ld c,&20 		; [2]

	;cp 2*&20 + BMP_YPOS_BASE	; reached point to roll ?
	cp ixh				; reached point to roll ?
	jp M,noRoll1

	;.Roll1
	ld (intDLRastersCall2+1),a
	ld l,a				; save A
	ld a,1*8 + &02:ld (split_A+1),a
	ld a,2*8 + &02:ld (split_B+1),a
	ld a,3*8 + &02:ld (split_C+1),a
	ld a,4*8 + &02:ld (split_D+1),a
	ld a,5*8 + &02:ld (split_E+1),a
	ld a,l				; restore A

	; added for XScroll
	ld hl,intSpriteX1:ld (intSpriteN+1),hl
	ld hl,intSpriteX2:ld (intSpriteN1+1),hl
	ld hl,intSpriteX3:ld (intSpriteN2+1),hl

	; set zoom enable (first line)
	ld hl,1*8+&6004:ld (enable_zoom+1),hl
	ld (hide_sprite_zoom+1),hl

	;sub 2*&20 + BMP_YPOS_BASE -1
	sub iyl
	ld (ScrollN3+1),a
	sub &20
	neg
	ld (ScrollN+1),a
	neg
	;add BMP_YPOS_BASE
	add ixl
	dec a			; FIXME

	.noRoll1
	ld (1*8 + &6002),a	; [4] Y offset
	ld (2*8 + &6002),a	; [4]
	ld (3*8 + &6002),a	; [4]
	ld (4*8 + &6002),a	; [4]
	ld (5*8 + &6002),a	; [4]

	add c			; [1]
	;cp 2*&20 + BMP_YPOS_BASE	; reached point to roll ?
	cp ixh
	jp M,noRoll2

	;.Roll2
	ld (intDLRastersCall2+1),a
	ld l,a				; save A
	ld a,6*8 + &02:ld (split_A+1),a
	ld a,7*8 + &02:ld (split_B+1),a
	ld a,8*8 + &02:ld (split_C+1),a
	ld a,9*8 + &02:ld (split_D+1),a
	ld a,10*8 + &02:ld (split_E+1),a
	ld a,l				; restore A
	
	; added for XScroll
	ld hl,intSpriteX2:ld (intSpriteN+1),hl
	ld hl,intSpriteX3:ld (intSpriteN1+1),hl
	ld hl,intSpriteX1:ld (intSpriteN2+1),hl

	; set zoom enable (first line)
	ld hl,6*8+&6004:ld (enable_zoom+1),hl
	ld (hide_sprite_zoom+1),hl

	;sub 2*&20 + BMP_YPOS_BASE -1
	sub iyl
	ld (ScrollN3+1),a
	sub &20
	neg
	ld (ScrollN+1),a
	neg
	;add BMP_YPOS_BASE
	add ixl
	dec a			; FIXME

	.noRoll2
	ld (6*8 + &6002),a	; [4] Y offset
	ld (7*8 + &6002),a	; [4]
	ld (8*8 + &6002),a	; [4]
	ld (9*8 + &6002),a	; [4]
	ld (10*8 + &6002),a	; [4]

	add c			; [1]
	;cp 2*&20 + BMP_YPOS_BASE	; reached point to roll ?
	cp ixh
	jp M,noRoll3

	;.Roll3
	ld (intDLRastersCall2+1),a
	ld l,a				; save A
	ld a,11*8 + &02:ld (split_A+1),a
	ld a,12*8 + &02:ld (split_B+1),a
	ld a,13*8 + &02:ld (split_C+1),a
	ld a,14*8 + &02:ld (split_D+1),a
	ld a,15*8 + &02:ld (split_E+1),a
	ld a,l				; restore A
	
	; added for XScroll
	ld hl,intSpriteX3:ld (intSpriteN+1),hl
	ld hl,intSpriteX1:ld (intSpriteN1+1),hl
	ld hl,intSpriteX2:ld (intSpriteN2+1),hl

	; set zoom enable (first line)
	ld hl,11*8+&6004:ld (enable_zoom+1),hl
	ld (hide_sprite_zoom+1),hl

	;sub 2*&20 + BMP_YPOS_BASE -1
	sub iyl
	ld (ScrollN3+1),a
	sub &20
	neg
	ld (ScrollN+1),a
	neg
	;add BMP_YPOS_BASE
	add ixl
	dec a			; FIXME

	.noRoll3
	ld (11*8 + &6002),a	; [4] Y offset
	ld (12*8 + &6002),a	; [4] 
	ld (13*8 + &6002),a	; [4] 
	ld (14*8 + &6002),a	; [4]
	ld (15*8 + &6002),a	; [4]

	; hide all sprites ;)
	; but set correct zoom
	ld hl,&2FF
	ld a,ZOOM_BMP
	let aux = 1
	repeat 15
		ld (aux*8 + &6000),hl
		ld (aux*8 + &6004),a
		let aux = aux + 1
		rend

	; set first sprite zoom to zero (hide artefact on right of the screen)
.hide_sprite_zoom
	ld hl,0
	xor a
	call intSpriteZoomN

.scrollXBase
	ld a,16
	add a		; *4
	add a		; because mode 0
	A_TO_BC

	ld (saveSP11+1),sp
	ld sp,BMAP_SCROLL_TABLE_BUFFER
	ld de,BMAP_SCROLL_TABLE

	ld a,16*2*3
	.loopCopyBuffer
		pop hl
		add hl,bc
		ex de,hl
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ex de,hl
		dec a
		jp nz,loopCopyBuffer

	.saveSP11
	ld sp,0

	; Build X Scroll table
	ld bc,BMP_XPOS_BASE
	ld de,BMAP_SCROLL_TABLE_BUFFER

	exx
	.sinXBMP1
	ld hl,sin		; -127<=a<=127
	.sinXBMP2
	ld de,sin		; -127<=a<=127
	ld b,16*2*3

	; test
	ld ixl,0

	.loopXScroll
		ld a,ixl
.decimalOndulX
		add 0;&E3
		ld ixl,a
		ld a,0
.integerOndulX
		adc 0
		add e
		
		ld e,a
		ld a,(de)	; -128<a<128
		sra a		; -64<a<64
		sra a		; -32<a<32
		ld c,a		; -32<c<32
		ld a,(hl)	; -128<a<128
		inc l:inc l
		sra a		; -64<a<64
		sra a		; -32<a<32
		ld (saveScrollX+1),a		; -32<(saveScrollX+1)<32
		sra a		; -16<a<16
		add c		; -48<a<48
	.saveScrollX
		add 0		; -80<a<80

.BMP_ENABLE_ONDULX
		nop;xor a
		;ld a,0		; TODO Remove after debug
		exx
		
		A_TO_HL

		add hl,bc	; add BMP_XPOS

		ex de,hl	; set value in BMAP_SCROLL_TABLE_BUFFER
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ex de,hl

		exx
		djnz loopXScroll
	
	ld hl,sinXBMP1+1
	ld a,(hl)
.BMP_WAVE2_ONDULX_SPEED
	add -2
	ld (hl),a

	ld hl,sinXBMP2+1
	ld a,(hl)
.BMP_WAVE1_ONDULX_SPEED
	add -3
	ld (hl),a

	ret

; ********
; TEST NOP counts ;)
; ********
.intDLRasters
	nop		; Used to synchronize (middle of screen)

	push af
	push bc
	push de
	push hl

        ; Connect Asic
 	ld bc,&7FB8		; [3]
 	out (c),c		; [4]

	; lock asic
 	ld a,&C9		; [2]
 	ld (deco_asic),a	; [4]

	; Ignore interrupt vector (SYNC)
	ld (intDLRasters),a

	; PROCESS
	; set PRI interrupt to be triggered for DISPLAY -1
	ld a, 0*&20 + BMP_YPOS_BASE - 1 - 2
	ld (&6800),a

	; It seems that the only way to ack the PRI seems to be the reset of the R52 register ???
	; Otherwise, it's triggered twice
	; To be tested on the real hardware ...
	if BUG_PRI
		ld bc,&7F00 + %10011100	; [3] Reset R52, Roms OFF, Mode 0
		out (c),c		; [4]
	endif

	ei
	halt
	

	; const
	ld bc,&40				; [3]

	; set stack pointer to XScroll
	ld (saveBMPSp+1),sp			; [6]
	ld sp,BMAP_SCROLL_TABLE			; [3]

	; wait some time
	;ds 64+6-4,0
	;ds 64+60,0
.delayBMP
	jr $:org $-1: db &35
	ds 64,0
	;ds 64,0

	
	pop hl				; [3]
	call intSpriteN			; [48]
	ds 64-3-48

;	ds 64-3-2-33,0
;.enable_zoom
;	ld hl,&6804		; [3]
;	ld a,ZOOM_BMP		; [2]
;	call intSpriteZoomN			; [33]

	; Comment on the operation to be performed below (every action padded to 64 NOPS)
	; 1 	- ShowSprite N
	;	- ScrollX N
	;
	; 2	- ScrollX N (16*2 - 1 - a time)
	;
	; 3	- ScrollX N+1 (16*2 time)
	;
	; 4	- ScrollX N+2 (16*2 time)
	;
	; 5	- DisplaySprite N
	;	- ScrollX N
	;
	; 6	- ScrollX N (a-1 time)
	;
	; 7	- Hide all sprites

; #1 align raster
	;pop hl				; [3]
	;call intSpriteN			; [48]
	;ds 64-3-48 - 2-1-3,0
.enable_zoom
	ld hl,&6804		; [3]
	ld a,ZOOM_BMP		; [2]
	call intSpriteZoomN	; [33]
	ds 64-3-2-33 -2-1-3,0

.ScrollN3
	ld d,1			; [2] get remaining lines to draw
	dec d			; [1]
	jp z,nomoreWaitN	; [3]	

; #2 align raster
	.waitN
	pop hl				; [3]
	call intSpriteN			; [48]
	call waitOneHBL+3+48+1+3	; [?]
	dec d				; [1]
	jp nz,waitN			; [3]

; #3 align raster - (fix the sprite line that disapear at the last phase of the skull wings move)
	pop hl				; [3]
	call intSpriteN1		; [48]
	ds 64-3-48,0

; #3 align raster - split_A
	.nomoreWaitN
	.intDLRastersCall2
	ld a,3*&20 + BMP_YPOS_BASE		; [2]
.split_A
	ld (1*8 + &6002),a		; [4] ;call intDisplayLastSprite1 	; [24]
	pop hl				; [3]
	call intSpriteN1		; [48]
	ds 64-2-4-3-48,0

; #3 align raster - split_B
.split_B
	ld (2*8 + &6002),a		; [4] ;call intDisplayLastSprite1 	; [24]
	pop hl				; [3]
	call intSpriteN1		; [48]
	ds 64-4-3-48,0

; #3 align raster - split_C
.split_C
	ld (3*8 + &6002),a		; [4] ;call intDisplayLastSprite1 	; [24]
	pop hl				; [3]
	call intSpriteN1		; [48]
	ds 64-4-3-48,0

; #3 align raster - split_D
.split_D
	ld (4*8 + &6002),a		; [4] ;call intDisplayLastSprite1 	; [24]
	pop hl				; [3]
	call intSpriteN1		; [48]
	ds 64-4-3-48,0

; #3 align raster - split_E
.split_E
	ld (5*8 + &6002),a		; [4] ;call intDisplayLastSprite1 	; [24]
	pop hl				; [3]
	call intSpriteN1		; [48]
	ds 64-4-3-48-2,0

	ld d,16*2-5-1-1			; [2] init next scroll

; #4 align raster +7
	.waitN1
	pop hl				; [3]
	call intSpriteN1		; [48]
	call waitOneHBL+3+48+1+3	; [?]
	dec d				; [1]
	jp nz,waitN1			; [3]

; #5 align raster +7
	ld d,16*2			; [2] !!!!! out of sync !!! init next scroll 

	.waitN2
	pop hl				; [3]
	call intSpriteN2		; [48]
	call waitOneHBL+3+48+1+3	; [?]
	dec d				; [1]
	jp nz,waitN2			; [3]

; #6 align raster +7 +2

.ScrollN
	ld d,1				; [2] !!!!! out of sync !!! get remaining lines to draw

	.waitN3
;	dec d				; [1]
;	jp z,hide_all			; [3]
;
;	pop hl				; [3]
;	call intSpriteN			; [48]
;	ds 64-1-3-3-48-3		; [?]
;	jp waitN3			; [3]

	pop hl				; [3]
	call intSpriteN			; [48]
	call waitOneHBL+3+48+1+3	; [?]
	dec d				; [1]
	jp nz,waitN3			; [3]

; #7 align raster +7 +2 +2
	; Hide all
.hide_all
	xor a			; [1]
	ld (1*8 + &6004),a	; [4]
	ld (6*8 + &6004),a	; [4]
	ld (11*8 + &6004),a	; [4]

	ld (2*8 + &6004),a	; [4]
	ld (7*8 + &6004),a	; [4]
	ld (12*8 + &6004),a	; [4]

	ld (3*8 + &6004),a	; [4]
	ld (8*8 + &6004),a	; [4]
	ld (13*8 + &6004),a	; [4]

	ld (4*8 + &6004),a	; [4]
	ld (9*8 + &6004),a	; [4]
	ld (14*8 + &6004),a	; [4]

	ld (5*8 + &6004),a	; [4]
	ld (10*8 + &6004),a	; [4]
	ld (15*8 + &6004),a	; [4]

	; set PRI interrupt to be triggered for SYNC
	ld a, 0*&20 + BMP_YPOS_BASE - 1 - 3
	ld (&6800),a

	; Enable interrupt vector
	xor a
	ld (intDLRasters),a

	; Deconnect Asic
	ld bc,&7FA0		; [3]
 	out (c),c		; [4]

	; unlock asic
	xor a
	ld (deco_asic),a

	.saveBMPSp
	ld sp,0

	if BUG_PRI
		ld bc,&7F00 + %10011100	; [3] Reset R52, Roms OFF, Mode 0
		out (c),c		; [4]
	endif

	pop hl	
	pop de
	pop bc
	pop af

	ei
	ret

.intSpriteN			; Total [48 = 5+3+40]
	jp intSpriteX1		; [43 = 3+40]
.intSpriteN1			; Total [48 = 5+3+40]
	jp intSpriteX2		; [43 = 3+40]
.intSpriteN2			; Total [48 = 5+3+40]
	jp intSpriteX3		; [43 = 3+40]

; IN A=ZOOM
.intSpriteZoomN ; [28+5=33]
	ld de,8			; [3]

	ld (hl),a		; [2]
	add hl,de		; [3]
	ld (hl),a		; [2]
	add hl,de		; [3]
	ld (hl),a		; [2]
	add hl,de		; [3]
	ld (hl),a		; [2]
	add hl,de		; [3]
	ld (hl),a		; [2]
	ret			; [3]

; X sprite 
; Input	HL = XPOS
; 	BC = X space const(&40)
.intSpriteX1			; Total = 40 nops
	ld (1*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (2*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (3*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (4*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (5*8 + &6000),hl	; [5]
	ret			; [3]

; X sprite 
.intSpriteX2			; Total = 40 nops
	ld (6*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (7*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (8*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (9*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (10*8 + &6000),hl	; [5]
	ret			; [3]

; X sprite 
.intSpriteX3			; Total = 40 nops
	ld (11*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (12*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (13*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (14*8 + &6000),hl	; [5]
	add hl,bc		; [3]
	ld (15*8 + &6000),hl	; [5]
	ret			; [3]

; This function swaps sprite order direct in the code
.swapSpritesX

	; swap the sprite table1
	ld hl,swapXBaseOffsetTable1
	ld e,l:ld d,h
	ld a,(hl):inc hl
	ex af,af'
	ld a,(hl):inc hl
	ex af,af'
	ldi:ldi:ldi:ldi
	ldi:ldi:ldi:ldi
	ld (de),a:inc de
	ex af,af'
	ld (de),a:inc de

	; swap the sprite table2
	ld a,(hl):inc hl
	ex af,af'
	ld a,(hl):inc hl
	ex af,af'
	ldi:ldi:ldi:ldi
	ldi:ldi:ldi:ldi
	ld (de),a:inc de
	ex af,af'
	ld (de),a:inc de

	; swap the sprite table1
	ld a,(hl):inc hl
	ex af,af'
	ld a,(hl):inc hl
	ex af,af'
	ldi:ldi:ldi:ldi
	ldi:ldi:ldi:ldi
	ld (de),a:inc de
	ex af,af'
	ld (de),a:inc de

	; swap sprites pos in code
	ld hl,(swapXBaseOffsetTable1):ld (moveSprite1+1),hl
	ld hl,(swapXBaseOffsetTable2):ld (moveSprite2+1),hl
	ld hl,(swapXBaseOffsetTable3):ld (moveSprite3+1),hl

	; First swaps the baseTable
	ld a,(swapXBaseTable)
	ld hl,swapXBaseTable+1
	ld de,swapXBaseTable
	ldi:ldi:ldi:ldi
	ld (swapXBaseTable+4),a

	; set intSpriteX
	ld hl,swapXBaseTable
	ld a,(hl):inc hl
	ld (intSpriteX1+1),a
	ld a,(hl):inc hl
	ld (intSpriteX1+5),a
	ld a,(hl):inc hl
	ld (intSpriteX1+9),a
	ld a,(hl):inc hl
	ld (intSpriteX1+13),a
	ld a,(hl):inc hl
	ld (intSpriteX1+17),a

	ld c,5*8
	ld hl,swapXBaseTable
	ld a,(hl):add c:inc hl
	ld (intSpriteX2+1),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX2+5),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX2+9),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX2+13),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX2+17),a

	ld c,2*5*8
	ld hl,swapXBaseTable
	ld a,(hl):add c:inc hl
	ld (intSpriteX3+1),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX3+5),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX3+9),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX3+13),a
	ld a,(hl):add c:inc hl
	ld (intSpriteX3+17),a
	
	ret

.swapXBaseTable ;(<-)
db 	1*8,2*8,3*8,4*8,5*8

.swapXBaseOffsetTable1 ;(<-)
dw 	&4145,&4241,&4342,&4443,&4544
.swapXBaseOffsetTable2 ;(<-)
dw 	&464A,&4746,&4847,&4948,&4A49
.swapXBaseOffsetTable3 ;(<-)
dw 	&4B4F,&4C4B,&4D4C,&4E4D,&4F4E

dw	0			; call ret
dw	0			; call ret
dw	0			; call ret
dw	0			; call ret
.BMAP_SCROLL_TABLE
repeat	16*2*3
	dw &300
rend


dw	0			; call ret
dw	0			; call ret
dw	0			; call ret
dw	0			; call ret
dw	0
dw	0
.BMAP_SCROLL_TABLE_BUFFER
repeat	16*2*3
	dw &300
rend