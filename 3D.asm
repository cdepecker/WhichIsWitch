; DT = 2*DPOW (distance from eyes to screen)
; ZT = DT+ZMAX (ZMAX is the max size of the object)
DPOW  EQU 6 ; 4,5,6,7 are ok

let DT = 2
repeat DPOW-1
let DT = 2*DT
rend
;let ZT = DT+ZMAX

; ******************
; * Init Sprite 3D *
; ******************

.set3DSpriteA1
	di
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; hide sprites
	call hideAllSprites

	; copy NB_BALLS balls sprite in asic memory
	ld a,6
	ld de,&4000
	call copyNSprite0

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ei
	ret

.set3DSpriteA2
	di

	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; copy NB_BALLS balls sprite in asic memory
	ld a,5
	ld de,&4600 
	call copyNSprite0

	; Set zoom
	ld hl,&6004
	ld de,8
	ld b,NB_BALLS_TOTAL
	.setZoomA
		ld (hl),ZOOM_3D
 		add hl,de
	djnz setZoomA

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ei
	ret

.set3DSpriteA3
	di

	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; copy NB_BALLS balls sprite in asic memory
	ld a,5
	ld de,&4B00 
	call copyNSprite0

	; Change Sprite inks
	ld a,15
	ld hl,ball1pal
	ld de,&6422
	.copy_balls_pal
		ldi:ldi
		dec a
		jp nz,copy_balls_pal

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	ei
	ret

.set3DSpriteB1
	di
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; hide sprites
	call hideAllSprites

	; copy NB_BALLS balls sprite in asic memory
	ld a,6
	ld de,&4000
	call copyNSprite1

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ei
	ret

.set3DSpriteB2
	di
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; copy NB_BALLS balls sprite in asic memory
	ld a,5
	ld de,&4600 
	call copyNSprite1

	; Set zoom
	;ld hl,&6004
	;ld de,8
	;ld b,NB_BALLS_TOTAL
	;.setZoomB
	;	ld (hl),ZOOM_3D
 	;	add hl,de
	;djnz setZoomB
	
	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ei
	ret

.set3DSpriteB3
	di
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; copy NB_BALLS balls sprite in asic memory
	ld a,5
	ld de,&4B00 
	call copyNSprite1

	; Set zoom
	ld hl,&6004
	ld de,8
	ld b,NB_BALLS_TOTAL
	.setZoomB
		ld (hl),ZOOM_3D
 		add hl,de
	djnz setZoomB

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	ei
	ret

; ************
; set3DSpriteC
; ************
; WARNING ASIC should be on !!!
.set3DSpriteC1
	; hide sprites
	call hideAllSprites

	; balles from the nearest
	ld c,0
	ld de,&4000
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect:inc d

	ld c,2
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect
	ret

.set3DSpriteC2	
	ld c,4
	ld de,&4400
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect:inc d

	ld c,6
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect
	ret

.set3DSpriteC3	
	ld c,8
	ld de,&4800
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect:inc d	

	ld c,10
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect
	ret

.set3DSpriteC4	
	ld c,10
	ld de,&4C00
	call copySpriteDeepEffect:inc d
	
	ld c,12
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect:inc d
	call copySpriteDeepEffect

	; Set zoom
	ld hl,&6004
	ld de,8
	ld b,NB_BALLS_TOTAL
	.setZoomC
		ld (hl),ZOOM_3D
 		add hl,de
	djnz setZoomC

	; Change Sprite inks
	ld a,15
	ld hl,ball3pal
	ld de,&6422
	.copy_balls3_pal
		ldi:ldi
		dec a
		jp nz,copy_balls3_pal
	ret

; ********
; END INIT
; ********

.morphing
	nop

	; morphing !!!
	.nextobj
	ld de,OBJ3D
	
	ld hl,morphobj
	ld a,(nbBallsMul3)
	ld b,a
	ld c,a
	.loopMorph
		ld a,(de)
		cp (hl)
		jr z,mzero	; morphing done
		jp p,minc	; need to be incremented
		
		; decremented
		dec (hl)
		inc hl
		inc de
		djnz loopMorph
		jp endmorphing		

		; incremente
		.minc
		inc (hl)
		inc hl
		inc de
		djnz loopMorph
		jp endmorphing

		; incremente
		.mzero
		dec c
		inc hl
		inc de
		djnz loopMorph

	; c>0 if morphing is not finished
	.endmorphing
	ld a,c
	or a
	ret nz	; return if morphing not finished

	; morphing finished
	ld a,&C9
	ld (morphing),a

	ret

	;ld a,(de)		; get object attribute
	;rlca			; speed effect in carry ?
	;jr c,enableSpeedFx
	
	; else disable speed FX visibility
	;ld a,0:;org $-1:;ret
	;ld (speedFx_visible),a
	;ret

	;.enableSpeedFx
	;xor a
	;ld (speedFx_visible),a
	;ret

; Change object in the sequence of objects
.change_object
	; morphing
	call morphing

	; if obj counter reached 0, then change object
	ld hl,(object_timing)
	inc hl
	ld (object_timing),hl
	ld a,h
	or a
	ret nz

	; if zero, reset counter
	ld hl,-OBJECTS_TIME
	ld (object_timing),hl

	; change object
	ld a,(objnum)
	or a
	jr nz,cont_objseq
	ld a,OBJECTS_NUMBER
	.cont_objseq
	dec a
	ld (objnum),a

	.forceObject ; (ld a,0-n)
	nop:nop

	ld hl,obj_seq
	ld d,a
	add a
	add a
	add d		; because of attributes
	A_TO_DE
	add hl,de 	;hl points on the object address

	; enable morphing
	xor a
	ld (morphing),a

	; set next object
	ld de,nextobj+1
	ldi
	ldi

	.enableSym3DFx
	nop

	; hl points on the symetrie routine to use
	ld de,symetrie+1
	ldi
	ldi

	; hl points on the speedFX to use
	ld a,(hl)
	call setSpeedFX

	ret

; *************
; SECOND OBJECT
; *************
.morphing2
	nop

	; morphing !!!
	.nextobj2
	ld de,OBJ3D
	ld hl,morphObj2
	ld a,(nbBallsMul3)
	ld b,a
	ld c,a
	.loopMorph2
		ld a,(de)
		cp (hl)
		jr z,mzero2	; morphing done
		jp p,minc2	; need to be incremented
		
		; decremented
		dec (hl)
		inc hl
		inc de
		djnz loopMorph2
		jp endmorphing2		

		; incremente
		.minc2
		inc (hl)
		inc hl
		inc de
		djnz loopMorph2
		jp endmorphing2

		; incremente
		.mzero2
		dec c
		inc hl
		inc de
		djnz loopMorph2

	; c>0 if morphing is not finished
	.endmorphing2
	ld a,c
	or a
	ret nz	; return if morphing not finished

	; morphing finished
	ld a,&C9
	ld (morphing2),a
	ret

; Change object in the sequence of objects
.change_object2
	; morphing
	call morphing2

	; if obj counter reached 0, then change object
	ld hl,(object_timing2)
	inc hl
	ld (object_timing2),hl
	ld a,h
	or a
	ret nz

	; if zero, reset counter
	ld hl,-OBJECTS_TIME2
	ld (object_timing2),hl

	; change object
	ld a,(objnum2)
	or a
	jr nz,cont_objseq2
	ld a,OBJECTS_NUMBER
	.cont_objseq2
	dec a
	ld (objnum2),a

	.forceObject2 ; (ld a,0-n)
	nop:nop

	ld hl,obj_seq
	ld d,a
	add a
	add a
	add d
	A_TO_DE
	add hl,de ;hl points on the object address
	ld de,nextobj2+1
	ldi
	ldi

	; hl points on the symetrie routine to use
	;ld de,symetrie+1
	;ldi
	;ldi

	; hl points on the speedFX to use
	;ld a,(hl)
	;call setSpeedFX

	; enable morphing
	xor a
	ld (morphing2),a

	ret

; copy NB_BALLS balls pos & zoom
; TODO - Need speed optimisation
; IN ; HL balls position
; CORRUPTED ; AF, BC, DE, HL
.copy_balls_pos
	; Connect Asic
	ld bc,&7FB8
	out (c),c
	
	ld a,NB_BALLS_TOTAL
	ld de,&6000
	.copy_balls_pos_loop
		ldi:ldi:ldi:ldi ;ldi
		inc e	; zoom
		inc e	; inc de is useless
		inc e
		inc e
		dec a
		jp nz,copy_balls_pos_loop

	; Deconnect Asic
	ld bc,&7FA0
	out (c),c
	ret

; ***************
; Translation vectorielle
; *************** 
.translation_vectorZ

	; get (a6 a7 a8) from rotation matrix and
	; stick them in the code below
	ld hl,(_a6+1)	; get int value
		repeat 4	; *unit
		add hl,hl
		rend
		ld d,h
		ld e,l
		add hl,hl	; 2*unit
		add hl,de	; 3*unit
	ld a,h
	ld (translateX+1),a
	ld hl,(_a7+1)		; get int value
	;ld hl,%10000
		repeat 4	; *unit
		add hl,hl
		rend
		ld d,h
		ld e,l
		add hl,hl	; 2*unit
		add hl,de	; 3*unit
	ld a,h
	ld (translateY+1),a
	ld hl,(_a8+1)	; get int value
		repeat 4	; *unit
		add hl,hl
		rend
		ld d,h
		ld e,l
		add hl,hl	; 2*unit
		add hl,de	; 3*unit
	ld a,h
	ld (translateZ+1),a

	ld hl,(crdEndNbBalls)
	ld de,NBCRD*NB_BALLS_TOTAL + ballpos3D - 1
	ld a,(nbBalls)
	add -NB_BALLS_TOTAL
	neg
	ld b,a
	.copyTrans
		; X
		ld a,(hl)
		dec hl
		.translateX
		add 0
		ld (de),a
		dec de
		; Y
		ld a,(hl)
		dec hl
		.translateY
		add 0
		ld (de),a
		dec de
		; Z
		ld a,(hl)
		dec hl
		.translateZ
		add 0
		ld (de),a
		dec de
		djnz copyTrans
	ret

; *****************
; Symetrie centrale
; *****************
.symetrie_centrale
	ld hl,(crdEndNbBalls)
	ld de,NBCRD*NB_BALLS_TOTAL + ballpos3D - 1
	ld a,(nbBalls)
	add -NB_BALLS_TOTAL
	neg
	ld b,a
	.symcenter
		; X
		ld a,(hl)
		dec hl
		neg
		ld (de),a
		dec de
		; Y
		ld a,(hl)
		dec hl
		neg
		ld (de),a
		dec de
		; Z
		ld a,(hl)
		dec hl
		neg
		ld (de),a
		dec de
		djnz symcenter
	ret
	
; ************
; Speed effect
; ************
.speed_effect
	nop	; enabled
	; We are going to make a speed effect
	; Save 6 old positions for NBS balls
	; All the saved positions are not z sorted
	;NBS	EQU 6	; number balls saved 


	; here we have the following pos ; (0-15) A B C D E F
	; Copy 5 previous positions for NBS balls
	ld de,6*NBS+NB_BALLS_TOTAL*NBCRD + ballpos3D - 1
	ld hl,5*NBS+NB_BALLS_TOTAL*NBCRD + ballpos3D - 1
	ld bc,5*NBCRD*NBS
	lddr

	; here we have the following pos ; (0-15) A A B C D E
	; copy 6 last balls defined
	ld hl,NB_BALLS*NBCRD + ballpos3D - 1
	ld bc,NBCRD*NBS
	lddr	

	; here we have the following pos ; (0-15) (10-15) A B C D E
	; Do we want to keep speed effect visible ?
	.speedFx_visible
	nop

	; Copy 6th oldest balls pos to the balls 6,7
	ld hl,6*NBS+NB_BALLS_TOTAL*NBCRD + ballpos3D - 1
	ld de,NB_BALLS*NBCRD + ballpos3D - 1
	repeat NBCRD*2
		ldd
	rend

	; Copy 4th oldest balls pos to the balls 4,5
	ld hl,4*NBS + NB_BALLS_TOTAL*NBCRD + ballpos3D - 1 - NBCRD - NBCRD
	repeat NBCRD*2
		ldd
	rend

	; Copy 2nd oldest balls pos to the balls 2,3
	ld hl,2*NBS+NB_BALLS_TOTAL*NBCRD + ballpos3D - 1 - NBCRD - NBCRD - NBCRD - NBCRD
	repeat NBCRD*2
		ldd
	rend
	ret

; This function tranform 3D position in 2D screen position
; x' = x*deye/z (deye is the distance from eye to screen)
; y' = y*deye/z
; deye = 2^DPOW
;.projection
;	ld hl,ballpos3D
;	ld e,NB_BALLS
;.projloop
;	ld c,(hl)	; get z (must be >= 2^DPOW)
;	inc hl
;	;inc hl		; speed effect

	;****
	; Y''
	;****
;	ld a,(hl)	; get y (ensure -127<=x<=127)

	; is y<0
;	or a
;	jp P,posY
;	call neg_8div8cust
;	jp saveY

;	.posY
;	call _8div8cust

;	.saveY	
;	add 128		; TODO Optimize ? Move ?
;	ld (hl),a
;	inc hl

	;****
	; X''
	;****
;	ld a,(hl)	; get x (ensure -127<=x<=127)

	; is x<0
;	or a
;	jp P,posX
;	call neg_8div8cust
;	jp saveX

;	.posX
;	call _8div8cust

;	.saveX
;	add 128		; TODO Optimize ? Move ?
;	ld (hl),a
;	inc hl	

;	dec e
;	jp nz,projloop
;	ret 

; ******
; SORT Z
; ******
.sortZ
	nop	; Could be disabled ...

	; Bubble sort addresses
	; init table sort
	ld hl,ballsortZinit
	ld de,ballsortZ
	repeat NB_BALLS_TOTAL
	ldi
	rend

	ld d,ballpos3D/256	; [2] d is high address of ballpos3D
	;ld IX,NB_BALLS_MIN1*256 + NB_BALLS_MIN1	; [4] IXh = NB cycles, IXl = NB cmp in cycle
	;ld IX,15*256 + 15	; [4] IXh = NB cycles, IXl = NB cmp in cycle
	ld IX,15*256 + 15	; [4] IXh = NB cycles, IXl = NB cmp in cycle
.bubblecycle
	ld IXl,IXh		; [2] IXl = NB cmp in cycle
	ld hl,ballsortZ		; [3] hl = ballsortZ	
	ld e,(hl)		; [2] de = Add1
	ld a,(de)		; [2] a = (de) = (Add1)
	ld b,e			; [1] save e

.bubbleloop
	inc hl			; [1]
	ld e,(hl)		; [2] de = Add2
	ex hl,de		; [1]
	ld c,(hl)		; [2] c = (de) = (Add2)
	ex hl,de		; [1]

	cp c			; [1]
	;jr nc,noswap		; [2,3] sort descending
	jp P,noswap		; [2,3] sort descending

	; swap
	ld (hl),b		; [2]
	dec hl			; [2]
	ld (hl),e		; [2]
	inc hl			; [2]
	jr bub_cycle_end	; [3]
.noswap
	ld a,c			; [1]
	ld b,e			; [1]

.bub_cycle_end
	dec IXl			; [2]
	jr nz,bubbleloop	; [2,3]

	dec IXh			; [2]
	jr nz,bubblecycle	; [2,3]
	ret

; translate a into bc
macro A_to_BC
	ld	c,a
	rlca		; carry hold sign
	sbc	a 	; 0x00 or 0xFF
	ld	b,a
endm


; ************************
; Translate and Save Pos2D
; ************************
.moveSave2D
	.sin3DTransX
	ld a,(sin+192+20)
	ld hl,sin3DTransX+1
	inc (hl)
	
	.invertX
	nop:nop

	; X (Beware we move 2pix at a time)
	A_TO_HL
	add hl,hl	; mult2
	ld de,96*8/2-16	; middle screen mode 2 minus ~ sprite width
	add hl,de
	ex hl,de

	; Y
	.sin3DTransY
	ld a,(sin+256-30)
	sra a		; div2
	add 128		; Relative to middle screen
	ld b,a

	.sin3DTransYDecimal
	ld a,0
	add &DD
	ld (sin3DTransYDecimal+1),a
	jr nc,nosin3DTransYinc
	ld hl,sin3DTransY+1
	inc (hl)
	.nosin3DTransYinc

	; One object
	.movY3D
	ld a,b		;A<-Y
	nop
	.movX3D
	ex hl,de	;HL<-X
	nop
	nop

	;jp savePos2D

; *********
; savePos2D
; *********
; This translates the 2D position (8 bytes) to screen (16 bytes)
; In 	; A = YTranslation [128-64; 128+64]
; 	; hl = XTranslation [384-256; 384+256] 
; Corrupted ; Almost everything
.savePos2D
	; ****************
	; Init Translation
	; ****************
	;ld hl,256
	;ld a,128
	ld (XTRANS+1),hl
	ld (YTRANS+1),a

	; **************
	; save ballpos2D
	; **************	
	ld (saveSP+1),sp
	.saveBall2D_SP
	ld sp,ballpos2Dend

	;ld d,ballpos3D/256	; TODO - May be already set by the code above
	ifnot SPRITE_TRANSPARENT
		ld hl,ballsortZ
	else
		ld hl,ballsortZInit
	endif
	ld a,NB_BALLS_TOTAL
	.saveBall2D
		ld d,ballpos3D/256	; TODO - May be already set by the code above
		ld e,(hl)
		inc hl
		inc e
		
		ex hl,de
		ld b,0
		ex af,af'
		ld a,(hl)
		.YTRANS
		add 0		;YTRANS
		ld c,a
		;ex af,af'
		push bc		; save Y
		inc hl
		
		ld a,(hl)
		add a		
		A_TO_BC

		.XTRANS
		ld hl,0		;XTRANS
		add hl,bc
		push hl		; save X
		;push bc
		ex hl,de

		ex af,af'
		dec a
		jr nz,saveBall2D  

	.saveSP
	ld sp,0

	; quick fix
	ret
	
; TODO - Optimise speed
;.translation
;	ld hl,ballpos3D
;	ld de,3
;	ld c,ZT
;	;ld b,NB_BALLS
;	ld b,NB_BALLS_TOTAL
;	.transloop
;		; Z
;		ld a,(hl)
;		add a,c
;		ld(hl),a
;		add hl,de
;	djnz transloop
;	ret

; Rotation
; ********

; negate HL
macro neg_HL
	ld 	a,l
	cpl
	ld	l,a
	ld 	a,h
	cpl
	ld	h,a
	inc	hl
endm

.rotation
; ***************************
; compute the rotation matrix
; to compute the new positions
; 	  (a0 a1 a2)
; (x y z)*(a3 a4 a5)
; 	  (a6 a7 a8)
;
; Note (0 0 1)*R = (a6 a7 a8)
; ***************************
	
;	Assumptions
;	regB = b
;	regC = c
;	regD = a
;	regHL = sin table address

	;set regE = pi/2 = 64
	ld e,64
	
; A = (cos(b-c) + cos(b+c))/2
	ld	a,b
	sub	c 	; b-c	
	add	e
	ld	l,a
	ld	a,(hl) 	; cos(b-c)
	
	; translate a into de'
	exx
	A_to_DE
	exx
	
	ld	a,b
	add	c 	; b+c	
	add	e
	ld	l,a
	ld	a,(hl) 	; cos(b+c)
	
	; translate a into hl'
	exx
	A_to_HL
	add	hl,de 	; hl = cos(b-c) + cos(b+c)
	ld	(a0+1),hl ; save a0
	neg_HL
	ld	(_a0+1),hl ; save _a0
	exx
	
; B = (sin(b-c) - sin(b+c))/2	
	ld	a,b
	add	c
	ld	l,a
	ld	a,(hl) ; sin(b+c)
	
	; translate a into de'
	exx
	A_to_DE
	exx	

	ld	a,b
	sub	c
	ld	l,a
	ld	a,(hl) ; sin(b-c)
	
	; translate a into hl'
	exx
	A_to_HL
	and a		; reset carry
	sbc hl,de 	; hl = sin(c+b) + sin(c-b)
	ld	(a1+1),hl ; save a1
	neg_HL
	ld	(_a1+1),hl ; save _a1
	exx

; C = sin(b)
	ld l,b
	ld a,(hl)

	; translate a into hl'
	exx
	A_to_HL
	add hl,hl	; sin(b)*2
	ld	(a2+1),hl ; save a2
	neg_HL
	ld	(_a2+1),hl ; save _a2
	exx

; D = ((cos(a-b+c) + cos(a-b-c) - (cos(a+b-c) + cos(a+b+c)))/2 + sin(c-a) + sin(c+a))/2
	; cos(a+b+c)
	ld a,d
	add b
	add c
	add e
	ld l,a
	ld a,(hl)
 	
	; translate a into de'
	exx
	A_to_DE
	exx

	; cos(a+b-c)
	ld a,d
	add b
	sub c
	add e
	ld l,a
	ld a,(hl)

	; translate a into hl'
	exx
	A_to_HL
	add hl,de 	; hl = cos(a+b-c) + cos(a+b+c)
	exx

	; cos(a-b-c)
	ld a,d
	sub b
	sub c
	add e
	ld l,a
	ld a,(hl)

	exx
	ex de,hl
	A_to_HL
	and	a	; reset carry
	sbc	hl,de 	; hl = cos(a-b-c) - (cos(a+b-c) + cos(a+b+c))
	exx

	; cos(a-b+c)
	ld a,d
	sub b
	add c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; cos(a-b+c) + cos(a-b-c) - (cos(a+b-c) + cos(a+b+c))
	sra h
	rr l		; /2
	exx

	; sin(c+a)
	ld a,c
	add d
	ld l,a
	ld a,(hl)
	
	exx
	A_to_DE
	add hl,de
	exx

	; sin(c-a)
	ld a,c
	sub d
	ld l,a
	ld a,(hl)
	
	exx
	A_to_DE
	add hl,de
	ld	(a3+1),hl ; save a3
	neg_HL
	ld	(_a3+1),hl ; save _a3
	exx

; E = ((sin(b-a-c) + sin(c-a-b) + sin(a-b-c) + sin(a+b+c))/2 + cos(a-c) + cos(a+c))/2	
	; sin(a+b+c)
	ld a,d
	add b
	add c
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	exx

	; sin(a-b-c)
	ld a,d
	sub b
	sub c
	ld l,a
	ld a,(hl)

	exx
	A_to_HL
	add hl,de
	exx

	; sin(c-a-b)
	ld a,c
	sub d
	sub b
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add	hl,de 	; sin(c-a-b) + sin(a-b-c) + sin(a+b+c)
	exx

	; sin(b-a-c)
	ld a,b
	sub d
	sub c
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; sin(b-a-c) + sin(c-a-b) + sin(a-b-c) + sin(a+b+c)
	sra h
	rr l		; /2
	exx

	; cos(a-c)
	ld a,d
	sub c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; hl = (sin(b-a-c) + sin(c-a-b) + sin(a-b-c) + sin(a+b+c))/2 + cos(a-c)
	exx

	; cos(a+c)
	ld a,d
	add c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; hl = (sin(b-a-c) + sin(c-a-b) + sin(a-b-c) + sin(a+b+c))/2 + cos(a-c) + cos(a+c)
	ld	(a4+1),hl ; save a4
	neg_HL
	ld	(_a4+1),hl ; save _a4
	exx

; F = (sin(b-a) - sin(a+b))/2 

	; sin(a+b)
	ld a,d
	add b
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	exx

	; sin(b-a)
	ld a,b
	sub d
	ld l,a
	ld a,(hl)

	exx
	A_to_HL
	and	a	; reset carry
	sbc	hl,de	; sin(b-a) - sin(a+b)
	ld	(a5+1),hl ; save a5
	neg_HL
	ld	(_a5+1),hl ; save _a5
	exx

; G = ((sin(a+c-b) + sin(a-b-c) + sin(c-a-b) - sin(b+a+c))/2 + cos(a-c) - cos(a+c))/2

	; sin(b+a+c)
	ld a,b
	add d
	add c
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	exx

	; sin(c-a-b)
	ld a,c
	sub d
	sub b
	ld l,a
	ld a,(hl)

	exx
	A_to_HL
	and a		; reset carry
	sbc hl,de	; sin(c-a-b) - sin(b+a+c)
	exx

	; sin(a-b-c)
	ld a,d
	sub b
	sub c
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; sin(a-b-c) + sin(c-a-b) - sin(b+a+c)
	exx

	; sin(a+c-b)
	ld a,d
	add c
	sub b
	ld l,a
	ld a,(hl)
	
	exx
	A_to_DE
	add hl,de	; sin(a+c-b) + sin(a-b-c) + sin(c-a-b) - sin(b+a+c)
	sra h
	rr l		; /2
	exx	

	;cos(a+c)
	ld a,d
	add c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	and a		; reset carry
	sbc hl,de	; (sin(a+c-b) + sin(a-b-c) + sin(c-a-b) - sin(b+a+c))/2 - cos(a+c)
	exx

	;cos(a-c)
	ld a,d
	sub c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; (sin(a+c-b) + sin(a-b-c) + sin(c-a-b) - sin(b+a+c))/2 + cos(a-c) - cos(a+c)
	ld	(a6+1),hl ; save a6
	neg_HL
	ld	(_a6+1),hl ; save _a6
	exx 

; H = ((cos(b-a-c) + cos(b+a-c) - (cos(b-a+c) + cos(b+a+c)))/2 + sin(a-c) + sin(a+c))/2

	; cos(b+a+c)
	ld a,b
	add d
	add c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	exx

	; cos(b-a+c)
	ld a,b
	sub d
	add c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_HL
	add hl,de	; cos(b-a+c) + cos(b+a+c)
	exx

	; cos(b+a-c)
	ld a,b
	add d
	sub c
	add e
	ld l,a
	ld a,(hl)

	exx
	ex de,hl
	A_to_HL
	and a		; reset carry
	sbc hl,de	; cos(b+a-c) - (cos(b-a+c) + cos(b+a+c))
	exx

	; cos(b-a-c)
	ld a,b
	sub d
	sub c
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; cos(b-a-c) + cos(b+a-c) - (cos(b-a+c) + cos(b+a+c))
	sra h
	rr l		; /2
	exx

	; sin(a+c)
	ld a,d
	add c
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; (cos(b-a-c) + cos(b+a-c) - (cos(b-a+c) + cos(b+a+c)))/2 + sin(a+c)
	exx

	; sin(a-c)
	ld a,d
	sub c
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	add hl,de	; (cos(b-a-c) + cos(b+a-c) - (cos(b-a+c) + cos(b+a+c)))/2 + sin(a-c) + sin(a+c)
	ld (a7+1),hl 	; save a7
	neg_HL
	ld(_a7+1),hl ; save _a7
	exx

; I = (cos(a-b) + cos(a+b))/2

	; cos(a+b)
	ld a,d
	add b
	add e
	ld l,a
	ld a,(hl)

	exx
	A_to_DE
	exx

	; cos(a-b)
	ld a,d
	sub b
	add e
	ld l,a
	ld a,(hl)

	exx
	
	A_to_HL
	add hl,de
	ld (a8+1),hl 	; save a8
	neg_HL
	ld(_a8+1),hl ; save _a8
	;exx

; *********************************
; Mult x,y,z by the rotation matrix
; *********************************
	;ld bc,NBCRD*NB_BALLS + ballpos3D - 1
	ld bc,(crdEndNbBalls)
.mult3D
	; get x
	; *****
	ld a,(bc)
	dec c

	; if a is neg, do the mult with -a and negate result
	or a
	jp M,negX

	; x*a0
	.a0
	ld de,0
	call _16mult8cust
	ex hl,de
	ld ixh,d
	ld ixl,e	

	; x*a1
	.a1
	ld de,0
	call _16mult8cust
	ex hl,de
	ld iyh,d
	ld iyl,e

	; x*a2
	.a2
	ld de,0
	call _16mult8cust
	push hl
	jp getY

.negX
	neg	; set Carry when not zero

	; -x*a0
	._a0
	ld de,0
	call _16mult8cust
	ex hl,de
	ld ixh,d
	ld ixl,e
	
	; -x*a1
	._a1
	ld de,0
	call _16mult8cust
	ex hl,de
	ld iyh,d
	ld iyl,e

	; -x*a2
	._a2
	ld de,0
	call _16mult8cust
	push hl

.getY
	; get y
	; *****
	ld a,(bc)
	dec c

	; if a is neg, do the mult with -a and negate invert result
	or a
	jp M,negY

	; y*a3
	.a3
	ld de,0
	call _16mult8cust
	ex hl,de
	add ix,de
	
	; y*a4
	.a4
	ld de,0
	call _16mult8cust
	ex hl,de
	add iy,de

	; y*a5
	.a5
	ld de,0
	call _16mult8cust
	pop de
	add hl,de
	push hl
	jp getZ

.negY
	neg	; set Carry when not zero

	; -y*a3
	._a3
	ld de,0
	call _16mult8cust
	ex hl,de
	add ix,de
	
	; -y*a4
	._a4
	ld de,0
	call _16mult8cust
	ex hl,de
	add iy,de

	; -y*a5
	._a5
	ld de,0
	call _16mult8cust
	pop de
	add hl,de
	push hl

.getZ
	; get z
	; *****
	ld a,(bc)

	; if a is neg, do the mult with -a and negate invert result
	or a
	jp M,negZ

	; z*a6
	.a6
	ld de,0
	call _16mult8cust
	ex hl,de
	add ix,de
	
	; z*a7
	.a7
	ld de,0
	call _16mult8cust
	ex hl,de
	add iy,de

	; z*a8
	.a8
	ld de,0
	call _16mult8cust
	pop de
	add hl,de
	jp savePos

.negZ
	neg	; set Carry when not zero

	; -z*a6
	._a6
	ld de,0
	call _16mult8cust
	ex hl,de
	add ix,de
	
	; -z*a7
	._a7
	ld de,0
	call _16mult8cust
	ex hl,de
	add iy,de

	; -z*a8
	._a8
	ld de,0
	call _16mult8cust
	pop de
	add hl,de

.savePos
	; here we have
	; ix= 256*z'
	; iy= 256*y'
	; hl= 256*x'
	; (bc) = x

	ld d,b
	ld e,c

	ld a,h
	ld (de),a	; save z' (/256)
	inc e
	ld a,iyh
	ld (de),a	; save y' (/256)
	inc e
	ld a,ixh
	ld (de),a	; save x' (/256)
	
	; next ball
	dec c	

	; BEWARE ! NB_BALLS * NB_COORD should be < 128
	;if NB_BALLS*NBCOORD < 128
	;	printf "Error, NB_BALLS * NB_COORD should be < 128"
	;	stop
	;endif
	jp P,mult3D
	ret

; Time 5 + 8*14 + 3
; hl = de * a
; a,b,c are safe
; de corrupted
; assumptions
;	0 <= a <= 127 
_16mult8cust
	ld hl,0			; [3]
repeat 7
	rrca			; [1]
	jr nc,$+3		; [2,3]
	add hl,de		; [3]
	sla e			; [2]
	rl d			; [2]
rend
	rrca			; last shift to get reg a back
	ret			; [3]

; This function does d' = 2pDPOW * d / c
; Assumptions
; 	c >= 2pDPOW (screen)
;	d >= 0 (meaning bit7 is zero)
; IN a=dividend, c=diviseur, a=0
; OUT a=quotient, b=0
; corrupted d
;_8div8cust
;	ld d,a
;	xor a	; a = 0
;	; avoid first bit (always zero)
;	sla d	; reset CF because bit7=0
;
;	; avoid DPOW loop
;	repeat DPOW-1
;	rl d
;	rla	
;	rend

;	ld b,8
;_8div8loop
;	rl d
;	rla
;	sub c
;	jr nc,_8div8end
;	add c
;_8div8end
;	djnz _8div8loop
;	
;	rl d	; get bit0 (in carry)
;	add a	; remainder * 2
;	cp c	; carry when no need to round
;	jr c,_8div8noround
;	dec d	; round result
;_8div8noround	
;	ld a,d
;	cpl	; compl result
;	ret

; NEG version
; This function does d' = 2pDPOW * d / c
; Assumptions
; 	c >= 2pDPOW (screen)
;	d >= 0 (meaning bit7 is zero)
; IN a=neg(dividend), c=diviseur, a=0
; OUT a=neg quotient, b=0
; corrupted d
;neg_8div8cust
;	neg
;	ld d,a
;	xor a	; a = 0
;	; avoid first bit (always zero)
;	sla d	; reset CF because bit7=0
;
;	; avoid DPOW loop
;	repeat DPOW-1
;	rl d
;	rla	
;	rend
;
;	ld b,8
;neg_8div8loop
;	rl d
;	rla
;	sub c
;	jr nc,neg_8div8end
;	add c
;neg_8div8end
;	djnz neg_8div8loop
;	
;	rl d	; get bit0 (in carry)
;	add a	; remainder * 2
;	cp c	; carry when no need to round
;	jr nc,neg_8div8round
;	inc d	; don't round result
;neg_8div8round	
;	ld a,d
;	ret


