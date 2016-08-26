; Contain parts of the 3D engine that cannot fit within the allowed memory
; IN a = 0 (nospeedFX)
;    a = 1 (simple speedFX)
.setSpeedFX
	or a
	jr nz,testSpeed1
	
	; no speedFx
	ld hl,idle
	ld (speed_FX+1),hl
	ret

	.testSpeed1

	; Copy 6 previous positions for NBS balls
	ld de,NB_BALLS_TOTAL*NBCRD + ballpos3D
	ld hl,NB_BALLS_TOTAL-NBS*NBCRD + ballpos3D
	ld bc,6*NBCRD*NBS
	ldir

	ld hl,speed_effect
	ld (speed_FX+1),hl
	ret

; ***************
; Symetrie Plan Z
; Bug = we need to multiply by Z stored in the morphObj table (fix)
; Corrupted = AF,BC,DE,HL,BC',DE',HL'
; *************** 
.symetrie_planZ
	
	; prepare step through morphObj
	ld ix,morphObj
	ld bc,3
	exx
	
	ld hl,ballpos3D
	ld de,NBCRD*NB_BALLS + ballpos3D
	ld a,(nbBalls)
	add -NB_BALLS_TOTAL
	neg
	ld b,a
	.copySymetrieZ
		; Z
		; *****
		ld a,(ix)

		or a
		jp P,$+3+2
		neg
		ld c,a		; save abs(Z) in c

		; ***********************************
		; compute 2 * |0 0 z| * (_a6 _a7 _a8)
		exx
		add ix,bc	; inc through MorphObj

		;ld a,2*16
		ld de,(_a8+1)
		call _16mult8cust	
		ld a,h
		add a		; a = 2*abs(z)*_a8
		exx
		; ***********************************
		add (hl)	; a = z + 2*abs(z)*_a8
		inc hl
		ld (de),a
		inc de

		; Y
		; *****
		; ***********************************
		; compute 2 * |0 0 z| * (_a6 _a7 _a8)
		ld a,c
		exx
		;ld a,2*16
		ld de,(_a7+1)
		call _16mult8cust	
		ld a,h
		add a		; a = 2*abs(z)*_a7
		exx
		; ***********************************
		add (hl)	; a = y + 2*abs(z)*_a7
		inc hl
		ld (de),a
		inc de

		; X
		; *****
		; ***********************************
		; compute 2 * |0 0 z| * (_a6 _a7 _a8)
		ld a,c
		exx
		;ld a,2*16
		ld de,(_a6+1)
		call _16mult8cust	
		ld a,h
		add a		; a = 2*abs(z)*_a6
		exx
		; ***********************************
		add (hl)	; a = x + 2*abs(z)*_a6
		inc hl
		ld (de),a
		inc de
		djnz copySymetrieZ
	ret