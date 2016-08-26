; Scenarii
; FKey Demo 2010

;**********************
; Scenario - idle
;**********************
.idle
	ret

;********************
; Scenario - Test END
;********************
forceNextScenario
	; reset all remainings framecount in scenarii table 
	ld hl,(frameCount+1)
	ld de,(testEnd+1)
	add hl,de
	ex de,hl		; de contains what should be added to remaining scenarii framecounts
	ld ix,(nextScenario+1)
	ld bc,4			; const
.loopForceScenario
	ld l,(ix)
	ld h,(ix+1)
	ld a,l
	or h			; reset carry flag
	jp z,forceNextScenario2	; if zero then we reached end of scenarii

	sbc hl,de
	ld (ix),l
	ld (ix+1),h
	add ix,bc
	jp loopForceScenario

.testEnd
	ld hl,-SCENARIO_STEP01		; first scenario should be hard coded
	add hl,de
	ret nc	; not finished

.forceNextScenario2
	push af
	; if the scenario is finished we should 
	; - set next call
	; - prepare the next test
	.nextScenario
	ld hl,scenarii+4
	ld de,testEnd+1
	ldi:ldi
	ld de,currentScenario+1
	ldi:ldi
	ld (nextScenario+1),hl	

	pop af

	ret

;**********************
; Scenario - delayStart
;**********************
;.delayStart
;	; end test
;	jp testEnd

;**********************
; Scenario - openScreen
;**********************
; in de=
; corrupt a,b,hl
.openScreen
	call testEnd
	ret c

	.sinOpen
	ld a,(sin_intro+128)
	ld b,a
	ld hl,sinOpen+1
	inc (hl)

	ld hl,delayIntro1-1
	ld (hl),a
	ret	

;*************************
; Scenario - setMode2Trick
;*************************
; in de=frameCount
; corrupt a,b,hl
.setMode2Trick
	halt

	; reset DMA
	ld hl,aylMultimode
	ld (DMA2ADD+1),hl	; set DMA2 to produce interrupts
	ld hl,DMACTL+1
	set 2,(hl)		; enable DMA2

	; swap interrupt vector
	ld hl,screen_on
	ld (intJMP+1),hl

	jp testEnd

; toggle witch's eyes
.toggleWitchEyes
	ld hl,blinkEyes
	ld a,&C9	; toggle between ret/nop
	xor (hl)
	ld (hl),a
	jp testEnd

;**********************
; Scenario - run3DBalls
;**********************
.run3DBalls
; end test
	; end test
	call testEnd
	jp c,endFirstBalls

.run3DBalls2
	; ***********************************************
	; The following code compute the new 2D positions
	; ***********************************************
	call change_object

	; copy original pos
	ld hl,morphobj
	ld bc,(nbBallsMul3)
	ld de,ballpos3D
	ldir

	.rotat_inc1
	ld a,0
	inc a
	ld (rotat_inc1+1),a
	ld (angleX+1),a

	.rotat_inc2
	ld hl,0
	ld bc,&C3
	add hl,bc
	ld (rotat_inc2+1),hl
	ld a,h
	ld (angleY+1),a

	.rotat_inc3
	ld hl,0
	ld bc,&131
	add hl,bc
	ld (rotat_inc3+1),hl
	ld a,h
	ld (angleZ+1),a

	;.rotation_inc
	;ld a,0
	;inc a
	;ld (rotation_inc+1),a

	; rotation
	ld h,sin/256
	;ld l,a

	; get all angles
	;ld d,32		; angle suivant X
	;ld b,(hl)	; angle suivant Y
	;ld c,(hl)	; angle suivant Z

.angleX
	ld l,0
	ld d,(hl)	; angle suivant X
.angleY
	ld l,0
	ld b,(hl)	; angle suivant Y
.angleZ
	ld l,0
	ld c,(hl)	; angle suivant Z
	
	call rotation	

	.speed_FX
	call speed_effect

	.symetrie
	call symetrie_centrale

	; project 3D to 2D position
	;call projection

	; sort balls using Z
	call sortZ

	; Translate object on screen & Save Positions
	call moveSave2D

	.enableSecondObject
	ret
	;*****************
	;* SECOND OBJECT *
	;*****************

	; direct memory update (4 balls)
	; second object
	ld hl,NB_BALLS_SECOND
	ld (nbBalls),hl
	ld hl,NB_BALLS_SECOND*3
	ld (nbBallsMul3),hl
	ld hl,NBCRD*NB_BALLS_SECOND + ballpos3D_second - 1
	ld (crdEndNbBalls),hl
	ld a,ballpos3D_second/256
	ld (saveBall2D+1),a
	ld hl,ballpos2Dend_second
	ld (saveBall2D_SP+1),hl
	ld hl,0 : org $-2 : ld a,64+128
	ld (movY3D),hl	

	; invert X
	ld hl,0 : org $-2 : neg
	ld (invertX),hl

	call change_object2

	; copy original pos
	ld hl,morphObj2
	ld bc,(nbBallsMul3)
	ld de,ballpos3D_second
	ldir


	.rotation_inc2
	ld a,0
	inc a
	ld (rotation_inc2+1),a

	; rotation
	ld h,sin/256
	ld l,a

	; get all angles
	ld d,(hl)	; angle suivant X
	ld b,32		; angle suivant Y
	ld c,(hl)	; angle suivant Z

	call rotation	

	; speed effect
	;call speed_effect

	; Symetrie centrale
	ld hl,(crdEndNbBalls)
	ld de,NBCRD*NB_BALLS_TOTAL + ballpos3D_second - 1
	ld a,(nbBalls)
	add -NB_BALLS_TOTAL
	neg
	ld b,a
	.symetrie2
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
		djnz symetrie2

	; Translate object on screen & Save Positions
	call moveSave2D

	; direct memory update (8 balls)
	; first object
	ld hl,NB_BALLS
	ld (nbBalls),hl
	ld hl,NB_BALLS*3
	ld (nbBallsMul3),hl
	ld hl,NBCRD*NB_BALLS + ballpos3D - 1
	;ld hl,NBCRD*NB_BALLS_SECOND + ballpos3D - 1
	ld (crdEndNbBalls),hl
	ld a,ballpos3D/256
	ld (saveBall2D+1),a
	ld hl,ballpos2Dend
	ld (saveBall2D_SP+1),hl
	ld hl,0 : org $-2 : ld a,60
	ld (movY3D),hl

	; invert X
	ld hl,0
	ld (invertX),hl

	;*********************
	;* END SECOND OBJECT *
	;*********************
	
	ret

.endFirstBalls
	; we reach the end of this scenario
	; - remove call
	; - exit
	;ld a,&C9			; disable 3D display
	;ld (display3D),a
	;ld (display3D_second),a

	; set null object
	ld hl,0 : org $-2 : ld a,OBJECTS_NUMBER
	ld (forceObject),hl
	ld (forceObject2),hl

	; reset timing (morphing next frame)
	ld hl,-1
	ld (object_timing),hl
	ld (object_timing2),hl

	; enable morphing
	xor a
	ld (morphing),a
	ld (morphing2),a

	; save last X position
	ld a,&21		; ld hl,xx
	ld (movX3D),a
	ld hl,(XTRANS+1)
	ld (movX3D+1),hl	; ld hl,(movX3D+1)
	ret	


;**********************
; Scenario - end3DBalls
;**********************
.collapse3DBalls
	; end test
	call testEnd
	jp c,endcollapse3DBalls

	; move to &300 (hidden)
	ld hl,(movX3D+1)
	ld bc,11
	add hl,bc

	ld a,&3
	cp h
	jp nz,nofixTo300

	ld hl,&300
	.nofixTo300
	ld (movX3D+1),hl
	jp run3DBalls2

.endcollapse3DBalls
	; we reach the end of this scenario
	; - remove call
	; - exit
	ld a,&C9			; disable 3D display
	ld (display3D),a
	ld (display3D_second),a
	ret	

;**********************
; Scenario - multiSprite
;**********************
	dw setMultiSprite1
	dw setMultiSprite2
	dw setMultiSprite3
; in de=frameCount
; corrupt a,b,hl
.initMulti
	; end test
	call testEnd
	jr c,endSetMulti

	.nextMulti
	ld hl,initMulti-6
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (nextMulti+1),hl
	ex hl,de

	halt
	jp (hl)
	ret

.endSetMulti
	; reset initVector
	ld hl,initMulti-6
	ld (nextMulti+1),hl

	call bg_compute
	jp setMultiSprite4
	;ret	

;**********************
; Scenario - drawMulti
;**********************
; in de=frameCount
; corrupt hl,a
.drawMulti
	; end test
	call testEnd
	jr c,endDrawMulti

	.dMOffset
	ld hl,&6004

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	ld (hl),ZOOM_MULTI

	; Deconnect Asic
	ld bc,&7FA0
	out (c),c

	ld a,8
	add l
	ld (dMOffset+1),a	; save next Offset

	; compute X position
	call bg_compute

	ret

.endDrawMulti
	; reset zoom init offset
	ld a,&04
	ld (dMOffset+1),a

	; compute X position
	call bg_compute
	ret	

;**********************
; Scenario - vanishMulti
;**********************
; in de=frameCount
; corrupt hl,a
.vanishMulti
	; end test
	call testEnd
	jr c,endFirstMulti;endVanishMulti

	.dMOffsetVanish
	ld hl,&6004

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	ld (hl),0

	; Deconnect Asic
	ld bc,&7FA0
	out (c),c

	ld a,8
	add l
	ld (dMOffsetVanish+1),a	; save next Offset

	; compute X position
	call bg_compute

	ret

;.endVanishMulti
;	; reset zoom init offset
;	ld a,&04
;	ld (dMOffsetVanish+1),a
;
;	; compute X position
;	call bg_compute
;
;	ret	

;***********************
; Scenario - runMulti
;***********************
.runMulti
	; end test
	call testEnd
	jp c,bg_compute
	;jr c,endFirstMulti

	; compute X position
	call bg_compute

	ret

.endFirstMulti
	; reset zoom init offset
	ld a,&04
	ld (dMOffsetVanish+1),a

	; disable RASTER interrupts
	ld hl,intVoid
	ld (intvect+4),hl
	ld (intvect+6),hl

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	; set PRI to END of screen
	ld hl,&6800	
	ld (hl),&FF

	; deco asic
	call deco_asic

	; change second multi text sprite
	; and prepare 3rd one
	.nextMultiSprite
	ld hl,txtSprite2_end-1-4
	ld (txtSpriteOffset+1),hl
	ld hl,txtSprite3_end-1-4
	ld (nextMultiSprite+1),hl
	ret

;*******************
; Scenario - set3DA
;*******************
; in de=frameCount
; corrupt a,b,hl
	dw set3DSpriteA1
	dw set3DSpriteA2
	dw set3DSpriteA3
.set3DA
	; end test
	call testEnd
	jr c,endSet3DA

	.nextSet3DA
	ld hl,set3DA-6
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (nextSet3DA+1),hl
	ex hl,de

	halt
	jp (hl)
	ret

.endSet3DA
	; we reach the end of this scenario
	; - enable display3D
	; - Reset scenario length

	; reset initVector
	ld hl,set3DA-6
	ld (nextSet3DA+1),hl

	; enable display3D 1
	xor a
	ld (display3D),a

	; enable speed effect
	ld (speed_effect),a

	; enable Z sorting (transparent)
	ld (sortZ),a

	; Fix Y coords to 60 (top)
	ld hl,0 : org $-2 : ld a,b : nop
	ld (movY3D),hl

	; disable second object rotation
	ld a,0 : org $-1 : ret
	ld (enableSecondObject),a

	; disable second display3D
	ld (display3D_second),a

	; enable morphing between objects
	ld hl,0
	ld (forceObject),hl

	; reset timing (morphing next frame)
	ld hl,-1
	ld (object_timing),hl

	; enable morphing
	xor a
	ld (morphing),a

	; reset X scroll
	ld a,0 : org $-1 : ex hl,de	;HL<-X
	ld (movX3D),a
	ld hl,0				; nop;nop
	ld (movX3D+1),hl

	; enable change in symetrie and speedFX
	xor a
	ld(enableSym3DFx),a

	ret

;*******************
; Scenario - set3DB
;*******************
; in de=frameCount
; corrupt a,b,hl
	dw set3DSpriteB1
	dw set3DSpriteB2
	dw set3DSpriteB3
.set3DB
	; end test
	call testEnd
	jr c,endSet3DB

	.nextSet3DB
	ld hl,set3DB-6
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (nextSet3DB+1),hl
	ex hl,de

	halt
	jp (hl)
	ret

.endSet3DB
	; we reach the end of this scenario
	; - enable display3D
	; - Reset scenario length

	; reset initVector
	ld hl,set3DB-6
	ld (nextSet3DB+1),hl

	; enable display3D
	xor a
	ld (display3D),a
	ld (display3D_second),a

	; disable speed effect
	ld a,0 : org $-1: ret
	ld (speed_effect),a

	; Fix Y coords to 60 (top)
	ld hl,0 : org $-2 : ld a,60
	ld (movY3D),hl

	; disable Z sorting (transparent)
	ld hl,sortZ
	ld (hl),0 : org $-1 : ret

	; enable second object rotation
	xor a
	ld (enableSecondObject),a

	; enable morphing between objects
	ld hl,0
	ld (forceObject),hl
	ld (forceObject2),hl

	; reset timing (morphing next frame)
	ld hl,-1
	ld (object_timing),hl
	ld (object_timing2),hl

	; enable morphing
	xor a
	ld (morphing),a
	ld (morphing2),a

	; reset X scroll
	ld a,0 : org $-1 : ex hl,de	;HL<-X
	ld (movX3D),a
	ld hl,0				; nop;nop
	ld (movX3D+1),hl

	; force symetrie centrale for object 1
	; and disable change in symetrie and speedFX
	ld hl,symetrie_centrale
	ld (symetrie+1),hl
	ld a,0:org $-1:ret
	ld(enableSym3DFx),a

	ret

;*******************
; Scenario - set3DC
;*******************
; in de=frameCount
; corrupt a,b,hl
	dw set3DSpriteC1
	dw set3DSpriteC2
	dw set3DSpriteC3
	dw set3DSpriteC4
.set3DC
	; end test
	call testEnd
	jr c,endSet3DC

	.nextSet3DC
	ld hl,set3DC-8
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (nextSet3DC+1),hl
	ex hl,de

	; call (hl) with asic locked 
	call HL_WithAsicLocked
	
	ret

.endSet3DC
	; Replace sphere2D by sphere3D
	ld hl,sphere3D
	ld (0*5+obj_seq),hl

	; set symetrie centrale
	ld hl,symetrie_centrale	
	ld (0*5+obj_seq+2),hl

	; reset speed FX
	xor a
	ld (0*5+obj_seq+4),a

	jp endSet3DA

;***********************
; Scenario - initrotoZoom
;***********************
.initrotoZoom
	; end test
	halt
	call setRotoZoom

	ld a,&C9			; disable 3D display
	ld (display3D),a

	jp testEnd

;***********************
; Scenario - rotoZoom
;***********************
.rotoZoom
	; end test
	call testEnd
	jr c,endrotoZoom
	jp displayRotoZoom

.endrotoZoom
	; wait to be in the bottom part of the screen (mode 2)
	halt:halt:halt

	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; hide sprites (brutally)
	call hideAllSprites

	; reset DMA
	ld hl,aylMultiModeIntro
	ld (DMA2ADD+1),hl	; set DMA2 to produce interrupts
	ld hl,DMACTL+1
	set 2,(hl)		; enable DMA2

	; Deconnect Asic
	ld bc,&7FA0
	out (c),c

	; swap interrupt vector
	ld hl,screen_intro
	ld (intJMP+1),hl

	ret


;***********************
; Scenario - initBMP
;***********************
dw setBumpMap0
dw setBumpMap1
dw setBumpMap2
.initBMP
	push de
	.nextInitBMP
	ld hl,initBMP-6
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (nextInitBMP+1),hl
	ex hl,de

	; call (hl) with asic locked 
	call HL_WithAsicLocked
	
	pop de
	jp testEnd

;***********************
; Scenario - MuteSound
;***********************
.runMuteSound
	; end test
	call testEnd
	jr c,endRunMuteSound
	; carry not set ...

	ld a,256
	dec a
	ld ($-2),a
	rra
	rra
	rra
	rra
	and &F

	; to mute music we need to :
	; 1 - mute psg volumes (reg 8,9,A)
	; 2 - mute sid voices playing
	ld hl,reg8
	cp (hl)
	jp P,$+3+1
	ld (hl),a
	ld hl,reg9
	cp (hl)
	jp P,$+3+1
	ld (hl),a
	ld hl,regA
	cp (hl)
	jp P,$+3+1
	ld (hl),a
	
	ld (shallWeMuteSid+2),a	; set sid mute value
	xor a
	ld (shallWeMuteSid),a	; enable sid mute

	ret

.endRunMuteSound
	; *****
	; reset
	; *****

	; stop music decoding
	ld a,0:org $-1:ret
	ld (player+3),a

	; disable all DMAs' but the one for the screen Mode 0/2 Trick
	; in the sound player code
	ld a,%100
	ld (DMACTL+1),a

	di

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	; disable DMA
	ld hl,DMACONTROL	; [3]
	ld a,%11111100		; [2] 
	and (hl)		; [2]
	ld (hl),a		; [2]

	call waitOneHBL
	call waitOneHBL
	call waitOneHBL

	; reset PSG using AYL on DMA0
	ld hl,resetPSGAYL
	ld (DMACHANNEL0),hl

	; Set DMA0 (reset PSG values)
	ld hl,DMACONTROL
	set 0,(hl)

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ei
	ret

;.playOnlyDMA2
;	; Connect Asic
;	ld bc,&7FB8		; [3]
;	out (c),c		; [4]
;
;	ld hl,aylMultiMode	; [3]
;	ld (DMACHANNEL2),hl	; [5]
;
;	ld hl,DMACONTROL	; [3]
;	set 2,(hl)
;
;	; Deconnect Asic
;	ld bc,&7FA0		; [3]
;	out (c),c		; [4]
;
;	ret

; Used to synchro with HBL
; Legal offsets 0 = [64] to 56 = [8] 
.waitOneHBL
	ds 64-5-3,0
	ret		; [3]

.resetPSGAYL
align 2
dw &0000
dw &0100
dw &0200
dw &0300
dw &0400
dw &0500
dw &0600
dw &073F
dw &0800
dw &0900
dw &0A00
dw &0B00
dw &0C00
dw &0D00
dw &4020

;***********************
; Scenario - runBMPScrollDownEnabled
;***********************
.runBMPScrollDownEnabled
	ld a,0 : org $ - 1 : inc a
	ld (BMP_SCROLL_DOWN),a
	jp runBMP

;***********************
; Scenario - runBMPScrollLeftEnabled
;***********************
.runBMPScrollLeftEnabled
	ld hl,0
	ld (setBMPPosition00),hl
	jp runBMP

;***********************
; Scenario - runBMPOndulXEnabled
;***********************
.runBMPOndulXEnabled
	ld a,0 : org $ - 1 : add 127 : org $ - 1
	ld (enableOndulX),a
	jp runBMP

;***********************
; Scenario - runBMP
;***********************
.lastRunBMP
	ld a,0 : org $ - 1 : ret nc
	ld (endRunBMP),a	; enable endRunBMP
	ld hl,runBMP
	ld (currentScenario+1),hl
.runBMP
	push de
	ld ixl,BMP_YPOS_BASE
	ld ixh,2*&20 + BMP_YPOS_BASE
	ld iyl,2*&20 + BMP_YPOS_BASE - 1

	;jp displayBumpMap
	call displayBumpMap

	; lock asic
; 	ld hl,deco_asic		; [3]
; 	ld (hl),&C9		; [3]

	; connect asic
; 	ld bc,&7FB8		; [3]
; 	out (c),c		; [4]

	; Debug Using Sprite 0 ;)
;	ld hl,40
;	ld (&6000),hl
;	ld (&6002),hl
;	ld a,%1111
;	ld (&6004),a

	; sprite 1
;	ld a,(intDLRastersCall2+1)		; N,J
;	ld de,&4000
;	call print8bits

	; sprite 6
;	ld a,(6*8 + &6002)		; N,J
;	ld de,&4008
;	call print8bits

	; sprite 11
;	ld a,(11*8 + &6002)		; N,J
;	ld de,&4060
;	call print8bits

;	ld a,(BMP_ENABLE_ONDULX+2)
;	ld de,&4068
;	call print8bits

	; **********
	; * OndulX *
	; **********
	.sinOndX
	ld a,(sin)
	ld hl,sinOndX+1
	inc (hl)
	
.enableOndulX
	ld a,127
	;add 127	; >0

	rlca
	rlca
	rlca
	;rlca
	ld e,a	; save a
	and %11111000	
	ld (decimalOndulX+1),a

	ld a,e
	and %00000111
	ld (integerOndulX+1),a
	
	; deconnect asic
; 	ld bc,&7FA0		; [3]
; 	out (c),c		; [4]

	; delock asic
;	xor a			; [1]
; 	ld (deco_asic),a	; [4]

	; exit if space is pressed
;	call getKey

;	.oldKbState
;	ld b,&FF		; nothing pressed
;	ld (oldKbState+1),a	; save new kb state

	; inc counter only if bit7=0 and old_bit7=1
;	cpl
;	and b

;	bit 7,a			; pause
;	jp z,no_pause

;	ld a,(frameCount+3)
;	or a
;	jr z,go_on
;		ld hl,frameCount+3
;		ld (hl),0
;		jr no_pause
;.go_on
;		ld hl,frameCount+3
;		ld (hl),0:;org $-1:;inc de
;.no_pause

;	bit 6,a		; n
;	jp z,no_inc

	; increase counter
;	ld hl,sinBmpY+1
;	inc (hl)
;.no_inc

;	bit 5,a		; j
;	jp z,no_res

	; decrease counter
;	ld hl,sinBmpY+1
;	ld (hl),&2F

;.no_res
;	bit 4,a		; h
;	jp z,no_inc_delay

	; increase delay
;	ld hl,delayBMP+1
;	inc (hl)

;.no_inc_delay
;	bit 3,a		; y
;	jp z,no_dec_delay

	; decrease delay
;	ld hl,delayBMP+1
;	dec (hl)

;.no_dec_delay
;	bit 2,a		; u
;	jp z,no_scrollX

	; scroll X
	;ld hl,(setBMPPosition00)
	;ld (debug_scrollX+1),hl			; save jump
	;ld hl,0
	;ld (setBMPPosition00),hl		; nop jump

;.no_scrollX

;	bit 1,a		; 7
;	jp z,no_inc_sinX

	; increase delay
;	ld hl,BMP_ENABLE_ONDULX+2
	;inc (hl)

;.no_inc_sinX

;	bit 0,a		; 8
;	jp z,no_dec_sinX

	; increase delay
;	ld hl,BMP_ENABLE_ONDULX+2
	;dec (hl)
;.no_dec_sinX

	pop de

	; end test
	call testEnd
.endRunBMP
	ret
	;ret nc		; return if not finished

	; *****
	; reset
	; *****

	ld hl,initBMP-6
	ld (nextInitBMP+1),hl

	; disable RASTER interrupts
	ld hl,intVoid
	ld (intvect+4),hl
	ld (intvect+6),hl

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	; set PRI to END of screen
	ld hl,&6800	
	ld (hl),&FF

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

;	di ; disable interrupt
;
;	; Deconnect Asic
;	;ld bc,&7FA0		; [3]
;	;out (c),c		; [4]
;
;	call lock_asic
;
;	; set memory bank to  b0;b1;b2;b3
;	ld bc,&7FC0	
;	out (c),c
;
;	; set screen &C000
;	ld bc,&BC00 + 12
;	out (c),c
;	ld bc,&BD00 + %110000
;	out (c),c
;
;	ld hl,reset_code
;	ld de,&4000
;	ld bc,end_reset_code-reset_code
;	ldir
;	jp &4000
;
	ret
;
;.reset_code
;ld bc,&7F00+%10010000	; mode 0 + rom enabled
;out (c),c
;rst 0
;.end_reset_code

;*************************
; Scenario - showEndScreen
;*************************
.showEndScreen
	call testEnd
	jr c,endShowEndScreen

	push de
	di
	; stop witch's eyes
	ld a,0 : org $-1 : ret
	ld (blinkEyes),a

	; set memory bank to  b0;b1;b2;03
	ld bc,&7FC1	
	out (c),c

	; copy screen from bank3 to bank1
	.orig_ldir
	ld hl,&C000
	.dest_ldir
	ld de,&4000
	
	; copy screen
	; LDIR is 6 nop (312*64/6 = 3328 = 0xD00)
	; We can  target 0x800 LDIR/frames ~ 8 frames
	ld bc,&4000/32
	ldir

	; save orig/dest
	ld (orig_ldir+1),hl
	ld (dest_ldir+1),de

	; INIT endWriter DATA
	; *******************
	; set memory bank to  b0;02;b2;b3
	ld bc,&7FC6	
	out (c),c
	
	; copy screen from bank3 to bank1
	.orig_ldir2
	ld hl,&4000
	.dest_ldir2
	ld de,&C000
	
	; copy DATA
	; Little hack here, we save 2 bytes for the Stack which gives us a max stack of 32 values
	ld bc,&4000/32 - 2
	ldir

	; save orig/dest
	ld (orig_ldir2+1),hl
	ld (dest_ldir2+1),de

	; reset memory bank to  b0;b1;b2;b3	
	ld bc,&7FC0	
	out (c),c

	; end test
	ei
	pop de
	ret

.endShowEndScreen
	; Screen palette (16+border)

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	ld bc,32 ; 16*2
	ld hl,pal2
	ld de,&6400
	ldir

	; deco asic
	call deco_asic

	ld hl,(pal2)
	ld (ink0_offset+1),hl
	ld hl,(pal2+2)
	ld (ink1_offset+1),hl
	ld (ink1_offset_intro+1),hl

	ret

;*****************************
; Scenario - Witch Beauty wink
;*****************************
.setBeautyWink
	call initBeautyWink
	jp testEnd

;*********************
; Scenario - EndWriter
;*********************
.setEndWriter
	call testEnd
	jp c,initEndWriter

	halt	; wait to be in visible area

	; clear all sprites
	; ***********************	
	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; lock asic
 	ld a,&C9
 	ld (deco_asic),a

	; clear sprites using sprite
	ld (saveSPClearSPrite+1),sp
	
	di
	ld sp,16*&100+&4000
	ld hl,0
	ld b,&100/2
	.loopClearSprite
		repeat 16
		push hl
		rend
		djnz loopClearSprite
	

	.saveSPClearSPrite	
	ld sp,0
	ei

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a

	ret
	

;*********************
; Scenario - RunWriter
;*********************
.runEndWriter
	call testEnd
	ret c
	
	;.callEndWriter
	jp moveEndWriter

.hideAllSprites
	ld hl,0
	ld (&6004),hl
	ld (&600C),hl
	ld (&6014),hl
	ld (&601C),hl
	ld (&6024),hl
	ld (&602C),hl
	ld (&6034),hl
	ld (&603C),hl
	ld (&6044),hl
	ld (&604C),hl
	ld (&6054),hl
	ld (&605C),hl
	ld (&6064),hl
	ld (&606C),hl
	ld (&6074),hl
	ld (&607C),hl

	ld hl,&300
	ld (&6000),hl
	ld (&6008),hl
	ld (&6010),hl
	ld (&6018),hl
	ld (&6020),hl
	ld (&6028),hl
	ld (&6030),hl
	ld (&6038),hl
	ld (&6040),hl
	ld (&6048),hl
	ld (&6050),hl
	ld (&6058),hl
	ld (&6060),hl
	ld (&6068),hl
	ld (&6070),hl
	ld (&6078),hl

	ret

.goodbye
	di ; disable interrupt

	; Connect Asic
	ld bc,&7FB8
	out (c),c

	; disable PRI
	; Disable Split Screen
	xor a
	ld (&6800),a
	ld (&6801),a

	; disable DMA
	ld hl,DMACONTROL	; [3]
	ld a,%11111000		; [2] 
	and (hl)		; [2]
	ld (hl),a		; [2]

	; clear sprites
	xor a
	ld hl,&4000
	ld b,0
	.loopClrSprite
		repeat 16
			ld (hl),a
			inc hl
		rend
		djnz loopClrSprite

	; clear sprite palette
	ld hl,&6422
	ld b,15*2
	.loopClrPalette
		ld (hl),a
		inc hl
		djnz loopClrPalette

	; sprite pos + zoom reset
	ld hl,&6000
	ld de,&6001
	ld (hl),a
	ld bc,16*8-1
	ldir
	

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; set memory bank to  b0;b1;b2;b3
	ld bc,&7FC0	
	out (c),c

	; set screen &C000
	ld bc,&BC00 + 12
	out (c),c
	ld bc,&BD00 + %00110000	; &C000 (no overscan)
	out (c),c

	ld hl,reset_code
	ld de,&8000
	ld bc,end_reset_code-reset_code
	ldir
	call lock_asic
	jp &8000

; If ASIC not locked = BUG
; block at reset &0658 


	;ret

.reset_code
ld bc,&7F00+%10010000	; mode 0 + rom enabled
out (c),c
jp 0
.end_reset_code

; Keyboard test
;.getKey
;	LD   BC,#F782   ; A Sortie/C Sortie
;	OUT  (C),C
;	LD   BC,#F40E   ; Valeur 14 sur port A
;	OUT  (C),C
;	LD   BC,#F6C0   ; C'est un index (R14)
;	OUT  (C),C
;	;LD   BC,#F600   ; On valide!
;	XOR  A
;	OUT  (C),A
;	LD   BC,#F792   ; A Entree/C Sortie
;	OUT  (C),C
;	LD   BC,#F645	; Ligne clavier numero 5
;	OUT  (C),C
;	LD   B,#F4      ; On lit le port A (PPI)
;	IN   A,(C)      ; Donc R14 (A du AY)
;	LD   BC,#F782   ; A en Sortie
;	OUT  (C),C
;	LD   BC,#F600   ; Reset PPI Write
;	OUT  (C),C
;	;BIT  7,A
;	;JP NZ,VBL
;	ret

; ********
; Scenarii
; ********
.scenarii
	; INIT
	dw -SCENARIO_STEP01:dw testEnd
	dw -SCENARIO_STEP02:dw openScreen
	dw -SCENARIO_STEP02_01:dw setMode2Trick
	dw -SCENARIO_STEP02_02:dw toggleWitchEyes
	dw -SCENARIO_STEP02_03:dw testEnd

	;dw -SCENARIO_STEP02_0C:;dw runMuteSound

	; First Multiballs (Spells Ready ?)
	dw -SCENARIO_STEP02_04:dw initMulti
	dw -SCENARIO_STEP02_04_01:dw drawMulti
	dw -SCENARIO_STEP02_05:dw runMulti
	dw -SCENARIO_STEP02_08:dw vanishMulti

	; BitMap
	dw -SCENARIO_STEP02_0A:dw initBMP
	dw -SCENARIO_STEP02_0B:dw runBMP
	dw -SCENARIO_STEP02_0C:dw runBMPOndulXEnabled
	dw -SCENARIO_STEP02_0D:dw runBMP
	dw -SCENARIO_STEP02_0E:dw runBMPScrollDownEnabled
	dw -SCENARIO_STEP02_0F:dw runBMP
	dw -SCENARIO_STEP02_10:dw runBMPScrollLeftEnabled
	dw -SCENARIO_STEP02_80:dw lastRunBMP

	; First 3D Balls
	dw -SCENARIO_STEP02_FF:dw set3DA
	dw -SCENARIO_STEP03:dw run3DBalls
	dw -SCENARIO_STEP03_01:dw collapse3DBalls

	; Second Multi Balls (Graphics by Ced)
	dw -SCENARIO_STEP04:dw initMulti
	dw -SCENARIO_STEP04_01:dw drawMulti
	dw -SCENARIO_STEP05:dw runMulti
	dw -SCENARIO_STEP06:dw vanishMulti
	
	;dw -SCENARIO_STEP06:;dw set3DB
	;dw -SCENARIO_STEP07:;dw run3DBalls
	;dw -SCENARIO_STEP07_01:;dw collapse3DBalls
	
	; Third Multi Balls (Z80 Code by FKey)
	dw -SCENARIO_STEP08:dw initMulti
	dw -SCENARIO_STEP08_01:dw drawMulti
	dw -SCENARIO_STEP09:dw runMulti
	dw -SCENARIO_STEP09_01:dw vanishMulti

	; Second 3D Balls (clor deep effect)
	dw -SCENARIO_STEP09_01_05:dw set3DC
	dw -SCENARIO_STEP09_02:dw run3DBalls
	dw -SCENARIO_STEP09_03:dw collapse3DBalls
	
	; RotoZoom FX
	dw -SCENARIO_STEP10:dw initrotoZoom
	dw -SCENARIO_STEP11:dw rotoZoom

	; background transition
	dw -SCENARIO_STEP11_01:dw toggleWitchEyes
	dw -SCENARIO_STEP11_02:dw openScreen
	dw -SCENARIO_STEP12:dw showEndScreen
	dw -SCENARIO_STEP12_01:dw openScreen
	dw -SCENARIO_STEP12_02:dw setMode2Trick
	dw -SCENARIO_STEP12_02_01:dw setBeautyWink

	; End Writer
	dw -SCENARIO_STEP12_03:dw setEndWriter
	dw -SCENARIO_STEP12_04:dw runEndWriter
	dw -SCENARIO_STEP13:dw runMuteSound
	dw -SCENARIO_STEP14:dw testEnd
	dw 0:dw goodbye