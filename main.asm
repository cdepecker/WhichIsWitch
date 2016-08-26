nolist
; Demo 2010
; FKey/RevivaL 2010
; Bugs
; + BUG Witch eyes (need to be recomputed) OK
; + EndWriter blink at end (JPaul) 	OK
; + double 3D unvisible on MONO		OK
; + 3D deep efect unvisible on MONO	OK
; + 3D deep effect should not feature 2D star OK
; + We need to enable/disable speedFX visibility when we start or change an Object OK
; + 2 objects flickering screen (Force symetrie centrale and speedFX off) OK
; + red dot line visible in screen_ON function TO BE TESTED
; + second multi sprites flickering screen
; + Y Scroll MultiLine ? (DISABLED)
; + Include LOADER
; - Artefact in beauty witch hat (During "FRED" Scrolling) OK
; - test Loud tune
; + No need to use 16 Y pos for multisprites (same line for a entire row)
;--> Changed multi2.asm (and moved in last bank of main memory)
;--> added pal change
; ASIC BUG ????
;	PRI Interrupt seems to be triggered twice (beginning of line, end of line)
;	And the second one doesn't seem to be ignored (must be trapped ?)
;	Solution = Reset the Interrupt Counter Register (the one that roll over 52)

; BMP = ld (?),hl for scrollX can lead to glitches visible to the left because 
; presumably the LSB is written to memory first and is picked up by the ray during tracing
; possible solutions 
; - use zoom to hide the particular sprite that could have its position set during this moment
;#6078-6079 BUG in witch hat (&1DD9)

;#0325 (artefact during BMAP) Line &0F/7
;==> Found it !!! (When changing X from &FF10 to a pos value i.e. &01xx)

INCLUDE_EW		EQU	1
WITCH_EYE_ENABLED	EQU	1	
BUG_PRI			EQU	1

; ***********
; PROGRAM MAP
; ***********
; #0000 - #18FF Tune deflate (buffers)						MANDATORY
; #1900 - #1B37 SID Replay (AYL)						MANDATORY
; #1B38 - #3F2D Main code + scenarii + witch eyes + EW Code + 3D + multi
; #4000 - #7FDF Background screen (old witch)
; #8000 - #895C AYC Player
; #895D - #B27B Tune + SID Data					MANDATORY
; #B27C - #???? Rotozoom + demodata						OPTIONAL

; 2nd Bank
; #8000 - #BFFF End Writer Data (assembled in &C000)
; #C000 - #FFFF Background screen (young witch)

; **********
; * MACROS *
; **********
; translate a into de
macro A_TO_DE
	ld	e,a
	rlca		; carry hold sign
	sbc	a 	; 0x00 or 0xFF
	ld	d,a
endm

; translate a into hl
macro A_to_HL
	ld	l,a
	rlca		; carry hold sign
	sbc	a 	; 0x00 or 0xFF
	ld	h,a
endm

macro GA_COLOR x,y
	push bc
	LD BC,&7F00 + x
       	OUT (C),C
       	LD C,y+64
       	OUT (C),C
	pop bc
endm

; ****************
; * DEBUG Consts *
; ****************
DEBUG			EQU 1

; ***************
; * Main Consts *
; ***************
BACKGROUND_OFFSET	EQU &4000
CODE_OFFSET1		EQU &1B38;&2F00

; **************
; * AYC Consts *
; **************
PLAYER_OFFSET		EQU &8000
DECRUBUF		EQU &0000;&1000
;AYC_OFFSET		EQU &C000	; not used
;DEMO_DATA		EQU &FFFF	; not used

; Max Rate for Sid voice (help to tune space)
; 0 = disabled, should be > 61hz and < 7.8khz
SIDB_MAXRATE		EQU 0
SIDA_MAXRATE		EQU 1241
SIDC_MAXRATE		EQU 2100
SIDOFFSET		EQU &1900;&0

; ****************
; * Balls Consts *
; ****************
ballsprite1		EQU ball1
ballspriteTrans		EQU ball2
SPRITE_TRANSPARENT	EQU 0
SYMETRIE_CENTER		EQU 0
SYMETRIE_PLAN		EQU 1
NB_BALLS 		EQU 8	; nb balls computed (rotation) max 12 when only 1 sid playing
NB_BALLS_TOTAL		EQU 16	; nb balls displayed
NB_BALLS_SECOND		EQU 4
NBCRD			EQU 3	; x,y,z
NBS			EQU 6	; number balls saved during speed effect 
ZMAX			EQU 30
ZOOM_3D			EQU %1001
OBJ3D 			EQU nullObject

; *********************
; * Interrupts Consts *
; *********************
; NOT TO BE MODIFIED !!! 
; They are hardcoded
; we move the screen up one raster so that the screen that fits in &4000-&7FFF is displayed
; from the first line
RINT_VBL		EQU 1
RINT_SCREEN_ON		EQU 8+48-1	; 132-48= 84 = (01010100)2
RINT_BMP		EQU &50
RINT_SCREEN_MIDDLE	EQU 8+132-1
RINT_SCREEN_OFF		EQU 8+216-1 ; 216-132= 84 = (01010100)2

; ***********************
; * Sprite Multiplexing *
; *****************"******
SPRITE_LINE     EQU 37			; first sprite line Y offset (May be used to scroll Y)
RST_BFORE_LINE	EQU 6
;RINT_LINE	EQU SPRITE_LINE-5	; first raster line interrupt (should be != 0)
NBL		EQU 14;17-0			; Number of lines (Number of raster interrupts that draw sprites)
;NBL		EQU 5
XWIDTH          EQU 16
YWIDTH          EQU 14			; Min=13
SINX_DISTO	EQU 5			;(0=disabled, x=disto)
SINX_SPEED	EQU NBL*SINX_DISTO-1 	;(16=stop, 0=max)
SINY_SPEED	EQU 0			;(0=disabled)
ZOOM_MULTI      EQU %0101
;SIN             EQU &4000-&100
;OUT_C_0         EQU &71ED

; *******************
; * Rotozoom Consts *
; *******************
ZOOM_ROTO	EQU %1110
ROTO_XPOS	EQU &260
ROTO_YPOS	EQU &40
ROTO_COUNT	EQU &200/3+1

; ******************
; * BumpMap Consts *
; ******************
ZOOM_BMP	EQU %1110
BMP_XPOS_BASE	EQU &170;
BMP_YPOS_BASE	EQU &60

; *********************
; * END WRITER CONSTS *
; *********************
MONOFONTE	EQU 1
ZOOM_EW		EQU %1101
NB_LINE		EQU 16
NB_COLUMNS	EQU 9
NB_MASK		EQU 7
INTER_CHAR	EQU 8
INTER_LINE	EQU 10
EW_X_POS	EQU 110+4
EW_Y_POS	EQU 4
EW_SCROLL_SPEED	EQU 3		; Scroll Speed (min 1)
VANISH_TIME	EQU 25		; # frames for the msg to disapear
 
; ********
; Scenarii
; ********
START_SCENARIO		EQU 0

; INIT
SCENARIO_STEP01		EQU SCENARIO_STEP02 - &180	; (Sinus Start)
SCENARIO_STEP02		EQU &181			; Screen opens
;SCENARIO_STEP01		EQU SCENARIO_STEP02 - &10	; (Sinus Start)
;SCENARIO_STEP02		EQU &20				; Screen opens
SCENARIO_STEP02_01	EQU SCENARIO_STEP02+1		; Set Mode 2 Trick
SCENARIO_STEP02_02	EQU SCENARIO_STEP02_01+1	; Toggle witche's eyes
SCENARIO_STEP02_03	EQU SCENARIO_STEP02_02+2;&50	; Idle

; StopSound
;SCENARIO_STEP02_0C	EQU SCENARIO_STEP02_03+256	; StopSound

; First Multiballs (Spells Ready ?)
SCENARIO_STEP02_04	EQU SCENARIO_STEP02_03+4	; Init Multi
SCENARIO_STEP02_04_01	EQU SCENARIO_STEP02_04+16+1	; Draw Multi
SCENARIO_STEP02_05	EQU SCENARIO_STEP02_08-16-1	; run Multi balls
SCENARIO_STEP02_08	EQU &300			; Vanish Multi

; Bump Mapping
SCENARIO_STEP02_0A	EQU SCENARIO_STEP02_08+3	; Init Bump Mapping
SCENARIO_STEP02_0B	EQU SCENARIO_STEP02_80 - &301	; Run Bump Mapping
SCENARIO_STEP02_0C	EQU SCENARIO_STEP02_80 - &300	; Enable OndulX
SCENARIO_STEP02_0D	EQU SCENARIO_STEP02_80 - &201	; Run Bump Mapping
SCENARIO_STEP02_0E	EQU SCENARIO_STEP02_80 - &200	; Enable scroll down
SCENARIO_STEP02_0F	EQU SCENARIO_STEP02_80 - &101	; Run Bump Mapping
SCENARIO_STEP02_10	EQU SCENARIO_STEP02_80 - &100	; Enable scroll left
SCENARIO_STEP02_80	EQU &780			; Run Bump Mapping

; First 3D Balls
SCENARIO_STEP02_FF	EQU SCENARIO_STEP02_80+4	; Init 3D
SCENARIO_STEP03		EQU SCENARIO_STEP03_01-70	; run 3D balls
;SCENARIO_STEP03_01	EQU &C00			; collapse 3D balls
SCENARIO_STEP03_01	EQU &1080 -16 -1 -&C0 -16 -1 -4 -16 -1 - &C0 -16 -1 -4

; Second Multi Balls (Graphics by Ced)
;SCENARIO_STEP04		EQU SCENARIO_STEP03_01+4	; Init Multi
;SCENARIO_STEP04_01	EQU SCENARIO_STEP04+16+1	; Draw Multi
;SCENARIO_STEP05		EQU SCENARIO_STEP04_01+&100	; run Multi balls
;SCENARIO_STEP06		EQU SCENARIO_STEP05+16+1	; Vanish Multi

; Third Multi Balls (Z80 Code by FKey)
;SCENARIO_STEP08		EQU SCENARIO_STEP06+4		; Init Multi
;SCENARIO_STEP08_01	EQU SCENARIO_STEP08+16+1	; Draw Multi
;SCENARIO_STEP09		EQU SCENARIO_STEP08_01+&C0	; Run Multi balls
;SCENARIO_STEP09_01	EQU SCENARIO_STEP09+16+1	; Vanish Multi

SCENARIO_STEP04		EQU &1080 -16 -1 -&C0 -16 -1 -4 -16 -1 - &C0 -16 -1	; Init Multi
SCENARIO_STEP04_01	EQU &1080 -16 -1 -&C0 -16 -1 -4 -16 -1 - &C0		; Draw Multi
SCENARIO_STEP05		EQU &1080 -16 -1 -&C0 -16 -1 -4 -16 -1			; run Multi balls
SCENARIO_STEP06		EQU &1080 -16 -1 -&C0 -16 -1 -4				; Vanish Multi

SCENARIO_STEP08		EQU &1080 -16 -1 -&C0 -16 -1				; Init Multi
SCENARIO_STEP08_01	EQU &1080 -16 -1 -&C0					; Draw Multi
SCENARIO_STEP09		EQU &1080 -16 -1						; Run Multi balls
SCENARIO_STEP09_01	EQU &1080						; Vanish Multi


; Second 3D Balls (color deep effect)
SCENARIO_STEP09_01_05	EQU SCENARIO_STEP09_01+5	; Init 3D
SCENARIO_STEP09_02	EQU SCENARIO_STEP09_01_05+&700	; run 3D balls
SCENARIO_STEP09_03	EQU SCENARIO_STEP09_02+70	; collapse 3D balls

; RotoZoom FX
SCENARIO_STEP10		EQU SCENARIO_STEP09_03+1	; Set RotoZoom
SCENARIO_STEP11		EQU ROTO_COUNT*7+1+SCENARIO_STEP10	; Rotozoom

; background transition
SCENARIO_STEP11_01	EQU SCENARIO_STEP11+1		; Toggle witche's eyes
SCENARIO_STEP11_02	EQU SCENARIO_STEP11_01+&80	; Screen close
SCENARIO_STEP12		EQU SCENARIO_STEP11_02+32+1	; Set Last screen
SCENARIO_STEP12_01	EQU SCENARIO_STEP12+&80		; Screen open
SCENARIO_STEP12_02	EQU SCENARIO_STEP12_01+1	; Mode 2 Trick
SCENARIO_STEP12_02_01	EQU SCENARIO_STEP12_02+1	; Set Beauty Wink

; End Writer
SCENARIO_STEP12_03	EQU SCENARIO_STEP12_02_01+2	; Set End Writer
SCENARIO_STEP12_04	EQU SCENARIO_STEP12_03+&1a00	; Run End Writer

; StopSound
SCENARIO_STEP13		EQU SCENARIO_STEP12_04+256	; StopSound

; Idle till reset
SCENARIO_STEP14		EQU SCENARIO_STEP13+&40		; End

nolist

; - clear memory
; - ret in &38
org &0 : ds &10000,0
org &38 : ret

; BANK 1
; ======
write direct -1,-1,&C2		; 00;01;02;03

org &C000,&8000

; End Writer Data
if INCLUDE_EW
let currOffset = $
read "endWriter2Data.asm"
._CODE3_END
let EwData_size = @-&8000
endif

; second background
org &C000
;incbin "witch_beauty_fixed.scr"
incbin "endWriter/witch_beauty_15colors_bg(1-15)_fixed.scr"
let BeautyScreen_size = @-&C000

; BANK 0
; ======
write direct -1,-1,&C0		; b0;b1;b2;b3

; first background
org BACKGROUND_OFFSET
incbin "witch_old/witch_old_eyes_closed_fixed.scr"

; little grafx hack
org &7449
db &9F
;org &44A9
;db #9f

; prog
org CODE_OFFSET1
run CODE_OFFSET1
._CODE_START
di

;call loader

; stack pointer
ld sp,&FFF0

call delock_asic

; Connect Asic
ld bc,&7FB8
out (c),c

; Change to IM2 mode
call im2

; TODO make the ball transparent
if SPRITE_TRANSPARENT
	ld hl,ballsprite1
	ld b,0	; 256
	ld d,1
	ld e,4 
	.transparent
		ld a,(hl)
		cp d
		jr z,hop
		cp e
		jr nc,hop
		ld (hl),0
		.hop
		inc hl
		djnz transparent
endif

; Screen palette (16+border)
ld bc,32+2 ; 16*2
ld hl,pal
ld de,&6400
ldir

; ink1 <- ink0
ld hl,ink0
ld (&6400+2),hl
	
; Reset R52, Roms off, mode 2
ld bc,&7F00+%10011110
out (c),c

; set PRI to end of screen so that it doesn't disturb us during first demo phase
ld hl,&6800
ld (hl),&FF;RINT_SCREEN_MIDDLE

; Intro
ld hl,aylMultiModeIntro
ld (DMA2ADD+1),hl	; set DMA2 to produce interrupts
ld hl,DMACTL+1
set 2,(hl)		; enable DMA2

; Rupture + (Line 48, Screen = &4000)
ld hl,&6801
ld (hl),RINT_SCREEN_ON
inc l
ld (hl),%010000	; &4000 no overscan
inc l
ld (hl),0

; Deconnect asic
ld bc,&7FA0
out (c),c

; crtc settings
ld b,&bc
ld hl,crtc_settings
ld a,6
.crtc_set_loop
	ld c,(hl)
	out (c),c	; set reg
	inc hl
	inc b
	ld c,(hl)
	out(c),c	; set value
	inc hl
	dec b
	dec a
	jr nz,crtc_set_loop

; mode 0
;ld bc,&7F00+%10001100
;out (c),c

; Effectively disable sprites (x=300)
ld hl,ballpos2D
call copy_balls_pos

ei
call player 	; init

.mainloop 
	ld b,&F5
.mainsync
	in a,(c)
	rra
	jr nc,mainsync

	; PLAY AYC+SID
	repeat 30
	nop
	rend	
	call player+6	; playvbl
	
	; copy first object positions
	call display3D

	call player+3	; playframe

	; witch eyes
	call blinkEyes

	; SCENARIO
	; - Open screen &0 - &180
	.frameCount
	ld de,START_SCENARIO
	inc de
	ld (frameCount+1),de

	; This code avoid the wich hat pb ?
	; Maybe because of the Flags ???
;	ld hl,-&06E6
;	add hl,de
;	jr nc,currentScenario
;	nop

	.currentScenario
	call testEnd

	jr mainloop

; copy the ball positions in ASIC
.display3D
	ret			; disabled
	ld hl,ballpos2D
	call copy_balls_pos
	ret

.display3D_second
	ret			; disabled
	ld hl,ballpos2D_second
	call copy_balls_pos
	ret

;********************
; Tools - copyNSprite
;********************
; Copy A sprites from HL to DE
; IN 	A=NB Sprites
;	DE=Dest in memory 	
; Corrupts HL, DE, AF, BC 
.copyNSprite0
	; sprite
	ld bc,&100
	.override_sprite1
	ld hl,ballsprite1
	ldir
	dec a
	jp nz,copyNSprite0
	ret
.copyNSprite1
	; sprite
	ld bc,&100
	ld hl,ballspriteTrans
	ldir
	dec a
	jp nz,copyNSprite1
	ret
.copyNSprite2
	; sprite
	ld bc,&100
	ld hl,ballsprite2
	ldir
	dec a
	jp nz,copyNSprite2
	ret

; DE=Sprite ASIC MEMORY
; C=INK offset for deep effect (0,3,6,9,12)
.copySpriteDeepEffect
	ld b,0
	ld hl,ball3	; sprite with only 3 colors
	.loopBallSprite3Cpy
		ld a,(hl)
		;or a
		;jr z,continueLoopBallSprite3Cpy
		cp 2
		jp M,continueLoopBallSprite3Cpy	; ignore for ink 0 and 1
		add c
		.continueLoopBallSprite3Cpy
		ld (de),a
		inc l
		inc e
		djnz loopBallSprite3Cpy
	ret

;*******************
; callWithAsicLocked 
;*******************
.HL_WithAsicLocked
	
	; lock asic
 	ld a,&C9		; [2]
 	ld (deco_asic),a	; [4]

	; Connect Asic
	ld bc,&7FB8
	out (c),c
	
	; Simulate "call (hl)"
	ld bc,retFromCall
	push bc
	jp (hl)
	.retFromCall

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	
	; delock asic
 	xor a			; [1]
 	ld (deco_asic),a	; [4]

	ret

;*****************
; Raster Interrupt
;*****************
.intDMA2
;.intRASTER	; override interrupt vector
;.intDMA0	; Futur Crew reported a bug in the interrupt.
		; It seems that the Asic sometimes calls the intDMA0 vector upon a raster int 
.intJMP
	;jp screen_on
	jp screen_intro

; Test DMA2
; We use a sound DMA to generate interrupts 
align 2
.aylMultimode
	dw &1000 + 16 + 15				; [VBL] vbl and adjust
	dw &1000 + RINT_SCREEN_ON-1		 	; [RINT_SCREEN_ON-1] pause
	dw &4010					; [RINT_SCREEN_ON] call interrupt vector (screen on)
	dw &1000 + RINT_BMP-RINT_SCREEN_ON-1		; [RINT_SCREEN_MIDDLE-1] pause
.BMP_INT
	dw &4000					; [RINT_BMP] call interrupt vector (enabled = &4010)
	dw &1000 + RINT_SCREEN_OFF-RINT_BMP-1		; [RINT_SCREEN_OFF-1] pause
	dw &4030					; [RINT_SCREEN_OFF] call interrupt vector (screen off)

align 2
.aylMultiModeIntro
	dw &1000 + 16 + 15		; [VBL] vbl and adjust
	dw &1000 + RINT_SCREEN_ON-1	; [RINT_SCREEN_ON-1] pause
	dw &4030			; call interrupt vector (intro)

;***************
; Deconnect Asic
;***************
; BC is corrupt
.deco_asic
	nop			; [1]

	ld bc,&7FA0		; [3]
	out (c),c		; [4]
	ret

; **************
; Mode 2 trick ;)
; **************
; - set screen mode 0
; - reset ink1 
; Screen becomes visible 
.screen_on
	push bc
	push hl

	; swap interrupt vector
	;ld hl,middle_screen
.intCallAfterScreenOn
	ld hl,screen_off
	ld (intJMP+1),hl

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	; ack int DMA2
 	; so that we let the raster interrupt a chance to be triggered asap
 	ld hl,DCSR		; [3]
 	set 4,(hl)		; [4]

	.ink1_offset
	ld hl,ink1		; hl <- ink1	
	ld c,%10011100		; Reset R52, Roms off, mode 0
	out (c),c

	; delay till HBL (EXPERIMENTAL)
	;ld bc,&bc00+10
	;out(c),c

	;ld b,&BF	; to read CRTC regs
	;.bob
	;in a,(c)
	;and %00010000
	;jr z,bob

	; delay
	repeat 10
	nop
	rend

	; Should not be visible ...
	ld (&6400+2),hl
	;out (c),c

	; set PRI to generate an interrupt line 200
	;ld hl,&6800
	;ld (hl),RINT_SCREEN_MIDDLE

	; deconnect asic (if not locked)
	call deco_asic

	; enable interrupt
	pop hl
	pop bc
	ei
	ret

; Middle screen
;.middle_screen
;	; enable interrupts so that
;	; the screen_off int is picked up in the middle of this one
;	push bc
;	push hl
;
;	; swap interrupt vector
;	ld hl,screen_off
;	ld (intJMP+1),hl
;
;	; Connect Asic
;	ld bc,&7FB8		; [3]
;	out (c),c		; [4]
;
;	; ack int DMA2
; 	; so that we let the raster interrupt a chance to be triggered asap
; 	ld hl,DCSR		; [3]
; 	set 4,(hl)		; [4]
;
;	push af
;	push de
;	; HACK second object
;	call display3D_second
;	pop de
;	pop af
;
;	; deconnect asic (if not locked)
;	call deco_asic
;
;	; enable interrupt
;	pop hl
;	pop bc
;	ei
;	ret

; - ink1 <- ink0 
; - set screen mode 2
; screen becomes unvisible 
.screen_off
	push bc
	push hl
	
	; mode 2
	ld bc,&7F00+%10011110	; Reset R52, Roms off, mode 2
	out (c),c

	; Connect Asic
	;ld bc,&7FB8		; [3]
	ld c,&B8
	out (c),c		; [4]

	; ack int DMA2
 	; so that we let the raster interrupt a chance to be triggered asap
 	ld hl,DCSR		; [3]
 	set 4,(hl)		; [4]

	; ink1 <- ink0
	.ink0_offset
	ld hl,ink0
	ld (&6400+2),hl

	; swap interrupt vector
	ld hl,screen_on;intVBL
	ld (intJMP+1),hl
	
	; reset DMA
	ld hl,aylMultimode
	ld (DMA2ADD+1),hl	; set DMA2 to produce interrupts
	ld hl,DMACTL+1
	set 2,(hl)		; enable DMA2

	; deconnect asic (if not locked)
	call deco_asic

	; enable interrupt
	pop hl
	pop bc
	ei
	ret

; - ink1 <- ink0 
; - set screen mode 2
; screen becomes unvisible 
.screen_intro
	push af
	push bc
	push hl

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

	; ack int DMA2
 	; so that we let the raster interrupt a chance to be triggered asap
 	ld hl,DCSR		; [3]
 	set 4,(hl)		; [4]

	; delay
	; All the conditions below handle the case where we have either a full window or none
	ld a,83
	.delayIntro1
	or a
	jr z,no1Loop
	cp 83
	jr z,endMultiModeIntro

	ld b,a
	.delayIntro1Loop
		repeat 12
		ld hl,(&6400)	; [5] ld hl,(ink0)
		rend
	djnz delayIntro1Loop	; [4]

.no1Loop

	; mode 0
	ld bc,&7F00+%10011100	; Reset R52, Roms off, mode 0
	out (c),c

	; ink1 <- ink1
	.ink1_offset_intro
	ld hl,ink1
	ld (&6400+2),hl

	ld b,a
	ld a,83
	sub b
	sla a
	ld b,a

	; delay
	;ld b,1
	.delayIntro2Loop
		repeat 12
		ld hl,(&6400)	; [5] ld hl,(ink0)
		rend
	djnz delayIntro2Loop		; [4]

	; ink1 <- ink0
	;ld hl,ink0
	ld (&6400+2),hl
	
	; mode 2
	ld bc,&7F00+%10011110	; Reset R52, Roms off, mode 2
	out (c),c

.endMultiModeIntro
	; reset DMA
	ld hl,aylMultiModeIntro
	ld (DMA2ADD+1),hl	; set DMA2 to produce interrupts
	ld hl,DMACTL+1
	set 2,(hl)		; enable DMA2

	; deconnect asic (if not locked)
	call deco_asic

	; enable interrupt
	pop hl
	pop bc
	pop af
	ei
	ret

; *********
; ** IM2 **
; *********
read "common/IM2_no_di_ei.ASM"

; **************
; ** Scenarii **
; **************
read "scenarii.asm"

; ****************
; ** Witch eyes **
; ****************
read "witch.asm"

; *********************
; ** End Writer Code **
; *********************
if INCLUDE_EW
;let currOffset = $
read "endWriter2.asm"
;print "# End Writer Code"
;print $-currOffset
endif

; ********
; ** 3D **
; ********
read "3D.asm"
._CODE_END

let testOffset = $/&4000
if testOffset
	print "CODE TOO LONG"
	print $
	stop
endif

; ************
; ** PLAYER **
; ************
org PLAYER_OFFSET
.player
._PLAYER_START
read "player_interruptible.asm"
._PLAYER_END

; ***********
; ** MUSIC **
; ***********
.AYC_START
._AYC_START
nolist
incbin "music/seagulls.ayc"	; NB VBL=0x2E81 ~ 4 mins
.SIDList
incbin "music/seagulls.sid"

; **********
; ** DATA **
; **********
;org DEMO_DATA
._DEMODATA_START
read 'demo2009Data.asm'
._DEMODATA_END

; **************
; ** RotoZoom **
; **************
let currOffset = $
._CODE2_START
read "Rotozoom.asm"

; ***************
; ** 3D part 2 **
; ***************
read "3D_part2.asm"

; *************************
; ** Sprite Multiplexing **
; *************************
read "multi2.asm"

; *************************
; ** Bump Mapping **
; *************************
read "bmap.asm"

; ***********
; ** Debug **
; ***********
read "Printsprite.asm"
._CODE2_END


;list 
; ***************
; ** Digidrums **
; ***************
; Addresses des digidrums (16 max)
DIGITABLE

; **********
; ** PACK **
; **********

macro decimal2Hex x
	ifnot x-15
		db 'F'
	endif
	ifnot x-14
		db 'E'
	endif
	ifnot x-13
		db 'D'
	endif
	ifnot x-12
		db 'C'
	endif
	ifnot x-11
		db 'B'
	endif
	ifnot x-10
		db 'A'
	endif
	ifnot x-9
		db '9'
	endif
	ifnot x-8
		db '8'
	endif
	ifnot x-7
		db '7'
	endif
	ifnot x-6
		db '6'
	endif
	ifnot x-5
		db '5'
	endif
	ifnot x-4
		db '4'
	endif
	ifnot x-3
		db '3'
	endif
	ifnot x-2
		db '2'
	endif
	ifnot x-1
		db '1'
	endif
	ifnot x-0
		db '0'
	endif
mend

macro word2String x
	let MSB = x/256
	let LSB = x mod 256

	let MSBh = MSB/16
	let MSBl = MSB mod 16
	let LSBh = LSB/16
	let LSBl = LSB mod 16 

	decimal2Hex MSBh
	decimal2Hex MSBl
	decimal2Hex LSBh
	decimal2Hex LSBl
mend