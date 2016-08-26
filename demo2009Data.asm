; Demo2009Data
; Static and Dynamic

; screen palette
ink0	EQU &000
ink1	EQU &0C0
.pal
	incbin "witch_old/witch_old.pal"

.pal2
	;incbin "witch_beauty.pal"
	incbin "endWriter/witch_beauty_15colors_bg(1-15).pal"

	; PATCH First beauty witch screen ink to black
	let myOffset = $
	org pal2
	dw 0
	org myOffset

; ***********
; SINUS table
; ***********
align 256
.sin
nolist
incbin "common/sin.dat"
;list

; sinus intro
.sin_intro
nolist
incbin "witch_old/sin_intro.bin"
;list

;.sin_ew
;nolist
;incbin "scrollEW.bin"

;.sin_ew2
;nolist
;incbin "scrollEW2.bin"

; *************
; CRTC Settings
; *************
; overscan [(48*2)*(34*8)]
; 16/9 [(48*2)*(21*8)] < 16Ko
.crtc_settings
db 1,48
db 2,50
;db 6,21 ; 34
db 6,34
;db 7,29 ; 35
db 7,35
db 12,%011101 ; &4200, 32K
db 13,0;

; ******
; STATIC
; ******

OBJECTS_TIME EQU 50*10/2	; 10 secs
OBJECTS_TIME2 EQU 50*7/2		; 7 secs
OBJECTS_NUMBER EQU 6

ATT_SYMCENTER	EQU %00000001
ATT_SYMPLAN	EQU %00000010
ATT_SPEEDFX	EQU %00000100

; Object Sequence
.obj_seq
dw star2D
	dw symetrie_centrale
	db 1
dw circles2D
	dw symetrie_centrale
	db 0
dw star3D
	dw symetrie_centrale
	db 0
;dw sphere3D
;	dw symetrie_centrale
;	db 0
dw XDouble
	dw translation_vectorZ
	db 0
dw tirebouchon
	dw translation_vectorZ
	db 0
dw circlesDouble
	dw symetrie_centrale;symetrie_planZ
	db 0
dw nullObject
	dw symetrie_centrale
	db 0

;dw sphere3D

; first second object
dw circles2D

;.obj_seq
;dw OBJ3D

.object_timing
dw -OBJECTS_TIME

.object_timing2
dw -1;-OBJECTS_TIME2

.objnum
db OBJECTS_NUMBER ;-1

.objnum2
db OBJECTS_NUMBER

;.morphdegree
;db 1

; OBJECTS
; *******
let unit = 16

.morphObj
	;ds NB_BALLS_TOTAL*3,0
	ds NB_BALLS*3,0

.morphObj2
	;ds NB_BALLS_TOTAL*3,0
	ds NB_BALLS*3,0

macro DB_COS_30 r,s
	db r*87/100*s
endm

macro DB_COS_45 r,s
	db r*71/100*s
endm

macro DB_SIN_45 r,s
	db r*71/100*s
endm

macro DB_COS_60 r,s
	db r/2*s
endm

macro DB_SIN_30 r,s
	db r/2*s
endm

macro DB_SIN_60 r,s
	db r*87/100*s
endm


; ***********
; Object List
; ***********

.nullObject
	DS 8*3,0

let unitt = 20
.XDouble
; ball 0
	db 3*unitt/2; Z
	db 0; Y
	db 3*unitt/2; X
; ball 1
	db 3*unitt/2; Z
	db 29*3*unitt/2/100; Y
	db 29*3*unitt/2/100; X
; ball 2
	db 3*unitt/2; Z
	db 3*unitt/2; Y
	db 0; X
; ball 3
	db 3*unitt/2; Z
	db 29*3*unitt/2/100; Y
	db 29*3*unitt/2/100*-1; X
; ball 4
	db 3*unitt/2; Z
	db 0; Y
	db 3*unitt/2*-1; X
; ball 5
	db 3*unitt/2; Z
	db 29*3*unitt/2/100*-1; Y
	db 29*3*unitt/2/100*-1; X
; ball 6
	db 3*unitt/2; Z
	db 3*unitt/2*-1; Y
	db 0; X
; ball 7
	db 3*unitt/2; Z
	db 29*3*unitt/2/100*-1; Y
	db 29*3*unitt/2/100; X

.circlesDouble
; ball 0
	db 3*unit/2; Z
	db 0; Y
	db 3*unit/2; X
; ball 1
	db 3*unit/2; Z
	DB_SIN_45 3*unit/2,1; Y
	DB_COS_45 3*unit/2,1; X
; ball 2
	db 3*unit/2; Z
	db 3*unit/2; Y
	db 0; X
; ball 3
	db 3*unit/2; Z
	DB_SIN_45 3*unit/2,1; Y
	DB_COS_45 3*unit/2,-1; X
; ball 4
	db 3*unit/2; Z
	db 0; Y
	db 3*unit/2*-1; X
; ball 5
	db 3*unit/2; Z
	DB_SIN_45 3*unit/2,-1; Y
	DB_COS_45 3*unit/2,-1; X
; ball 6
	db 3*unit/2; Z
	db 3*unit/2*-1; Y
	db 0; X
; ball 7
	db 3*unit/2; Z
	DB_SIN_45 3*unit/2,-1; Y
	DB_COS_45 3*unit/2,1; X

.circles2D
; ball 0
	db 0 ; Z
	db 0 ; Y
	db unit ; X
; ball 1
	db unit*7/10 ; Z
	db 0 ; Y
	db unit*7/10 ; X
; ball 2
	db unit ; Z
	db 0 ; Y
	db 0 ; X
; ball 3
	db 0 ; Z
	db 0 ; Y
	db 2*unit ; X
; ball 4
	db 0 ; Z
	db 2*unit*38/100 ; Y
	db 2*unit*92/100 ; X
; ball 5
	db 0 ; Z
	db 2*unit*7/10 ; Y
	db 2*unit*7/10 ; X
; ball 6
	db 0 ; Z
	db 2*unit*92/100 ; Y
	db 2*unit*38/100 ; X
; ball 7
	db 0 ; Z
	db 2*unit ; Y
	db 0 ; X

let unitStar2D = 15

.star2D
; ball 0
	db 0 ; Z
	db 0 ; Y
	db unitStar2D ; X
; ball 1
	db 0 ; Z
	db unitStar2D ; Y
	db 0 ; X
; ball 2
	db 0 ; Z
	db 0 ; Y
	db 2*unitStar2D ; X
; ball 3
	db 0 ; Z
	db 2*unitStar2D ; Y
	db 0 ; X
; ball 4
	db 0 ; Z
	db 0 ; Y
	db 3*unitStar2D ; X
; ball 5
	db 0 ; Z
	db 3*unitStar2D ; Y
	db 0 ; X
; ball 6
	db 0 ; Z
	db 0 ; Y
	db 4*unitStar2D ; X
; ball 7
	db 0 ; Z
	db 4*unitStar2D ; Y
	db 0 ; X

.star3D
; ball 0
	db 0 ; Z
	db 0 ; Y
	db unit ; X
; ball 1
	db 0 ; Z
	db unit ; Y
	db 0 ; X
; ball 2
	db unit ; Z
	db 0 ; Y
	db 0 ; X
; ball 3
	db -unit ; Z
	db unit; Y
	db -unit ; X
; ball 4
	db 0 ; Z
	db 0 ; Y
	db 2*unit ; X
; ball 5
	db 0 ; Z
	db 2*unit ; Y
	db 0 ; X
; ball 6
	db 2*unit ; Z
	db 0 ; Y
	db 0 ; X
; ball 7
	db unit ; Z
	db unit ; Y
	db unit ; X

.sphere3D	; Rayon=2*unit
; **** Center Circle = 3*2=6 balls
; ball 0
	db 0 ; Z
	db 0 ; Y
	db 2*unit ; X
; ball 1
	db 0 ; Z
	db 2*87*unit/100; Y
	db unit ; X
; ball 2
	db 0 ; Z
	db 2*87*unit/100*-1; Y
	db unit ; X

; **** 1/2 Circle = 3 balls, Rayon = 2*unit*cos(45)
; ball 3
	db 71*2*unit/100 ; Z
	db 0 ; Y
	db 71*2*unit/100 ; X
; ball 4	
	db 71*2*unit/100 ; Z
	db 0; Y
	db 71*2*unit/100*-1 ; X
; ball 5
	db 71*2*unit/100 ; Z
	db 71*2*unit/100; Y
	db 0 ; X
; ball 6
	db 71*2*unit/100 ; Z
	db 71*2*unit/100*-1; Y
	db 0 ; X

; **** Top Circle = 1 balls, Rayon = 2*unit
; ball 7
	db 2*unit ; Z
	db 0 ; Y
	db -unit/2 ;unit ; X

.tirebouchon
; ball 0
	db 3*unit*0/8 ; Z
	db 0; Y
	db 3*unit/2; X
; ball 1
	db 3*unit*1/8 ; Z
	DB_SIN_45 3*unit/2,1; Y
	DB_COS_45 3*unit/2,1; X
; ball 2
	db 3*unit*2/8 ; Z
	db 3*unit/2; Y
	db 0; X
; ball 3
	db 3*unit*3/8 ; Z
	DB_SIN_45 3*unit/2,1; Y
	DB_COS_45 3*unit/2,-1; X
; ball 4
	db 3*unit*4/8 ; Z
	db 0; Y
	db 3*unit/2*-1; X
; ball 5
	db 3*unit*5/8 ; Z
	DB_SIN_45 3*unit/2,-1; Y
	DB_COS_45 3*unit/2,-1; X
; ball 6
	db 3*unit*6/8 ; Z
	db 3*unit/2*-1; Y
	db 0; X
; ball 7
	db 3*unit*7/8 ; Z
	DB_SIN_45 3*unit/2,-1; Y
	DB_COS_45 3*unit/2,1; X

; Ball Sprite
align 256

.ball1
incbin	"3D/ball_3D.bin"                                     

.ball3
	db 0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0
	db 0,0,0,0,1,1,2,2,2,2,1,1,0,0,0,0
	db 0,0,0,1,2,2,2,2,2,2,2,2,1,0,0,0
	db 0,0,1,2,2,2,2,2,2,3,2,2,2,1,0,0
	db 0,1,2,2,2,2,2,2,3,3,3,2,2,2,1,0
	db 0,1,2,2,2,2,2,2,3,3,3,3,2,2,1,0
	db 1,2,2,2,2,2,2,2,2,3,3,3,2,2,2,1
	db 1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1
	db 1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1
	db 1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1
	db 0,1,2,2,2,2,2,2,2,2,2,2,2,2,1,0
	db 0,1,2,2,2,2,2,2,2,2,2,2,2,2,1,0
	db 0,0,1,2,2,2,2,2,2,2,2,2,2,1,0,0
	db 0,0,0,1,2,2,2,2,2,2,2,2,1,0,0,0
	db 0,0,0,0,1,1,2,2,2,2,1,1,0,0,0,0
	db 0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0

; ball tansparent
.ball2
	db 00,00,00,00,00,00,14,11,10,10,00,00,00,00,00,00
	db 00,00,00,00,14,14,14,14,14,14,10,10,00,00,00,00
	db 00,00,00,14,14,14,14,00,00,14,14,14,10,00,00,00
	db 00,00,14,14,14,00,00,00,00,14,00,14,14,10,00,00
	db 00,14,14,14,00,00,00,00,15,14,13,00,14,14,11,00
	db 00,14,14,00,00,00,00,00,15,14,14,13,00,14,11,00
	db 14,14,14,00,00,00,00,00,00,15,15,13,00,14,14,11
	db 14,14,00,00,00,00,00,00,00,00,00,00,00,00,14,14
	db 14,14,00,00,00,00,00,00,00,00,00,00,00,00,14,14
	db 14,14,00,00,00,00,00,00,00,00,00,00,00,14,14,14
	db 00,14,14,00,00,00,00,00,00,00,00,00,00,14,14,00
	db 00,14,14,14,00,00,00,00,00,00,00,00,14,14,14,00
	db 00,00,14,14,14,00,00,00,00,00,00,14,14,14,00,00
	db 00,00,00,14,14,14,14,00,00,14,14,14,14,00,00,00
	db 00,00,00,00,14,14,14,14,14,14,14,14,00,00,00,00
	db 00,00,00,00,00,00,14,14,14,14,00,00,00,00,00,00

; rotozoom
.tile1
incbin	"rotozoom/tile10.bin"
.tile2
incbin	"rotozoom/tile2.bin"
.tile3
incbin	"rotozoom/tile3.bin"
.tile4
incbin	"rotozoom/tile4.bin"
.tile5
incbin	"rotozoom/tile5.bin"
.tile6
incbin	"rotozoom/tile6.bin"
;.tile7
;incbin	"rotozoom/tile7bis.bin"
.tile8
incbin	"rotozoom/tile8.bin"
.tile9
incbin	"rotozoom/tile9.bin"

.tile1pal
incbin	"rotozoom/tile1.pal"
.tile2pal
incbin	"rotozoom/tile2.pal"
.tile3pal
incbin	"rotozoom/tile3.pal"
.tile4pal
incbin	"rotozoom/tile4.pal"
.tile5pal
incbin	"rotozoom/tile5.pal"
.tile6pal
incbin	"rotozoom/tile6.pal"
;.tile7pal
;incbin	"rotozoom/tile7.pal"
.tile8pal
incbin	"rotozoom/tile8.pal"
.tile9pal
incbin	"rotozoom/tile9.pal"

; Sprite palette
.ball1pal                                  
incbin	"3D/ball_3D.pal"

; 7 shade level (2+2+2+2+2+3+3)
; first color is black
.ball3pal
	dw &0000 ; 1 - zero, green, red, blue
	dw &0AF0 ; 2
	dw &0FFF ; 3
	dw &09E0 ; 4
	dw &0EEE ; 5
	dw &08D0 ; 6
	dw &0DDD ; 7
	dw &07C0 ; 8
	dw &0CCC ; 9
	dw &05A0 ; 10
	dw &0BBB ; 11
	dw &0380 ; 12
	dw &0AAA ; 13
	dw &0270 ; 14
	dw &0999 ; 15

;	dw &0000 ; 1 - zero, green, red, blue
;	dw &0AF0 ; 2
;	dw &0FFF ; 3
;	dw &09E0 ; 4
;	dw &0EED ; 5
;	dw &08D0 ; 6
;	dw &0DDD ; 7
;	dw &07C0 ; 8
;	dw &0CCC ; 9
;	dw &06B0 ; 10
;	dw &0BBB ; 11
;	dw &05A0 ; 12
;	dw &0AAA ; 13
;	dw &0490 ; 14
;	dw &0999 ; 15

 	;dw &0000 ; 1 - zero, green, red, blue
	;dw &0FF0 ; 2
	;dw &0FFF ; 3
	;dw &0EE0 ; 4
	;dw &0EED ; 5
	;dw &0DD0 ; 6
	;dw &0DDD ; 7
	;dw &0CC0 ; 8
	;dw &0CCC ; 9
	;dw &0BB0 ; 10
	;dw &0BBB ; 11
	;dw &0AA0 ; 12
	;dw &0AAA ; 13
	;dw &0990 ; 14
	;dw &0999 ; 15

 	;dw &0000 ; 1 - zero, green, red, blue
	;dw &0EE0 ; 2
	;dw &0FFF ; 3
	;dw &0000 ; 4
	;dw &0DD0 ; 5
	;dw &0DDD ; 6
	;dw &0000 ; 7
	;dw &0BB0 ; 8
	;dw &0BBB ; 9
	;dw &0000 ; 10
	;dw &0aa0 ; 11
	;dw &0aaa ; 12
	;dw &0000 ; 13
	;dw &0990 ; 14
	;dw &0999 ; 15

; **********************
; DYNAMIC - First Object
; **********************

; NB_BALLS Computed
nbBalls		dw NB_BALLS
nbBallsMul3	dw NB_BALLS*3
crdEndNbBalls	dw NBCRD*NB_BALLS + ballpos3D - 1

; Ball position on the screen X(2) Y (2) zoom (4bits)
	dw &0	; INT callback
	dw &0	; INT callback
	dw &0	; INT callback
	dw &0	; INT callback
.ballpos2D
repeat NB_BALLS_TOTAL
	dw &300
	dw &0
rend
.ballpos2Dend

; hack --- second object
.ballpos2D_second
repeat NB_BALLS_TOTAL
	dw &300
	dw &0
rend
.ballpos2Dend_second

; ball position 3D
; TODO Optimize address
align 256
.ballpos3D
	; 16 balls
	ds NBCRD*NB_BALLS_TOTAL,0 	; Z (8bits); Y (8bits), X (8bits)
	
	; 6 save pos for NBS balls (speed effect)
	repeat 6
	ds NBCRD*NBS,0 
	rend

align 256
.ballpos3D_second
	; 16 balls
	ds NBCRD*NB_BALLS_TOTAL,0 	; Z (8bits); Y (8bits), X (8bits)
	
; Used to bubble sort the balls positions
; using lower 8 bits af te address
; make sure that the ballpos3D table is located at a &XX00 Address
.ballsortZinit
LET AUX=0
	repeat NB_BALLS_TOTAL
	db AUX
	LET AUX = AUX + NBCRD
	rend
.ballsortZ
	ds NB_BALLS_TOTAL,0

; ***********************
; * Sprite Multiplexing *
; ***********************

.ballsprite2                                     
incbin	"3D/ball_3D_multi.bin" 
ds 16*8,0

; ***************
; * Skull Wings *
; ***************
.skull_wings
incbin	"skull_wings/skull_wings_compressed.bin" 
;repeat 16*16/2
;db &ee
;rend
;repeat 16*16/2
;db &22
;rend
;repeat 16*16/2
;db &33
;rend
;repeat 16*16/2
;db &44
;rend
;repeat 16*16/2
;db &55
;rend
;repeat 16*16/2
;db &66
;rend
;repeat 16*16/2
;db &77
;rend
;repeat 16*16/2
;db &88
;rend
;repeat 16*16/2
;db &99
;rend
;repeat 16*16/2
;db &aa
;rend
;repeat 16*16/2
;db &bb
;rend
;repeat 16*16/2
;db &dd
;rend
.skull_wings_pal
incbin	"skull_wings/skull_wings_compressed.pal" 

; **********
; End Writer
; **********

;.font	; from char 32
;incbin "font_amstrad (&3900-&3BFF).bin"
                                    
; 14 lines
; F-Key
; RevivaL                     
.txtSprite
db %00000111,%00000000,%00000000,%00000000,0
db %01100100,%00100000,%00000011,%11111000,0
db %01010110,%01010100,%00000111,%11110000,0
db %01100100,%01110100,%00001100,%00000000,0
db %01010111,%01010100,%00011111,%10000000,0
db %01010000,%01010100,%00011111,%00000000,0
db %00000111,%00000111,%00011000,%00000000,0
db %01110010,%01010000,%00011001,%10000000,0
db %00100010,%01110111,%00000000,%11000011,0
db %00100010,%01010100,%00000000,%01101100,0
db %00100111,%01010110,%00000000,%00110000,0
db %00100000,%01010100,%00000000,%11011000,0
db %00000000,%00000111,%00000011,%00001100,0
db %00000000,%00000000,%00001100,%00000000,0
; db %00000111,%00000000,%00000111,%00000000,0
; db %01100100,%00100000,%01110010,%01010000,0
; db %01010110,%01010100,%00100010,%01110111,0
; db %01100100,%01110100,%00100010,%01010100,0
; db %01010111,%01010100,%00100111,%01010110,0
; db %01010000,%01010100,%00100000,%01010100,0
; db %00000000,%00000111,%00000000,%00000111,0
; db %00000011,%10000000,%00000011,%10000000,0
; db %00111010,%00111000,%00011001,%00111000,0
; db %00100011,%00100011,%10100001,%00100000,0
; db %00110010,%00110010,%00100001,%00010000,0
; db %00100010,%00100011,%00100001,%00001000,0
; db %00111000,%00100010,%00011000,%00111000,0
; db %00000000,%00000011,%10000000,%00000000,0
.txtSprite_end

.txtSprite2
 db %00000000,%00000000,%00000000,%00000000,0
 db %01110110,%00100110,%01010100,%11011100,0
 db %01000101,%01010101,%01010101,%00010000,0
 db %01100110,%01110110,%01110101,%00001000,0
 db %01010101,%01010100,%01010101,%00000100,0
 db %01110101,%01010100,%01010100,%11011100,0
 db %00000000,%00000000,%00000000,%00000000,0
 db %00000000,%00000000,%00000000,%00000000,0 
 db %00011001,%00010000,%00110111,%01100000,0
 db %00010100,%10100000,%01000100,%01010000,0
 db %00011000,%01000000,%01000110,%01010000,0
 db %00010100,%01000000,%01000100,%01010000,0
 db %00011000,%01000000,%00110111,%01100000,0
 db %00000000,%00000000,%00000000,%00000000,0
.txtSprite2_end

.txtSprite3
 db %00000000,%00000000,%00000000,%00000000,0
 db %01110010,%00100000,%11001001,%10011100,0
 db %00010101,%01010001,%00010101,%01010000,0
 db %00100010,%01010001,%00010101,%01011000,0
 db %01000101,%01010001,%00010101,%01010000,0
 db %01110010,%00100000,%11001001,%10011100,0
 db %00000000,%00000000,%00000000,%00000000,0
 db %00000000,%00000000,%00000000,%00000000,0
 db %11001000,%10000011,%10100101,%11010001,0
 db %10100101,%00000010,%00101001,%00001010,0
 db %11000010,%00000011,%00110001,%10000100,0
 db %10100010,%00000010,%00101001,%00000100,0
 db %11000010,%00000010,%00100101,%11000100,0
 db %00000000,%00000000,%00000000,%00000000,0
.txtSprite3_end

;.txtSprite3
; db %00000000,%00000011,%11000000,%00000000,0
; db %00000000,%00111100,%00111100,%00000000,0
; db %00000001,%11000000,%00000011,%10000000,0
; db %00001110,%00000000,%00000000,%01110000,0
; db %00110000,%00110000,%00001100,%00001100,0
; db %01000000,%00110000,%00001100,%00000010,0
; db %10000000,%00000001,%10000000,%00000001,0
; db %10000000,%00000001,%10000000,%00000001,0
; db %01000000,%00000000,%00000000,%00000010,0
; db %00110000,%01100000,%00000110,%00001100,0
; db %00001110,%00011111,%11111000,%01110000,0
; db %00000001,%11000000,%00000011,%10000000,0
; db %00000000,%00111100,%00111100,%00000000,0
; db %00000000,%00000011,%11000000,%00000000,0

; db %11100010,%01011101,%01000000,%00111100,0
; db %10000010,%10010001,%01000000,%01000010,0
; db %11000011,%00011000,%10000000,%00100100,0
; db %10011010,%10010000,%10000000,%00011000,0
; db %10000010,%01011100,%10000000,%00100100,0
; db %00000000,%00000000,%00000000,%00000000,0
; db %00011000,%00011000,%00011000,%00011000,0
; db %00011000,%00011000,%00011000,%00011000,0
; db %00000000,%00000000,%00000000,%00000000,0
; db %11001110,%10001011,%10100010,%01100100,0
; db %10101000,%10001001,%00100010,%10010100,0
; db %11001100,%10001001,%00100010,%11110100,0
; db %10101000,%01010001,%00010100,%10010100,0
; db %10101110,%00100011,%10001000,%10010111,0
;.endTxt


; !!! WARNING
; Those variables have to be rebuild everyframe
; Because it's stack processed (can't rely on values after that)
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
.ballposfast
let aux_index = NBL
let aux = SPRITE_LINE
repeat NBL
	dw &300:dw &300:dw &300:dw &300	; 4*X
	dw &300:dw &300:dw &300:dw &300	; 4*X
	dw &300:dw &300:dw &300:dw &300	; 4*X
	dw &300:dw &300:dw &300:dw &300	; 4*X
	db aux				; 1*Y
	let aut_index = aut_index - 1
	let aux = aux + YWIDTH
	if aut_index
	 db aux - RST_BFORE_LINE		; next rester interrupt (line number)
	else
	 db SPRITE_LINE - RST_BFORE_LINE
	endif
	;incbin	"ball_3D.pal"		; palette
	dw setRasterPal0	
rend

dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
dw &0	; INT callback
.ballposfast2
let aux_index = NBL
let aux = SPRITE_LINE
repeat NBL
	dw &300:dw &300:dw &300:dw &300	; 4*X
	dw &300:dw &300:dw &300:dw &300	; 4*X
	dw &300:dw &300:dw &300:dw &300	; 4*X
	dw &300:dw &300:dw &300:dw &300	; 4*X
	db aux				; 1*Y
	let aut_index = aut_index - 1
	let aux = aux + YWIDTH
	if aut_index
	 db aux - RST_BFORE_LINE		; next rester interrupt (line number)
	else
	 db SPRITE_LINE - RST_BFORE_LINE
	endif
	;incbin	"ball_3D.pal"		; palette
	dw setRasterPal0	
rend
;ballposfast_end

;print "************************"
;print ballposfast
;print ballposfast_end
;print "************************"