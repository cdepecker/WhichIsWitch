nolist

; SCROLL X TABLE
align 256
let posY = EW_X_POS+2	; first pos unvisible (left)
.translationXTable
repeat INTER_CHAR*NB_COLUMNS + 1 + EW_X_POS
	db	posY
	let posY = posY - 1
rend

; POSY table (168 pos)
align 2		; ensure en inc l instead of an inc hl
let line = 0
.posYTable
repeat 168
	let char_line = line / 8
	let raster_line = line mod 8 
	let off_char_line = char_line * 96
	let off_raster_line = raster_line * &800
	let line = line + 1
	dw &4000 + off_char_line + off_raster_line
rend

; *****
; FONTE
; *****
if MONOFONTE
.fonteSprite
	incbin "endWriter/fonte2colors_mode0_sprite_8x8.bin"

.fonteScreen
	incbin "endWriter/fonte2colors_mode0_screen_8x8.bin"
else
.fonteSprite
	incbin "endWriter/fonte15colors_mode0_sprite_8x8.bin"

.fonteScreen
	incbin "endWriter/fonte15colors_mode0_screen_8x8.bin"
endif

; ****
; MASK
; ****
.spriteWitchMask
incbin "endWriter/sprite_witch_mask_packed_21_56_fixed.bin"

; Char availables ./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
.ewMsg1
	; 6 lines * 10 chars
	text	0
	text	" WHICH IS"
	text	"  WITCH ?"
	text	0
	text	0
	text	"<REVIVAL>"
	text	0
	text	"6128 PLUS"
	text	"DEMO/PART"
	text	0
	text	"RELEASED "
	text	"  08/2011"
	text	0
	text	"GFX: CED "
	text	"ASM: FKEY"
	text	0

;.ewMsg3
	text	"THANKS:  "
	text	0
	text	">DEMONIAK"
	text	" CVIMGCPC"
	text	" MNGDSK  "
	text	0
	text	">R.WILSON"
	text	" WINAPE  "
	text	0
	text	">MADRAM  "
	text 	" ORIGINAL"
	text 	"   PLAYER"
	text	0
	text	">TJ/GPA  "	
	text	" DEEXO   "
	text	0

;.ewMsg4
	text	"CREDITS: "
	text	0
	text	">CED     "
	text	" ALL GFX "
	text	0
	text	">TAO     "	
	text	" YM TUNE "
	text	" SEAGULLS"
	text	0
	text	">FKEY    "
	text	" CODING  "
	text	0
	text	"COOP DEMO"
	text	"WITH GOUP"
	text	" IMPACT  "
	text	0

;.ewMsg4
;	text	"TEAM:;    "
;	text	0
;	text	">CED     "
;	text	" GFX     "
;	text	" FONTE   "
;	text	" DESIGN  "
;	text	0
;	text	">FKEY    "
;	text	" SKULL FX"
;	text	" 3D BALLS"
;	text	" ROTOZOOM"
;	text	" SID PLAY"
;	text	" BALLS   "
;	text	"   WRITER"
;	text	" END     "
;	text	"   WRITER"

;.ewMsg5
	text	"HELLO TO:"
	text	0
	text	"PLISSKEN "
	text	"  ELIOT  "
	text	" HERMOL  "
	text	"  FANO   "
	text	"   BDC   "
	text	" NORECESS"
	text	0
	text	" ALL CPC "
	text	"DEMOMAKRS"
	text	"OUT THERE"
	text	0
	text	" AND THE "
	text	" PHENIX  "
	text	"  TEAM   "

;.ewMsg6
	text	"HELLO TO:"
	text	0
	text	"   AC    "
	text	"ALEXANDRE"
	text	" JUSTINE "
	text	"  PAPA   "
	text	" SOPHIE  "
	text	"  AUDE   "
	text	" CEDRIC  "
	text	" PATRICE "
	text	0
	text	" FAMILY  "
	text	" FRIENDS "
	text	0
	text 	" IMPACT  "
	text 	"    TEAM "

;.ewMsg7
	text	"WE HOPE U"
	text	"ENJOYED  "
	text	"THIS PART"
	text	0
	text	0
	text	0
	text	0
	text	0
	text	0
	text	0
	text	0
	text	0
	text 	"FKEY SAYS"
	text 	"SEE YOU.."
	text 	"NEXT PROD"
	text	0

; usefull to know when to stop the message engine
EW_PAGES	EQU 6