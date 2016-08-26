; Print 8-16 bits numbers in sprite
; FKey/Revival 2011

TEST_MAIN	EQU 0
DIGIT_INK0		EQU 1
DIGIT_INK1		EQU 3

if TEST_MAIN
nolist

org &1000
run &1000

.start
	di

	call delock_asic

	; Connect Asic
	ld bc,&7FB8	; [3]
	out (c),c	; [4]

	ld hl,&FFF
	ld (&6422),hl	; change ink1

	ld hl,140
	ld (&6000),hl
	ld (&6002),hl
	ld a,%1111
	ld (&6004),a

	di
	ld a,&FC
	ld de,&4000
	call print8bits
	
	ld hl,&ABCD
	ld de,&4050
	call print16bits
	;ei

.waitVBL
	ld b,&F5
.syncVBL
	in a,(c)
	rra
	jr nc,syncVBL

	; wait
	ld b,0:nop:djnz $

.counter
	ld a,&FC
	ld de,&4000
	call print8bits

	call getKey

	.oldKbState
	ld b,&FF		; nothing pressed
	ld (oldKbState+1),a	; save new kb state

	; inc counter only if bit7=0 and old_bit7=1
	cpl
	and b
	bit 6,a
	jp z,no_inc

	; increase counter
	ld hl,counter+1
	inc (hl)

	.no_inc
	bit 5,a
	jp z,no_dec

	; decrease counter
	ld hl,counter+1
	dec (hl)

	.no_dec


	jp waitVBL ; wait forever

; Delock/Relock Asic
read "common/ASIC_ON.ASM"

; Keyboard test
.getKey
	LD   BC,#F782   ; A Sortie/C Sortie
	OUT  (C),C
	LD   BC,#F40E   ; Valeur 14 sur port A
	OUT  (C),C
	LD   BC,#F6C0   ; C'est un index (R14)
	OUT  (C),C
	;LD   BC,#F600   ; On valide!
	XOR  A
	OUT  (C),A
	LD   BC,#F792   ; A Entree/C Sortie
	OUT  (C),C
	LD   BC,#F645	; Ligne clavier numero 5
	OUT  (C),C
	LD   B,#F4      ; On lit le port A (PPI)
	IN   A,(C)      ; Donc R14 (A du AY)
	LD   BC,#F782   ; A en Sortie
	OUT  (C),C
	LD   BC,#F600   ; Reset PPI Write
	OUT  (C),C
	;BIT  7,A
	;JP NZ,VBL
	ret

.deco_asic
nop

ENDIF

; IN 	DE = ASIC Sprite Memory
;	HL = Number
; Condition = interrupts disabled
.print16bits
	push af
	push bc
	
	ld a,h
	call print8bits ; restore hl,de

	ex de,hl
	ld bc,8
	add hl,bc
	ex de,hl

	ld a,l
	call print8bits	; restore hl,de
	
	pop bc
	pop af
	ret


; IN 	DE = ASIC Sprite Memory
;	A = Number
; Condition = interrupts disabled
.print8bits
	push bc
	push de
	push hl
	push af

	; lock asic
 	ld a,&C9
 	ld (deco_asic),a

	; Connect Asic
	ld bc,&7FB8	; [3]
	out (c),c	; [4]

	pop af:push af
	rra:rra:rra:rra
	and &F
	call printNumber
	
	ex de,hl
	ld bc,16*5*-1+4
	add hl,bc
	ex de,hl

	pop af:push af
	and &F
	call printNumber

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; unlock asic
 	xor a
 	ld (deco_asic),a

	pop af
	pop hl
	pop de
	pop bc
	ret

; IN 	DE = ASIC Sprite Memory
;	A = Number
; Coditions	ASIC ON
; Corrupted	A,BC,DE,HL
.printNumber
	; get digit offset
	add a	; *2
	add a	; *4
	add a	; *8
	add a	; *16
	ld c,a
	ld b,0
	ld hl,digits
	add hl,bc	; digit offset in hl
	
	xor a	; space between chars

	repeat 5
	ldi:ldi:ldi:ld (de),a
	ex de,hl
	ld bc,16-3
	add hl,bc
	ex de,hl
	rend

	ret

.digits
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; zero
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1	; one
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; two
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; three
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0	; four
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; five
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0	; six
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; seven
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK0,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK0,DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; height
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; nine
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK0,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; ten
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0	; eleven
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; twelve
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0	; thirteen
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK1
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; forteen
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK1	; fivteen
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK1,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0
db	DIGIT_INK1,DIGIT_INK0,DIGIT_INK0,DIGIT_INK0