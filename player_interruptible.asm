;
;PLAY-AY.SCE  OVerLanders 24/5/2000
;v0.6a
;
; Concu avec dams en #3e8 / #C0 / M#6600
;
;   Pour placer manuellement ses tampons de decompression, definir OFTAB0 etc,
;mettre  les bons JUMP en V0 etc et enlever CALL ATTRIBU
;
;C'est quand on vit seul qu'on a le plus besoin d'un lit a deux places.
;

; ********************************
; Player modifie par F-Key/RevivaL
; (03/2010, ca fait un peu tard !)
;
; - Ajout du support pour les digidrums Atari
; - Attention, le code et les donnees digidrums ne doivent 
;   pas se trouver entre #4000-#7FFF (Asic) 
; ********************************


;ORG	#8000
;write "player.bin"

; Jump table
JP INIT
JP PLAYFRAME
JP PLAYVBL

.trem db 16

; Constantes
;DECRUBUF	EQU	&C000
;YMLZ		EQU	#A800		; Adresse fichier compacte
YMLZ		EQU	AYC_START	; Adresse fichier compacte
DUREE		EQU	YMLZ		; Indiquee dans le header
AYC_REGS	EQU	16
DMACHANNEL0	EQU	#6C00
DMACHANNEL1	EQU	#6C04
DMACHANNEL2	EQU	#6C08
DMACONTROL	EQU	#6C0F
;SIDMIDRES	EQU	1
SELECT_REG	EQU 	&C0
SELECT_DATA	EQU 	&80
PPI_PORTA	EQU 	&F4
PPI_PORTC	EQU 	&F6

macro OUT_C_0
db	&ED,&71
mend

; mute counters
.mute_count db 0,0,0

; This table contains the values extracted from the AYC file
; It's processed to find special FXs and registers and is further copied 
; to the frame_start buffer
; the register number is shifted left once and bit 0 if set means that this register needs to be ignored
; i.e. dw &0307 means that we shouldn't set the 07 value to register 1
.preFrame
.reg0	dw &0000
.reg1	dw &0200	; voice number + fx type + high period 0
.reg2	dw &0400
.reg3	dw &0600	; voice number + fx type + high period 1
.reg4	dw &0800
.reg5	dw &0A00
.reg6	dw &0C00
.reg7	dw &0E00
.reg8	dw &1000	; tp1 + (volume | sample | vmax | SBenv)
.reg9	dw &1200	; tp2 + (volume | sample | vmax | SBenv)
.regA	dw &1400
.regB	dw &1600
.regC	dw &1800
.regD	dw &1A00
.regE	db &00		; tc1
.regF	db &00		; tc2

dw 0	; bug ??

;***********
;* PLAYVBL *
;***********
PLAYVBL
	; This function should be very quick
	; it sets the DMA Addresses

	; Connect Asic
	ld bc,&7FB8		; [3]
	out (c),c		; [4]

.DMACTL
	ld b,0			; [2]
	ld a,b			; [1]
	
	rra			; [1]
	jr nc,DMA0ADDEnd	; [2,3]
.DMA0ADD
	ld hl,0			; [3]
	ld (DMACHANNEL0),hl	; [5]
.DMA0ADDEnd

	rra			; [1]
	jr nc,DMA1ADDEnd	; [2,3]
.DMA1ADD
	ld hl,0			; [3]
	ld (DMACHANNEL1),hl	; [5]
.DMA1ADDEnd

	rra			; [1]
	jr nc,DMA2ADDEnd	; [2,3]
.DMA2ADD
	ld hl,0			; [3]
	ld (DMACHANNEL2),hl	; [5]
.DMA2ADDEnd

	ld hl,DMACONTROL	; [3]
	ld a,%11111100		; [2] 
	and (hl)		; [2]
	or b			; [1]
	ld (hl),a		; [2]

	; Deconnect Asic
	ld bc,&7FA0		; [3]
	out (c),c		; [4]

	; *****************************
	; play PSG using old PPI method
	; *****************************
	ld hl,14*2 + preFrame - 1

	; speed
	ld e,PPI_PORTA

	; index
	ld d,14

	.loop_psg
	; get reg number
	ld a,(hl)				; [2]

	; should we set this reg
	sra a					; [2]
	ld (hl),a
	dec hl					; [2]
	jp c,endLoopPsg

	; select reg (bug on cpc+)
	;ld bc,PPI_PORTC*256 + SELECT_REG	; [3]
	;out(c),c				; [4]
	;ld b,e					; [1]	
	;out(c),a				; [4]
	;ld b,PPI_PORTC				; [2]
	;OUT_C_0					; [4] wait

	ld b,e
	out(c),a
	ld bc,PPI_PORTC*256 + SELECT_REG	; [3]
	out(c),c				; [4]
	OUT_C_0

	; get value
	ld a,(hl)				; [2]

	; set value
	ld b,e					; [1]
	out(c),a				; [4]
	ld bc,PPI_PORTC*256 + SELECT_DATA	; [3]
	out(c),c				; [4]
	OUT_C_0

	.endLoopPsg
	dec hl					; [2]
	dec d
	jp nz,loop_psg

	ret

;********
;* INIT *
;********
INIT
	DI
	EXX
	EX AF,AF'
	PUSH AF
	PUSH BC
	PUSH DE
	PUSH HL
	
	;CALL delock_asic
	
	;CALL	LOAD
	CALL	READHEAD
	CALL	ATTRIBU
	CALL	CREEPLAY
	;CALL	POKECODE
	CALL	RAZVAR
	CALL	RAZ_PSG
;
;Il faut preparer quelques donnees d'avances
;
	LD	A,(NBR_REG)
AMORCE
	PUSH	AF
	CALL	GETREG
	POP	AF
	DEC	A
	JR	NZ,AMORCE
	POP HL
	POP DE
	POP BC
	POP AF
	EX AF,AF'
	EXX	
	EI	
	RET
;
;
;
;*************
;* PLAYFRAME *
;*************
PLAYFRAME
	;nop
	;DI
	EXX
	EX AF,AF'
	PUSH AF
	PUSH BC
	PUSH DE
	PUSH HL
	CALL	GETREG

	repeat 0
	ld b,60
	djnz $
	rend

	CALL	PLAYREG
.afterPlayReg
	POP HL
	POP DE
	POP BC
	POP AF
	EX AF,AF'
	EXX
	;EI	
	RET

;
;
;
RAZ_PSG
;
;La routine de PLAY ne met pas a jour les registres s'ils ne sont pas modifies.
;Or, ils sont consideres a 0 lors de la 1er interation.
;On doit donc reelement les mettre a 0. (nb 7 a 0  canaux et bruit ouverts)

	; RESET values to zero
	ld hl,preFrame
	ld b,AYC_REGS
	xor a
.loop_reset
	ld (hl),a
	inc hl
	inc hl
	djnz loop_reset	
	RET

;
GETREG
	DI
	LD	(SAVETMP+1),SP
	LD	A,(NBR_REG)	;Necessaire pour V1 a V15
	DEC	A		;mais pas pour VMESURE/V0
	DB	#DD
DECALEH	LD	H,0
	LD	IY,GET_RET
GETWITCH	JP	V0	;Quelle routine ?
GET_RET
	LD	HL,(GETWITCH+1)
	DEC	HL
	LD	D,(HL)	;Recupere adresse
	DEC	HL
	LD	E,(HL)
	LD	(GETWITCH+1),DE
SAVETMP	LD	SP,0
	EI
	RET
;
;
	DW	V1	;Adresse prochaine routine
VMESURE
;
;S'il reste moins que NBR_REG donnees a recuperer,
;on recupere ces quelques donnees, on reset (methode brute pour l'instant),
;puis on recupere qq donnees complementaire.
;
;Sinon, on saute normallement a V0, et le test ne sera pas effectue
;de V1 a V15 ...
;
MESURE	LD	HL,0
	LD	C,L
	LD	D,0
	LD	A,(NBR_REG)
	LD	E,A
	OR	A
	SBC	HL,DE
	LD	(MESURE+1),HL
	DEC	A
	JP	NC,V0
;
;Pour V1 etc..., on ne refera pas le test precedent
;
	; debug
	;ld a,0:;org $-1:;ret
	;ld (PLAYFRAME),a	; STOP music ?

	;xor a
	;ld (DMACTL+1),a		; STOP DMA
	
	;call RAZ_PSG		; reset next PSG frame	
	; RESET values to zero
	;ld hl,preFrame
	;ld b,AYC_REGS
	;xor a			; reset carry
	;.loop_reset_end
	;	ld (hl),a
	;	inc hl		; flags not affected
	;	rl (hl)
	;	inc hl
	;	djnz loop_reset_end

	;pop hl			; ignore last ret
	;jp afterPlayReg		; jump to end of PlayFrame

	LD	B,D
	LD	DE,RETIENT
	LD	(GETWITCH+1),DE
	LD	DE,(DUREE)
	ADD	HL,DE
	LD	(MESURE+1),HL
	LD	A,C
	LD	(COMPLETE+1),A
	LD	(RETIENT+1),A
;
;On doit determiner la position destination dans les buffers
;
	LD	HL,(PLAYPLAG+1)
	ADD	HL,BC
	LD	A,(NBR_REG)
	LD	C,A
	ADD	HL,BC
	LD	A,H
	AND	#3
	LD	(DECALEH+1),A
	LD	A,L
	LD	(DECALEL+1),A
;
RETIENT	LD	A,0
	DEC	A
	JP	M,CAS0	;0 data ? Il faut reseter
	LD	IY,GET_RET_
GETWITC_	JP	V0
;
CAS0
	LD	A,(NBR_REG)
	JR	REZET
;
GET_RET_
	LD	A,(NBR_REG)
COMPLETE	LD	B,0
	SUB	B
;
REZET
	LD	HL,(GETWITC_+1)
	INC	HL
	LD	E,(HL)	;Plage de variable
	INC	HL
	LD	D,(HL)
	INC	DE
	INC	DE
	INC	DE
	INC	DE
	LD	HL,REGSCOPY-REGS+1
	ADD	HL,DE
	EX	DE,HL
DECALEL	LD	(HL),0
	INC	HL
	EX	DE,HL
	LDI
	LDI
	LDI
	LDI
	LDI
;
	LD	IY,GET_RET2
	DEC	A
	JR	GETWITC_
GET_RET2
	LD	HL,(GETWITC_+1)
	DEC	HL
	LD	D,(HL)	;Recupere adresse
	DEC	HL
	LD	E,(HL)
	LD	HL,-VMESURE-1	;Pour redirection, il faut boucler sur
	ADD	HL,DE	;V0
	JR	C,REGLP_OK
	LD	HL,VMESURE
	LD	(GETWITCH+1),DE
	LD	DE,V0
REGLP_OK
	LD	(GETWITC_+1),DE
	LD	SP,(SAVETMP+1)
	EI
	RET
;
;
	DW	V1
V0
	LD	SP,REGS	;!!! PLACER le LD SP apres le label !!!
VROUT	JP	DECOMP4	;ATTENTION ! L'ecart doit rester
	DW	V2
V1
	LD	SP,REGS+10
VROUT_	JP	DECOMP0	;constant pour les modifs d'ATTRIBU
	DW	V3
V2
	LD	SP,REGS+20
	JP	DECOMP4
	DW	V4
V3
	LD	SP,REGS+30
	JP	DECOMP0
	DW	V5
V4
	LD	SP,REGS+40
	JP	DECOMP4
	DW	V6
V5
	LD	SP,REGS+50
	JP	DECOMP0
	DW	V7
V6
	LD	SP,REGS+60
	JP	DECOMP0
	DW	V8
V7
	LD	SP,REGS+70
	JP	DECOMP0
	DW	V9
V8
	LD	SP,REGS+80
	JP	DECOMP0
	DW	V10
V9
	LD	SP,REGS+90
	JP	DECOMP0
	DW	V11
V10
	LD	SP,REGS+100
	JP	DECOMP0
	DW	V12
V11
	LD	SP,REGS+110
	JP	DECOMP0
	DW	V13
V12
	LD	SP,REGS+120
	JP	DECOMP0
	;DW	VMESURE	;!!! BOUCLE EN CONCORDANCE AVEC NBR_REG
	DW	V14
V13
	LD	SP,REGS+130
	JP	DECOMP0
	DW	V15
V14
	LD	SP,REGS+140
	JP	DECOMP0
	;DW	V0
	DW	VMESURE	;!!! BOUCLE EN CONCORDANCE AVEC NBR_REG
V15
	LD	SP,REGS+150
	JP	DECOMP0
;
;
D0_CHR
;
;Place  en premier pour etre atteint par JR
;
	EX	AF,AF'
	LD	A,(HL)
	INC	HL
	EXX
	LD	(DE),A
	INC	E
	EX	AF,AF'
;
;On decremente nbr de caracteres restants.
;
	DEC	A
	EXX
	JP	P,D0_NEXT
;
;
	PUSH	HL
	PUSH	BC
	EXX
;
	PUSH	BC	;B DOIT ETRE NUL ICI
	PUSH	HL	;Bidon
	PUSH	DE
	JP	(IY)
;
;
DECOMP0
;
;Entree  A  = nbr de donnees a decompacter - 1
;          IY = adr de retour
;On suppose que longueur est code en negatif (ie -2 -> 2 caracteres)
;
;On recupere adr destination dans tous les cas
;(Remarque D ne change pas, il y a peut etre moyen d'optimiser cela)
;
	POP	DE
	POP	HL	;Adresse source pour copie chaine
;
;On recupere B = nbr de caracteres restant a copier (d'une précédente Séquence dans le buffer de decompression)
;C est inutilise
; 
;
	POP	BC
	INC	B
	DEC	B
	JR	Z,D0_FLAG
;
D0_MESUR
;
;On regarde si longueur de chaine restante > nbr de donnees a fournir
;
	EXX
	LD	D,A
	EXX
	ADD	A,B	;longueur codee en negatif
	JR	NC,D0_AL_
	EX	AF,AF'
D0_LP1
	LD	A,(HL)
	INC	L
	LD	(DE),A
	INC	E
	INC	B
	JR	NZ,D0_LP1
	EX	AF,AF'
;
D0_FLAG
;
;On recupere FLAGs et pointeur donnees compressees
;(B inutilise)
;
	EXX
	POP	BC
	POP	HL
;
;
;On extrait nouveau flag
;
D0_NEXT
	SLA	C
	JR	NZ,D0_FLGOK
;
	LD	C,(HL)
	INC	HL
	DB	#CB,#31	;SLL C
D0_FLGOK
	JR	NC,D0_CHR
;
;Test similaire au precedent
;
	LD	B,(HL)
	INC	HL
	LD	D,A	;Sauve pour D0_LEFT
	ADD	A,B
	JR	NC,D0_LEFT
;
;Il restera (A+1) donnees a fournir apres copie de la chaine
;
	EX	AF,AF'
	LD	A,B
	EXX
	LD	B,A
	EXX
	LD	A,(HL)
	INC	HL
	EXX
	ADD	A,C
	LD	L,A
D0_LP2
	LD	A,(HL)
	INC	L
	LD	(DE),A
	INC	E
	INC	B
	JR	NZ,D0_LP2
	EX	AF,AF'
	EXX
	JR	D0_NEXT
;
D0_LEFT
;
;Idem que D0_ALL mais sur moins de donnees.
;
	EX	AF,AF'	;Pour l'instant on conserve A-B
	LD	A,D	;Nombre de valeur restantes a copier-1
	EXX
	LD	B,A
	INC	B
	EXX
	LD	A,(HL)
	INC	HL
	PUSH	HL
	PUSH	BC
	EXX
	ADD	A,C
	LD	L,A
D0_LP3
	LD	A,(HL)
	INC	L
	LD	(DE),A
	INC	E
	DJNZ	D0_LP3
	EX	AF,AF'
	LD	B,A
	INC	B	;Longueur restante pour prochaine fois
	PUSH	BC
;
	PUSH	HL
	PUSH	DE
	JP	(IY)
;
D0_AL_
;
;  D0_ALL ne convient pas quand on veut changer dynamiquement le nombre
;  de valeurs a recuperer (c'est le cas pour le bouclage).
;
	INC	A
	LD	B,A
	PUSH	BC
	EXX
	LD	A,D
	EXX
	LD	B,A
	INC	B
;
D0_AL_LP	LD	A,(HL)
	LD	(DE),A
	INC	L
	INC	E
	DJNZ	D0_AL_LP
;
	PUSH	HL
	PUSH	DE
	JP	(IY)
;
;
;
D4_CHR
;
;Place  en premier pour etre atteint par JR
;
	EX	AF,AF'
	LD	A,(HL)
	INC	HL
	EXX
	LD	(DE),A
	INC	DE
	RES	2,D
	EX	AF,AF'
;
;On decremente nbr de caracteres restants.
;
	DEC	A
	EXX
	JP	P,D4_NEXT
;
;
	PUSH	HL
	PUSH	BC
	EXX
;
	PUSH	BC	;B DOIT ETRE NUL ICI
	PUSH	HL	;Bidon
	PUSH	DE
	JP	(IY)
;
;
DECOMP4
;
;Base sur DECOMP0
;Entree  A  = nbr de donnees a decompacter - 1
;          IY = adr de retour
;On suppose que longueur est code en negatif (ie -2 -> 2 caracteres)
;
;On recupere adr destination dans tous les cas
;(Remarque D ne change pas, il y a peut etre moyen d'optimiser cela)
;
	POP	DE
	POP	HL	;Adresse source pour copie chaine
;
;On recupere B = nbr de caracteres a copier   C est inutilise
;
	POP	BC
	INC	B
	DEC	B
	JR	Z,D4_FLAG
;
D4_MESUR
;
;On regarde si longueur de chaine restante > nbr de donnees a fournir
;
	EXX
	LD	D,A
	EXX
	ADD	A,B	;longueur codee en negatif
	JR	NC,D4_AL_
	EX	AF,AF'
D4_LP1
	LD	A,(HL)
	INC	HL
	RES	2,H
	LD	(DE),A
	INC	DE
	RES	2,D
	INC	B
	JR	NZ,D4_LP1
	EX	AF,AF'
;
D4_FLAG
;
;On recupere FLAGs et pointeur donnees compressees
;(B inutilise)
;
	EXX
	POP	BC
	POP	HL
;
;
;On extrait nouveau flag
;
D4_NEXT
	SLA	C
	JR	NZ,D4_FLGOK
;
	LD	C,(HL)
	INC	HL
	DB	#CB,#31	;SLL C
D4_FLGOK
	JR	NC,D4_CHR
;
;Test similaire au precedent
;
	LD	B,(HL)
	INC	HL
	LD	D,A	;Sauve pour D4_LEFT
	ADD	A,B
	JR	NC,D4_LEFT
;
;Il restera (A+1) donnees a fournir apres copie de la chaine
;
	EX	AF,AF'
	LD	A,B
	EXX
	LD	B,A
	EXX
	LD	A,(HL)
	INC	HL
	EXX
	ADD	A,C
	LD	L,A
	LD	A,D
	RES	0,A
	RES	1,A
	EXX
	ADC	A,(HL)
	DB	#DD
	ADD	A,H
	AND	#FB
	INC	HL
	EXX
	LD	H,A
D4_LP2
	LD	A,(HL)
	INC	HL
	RES	2,H
	LD	(DE),A
	INC	DE
	RES	2,D
	INC	B
	JR	NZ,D4_LP2
	EX	AF,AF'
	EXX
	JR	D4_NEXT
;
D4_LEFT
;
;Idem que D4_ALL mais sur moins de donnees.
;
	EX	AF,AF'	;Pour l'instant on conserve A-B
	LD	A,D	;Nombre de valeur restantes a copier-1
	EXX
	LD	B,A
	INC	B
	EXX
	LD	A,(HL)
	INC	HL
	EXX
	ADD	A,C
	LD	L,A
	LD	A,D
	RES	0,A
	RES	1,A
	EXX
	ADC	A,(HL)
	DB	#DD
	ADD	A,H
	AND	#FB
	INC	HL
	PUSH	HL
	PUSH	BC
	EXX
	LD	H,A
D4_LP3
	LD	A,(HL)
	INC	HL
	RES	2,H
	LD	(DE),A
	INC	DE
	RES	2,D
	DJNZ	D4_LP3
	EX	AF,AF'
	LD	B,A
	INC	B	;Longueur restante pour prochaine fois
	PUSH	BC
;
	PUSH	HL
	PUSH	DE
	JP	(IY)
;
D4_AL_
;
;  D0_ALL ne convient pas quand on veut changer dynamiquement le nombre
;  de valeurs a recuperer (c'est le cas pour le bouclage).
;
	INC	A
	LD	B,A
	PUSH	BC
	EXX
	LD	A,D
	EXX
	LD	B,A
	INC	B
;
D4_AL_LP	LD	A,(HL)
	LD	(DE),A
	INC	HL
	RES	2,H
	INC	DE
	RES	2,D
	DJNZ	D4_AL_LP
;
	PUSH	HL
	PUSH	DE
	JP	(IY)
;
;
;
READHEAD
;
;On va   analyser le header
;
	LD	HL,(DUREE)
	LD	(MESURE+1),HL
;
	RET
;
;
ATTRIBU
;
;On reparti les tampons de decompressions. Ceux de #400 de long se placent
;     en #?000 ou #?800 pour faciliter le modulo, et la routine intercale
;ceux de #100 dans les trous (pile poil).
;
;On place d'abord ceux de #400
;
	LD	HL,OFBUF0
	LD	D,DECRUBUF/#100
	EXX
	LD	HL,(ADRTEMP)
	INC	HL
	INC	HL	;Flag Decomp400 ou Decomp100
	PUSH	HL
	LD	DE,3
	LD	A,(NBR_REG)
	LD	B,A	;B=cpt loop, C = nbr de buffer400
	LD	C,0
ATT_LP
	LD	A,(HL)
	CP	1
	JR	Z,ATT_BUF1
	EXX
	LD	(HL),D
	INC	HL
	LD	(HL),4
	DEC	HL
	LD	A,D
	ADD	A,8
	LD	D,A
	EXX
	INC	C
ATT_BUF1
	EXX
	INC	HL
	INC	HL
	EXX
	ADD	HL,DE
	DJNZ	ATT_LP
;
;Maintenant on va placer les buffer100
;
	LD	HL,OFBUF0
	LD	D,DECRUBUF/#100
	LD	B,3	;Pour intercaler 4 buffer100
	EXX
	POP	HL
	PUSH	HL
	LD	DE,3
	LD	A,(NBR_REG)
	LD	B,A
ATT_LP2
	LD	A,(HL)
	CP	4	;On l'a deja traite
	JR	Z,ATT_BUF4
	EXX
	LD	A,B
	INC	A
	AND	3
	LD	B,A
	JR	NZ,ATT_OK	;On est pas sur une adr congrue a #400
	LD	A,C
	OR	A
	JR	Z,ATT_OK	;On a passe tout les buffer #100
	DEC	C
	LD	A,D	;Sinon on saute buffer #400
	ADD	A,4
	LD	D,A
ATT_OK	LD	(HL),D
	INC	HL
	LD	(HL),1
	DEC	HL
	INC	D
	EXX
ATT_BUF4
	EXX
	INC	HL
	INC	HL
	EXX
	ADD	HL,DE
	DJNZ	ATT_LP2
;
;Un dernier passage pour passer les bons JUMP
;
	LD	HL,VROUT+1
	LD	BC,VROUT_-VROUT-1
	EXX
	POP	HL
	LD	DE,3
	LD	A,(NBR_REG)
	LD	B,A
ATT_LP3
	LD	A,(HL)
	CP	1
	EXX
	LD	DE,DECOMP0
	JR	Z,ATT_R1
	LD	DE,DECOMP4
ATT_R1
	LD	(HL),E
	INC	HL
	LD	(HL),D
	ADD	HL,BC
	EXX
	ADD	HL,DE
	DJNZ	ATT_LP3
	RET
;
;POKECODE
;
;Code bon nombre de LDIs dans routines de decompression
;
;	RET
;

; Register 13 specific
MODELE_13
	LD A,(DE)
	INC A
	SCF
	JR Z,MO_SAME_13		; zero set if value was 0xFF
	DEC A
ENDMODELE_13
	OR	A		; copy of MO_SUITE
	LD	(HL),A		; to match the relative jump position
MO_SAME_13

; TODO
; Register 14 and 15 are set everytime
MODELE_14
	LD	A,(DE)
	LD	(HL),A		; value
	INC	HL
MO_ENDIF_14

MODELE
	LD	A,(DE)
	CP	(HL)
	SCF			; [1] set carry flag
	JR	Z,MO_SAME
MO_SUITE
	OR	A		; [1] reset carry flag	
	LD	(HL),A		; value

MO_SAME
	INC	HL
	RL	(HL)		; [2] double reg number + carry
	INC	HL
MO_ENDIF
;
MO2
	INC	BC
	LD	A,B
	AND	#3
	LD	B,A
	LD	(PLAYPLAG+1),BC

	; reset all dmas
	;ld a,%00000001	; force dma0 hack iceage
	xor a
	ld (DMACTL+1),a

	; Force volumes to be send to the PSG
	; They may be disabled by SID SubFunctions
	ld hl,reg8+1:res 0,(hl)
	ld hl,reg9+1:res 0,(hl)
	ld hl,regA+1:res 0,(hl)

; HACK TAO - seagulls
; increase left and right volumes
ld hl,reg8
inc (hl)
ld hl,regA
inc (hl)
;ld hl,reg7
;res 1,(hl)
;res 4,(hl)
; END HACK



	;halt
	;halt

	; prepare DMAs for Atari SFXs
	call testSFXs	

	ret	; comment this out for DIGIDRUMS !

	; Shall we mute a voice (i.e. digidrum) ?
	call muteVoice

	ret

	; DEBUG
	; - disable all mixers
	; - set all volumes to zero
	; - let only C
	;ld hl,reg7
	;ld a,%110110
	;ld a,%111111
	;or (hl)
	;ld (hl),a
	;ld hl,reg7+1:;res 0,(hl)
	;ld hl,reg8+1;res 0,(hl)
	;ld hl,reg9+1:;set 0,(hl)
	;ld hl,regA+1:;set 0,(hl)

	; BORDER raster
	;ld bc,&7F10
	;out (c),c
	;ld c,&40+10	; jaune vif
	;out (c),c

	ret

	; copy preFrame to frame
	;ld hl,14*2 + preFrame - 1
	;ld de,14*2 + frame - 1
	;ld bc, 14*256 + 28 ; (28 = 14*2)
	;.copy_frame
	;	; num reg
	;	ld a,(hl)
	;	srl a		; a/2
	;	ld (hl),a
	;	dec hl
	;	; Play register when carry is null
	;	jr nc,reg_to_dma0
	;		ld a,&40
	;		ld (de),a	
	;		dec de
	;		xor a
	;		ld (de),a
	;		dec de
	;		dec hl
	;		djnz copy_frame
	;		jr end_copy_frame
	;	.reg_to_dma0
	;		ld (de),a
	;		dec de
	;		ldd
	;		djnz copy_frame	
	;	.end_copy_frame
	;RET
MO2_ENDIF
;
;
PLAYREG
PLAYPLAG	LD	BC,0	;Fourni poids faible (sur 8 et 10 bits)
				; PLAYPLAG est remis a jour la fin
	LD	L,C
	LD	DE,preFrame
	XOR	A	; reg 0
	EX	HL,DE

PLAYCODE	DS	MO_ENDIF-MODELE+4*AYC_REGS
		DS	MO2_ENDIF-MO2


;IN A=VMAX
;Corrupted E
.shallWeMuteSid
	ret
	ld e,16
	cp e
	ret M
	ld a,e
	ret

; *********
; Test SFXs
; *********
; Corrupted AF,BC,DE,HL,IX,AF'
.testSFXs
	; hack for iceage
	;ld hl,prepFX1
	;push hl

	; Set values for 1st FX
	;ld hl,DMA0ADD+1
	;ld (DMAChannelAddress+2),hl

	;ld hl,0 ; org $-2 ; set 0,(hl)
	;ld (DMAChannelEnableCmd),hl
	
	;ld a,(reg5)	
	;jp startTestSFX
.prepFX1
	; set callback address (ret)
	ld hl,prepFX2
	push hl

	; Set values for 1st FX
	ld hl,DMA1ADD+1
	ld (DMAChannelAddress+2),hl

	ld hl,0 : org $-2 : set 1,(hl)
	ld (DMAChannelEnableCmd),hl

	ld a,(regE)
	ld (reg14or15+1),a
	
	ld a,(reg1)	
	jp startTestSFX
	
.prepFX2
	; Set values for 2nd FX
	; We use DMA0 so that DMA2 is free for INTs
	ld hl,DMA0ADD+1
	ld (DMAChannelAddress+2),hl

	ld hl,0 : org $-2 : set 0,(hl)
	ld (DMAChannelEnableCmd),hl

	ld a,(regF)
	ld (reg14or15+1),a

	ld a,(reg3)

.startTestSFX
	; A contains the reg1 or reg3 value
	; Bits 7,6 of reg1 or reg3 gives the voice to play fx
	rlca
	rlca
	ld b,a		; save value
	and %00000011
	ret z		; a=voice+1, zero means no fx

	ld c,a
	dec c		; voice in c

	;hack only play sid in C
	;cp 2
	;ret nz

	; fx detected on voice c
	; test fx's type (in rega bits 7,6)
	ld a,b
	and %11000000
	jp z,sidvoice	; 00
	cp %01000000
	jp z,digidrum	; 01
	cp %10000000
	jp z,sidvoice	; 10 sinussid
	; else Sync buzzer 11
	;ret
	nop

.sidvoice
	; get the register number (regC + 8)
	ld a,8
	add c	
	ld c,a ; c contains the register to sid

	; get sample number, VMAX or ENV and get high nibble of rate step (xyy)
	ld hl,preFrame
	ld a,c
	add a
	ld e,a
	ld d,0
	add hl,de
	; HL points either on reg 8, 9 or 10 value

	; get VMAX
	ld a,(hl)
	ld d,a		; save tp
	and &1F
	call shallWeMuteSid
	ex af,af'	; a' contains VMAX

	; Get sid number in de (11bits)
	ld a,%11100000
	and d		; a<-d and reset carry
	rlca
	rlca
	rlca
	ld d,a
.reg14or15
	ld e,0	; reg14 or reg15 value (replaced before call)

	; get semiperiod in de
	ex hl,de
	ld de,SIDList
	add hl,hl
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)

	; We still have B = shifted reg1(3) value
	; so we can check for sinussid
	bit 7,b
	jr z,sidclassic
	;ret

	; mult semiperiod by 4 ???
	ex hl,de
	add hl,hl
	add hl,hl
	ex hl,de

	; simulate sinussid using a sidvoice
	; but a sinsid is a 8 values sample
	; wich means that a semiperiod is 4 samples value
	; so we should divide the frequency by 4
	;srl d
	;rr e 
	;srl d
	;rr e

	.sidclassic
	; !!!
	; At this point we should have
	; A' = VMAX
	; C = register (8,9,A)
	; DE = Step	
	; simulate call with different return address
	ld hl,DMAChannelAddress:push hl
	ld a,10
	sub c
	jp z,sidVoiceC
	dec a
	jp z,sidVoiceB
	jp sidVoiceA
	ret

.digidrum
	; get sample number, VMAX or ENV and get high nibble of rate step (xyy)
	ld hl,reg8
	ld a,c
	add a
	ld e,a
	ld d,0
	add hl,de
	; HL points either on reg 8, 9 or 10 value

	; get sample number
	; we don't care about the rate for digidrums ;)
	ld a,(hl)
	; A contains either reg 8,9 or A sample number + high nibble of the rate step
	
	and &1F
	add a
	ld e,a		; d has already been reset by the code above
	ld hl,DIGITABLE
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)	; de points on the digidrum size address'

	; first thing to do is to set the counter that will mute the voice off
	; for the length of the digidrum
	ld hl,mute_count
	ld b,0		; TODO optimize (d = zero in the previous code)
	add hl,bc	; hl points on the mute offset

	; get the mute counter and set it for the voice
	ld a,(de)
	ld (hl),a

	; set Drum Address
	inc de

.DMAChannelAddress
	ld (DMA1ADD),de
	
	; enable DMA1
	ld hl,DMACTL+1
.DMAChannelEnableCmd
	set 1,(hl)
	ret

; *********
; sidVoiceA
; *********
; in A' = VMAX
; in C = reg (8,9,A)
; in DE = SemiPeriod en base 256 (i.e, max = 255,255)
; out DE = AYL address
.sidVoiceA
ifnot SIDA_MAXRATE
	ret
else
	.currFrameA
	ld bc,frameA0Init	; AYL Init Address

	;DMA Delay because of previous DMA ?
	.delaySidA
	ld a,0		; a <- HBL to pause
	ld (bc),a
	inc bc
	inc bc
		
	ex af,af'
	ld ixl,a	; ixl <- VMAX
	.sidVolInitA
	ld a,0		; a <- oldVMAX
	or a
	jr z,noresetA	; if oldVMAX != 0 
	ld a,ixl	; then a <- VMAX
	.noresetA
	ld (bc),a
	inc bc
	inc bc
	ex af,af'

	; compute sidvoice on that frame
	; in a' = Vmax or zero
	; in a = nbHL already rendered in Init
	exx
	ld h,0
	ld l,a			; hl' = count nbHBL done in init
	ld d,h			; d' = 0
	ld c,56			; c' = 312-256=56
.loopSidA
	exx

	.remainingA
	ld a,0			; remaining 256th of HBL
	add e			; add semi-period 256th
	ld (remainingA+1),a	; save next remaining
		
	rla			; get carry
	and %00000001		; in bit 0
	add d			; add d (a = nbHBL for this sid/swap)

	exx
	ld e,a			; e' = nbHBL for this sid/swap
	exx

	dec a			; set pause
	ld (bc),a		; to nbHL-1
	inc bc
	inc bc

	ex af,af'
	xor ixl			; swap and set
	ld (bc),a		; VMAX or zero
	ex af,af'
	inc bc
	inc bc

	; loop check
	exx
	add hl,de
	bit 0,h
	jp z,loopSidA		; if hl<256 loop
	ld a,l
	sub c	
	jp c,loopSidA		; if l>56 loop
	exx

	; here we have rendered the sid and may have remaining HBL
	; 1 - get remaining HBLs (a)
	; 2 - set next VMAX ? (a')
	; a=0 means no delay, nextVol should be (a')
	; a>0 means delay, nextVol should be (a')
		
	; IMPORTANT
	; It seems that the previous pause continues on next frame
	; even if the DMA Address is reset
	; So we should cut the last pause command so that the ayl matches 312 HBL exactly

	; save delay
	ld (delaySidA+1),a

	; cut last pause to match exactly 312 HBLs
	neg		; a=-a
	ld hl,-4
	add hl,bc	; previous pause address >=a
	add (hl)
	ld (hl),a		
		
	; save next vmax
	ex af,af'
	ld (sidVolInitA+1),a

	; don't set PSG frame volume
	ld hl,reg8+1
	set 0,(hl)

	; swap frames
	.nextFrameA
	ld hl,frameA1Init
	ld de,(currFrameA+1)
	ld (currFrameA+1),hl
	ld (nextFrameA+1),de

	; Here we should handle the case when the pre-pause in zero
	ld a,(de)
	or a
	ret nz
	inc de
	inc de
	ret
endif

; *********
; sidVoiceB
; *********
; in A' = VMAX
; in C = reg (8,9,A)
; in DE = SemiPeriod en base 256 (i.e, max = 255,255)
; out DE = AYL address
.sidVoiceB
ifnot SIDB_MAXRATE
	ret
else
	.currFrameB
	ld bc,frameB0Init	; AYL Init Address

	;DMA Delay because of previous DMA ?
	.delaySidB
	ld a,0		; a <- HBL to pause
	ld (bc),a
	inc bc
	inc bc
		
	ex af,af'
	ld ixl,a	; ixl <- VMAX
	.sidVolInitB
	ld a,0		; a <- oldVMAX
	or a
	jr z,noresetB	; if oldVMAX != 0 
	ld a,ixl	; then a <- VMAX
	.noresetB
	ld (bc),a
	inc bc
	inc bc
	ex af,af'

	; compute sidvoice on that frame
	; in a' = Vmax or zero
	; in a = nbHL already rendered in Init
	exx
	ld h,0
	ld l,a			; hl' = count nbHBL done in init
	ld d,h			; d' = 0
	ld c,56			; c' = 312-256=56
.loopSidB
	exx

	.remainingB
	ld a,0			; remaining 256th of HBL
	add e			; add semi-period 256th
	ld (remainingB+1),a	; save next remaining
		
	rla			; get carry
	and %00000001		; in bit 0
	add d			; add d (a = nbHBL for this sid/swap)

	exx
	ld e,a			; e' = nbHBL for this sid/swap
	exx

	dec a			; set pause
	ld (bc),a		; to nbHL-1
	inc bc
	inc bc

	ex af,af'
	xor ixl			; swap and set
	ld (bc),a		; VMAX or zero
	ex af,af'
	inc bc
	inc bc

	; loop check
	exx
	add hl,de
	bit 0,h
	jp z,loopSidB		; if hl<256 loop
	ld a,l
	sub c	
	jp c,loopSidB		; if l>56 loop
	exx

	; here we have rendered the sid and may have remaining HBL
	; 1 - get remaining HBLs (a)
	; 2 - set next VMAX ? (a')
	; a=0 means no delay, nextVol should be (a')
	; a>0 means delay, nextVol should be (a')
		
	; IMPORTANT
	; It seems that the previous pause continues on next frame
	; even if the DMA Address is reset
	; So we should cut the last pause command so that the ayl matches 312 HBL exactly

	; save delay
	ld (delaySidB+1),a

	; cut last pause to match exactly 312 HBLs
	neg		; a=-a
	ld hl,-4
	add hl,bc	; previous pause address >=a
	add (hl)
	ld (hl),a		
		
	; save next vmax
	ex af,af'
	ld (sidVolInitB+1),a

	; don't set PSG frame volume
	ld hl,reg9+1
	set 0,(hl)

	; swap frames
	.nextFrameB
	ld hl,frameB1Init
	ld de,(currFrameB+1)
	ld (currFrameB+1),hl
	ld (nextFrameB+1),de

	; Here we should handle the case when the pre-pause in zero
	ld a,(de)
	or a
	ret nz
	inc de
	inc de
	ret
endif

; *********
; sidVoiceC
; *********
; in A' = VMAX
; in C = reg (8,9,A)
; in DE = SemiPeriod en base 256 (i.e, max = 255,255)
; out DE = AYL address
.sidVoiceC
ifnot SIDC_MAXRATE
	ret
else
	; Hack, log the delay in &4000
	;.log
	;ld bc,&4000
	;ld a,(delaySidC+1)
	;ld (bc),a
	;inc bc
	;ld (log+1),bc

	.currFrameC
	ld bc,frameC0Init	; AYL Init Address

	;DMA Delay because of previous DMA ?
	.delaySidC
	ld a,0		; a <- HBL to pause
	ld (bc),a
	inc bc
	inc bc
		
	ex af,af'
	ld ixl,a	; ixl <- VMAX
	.sidVolInitC
	ld a,0		; a <- oldVMAX
	or a
	jr z,noresetC	; if oldVMAX != 0 
	ld a,ixl	; then a <- VMAX
	.noresetC
	ld (bc),a
	inc bc
	inc bc
	ex af,af'

	; compute sidvoice on that frame
	; in a' = Vmax or zero
	; in a = nbHL already rendered in Init
	exx
	ld h,0
	ld l,a			; hl' = count nbHBL done in init
	ld d,h			; d' = 0
	ld c,56			; c' = 312-256=56
.loopSidC
	exx

	.remainingC
	ld a,0			; remaining 256th of HBL
	add e			; add semi-period 256th
	ld (remainingC+1),a	; save next remaining
		
	rla			; get carry
	and %00000001		; in bit 0
	add d			; add d (a = nbHBL for this sid/swap)

	exx
	ld e,a			; e' = nbHBL for this sid/swap
	exx

	dec a			; set pause
	ld (bc),a		; to nbHL-1
	inc bc
	inc bc

	ex af,af'
	xor ixl			; swap and set
	ld (bc),a		; VMAX or zero
	ex af,af'
	inc bc
	inc bc

	; loop check
	exx
	add hl,de
	bit 0,h
	jp z,loopSidC		; if hl<256 loop
	ld a,l
	sub c	
	jp c,loopSidC		; if l>56 loop
	exx

	; here we have rendered the sid and may have remaining HBL
	; 1 - get remaining HBLs (a)
	; 2 - set next VMAX ? (a')
	; a=0 means no delay, nextVol should be (a')
	; a>0 means delay, nextVol should be (a')
		
	; IMPORTANT
	; It seems that the previous pause continues on next frame
	; even if the DMA Address is reset
	; So we should cut the last pause command so that the ayl matches 312 HBL exactly

	; save delay
	ld (delaySidC+1),a

	; cut last pause to match exactly 312 HBLs
	neg		; a=-a
	ld hl,-4
	add hl,bc	; previous pause address >=a
	add (hl)
	ld (hl),a		
		
	; save next vmax
	ex af,af'
	ld (sidVolInitC+1),a

	; don't set PSG frame volume
	ld hl,regA+1
	set 0,(hl)

	; swap frames
	.nextFrameC
	ld hl,frameC1Init
	ld de,(currFrameC+1)
	ld (currFrameC+1),hl
	ld (nextFrameC+1),de

	; Here we should handle the case when the pre-pause in zero
	ld a,(de)
	or a
	ret nz
	inc de
	inc de
	ret
endif

; **************
; sid Voice AYLs
; **************
; We need to prepare a frame while playing the current
; so we reserve 2 frames per channels
;macro defaultFrame v
;	if SIDMIDRES
;		repeat 156
;			db 00
;			db v
;			dw &4000	; This can be interleaved with another channel (save space)
;		rend	
;			db 00
;			db v
;	else
;		repeat 313
;			db 00
;			db v
;		rend
;	endif
;mend

LET PREVIOUS_ORG = $
nolist

org SIDOFFSET
align 2
if SIDA_MAXRATE
.frameA0Init
	dw &1000	; Pause NN
	dw &0800	; LD VolA,NN
.frameA0
	repeat SIDA_MAXRATE/50 +1	; = nb setVol per Frame + STOP
	dw &1000	; Pause NN
	dw &0800	; LD VolA,NN
	rend
	dw &4020	; STOP (just in case)
.frameA1Init
	dw &1000	; Pause NN
	dw &0800	; LD VolA,NN
.frameA1
	repeat SIDA_MAXRATE/50 +1
	dw &1000	; Pause NN
	dw &0800	; LD VolA,NN
	rend
	dw &4020	; STOP
endif

if SIDB_MAXRATE
.frameB0Init
	dw &1000	; Pause NN
	dw &0900	; LD VolB,NN
.frameB0
	repeat SIDB_MAXRATE/50 +1
	dw &1000	; Pause NN
	dw &0900	; LD VolB,NN
	rend
	dw &4020	; STOP
.frameB1Init
	dw &1000	; Pause NN
	dw &0900	; LD VolB,NN
.frameB1
	repeat SIDB_MAXRATE/50 +1
	dw &1000	; Pause NN
	dw &0900	; LD VolB,NN
	rend
	dw &4020	; STOP
endif

if SIDC_MAXRATE
.frameC0Init
	dw &1000	; Pause NN
	dw &0A00	; LD VolC,NN
.frameC0
	repeat SIDC_MAXRATE/50 +1
	dw &1000	; Pause NN
	dw &0A00	; LD VolC,NN
	rend
	dw &4020	; STOP
.frameC1Init
	dw &1000	; Pause NN
	dw &0A00	; LD VolC,NN
.frameC1
	repeat SIDC_MAXRATE/50 +1
	dw &1000	; Pause NN
	dw &0A00	; LD VolC,NN
	rend
	dw &4020	; STOP
endif

; IN 	DE = offset(max)
;	A = channel (8,9,10)
;.genSidBuffer
;	ld hl,frameA0Init
;	ldi
;	ldi					; dw &1000 = Pause NN
;	ldi
;	ld (de),a
;	inc hl
;	inc de		; dw &0800 = LD regA,NN
;	ret

;print "# SIDs AYL End Offset"
;print $

;org FRAMEA0_OFFSET * 256
;.frameA0
;ifnot FRAMEA0_OFFSET = &FFF
;	defaultFrame &8	; FRAME 0
;	dw &4020	; stop
;endif
;
;org FRAMEA1_OFFSET * 256
;.frameA1
;ifnot FRAMEA1_OFFSET = &FFF
;	defaultFrame &8	; FRAME 1
;	dw &4020	; stop
;endif
;
;org FRAMEB0_OFFSET * 256
;.frameB0
;ifnot FRAMEB0_OFFSET = &FFF
;	defaultFrame &9	; FRAME 0
;	dw &4020	; stop
;endif
;
;org FRAMEB1_OFFSET * 256
;.frameB1
;ifnot FRAMEB1_OFFSET = &FFF
;	defaultFrame &9	; FRAME 1
;	dw &4020	; stop
;endif
;
;org FRAMEC0_OFFSET * 256
;.frameC0
;ifnot FRAMEC0_OFFSET = &FFF
;	defaultFrame &A	; FRAME 0
;	dw &4020	; stop
;endif
;
;org FRAMEC1_OFFSET * 256
;.frameC1
;ifnot FRAMEC1_OFFSET = &FFF
;	defaultFrame &A	; FRAME 1
;	dw &4020	; stop
;endif
;list

org PREVIOUS_ORG
; *************
; Mute Voices ?
; *************
; Based on mute counters
; This function has 2 purposes
; 1- set the period/noise Mixer bits to zero
; 2- forbid the volume value to be set (sample is playing) 
; Corrupted AF, BC, sHL
.muteVoice
	ld de,regA+1		; de points on regA

	xor a
	ld b,a
	ld hl,mute_count+2
	or (hl)			; reset Cary Flag
	jr z,voiceC_on
	; voiceC is off
	ex hl,de
	;set 0,(hl)		; forbid volumeC
	ld (hl),1		; test du futur
	ex hl,de
	dec (hl)		; dec mute counter
	scf			; set carry flag
.voiceC_on
	rl b
	dec de:dec de		; de points on reg9
	dec hl
	xor a
	or (hl)			; reset Cary Flag
	jr z,voiceB_on
	; voiceB is off
	ex hl,de
	;set 0,(hl)		; forbid volumeB
	ld (hl),1		; test du futur
	ex hl,de
	dec (hl)		; dec mute counter
	scf			; set carry flag
.voiceB_on
	rl b
	dec de:dec de		; de points on reg8
	dec hl
	xor a
	or (hl)			; reset Cary Flag
	jr z,voiceA_on
	; voiceA is off
	ex hl,de
	;set 0,(hl)		; forbid volumeA
	ld (hl),1		; test du futur
	ex hl,de
	dec (hl)		; dec mute counter
	scf			; set carry flag
.voiceA_on
	rl b

	; b contains the mute filter
	xor a
	or b
	ret z	; no mute

	; here a, b contain the tone filter ttt (reg7 = xxnnnttt)
	; we need to duplicate it in the noise filter nnn
	add a
	add a
	add a
	or b

	; mute the channel + noise generator
	ld hl,reg7
	or (hl)		; mixer filter
	ld (hl),a
	inc hl
	res 0,(hl)	; play reg 7

	ret

CREEPLAY
;
;Cree routine PLAYREG suivant taille des BUFFERS
;
	LD	HL,OFBUF0
	LD	DE,PLAYCODE
;
	LD	B,(HL)	;Poids fort du 1er tampon
	INC	HL
	LD	A,(HL)	;Taille
	INC	HL
	CP	1
	CALL	Z,CP_1
	CALL	NZ,CP_4
;
	LD	B,13+2	;15 premiers registre
CP_LP
	PUSH	BC
	CALL CP_COPY
;
	LD	B,(HL)
	INC	HL
	LD	A,(HL)
	CP	4
	CALL	Z,CP_4
	JR	Z,CP_SUI

	;on verifie si buffer precedent etait de taille 1,
	;et s'il etait place cote @ cote, pour pouvoir mettre  INC L
	DEC	HL
	DEC	HL
	CP	(HL)
	CALL	NZ,CP_1
	JR	NZ,CP_SUI0
;
	DEC	HL
	LD	A,(HL)	;Adr precedente buffer
	INC	HL
	SUB	B
	INC	A
	CALL	Z,CP_1INC
	CALL	NZ,CP_1
CP_SUI0
	INC	HL
	INC	HL
;
CP_SUI
	INC	HL
	POP	BC
	DJNZ	CP_LP

	; TODO, we don't need to roll the register number
	call CP_COPY	; register 15

;	DEC	DE	;On ecrase dernier INC HL
	LD	HL,MO2
	LD	BC,MO2_ENDIF-MO2
	LDIR
	RET

; *************
; register 0-12
; *************
CP_COPY
	ld a,b
	cp 15-13
	jp z,CP_COPY13
	jp c,CP_COPY14_15

	PUSH	HL
	LD	HL,MODELE
	LD	BC,MO_ENDIF-MODELE
	LDIR
	POP	HL
	RET

; ***********
; register 13
; ***********
CP_COPY13
	;le registre 13 a un traitement different
	;on le joue meme en cas de valeur identique, sauf si c'est #FF
	push hl
	LD	HL,MODELE_13
	LD	BC,ENDMODELE_13-MODELE_13
	LDIR

	LD	HL,MO_SUITE
	LD	BC,MO_ENDIF-MO_SUITE
	LDIR
	pop hl
	ret

; ************
; register >13
; ************
CP_COPY14_15
	PUSH	HL
	LD	HL,MODELE_14
	LD	BC,MO_ENDIF_14-MODELE_14
	LDIR
	POP	HL
	RET	
;
;
CP_1
;
;Si tampon de taille #100 on code
;    LD   D,n

	EX	DE,HL
	;LD	(HL),#26
	LD	(HL),#16
	INC	HL
	LD	(HL),B
	INC	HL
	EX	DE,HL
	RET
;
CP_1INC
;
;Quand 2 tampon de taille #100 successif, on code
;    INC  D 

	;LD	A,#24
	LD	A,#14
	LD	(DE),A
	INC	DE
	RET
;
CP_4
;
;Si c'est un tampon de taille #400, on code
;     LD   A,n
;     OR   B	
;     LD   D,A 

	EX	DE,HL
	LD	(HL),#3E
	INC	HL
	LD	(HL),B
	INC	HL
	;LD	(HL),#B2
	LD	(HL),#B0
	INC	HL
	;LD	(HL),#67
	LD	(HL),#57
	INC	HL
	EX	DE,HL
	RET

RAZVAR
;
;Toutes les auto-modifs pour la gestion
;
	LD	HL,VMESURE
	LD	(GETWITCH+1),HL
	LD	HL,V0
	LD	(GETWITC_+1),HL
	XOR	A
	LD	(DECALEH+1),A
	LD	HL,0
	LD	(PLAYPLAG+1),HL
;
	CALL	SETVAR
;
	LD	HL,REGS	;On copier variable pour reset/bouclage
	LD	DE,REGSCOPY
	LD	BC,AYC_REGS*10
	LDIR
;
	RET
;
;
SETVAR
;
;Init variables REGS pour la decompression.
;
	LD	HL,OFBUF0
	EXX
	LD	A,(NBR_REG)
	LD	B,A	;Nombre registres traites
	LD	DE,(ADRTEMP)	;Pointe sur donnees (en relatif)
	INC	DE
	INC	DE	;Saute "longueur"
	LD	HL,REGS
RAZLOOP
	PUSH	BC
;
;On place adr DEST
;
	EXX
	LD	A,(HL)
	INC	HL
	INC	HL
	EXX
	LD	(HL),0
	INC	HL
	LD	(HL),A
	INC	HL
;
;Adr source pour copie chaine forcement meme poids fort qd fenetre #100
;
	INC	HL
	LD	(HL),A
	INC	HL
;
;Valeur decalage (quand boucle, les donnees ne sont plus placees a partir de 0,
;les references absolues doivent etre corrigees)
;
	LD	(HL),0
	INC	HL
;
;On place nbr de chr restant a copier = 0
;
	LD	(HL),0
	INC	HL
;
;Octet flag a #40 pour copie 1er octet et enclencher lecture nouveaux flags
;
	LD	(HL),#40
	INC	HL
	INC	HL
;
;Maintenant il faut lire adr debut donnees compresses,
;donnees en relatif par rapport a position courante dans header
;
	EX	DE,HL
	INC	HL	;On saute type compression
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
;
	PUSH	HL
	ADD	HL,BC
	LD	B,H
	LD	C,L
	POP	HL
;
	INC	HL
	EX	DE,HL
	LD	(HL),C
	INC	HL
	LD	(HL),B
	INC	HL
;
	POP	BC
	DJNZ	RAZLOOP
	RET
;
;
;
;STORE
;	LD	BC,#7FC5	;Sauve DAMS
;	OUT	(C),C
;	LD	HL,#800
;	LD	DE,#4000
;	LD	BC,#3800
;	LDIR
;	RET
;;
;LOAD
;	LD	A,(FLAGLOAD)
;	OR	A
;	RET	NZ
;	INC	A
;	LD	(FLAGLOAD),A
;;
;	LD	HL,YMNAME
;	LD	DE,YMLZ
;	PUSH	DE
;	LD	B,14
;	CALL	#BC77
;	POP	HL	;Adresse
;	CALL	#BC83
;	CALL	#BC7A
;;
;	RET
;;
;;
;RESTORE
;	LD	BC,#7FC6
;	OUT	(C),C
;	LD	HL,#4000
;	LD	DE,#3000
;	LD	BC,#1000
;	LDIR
;	RET

;ifnot delock_asic
;delock_asic
;	ld      bc, &BC00
;        ld      hl, sequence_asic
;        ld      a, 19

;da_loop
;	ld      c, (hl)
;        inc     hl
;        out     (c), c
;        dec     a
;        jr      nz, da_loop
;        ret

;sequence_asic	db #01, #00, #FF, #77, #B3, #51, #A8, #D4
;                db #62, #39, #9C, #46, #2B, #15, #8A, #CD
;                db #EE, #FF, #00
;endif

; Variables
;FLAGLOAD	DS	1
;YMNAME	DB	"FOFT    .BIN"

; Pour chaque registre, on a :
;
; Adresse destination     (DE)
; Adresse source chaine   (HL)  ne sert pas forcement
; Flag/compteur chaine    (BC)  C poids faible decalage
; Octet flags             (BC') B' inutilise
; Source data compresses  (HL')
REGS		DS	AYC_REGS*10	;Variables pour chaque registre
REGSCOPY	DS	AYC_REGS*10	;Pour reset lors du bouclage

;NBR_REG est une constante qui permet de determiner combien recuperer
;de donnees a la fois. Si NBR_REG = 14, on recupere 14 donnees par registre et
;par VBL. Au bout de 14 VBL, on peut jouer 14 fois tous les reg., le temps de
;recuperer 14*14 nouvelles donnees.
NBR_REG	DB	AYC_REGS	;!!! MODIFIER (V14-2) EN CONSEQUENCE !!

ADRTEMP	DW	YMLZ
OFBUF0	DB	#C0	;Poids fort adresse
	DB	4	;Taille (1 ou 4) pour CREEPLAY
;
;Attention les tampons de #400 doivent commencer en #x000 ou #x800
;
	DB	#C4,1,#C8,4,#C5,1,#D0,4
	DB	#C6,1,#C7,1,#CC,1,#CD,1
	DB	#CE,1,#CF,1,#D4,1,#D5,1
	DB	#D6,1,#D7,1,#D8,1