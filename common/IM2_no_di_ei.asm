; F-Key/RevivaL 2009
; IM2 BUG CPC+
; 4 interrupt vectors can be defined (if not simple interrupt vector ei, ret)
; - intDMA0
; - intDMA1
; - intDMA2
; - intRASTER

DCSR 		EQU &6C0F
IVR 		EQU &6805
INT_SPECIFIC	EQU 1 

; IM2
.im2

; disable interrupts
;di

; delock asic
;call delock_asic

; Connect Asic
;ld bc,&7FB8
;out (c),c

ld bc,intvect
if INT_SPECIFIC
set 0,c
endif
; set I
ld a,b
ld i,a

; set IVR
ld a,c
ld (IVR),a

; Deconnect Asic
;ld bc,&7FA0
;out (c),c

; go go baby
im 2

; enable interrupts
;ei

;end
ret

; ***************************
; INTERRUPT VECTOR DEFINITION
; ***************************

; This is to ensure that the interrupt vector
; is located on an correct address (IVR is like xxxxxYY0 for universal aks)
align 8

; Interrupt Vector jumps
.intvect
	ifdef intDMA2
		dw intDMA2
	else
		dw intVoid
	endif
	ifdef intDMA1
		dw intDMA1
	else
		dw intVoid
	endif
	ifdef intDMA0
		dw intDMA0
	else
		dw intVoid
	endif
	ifdef intRASTER
		dw intRASTER
	else
		dw intVoid
	endif

;print intvect

; Simple interrupt vector for sync
.intVoid
	ei
	ret

; import
read "common/asic_on.asm"
