; ***********
; DELOCK ASIC
; ***********
.lock_asic
	ld a,17-1
	jp asic_on_off

.delock_asic
	ld      a, 17
.asic_on_off
	ld      bc, &BC00
	ld      hl, sequence_asic
.da_loop
	ld      c, (hl)
        inc     hl
        out     (c), c
        dec     a
        jr      nz, da_loop
        ret

.sequence_asic	db #01, #00, #FF, #77, #B3, #51, #A8, #D4
                db #62, #39, #9C, #46, #2B, #15, #8A, #CD
.sequence_asic_last_byte
               db #EE ; = delock
;		db #00 ; = lock