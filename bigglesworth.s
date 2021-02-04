;===========================================================
;biggleswoth simplified SD card DOS replacement for MEGA65
;
;Version 0.10A
;Written by Daniel England of Ecclestial Solutions.
;
;Copyright 2021, Daniel England. All Rights Reserved.
;
;-----------------------------------------------------------
;
;A simple DOS replacement library.  Use bigglesSetFileName
;to describe your file and bigglesOpenFile to open it.
;
;You must put the pages to use for the read buffer and file
;name into ptrBigglesBufHi and ptrBigglesFNmHi, respectively.
;
;Supports only character reads but block reads will be added
;in the future.  Read a byte with bigglesReadByte.
;
;-----------------------------------------------------------
;
;I want to release this under the LGPL.  I'll make the 
;commitment and include the licensing infomation soon.
;
;===========================================================


ptrBigglesBufHi	=	$DE
ptrBigglesFNmHi	=	$DF
ptrBigglesBufOff=	$E2


;-----------------------------------------------------------
bigglesSetFileName:
;	.X		IN	File Name Addr Lo
;	.Y		IN	File Name Addr Hi
;
;	.C		OUT	Set if error
;-----------------------------------------------------------
;***Do error checking 'cause this is the interface

		PHX
		PHY

		JSR	_bigglesCloseAll

		PLY
		PLX

		STX	ptrBigglesBufOff
		STY	ptrBigglesBufOff + 1

		LDA	ptrBigglesFNmHi
		STA	@nmsta + 2

		LDY	#$00
@NameCopyLoop:
		LDA	(ptrBigglesBufOff), Y
@nmsta:
		STA	$0300, Y
		INY
		CMP	#$00
		BNE	@NameCopyLoop

		LDX	#$00
		LDY	ptrBigglesFNmHi

		JSR	_bigglesSetFN

		RTS


;-----------------------------------------------------------
bigglesOpenFile:
;-----------------------------------------------------------
		LDA	flgBigglesErr
		BNE	@err

		JSR	_bigglesOpenFile

		RTS

@err:
		SEC
		RTS


;-----------------------------------------------------------
bigglesCloseFile:
;-----------------------------------------------------------
		JSR	_bigglesCloseAll

		RTS


;-----------------------------------------------------------
bigglesReadByte:
;	.A		OUT	Data
;	.ST_C	OUT	Set if error
;-----------------------------------------------------------
;***Do more error checking 'cause this is the interface
		PHX
		PHY
		PHZ

		JSR	_bigglesReadByte

		PLZ
		PLY
		PLX

		RTS


;===========================================================
;===========================================================
;===========================================================

flgBigglesErr:
		.byte	$01
sizBigglesBuf:
		.word	$0000
adrBigglesFN:
		.word	$0000


lstBigglesDMA:
	;; Copy $FFD6E00 - $FFD6FFF down to low memory 
	;; MEGA65 Enhanced DMA options
        .byte $0A  ;; Request format is F018A
        .byte $80,$FF ;; Source is $FFxxxxx
        .byte $81,$00 ;; Destination is $FF
        .byte $00  ;; No more options
        ;; F018A DMA list
        ;; (MB offsets get set in routine)
        .byte $00 ;; copy + last request in chain
        .word $0200 ;; size of copy is 512 bytes
        .word $6E00 ;; starting at $6E00
        .byte $0D   ;; of bank $D
adrBigglesDest:
        .word $8000 ;; destination address is $8000
        .byte $00   ;; of bank $0
        .word $0000 ;; modulo (unused)



;-----------------------------------------------------------
_bigglesOpenFile:
;-----------------------------------------------------------
;		JSR	_bigglesCloseAll

		LDX	adrBigglesFN
		LDY	adrBigglesFN + 1

;		JSR	_bigglesSetFN

		LDA	#$34
		STA	$D640
		NOP
		BCC	@fail

		LDA	#$00
		STA	$D640
		NOP

		LDA	#$18
		STA	$D640
		NOP

		LDA	#$00
		STA	flgBigglesErr

		CLC

		RTS

@fail:
		LDA	#$01
		STA	flgBigglesErr

		SEC
		RTS

;-----------------------------------------------------------
_bigglesReadByte:
;-----------------------------------------------------------
		LDA	flgBigglesErr
		BEQ	@begin

		SEC
		RTS

@begin:
		LDA	sizBigglesBuf
		ORA	sizBigglesBuf + 1

		BNE	@cont0

		JSR	_bigglesReadSect

		LDA	sizBigglesBuf
		ORA	sizBigglesBuf + 1

		BNE	@cont0

		LDA	#$01
		STA	flgBigglesErr

		SEC
		RTS

@cont0:
		LDY	#$00
		LDA	(ptrBigglesBufOff), Y

		PHA

		CLC
		LDA	ptrBigglesBufOff
		ADC	#$01
		STA	ptrBigglesBufOff
		LDA	ptrBigglesBufOff + 1
		ADC	#$00
		STA	ptrBigglesBufOff + 1

		SEC
		LDA	sizBigglesBuf
		SBC	#$01
		STA	sizBigglesBuf
		LDA	sizBigglesBuf + 1
		SBC	#$01
		STA	sizBigglesBuf + 1

		PLA

		CLC
		RTS


;-----------------------------------------------------------
_bigglesReadSect:
;-----------------------------------------------------------
		LDA	#$00
		STA	ptrBigglesBufOff

		LDA	ptrBigglesBufHi
		STA	ptrBigglesBufOff +1
		STA	adrBigglesDest + 1

		LDA	#$1A
		STA	$D640
		NOP

;@halt:
;		INC	$D020
;		JMP	@halt


		STX	sizBigglesBuf
		STY	sizBigglesBuf + 1

		LDA sizBigglesBuf
		ORA	sizBigglesBuf + 1

		BEQ	@exit


		LDA	#$80
		TSB	$D689

		LDA	#$00
		STA	$D702
		STA	$D704
		LDA	#>lstBigglesDMA
		STA	$D701
		LDA	#<lstBigglesDMA
		STA	$D705

@exit:
		RTS


;-----------------------------------------------------------
_bigglesSetFN:
;-----------------------------------------------------------
		STX	adrBigglesFN
		STY	adrBigglesFN + 1

		LDA	#$2E				; dos_setname Hypervisor trap
		STA	$D640				; Do hypervisor trap
		NOP
		BCS	@ok

		LDA	#$01
		STA	flgBigglesErr

		SEC
		RTS

@ok:
		LDA	#$00
		STA	flgBigglesErr

		CLC
		RTS


;-----------------------------------------------------------
_bigglesCloseAll:
;-----------------------------------------------------------
		LDA	#$22
		STA	$D640
		NOP

		LDA	#$00
		STA	flgBigglesErr

		RTS