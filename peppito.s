;===========================================================
;Peppito MOD Playback Driver
;
;Version 0.10A
;Written by Daniel England of Ecclestial Solutions.
;
;Copyright 2021, Daniel England. All Rights Reserved.
;
;-----------------------------------------------------------
;
;
;-----------------------------------------------------------
;
;I want to release this under the LGPL.  I'll make the 
;commitment and include the licensing infomation soon.
;
;===========================================================

;-----------------------------------------------------------
TBL_PPTO_JUMP:
	JMP	peppitoInit				;+0
	JMP	peppitoPlay				;+3
	JMP	peppitoNOP				;+6
	JMP	peppitoPrepare			;+9
	JMP	peppitoStop				;+C


ptrPepTmpA	=	$A0
ptrPepChan	=	$A2

ptrPepModF	=	$B0
ptrPepMSeq	=	$B4
ptrPepPtrn	=	$B8
ptrPepSmpD	=	$BC


VAL_FAC_AMIGARATEL	=	$AB6F
VAL_FAC_AMIGARATEH	=	$000D


;This is "logical" but may be slightly off?
; 2^16   *  40500000 / 2 / 28800 / 428
;VAL_FAC_M65RATERTL	=	$A490
;VAL_FAC_M65RATERTH	=	$0001

;I read somewhere about 28837 being the actual rate on Amiga
;VAL_FAC_M65RATERTL	=	$A406
;VAL_FAC_M65RATERTH	=	$0001

;From testing
VAL_FAC_M65RATERTL	=	$A3A0
VAL_FAC_M65RATERTH	=	$0001

;experiment
;VAL_FAC_M65RATERTL	=	$8C00
;VAL_FAC_M65RATERTH	=	$0001





	.struct	PEP_INSMHDR
		strName		.res	22
		bewSampLen	.res	2
		sbyFineTune	.byte
		sbyVolume	.byte
		bewLoopStrt	.res	2
		bewLoopLen	.res	2
	.endstruct

	.struct	PEP_INSDATA
		ptrHdr		.res	4
		valVol		.byte
		valFTune	.byte
		valSLen		.word
		valLStrt	.word
		valLLen		.word
		ptrSmpD		.res	4
	.endstruct

	.struct	PEP_NOTDATA
		valKey		.word
		valIns		.byte
		valEff		.byte
		valPrm		.byte
	.endstruct

	.struct	PEP_CHNDATA
		recNote		.tag	PEP_NOTDATA
		valVol		.byte
		valFTune	.byte
		valAssIns	.byte
		valSelIns	.byte
		valSmpOffs	.word
		valPeriod	.word
	.endstruct


adrPepMODL:
	.word	$0000
adrPepMODH:
	.word	$0000

cntPepTick:
	.byte	$00
cntPepPRow:
	.byte	$00
cntPepSeqP:
	.byte	$00

valPepSped:
	.byte	$06
valPepPBrk:
	.byte	$00
valPepNRow:
	.byte	$00
valPepSLen:
	.byte	$7F
valPepSRst:
	.byte	$00
valPepChan:
	.byte	$00

valPepMaxP:
	.byte	$00

valPepTmp0:
	.word	$0000
valPepTmp1:
	.word	$0000
valPepTmp2:
	.word	$0000
valPepTmp3:
	.word	$0000
valPepTmp4:
	.word	$0000

adrPepPtn0:
	.repeat	128
	.word	$0000
	.word	$0000
	.endrepeat

recPepIns0:
	.repeat	32
	.res	.sizeOf(PEP_INSDATA)
	.endrepeat

idxPepIns0:
	.repeat	32, ins
	.byte	<(recPepIns0 + (.sizeOf(PEP_INSDATA) * ins))
	.byte	>(recPepIns0 + (.sizeOf(PEP_INSDATA) * ins))
	.endrepeat

recPepChn0:
	.repeat	4
	.res	.sizeOf(PEP_CHNDATA)
	.endrepeat

idxPepChn0:
	.repeat	4, chn
	.byte	<(recPepChn0 + (.sizeof(PEP_CHNDATA) * chn))
	.byte	>(recPepChn0 + (.sizeof(PEP_CHNDATA) * chn))
	.endrepeat

valPepMRg0:
	.res	8, $00


	.macro	__PEP_SET_MODF_OFFS	ptr, offs
		CLC
		LDA	adrPepMODL
		ADC	#<offs
		STA	ptr
		LDA	adrPepMODL + 1
		ADC	#>offs
		STA	ptr + 1
		LDA	#$00
		ADC	adrPepMODH
		STA	ptr + 2
		LDA	#$00
		ADC	adrPepMODH + 1
		STA	ptr + 3
	.endmacro

	.macro	__PEP_ADD_PTR32_IMM16	ptr, offs
		CLC
		LDA	#<offs
		ADC	ptr
		STA	ptr
		LDA	#>offs
		ADC	ptr + 1
		STA	ptr + 1
		LDA	#$00
		ADC	ptr + 2
		STA	ptr + 2
		LDA	#$00
		ADC	ptr + 3
		STA	ptr + 3
	.endmacro

	.macro	__PEP_ADD_PTR32_MEM16	ptr, mem
		CLC
		LDA	mem
		ADC	ptr
		STA	ptr
		LDA	mem + 1
		ADC	ptr + 1
		STA	ptr + 1
		LDA	#$00
		ADC	ptr + 2
		STA	ptr + 2
		LDA	#$00
		ADC	ptr + 3
		STA	ptr + 3
	.endmacro

	.macro	__PEP_ADD_PTR16_MEM16	ptr, mem
		CLC
		LDA	mem
		ADC	ptr
		STA	ptr
		LDA	mem + 1
		ADC	ptr + 1
		STA	ptr + 1
	.endmacro

	.macro	__PEP_ADD_PTR16_IMM16	ptr, offs
		CLC
		LDA	#<offs
		ADC	ptr
		STA	ptr
		LDA	#>offs
		ADC	ptr + 1
		STA	ptr + 1
	.endmacro

	.macro	__PEP_ASL_MEM16	mem
		CLC
		LDA	mem
		ASL
		STA	mem
		LDA	mem + 1
		ROL
		STA	mem + 1
	.endmacro

	.macro __PEP_MOV_PTR32_IND32 ptr, ind
		LDA	ptr
		STA	(ind), Y
		INY
		LDA	ptr + 1
		STA	(ind), Y
		INY
		LDA	ptr + 2
		STA	(ind), Y
		INY
		LDA	ptr + 3
		STA	(ind), Y
		INY
	.endmacro

;-----------------------------------------------------------
peppitoNOP:
;-----------------------------------------------------------
		RTS

;-----------------------------------------------------------
peppitoInit:
;-----------------------------------------------------------
;@halt:
;		INC	$D020
;		JMP	@halt

		LDA	#$00
		STA	valPepPBrk
		STA	valPepNRow

		STA	cntPepSeqP

		LDA	#$01
		STA	cntPepTick
		
		LDA	#$06
		STA	valPepSped

		LDA	#$00
		STA	valPepMaxP

		__PEP_SET_MODF_OFFS ptrPepModF, $03B6

		LDZ	#$00
		NOP
		LDA	(ptrPepModF), Z

		AND	#$7F
		STA	valPepSLen

		INZ

		NOP
		LDA	(ptrPepModF), Z

		AND	#$7F
		STA	valPepSRst

		LDA	valPepSLen
		CMP	valPepSRst
		BCS	@cont0

		LDA	#$00
		STA	valPepSRst

@cont0:
		__PEP_SET_MODF_OFFS ptrPepMSeq, $03B8

		LDZ	#$00
@loop0:
		NOP
		LDA	(ptrPepMSeq), Z

		CMP	valPepMaxP
		BCC	@next0

		STA	valPepMaxP

@next0:
		INZ
		CPZ	#$80
		BNE	@loop0

		INC	valPepMaxP

		JSR	peppitoReadSeq

		JSR	peppitoReadIns

		JSR	peppitoClearChans

		RTS


;-----------------------------------------------------------
peppitoPlay:
;-----------------------------------------------------------
		JSR	peppitoSaveState

		DEC	cntPepTick
		BNE	@procTick

		LDA	valPepSped
		STA	cntPepTick

		JSR	peppitoPerformRow
		JMP	@exit

@procTick:
		JSR	pepptioPerformTick

@exit:
		JSR	peppitoRestoreState

		RTS


;-----------------------------------------------------------
peppitoPrepare:
;-----------------------------------------------------------
		RTS


;-----------------------------------------------------------
peppitoStop:
;-----------------------------------------------------------
;	Disable DMA audio
		LDA	#$00
		STA	$D711

		LDA	#$01
		STA	cntPepTick

		RTS


;-----------------------------------------------------------
peppitoClearChans:
;-----------------------------------------------------------
		LDX	#$00
@loop0:
		PHX
		TXA
		ASL
		TAX

		LDA	idxPepChn0, X
		STA	ptrPepTmpA
		LDA	idxPepChn0 + 1, X
		STA	ptrPepTmpA + 1

		LDY	#$00
		LDA	#$00
@loop1:
		STA	(ptrPepTmpA), Y
		INY

		CPY	#.sizeOf(PEP_CHNDATA)
		BNE	@loop1

		PLX
		INX	
		CPX	#$04
		BNE	@loop0

		LDA	#$00
		STA	$D720
		STA	$D730
		STA	$D740
		STA	$D750

		RTS


;-----------------------------------------------------------
peppitoSaveState:
;-----------------------------------------------------------
		LDX	#$00

@loop0:
		LDA	$D770, X
		STA	valPepMRg0, X

		INX
		CPX	#$08
		BNE	@loop0

		RTS


;-----------------------------------------------------------
peppitoRestoreState:
;-----------------------------------------------------------
		LDX	#$00

@loop0:
		LDA	valPepMRg0, X
		STA	$D770, X

		INX
		CPX	#$08
		BNE	@loop0
		
		LDA	$D020
		STA	$D020
		LDA	$D020
		STA	$D020
		LDA	$D020
		STA	$D020
		LDA	$D020
		STA	$D020

		RTS


;-----------------------------------------------------------
peppitoReadIns:
;-----------------------------------------------------------
		__PEP_SET_MODF_OFFS ptrPepModF, $0014

		LDX	#$01
@loop:

;@halt:
;		INC	$D020
;		JMP	@halt

		TXA
		ASL
		TAY
		LDA	idxPepIns0, Y
		STA	ptrPepTmpA
		INY
		LDA	idxPepIns0, Y
		STA	ptrPepTmpA + 1

		LDY	#PEP_INSDATA::ptrHdr
		__PEP_MOV_PTR32_IND32 ptrPepModF, ptrPepTmpA

;	Sample length
		LDZ	#$16

		NOP
		LDA	(ptrPepModF), Z
		STA	valPepTmp0 + 1
		INZ
		
		NOP
		LDA	(ptrPepModF), Z
		STA	valPepTmp0 

		__PEP_ASL_MEM16 valPepTmp0

		LDY	#PEP_INSDATA::valSLen
		LDA	valPepTmp0
		STA	valPepTmp4
		STA	(ptrPepTmpA), Y
		INY
		LDA	valPepTmp0 + 1
		STA	valPepTmp4 + 1
		STA	(ptrPepTmpA), Y


;	Sample data
		LDY	#PEP_INSDATA::ptrSmpD
		__PEP_MOV_PTR32_IND32 ptrPepSmpD, ptrPepTmpA

		__PEP_ADD_PTR32_MEM16 ptrPepSmpD, valPepTmp0

;	Loop start
		LDZ	#$1A

		NOP
		LDA	(ptrPepModF), Z
		STA	valPepTmp0 + 1
		STA	valPepTmp2 + 1
		INZ
		
		NOP
		LDA	(ptrPepModF), Z
		STA	valPepTmp0
		STA	valPepTmp2
		INZ

		__PEP_ASL_MEM16 valPepTmp0

;	Loop length
		NOP
		LDA	(ptrPepModF), Z
		STA	valPepTmp1 + 1
		INZ
		
		NOP
		LDA	(ptrPepModF), Z
		STA	valPepTmp1
		INZ

		__PEP_ASL_MEM16 valPepTmp1

;	Check if module stores loop start in bytes

;	If loop start + loop length > sample length
;		valPepTmp3 > valPepTmp4
		CLC
		LDA	valPepTmp0			;start
		ADC	valPepTmp1			;length
		STA	valPepTmp3
		LDA	valPepTmp0 + 1
		ADC	valPepTmp1 + 1
		STA	valPepTmp3 + 1

;	branches to @bytes0 if valPepTmp4 < valPepTmp3
		LDA	valPepTmp4 + 1
		CMP	valPepTmp3 + 1
		BCC	@bytes0
		BNE	@skip0
		LDA	valPepTmp4
		CMP	valPepTmp3
		BCC	@bytes0

		JMP	@skip0

@bytes0:
;	If (loop start / 2) + loop length <= sample length
;		slen < lstrt / 2 + llen
		CLC
		LDA	valPepTmp2			;start / 2
		ADC	valPepTmp1			;length
		STA	valPepTmp3
		LDA	valPepTmp2 + 1
		ADC	valPepTmp1 + 1
		STA	valPepTmp3 + 1

;	branches to @less0 if valPepTmp4 < valPepTmp3
		LDA	valPepTmp4 + 1
		CMP	valPepTmp3 + 1
		BCC	@less0
		BNE	@bytes1
		LDA	valPepTmp4
		CMP	valPepTmp3
		BCC	@less0

@bytes1:
;	loop len:= sample len - loop start
		SEC
		LDA	valPepTmp4
		SBC	valPepTmp0
		STA	valPepTmp1
		LDA	valPepTmp4 + 1
		SBC	valPepTmp0 + 1
		STA	valPepTmp1 + 1

		JMP	@skip0

@less0:
;	loop start:= loop start / 2
		LDA	valPepTmp2
		STA	valPepTmp0
		LDA	valPepTmp2 + 1
		STA	valPepTmp0 + 1

@skip0:
;	if LoopLength < 4 then
;		LoopStart := SampleLength - LoopLength;
		LDA	valPepTmp1 + 1
		BNE	@cont1
		LDA	valPepTmp1 
		CMP	#04
		BCS	@cont1

		SEC
		LDA	valPepTmp4
		SBC	valPepTmp1
		STA	valPepTmp0
		LDA	valPepTmp4 + 1
		SBC	valPepTmp1 + 1
		STA	valPepTmp0 + 1

@cont1:
;	Store loop start and loop length
		LDY	#PEP_INSDATA::valLStrt
		LDA	valPepTmp0
		STA	(ptrPepTmpA), Y
		INY
		LDA	valPepTmp0 + 1
		STA	(ptrPepTmpA), Y
		INY

		LDA	valPepTmp1
		STA	(ptrPepTmpA), Y
		INY
		LDA	valPepTmp1 + 1
		STA	(ptrPepTmpA), Y


;	Fine tune
		LDZ	#$18

		NOP
		LDA	(ptrPepModF), Z
		AND	#$0F
		STA	valPepTmp0
		INZ

		AND	#$08
		STA	valPepTmp0 + 1

;	(FineTune and $07) - (FineTune and $08) + 8;
		SEC
		LDA	valPepTmp0
		AND	#$07
		SBC	valPepTmp0 + 1
		STA	valPepTmp0

		CLC
;		LDA	valPepTmp0
		ADC	#$08
;		STA	valPepTmp0

		LDY	#PEP_INSDATA::valFTune
		STA	(ptrPepTmpA), Y

;	Volume
		NOP
		LDA	(ptrPepModF), Z
		AND	#$7F
;		STA	valPepTmp0

		CMP	#$40
		BCC	@skip1

		LDA	#$40
;		STA	valPepTmp0

@skip1:
		LDY	#PEP_INSDATA::valVol
;		LDA	valPepTmp0
		STA	(ptrPepTmpA), Y


;	Next
		__PEP_ADD_PTR32_IMM16 ptrPepModF, $001E

		INX
		CPX	#$20
		LBNE	@loop


		RTS


;-----------------------------------------------------------
peppitoReadSeq:
;-----------------------------------------------------------
		LDA	#<adrPepPtn0
		STA	ptrPepTmpA
		LDA	#>adrPepPtn0
		STA	ptrPepTmpA + 1

		__PEP_SET_MODF_OFFS ptrPepModF, $043C

		LDZ	#$00
@loop0:
		LDY	#$00
		LDA	ptrPepModF
		STA	(ptrPepTmpA), Y
		INY
		LDA	ptrPepModF + 1
		STA	(ptrPepTmpA), Y
		INY
		LDA	ptrPepModF + 2
		STA	(ptrPepTmpA), Y
		INY
		LDA	ptrPepModF + 3
		STA	(ptrPepTmpA), Y
;		INY

		__PEP_ADD_PTR16_IMM16 ptrPepTmpA, $04
		__PEP_ADD_PTR32_IMM16 ptrPepModF, $0400

		INZ
		CPZ	valPepMaxP
		BNE	@loop0


		LDA	ptrPepModF
		STA	ptrPepSmpD
		LDA	ptrPepModF + 1
		STA	ptrPepSmpD + 1
		LDA	ptrPepModF + 2
		STA	ptrPepSmpD + 2
		LDA	ptrPepModF + 3
		STA	ptrPepSmpD + 3

		RTS


;-----------------------------------------------------------
peppitoChanVolSlide:
;-----------------------------------------------------------
		LDA	valPepTmp0 + 1
		AND	#$0F
		STA	valPepTmp1

		LDA	valPepTmp0 + 1
		LSR
		LSR
		LSR
		LSR

		SEC	
		SBC	valPepTmp1
		STA	valPepTmp1

		LDY	#PEP_CHNDATA::valVol
		CLC
		LDA (ptrPepChan), Y
		ADC	valPepTmp1

		BMI	@setzero

		CMP	#$40
		BCC	@update

		LDA	#40
@update:
		STA	(ptrPepChan), Y
		RTS

@setzero:
		LDA	#$00
		JMP	@update


;-----------------------------------------------------------
peppitoChanPortaUp:
;-----------------------------------------------------------
		LDY	#PEP_CHNDATA::valPeriod
		SEC
		LDA	(ptrPepChan), Y
		SBC	valPepTmp0 + 1
		STA	(ptrPepChan), Y
		INY
		LDA	(ptrPepChan), Y
		SBC	#$00
		STA	(ptrPepChan), Y

		BMI	@setzero

		RTS

@setzero:
		DEY
		LDA	#$00
		STA	(ptrPepChan), Y
		INY
		STA	(ptrPepChan), Y

		RTS


;-----------------------------------------------------------
peppitoChanPortaDown:
;-----------------------------------------------------------
		LDY	#PEP_CHNDATA::valPeriod
		CLC
		LDA	(ptrPepChan), Y
		ADC	valPepTmp0 + 1
		STA	(ptrPepChan), Y
		INY
		LDA	(ptrPepChan), Y
		ADC	#$00
		STA	(ptrPepChan), Y

		BMI	@setmax

		RTS

@setmax:
		DEY
		LDA	#$FF
		STA	(ptrPepChan), Y
		INY
		LDA	#$7F
		STA	(ptrPepChan), Y

		RTS


;-----------------------------------------------------------
peppitoChanTick:
;-----------------------------------------------------------
		LDY	#PEP_NOTDATA::valEff
		
		LDA	(ptrPepChan), Y
		STA	valPepTmp0
		INY

		LDA	(ptrPepChan), Y
		STA	valPepTmp0 + 1


		LDA	valPepTmp0
		CMP	#$0A
		BNE	@tstnext0

;	Volume slide
		JSR	peppitoChanVolSlide
		JSR	peppitoChanUpdVol
		RTS

@tstnext0:
		CMP	#$01
		BNE	@tstnext1

;	Portamento up
		JSR	peppitoChanPortaUp
		JSR	peppitoChanUpdFreq
		RTS

@tstnext1:
		CMP	#$02
		BNE	@tstnext2

;	Portamento down
		JSR	peppitoChanPortaDown
		JSR	peppitoChanUpdFreq
		RTS

@tstnext2:
		RTS

		RTS

;-----------------------------------------------------------
pepptioPerformTick:
;-----------------------------------------------------------
		LDX	#$00
@loop1:
;		CPX	#$01
;		BEQ	@next
;		CPX	#$02
;		BEQ	@next

		PHX

		STX	valPepChan

		TXA
		ASL
		TAY

		LDA	idxPepChn0, Y
		STA	ptrPepChan
		LDA	idxPepChn0 + 1, Y
		STA	ptrPepChan + 1

		JSR	peppitoChanTick

		PLX
@next:
		INX
		CPX	#$04
		BNE	@loop1

		RTS


;-----------------------------------------------------------
peppitoPerformRow:
;-----------------------------------------------------------
		LDA	valPepNRow
		BPL	@cont0

		LDX	cntPepSeqP
		INX
		STX	valPepPBrk

		LDA	#$00
		STA	valPepNRow

@cont0:
		LDA	valPepPBrk
		BMI	@cont1

		CMP	valPepSLen
		BCC	@more0

		LDA	#$00
		STA	valPepPBrk
		STA	valPepNRow

@more0:
		LDA	valPepPBrk
		BMI	@restart

		CMP	cntPepSeqP
		BCC	@restart

		JMP	@skip0

@restart:

;***FIXME:
;	Do we restart or do we end??

		LDA	valPepSRst
		STA	valPepPBrk

@skip0:
		LDA	valPepPBrk
		STA	cntPepSeqP

;		For Chan := 0 To NumChannels - 1 Do Channels[ Chan ].PLRow := 0;

		LDA	#$FF
		STA	valPepPBrk

@cont1:
		LDA	valPepNRow
		STA	cntPepPRow

		INC	valPepNRow
		LDA	#$C0
		AND	valPepNRow
		BEQ	@update

		LDA	#$FF
		STA	valPepNRow

@update:
		LDZ	cntPepSeqP
		
		NOP
		LDA	(ptrPepMSeq), Z

		ASL
		ASL
		TAX

		LDA	adrPepPtn0, X
		STA	ptrPepPtrn
		LDA	adrPepPtn0 + 1, X
		STA	ptrPepPtrn + 1
		LDA	adrPepPtn0 + 2, X
		STA	ptrPepPtrn + 2
		LDA	adrPepPtn0 + 3, X
		STA	ptrPepPtrn + 3

		LDA	cntPepPRow
		STA	valPepTmp0
		LDA	#$00
		STA	valPepTmp0 + 1

		__PEP_ASL_MEM16 valPepTmp0
		__PEP_ASL_MEM16 valPepTmp0
		__PEP_ASL_MEM16 valPepTmp0
		__PEP_ASL_MEM16 valPepTmp0

		__PEP_ADD_PTR32_MEM16 ptrPepPtrn, valPepTmp0

		LDZ	#$00
		LDX	#$00
@loop0:
		TXA
		ASL
		TAY

;@halt:
;		INC	$D020
;		JMP	@halt

		LDA	idxPepChn0, Y
		STA	ptrPepTmpA
		LDA	idxPepChn0 + 1, Y
		STA	ptrPepTmpA + 1

;	Note period (key)
		NOP
		LDA	(ptrPepPtrn), Z
		INZ

		STA	valPepTmp0

		AND	#$0F

		LDY	#PEP_NOTDATA::valKey + 1
		STA	(ptrPepTmpA), Y
		DEY

		NOP
		LDA	(ptrPepPtrn), Z
		INZ

		STA	(ptrPepTmpA), Y

;	Note instrument
		LDA	valPepTmp0
		AND	#$10
		STA	valPepTmp0

		NOP
		LDA	(ptrPepPtrn), Z
		INZ

		STA	valPepTmp0 + 1

		AND	#$F0
		LSR
		LSR
		LSR
		LSR
		ORA	valPepTmp0

		LDY	#PEP_NOTDATA::valIns
		STA	(ptrPepTmpA), Y

;	Effect
		LDA	valPepTmp0 + 1
		AND	#$0F

		LDY	#PEP_NOTDATA::valEff
		STA	(ptrPepTmpA), Y

;	Param
		NOP
		LDA	(ptrPepPtrn), Z
		INZ

		LDY	#PEP_NOTDATA::valPrm
		STA	(ptrPepTmpA), Y

;***FIXME:
;	effect $0E and $10

		INX
		CPX	#$04
		BNE	@loop0


		LDX	#$00
@loop1:
;		CPX	#$01
;		BEQ	@next
;		CPX	#$02
;		BEQ	@next

		PHX

		STX	valPepChan

		TXA
		ASL
		TAY

		LDA	idxPepChn0, Y
		STA	ptrPepChan
		LDA	idxPepChn0 + 1, Y
		STA	ptrPepChan + 1

		JSR	peppitoChanRow

		PLX
@next:
		INX
		CPX	#$04
		BNE	@loop1

;	Enable DMA audio
		LDA	#$80
		STA	$D711


		RTS


;-----------------------------------------------------------
peppitoChanRow:
;-----------------------------------------------------------
;@halt:
;		INC	$D020
;		JMP	@halt

		LDY	#PEP_NOTDATA::valEff
		
		LDA	(ptrPepChan), Y
		STA	valPepTmp0
		INY

		LDA	(ptrPepChan), Y
		STA	valPepTmp0 + 1

		LDA	valPepTmp0
		CMP	#$1D
		BNE	@trigger

		LDA	valPepTmp0 + 1
		BEQ	@trigger

		JMP	@update

@trigger:
		JSR	peppitoChanTrigger

@update:
		JSR	peppitoChanTrigEffect

		RTS


;-----------------------------------------------------------
peppitoMULT10:
;-----------------------------------------------------------
		ASL					;multiply by 2
		STA valPepTmp4		;temp store in TEMP
		ASL					;again multiply by 2 (*4)
		ASL					;again multiply by 2 (*8)
		CLC
		ADC	valPepTmp4		;as result, A = x*8 + x*2
		RTS

;-----------------------------------------------------------
peppitoChanTrigEffect:
;-----------------------------------------------------------
		LDY	#PEP_NOTDATA::valEff
		
		LDA	(ptrPepChan), Y
		STA	valPepTmp0
		INY
		LDA	(ptrPepChan), Y
		STA	valPepTmp0 + 1

		LDA	valPepTmp0

		CMP	#$0D
		BNE	@tstnxt0
;	Pattern break
;***FIXME:
;		If PLCount < 0 Then Begin
		LDA	valPepPBrk
		BPL	@pbrk0

		LDX	cntPepSeqP
		INX
		STX	valPepPBrk

@pbrk0:
		LDA	valPepTmp0 + 1
		LSR
		LSR
		LSR
		LSR
		JSR	peppitoMULT10
		STA	valPepTmp1

		LDA	valPepTmp0 + 1
		AND	#$0F

		CLC
		ADC	valPepTmp1
		STA	valPepNRow

		CMP	#$40
		BCC	@exit

		LDA	#$00
		STA	valPepNRow

		JMP	@exit

@tstnxt0:
		CMP	#$0F
		BNE	@tstnxt1

		LDA	valPepTmp0 + 1
		BEQ	@exit

		CMP	#$20
		BCS	@spdch0

		STA	valPepSped
		STA	cntPepTick

		JMP	@exit

@spdch0:
;***FIXME:
;		Tempo := Param
		JMP	@exit

@tstnxt1:
		CMP	#$0C
		BNE	@exit
;	Set volume
		LDA	valPepTmp0 + 1
		CMP	#$40
		BCC	@svol0

		LDA	#40

@svol0:
		LDY	#PEP_CHNDATA::valVol
		STA	(ptrPepChan), Y

		JSR	peppitoChanUpdVol
		
		JMP	@exit

@exit:
		RTS


;-----------------------------------------------------------
peppitoChanUpdFreq:
;-----------------------------------------------------------
		JSR	peppitoChanCalcFreq

		LDA	valPepChan
		ASL
		ASL
		ASL
		ASL
		TAX

;	Rate
		LDA	$D77A
		STA	$D724, X
		LDA	$D77B
		STA	$D725, X
		LDA	#$00
		STA	$D726, X

		RTS

;-----------------------------------------------------------
peppitoChanUpdVol:
;-----------------------------------------------------------
		LDA	valPepChan
		ASL
		ASL
		ASL
		ASL
		TAX

		LDY	#PEP_CHNDATA::valVol
		LDA	(ptrPepChan), Y

		STA	$D729, X
		LDA	#$00
		LDX	valPepChan
		STA	$D71C, X

		RTS


;-----------------------------------------------------------
peppitoChanTrigger:
;-----------------------------------------------------------
;@halt:
;		INC	$D020
;		JMP	@halt

		LDY	#PEP_NOTDATA::valIns
		LDA	(ptrPepChan), Y
		STA	valPepTmp0

		BEQ	@cont0

		LDY	#PEP_CHNDATA::valAssIns
		STA	(ptrPepChan), Y

		ASL
		TAX
		LDA	idxPepIns0, X
		STA	ptrPepTmpA
		LDA	idxPepIns0 + 1, X
		STA	ptrPepTmpA + 1

		LDY	#PEP_CHNDATA::valSmpOffs
		LDA	#$00
		STA	(ptrPepChan), Y
		INY
		STA	(ptrPepChan), Y

		LDY	#PEP_CHNDATA::valFTune
		LDZ	#PEP_INSDATA::valFTune

		LDA	(ptrPepTmpA), Z
		STA	(ptrPepChan), Y

		LDY	#PEP_CHNDATA::valVol
		LDZ	#PEP_INSDATA::valVol

		LDA	(ptrPepTmpA), Z
		STA	(ptrPepChan), Y

;	If (Instruments[ Ins ].LoopLength > 0) And ( Channel.Instrument > 0 ) Then
		LDY	#PEP_INSDATA::valLLen
		LDA	(ptrPepTmpA), Y
		INY
		ORA (ptrPepTmpA), Y
		BEQ	@cont0

		LDY	#PEP_CHNDATA::valSelIns
		LDA	(ptrPepChan), Y
		BEQ	@cont0

		LDY	#PEP_CHNDATA::valSelIns
		LDA	valPepTmp0
		STA	(ptrPepChan), Y

@cont0:
;***FIXME:
;	If Channel.Note.Effect = $9 Then Begin
;		Channel.SampleOffset := ( Channel.Note.Param And $FF ) Shl 8;
;	End Else If Channel.Note.Effect = $15 Then Begin
;		Channel.FineTune := Channel.Note.Param;
;	End;

		LDY	#PEP_NOTDATA::valKey
		LDA	(ptrPepChan), Y
		STA	valPepTmp0
		INY
		ORA	(ptrPepChan), Y
		BEQ	@done

		LDA	(ptrPepChan), Y
		STA	valPepTmp0 + 1

;***FIXME:
;		Period := ( Channel.Note.Key * FineTuning[ Channel.FineTune And $F ] ) Shr 11;
;		Channel.PortaPeriod := ( Period Shr 1 ) + ( Period And 1 );

		LDY	#PEP_NOTDATA::valEff
		LDA	(ptrPepChan), Y
		CMP	#$03
		BEQ	@done
		CMP	#$05
		BEQ	@done

		LDY	#PEP_CHNDATA::valAssIns
		LDA	(ptrPepChan), Y
		LDY	#PEP_CHNDATA::valSelIns
		STA	(ptrPepChan), Y

		LDY	#PEP_CHNDATA::valPeriod
		LDA	valPepTmp0
		STA	(ptrPepChan), Y
		INY
		LDA	valPepTmp0 + 1
		STA	(ptrPepChan), Y

;		Channel.VTPhase := 0;

		JSR	peppitoChanNoteOn

@done:
		RTS


;-----------------------------------------------------------
peppitoChanCalcFreq:
;-----------------------------------------------------------
;	Amiga frequency
		LDA	#<VAL_FAC_AMIGARATEL
		STA	$D770
		LDA	#>VAL_FAC_AMIGARATEL
		STA	$D771
		LDA	#<VAL_FAC_AMIGARATEH
		STA	$D772
		LDA	#>VAL_FAC_AMIGARATEH
		STA	$D773

		LDY	#PEP_CHNDATA::valPeriod
		LDA	(ptrPepChan), Y
		STA	$D774
		INY

		LDA	(ptrPepChan), Y
		STA	$D775

		LDA	#$00
		STA	$D776
		STA	$D777
		
		LDA	$D020
		STA	$D020
		LDA	$D020
		STA	$D020
		LDA	$D020
		STA	$D020
		LDA	$D020
		STA	$D020

		LDA	$D76C
		STA	valPepTmp0
		LDA	$D76D
		STA	valPepTmp0 + 1
		LDA	$D76E
		STA	valPepTmp0 + 2
		LDA	$D76F
		STA	valPepTmp0 + 3

;	M65 frequency
		LDA	#<VAL_FAC_M65RATERTL
		STA	$D770
		LDA	#>VAL_FAC_M65RATERTL
		STA	$D771
		LDA	#<VAL_FAC_M65RATERTH
		STA	$D772
		LDA	#>VAL_FAC_M65RATERTH
		STA	$D773

		LDA	valPepTmp0
		STA	$D774
		LDA	valPepTmp0 + 1
		STA	$D775
		LDA	valPepTmp0 + 2
		STA	$D776
		LDA	valPepTmp0 + 3
		STA	$D777

		RTS


;-----------------------------------------------------------
peppitoChanNoteOn:
;-----------------------------------------------------------
		LDA	valPepChan
;		CMP	#$02
;		BNE	@start
;
;@halt:
;		INC	$D020
;		JMP	@halt
;
;@start:

		ASL
		ASL
		ASL
		ASL
		STA	valPepTmp2

		JSR	peppitoChanCalcFreq

		LDX	valPepTmp2

;	Silence
		LDA	#$00
		STA	$D720, X

		LDY	#PEP_CHNDATA::valSelIns
		LDA	(ptrPepChan), Y

		ASL
		TAY

		LDA	idxPepIns0, Y
		STA	ptrPepTmpA
		LDA	idxPepIns0 + 1, Y
		STA	ptrPepTmpA + 1

;	Rate
		LDA	$D77A
		STA	$D724, X
		LDA	$D77B
		STA	$D725, X
		LDA	#$00
		STA	$D726, X


;	Sample data, is it looping?
		LDY	#PEP_INSDATA::valLLen
		LDA	(ptrPepTmpA), Y
		INY
		ORA	(ptrPepTmpA), Y
		BEQ	@noloop

;	Sample start
		LDY	#PEP_INSDATA::valLStrt
		LDA	(ptrPepTmpA), Y
		STA	valPepTmp0
		INY
		LDA	(ptrPepTmpA), Y
		STA	valPepTmp0 + 1

		LDY	#PEP_INSDATA::ptrSmpD
		CLC
		LDA	(ptrPepTmpA), Y
		ADC	valPepTmp0
		STA	valPepTmp3
		STA	$D721, X
		INY
		LDA	(ptrPepTmpA), Y
		ADC	valPepTmp0 + 1
		STA	valPepTmp3 + 1
		STA	$D722, X
		INY
		LDA	(ptrPepTmpA), Y
		ADC	#$00
		STA	valPepTmp3 + 2
		STA	$D723, X
;		INY

;	Sample play offset
;***FIXME:
;	Should include PEP_CHNDATA::valSmpOffs

		LDY	#PEP_INSDATA::ptrSmpD
		LDA	(ptrPepTmpA), Y
		STA	$D72A, X
		INY
		LDA	(ptrPepTmpA), Y
		STA	$D72B, X
		INY
		LDA	(ptrPepTmpA), Y
		STA	$D72C, X

;	Sample end
		SEC
		LDA	valPepTmp3
		SBC	#$01
		STA	valPepTmp3
		LDA	valPepTmp3 + 1
		SBC	#$00
		STA	valPepTmp3 + 1

		LDY	#PEP_INSDATA::valLLen
		CLC
		LDA	(ptrPepTmpA), Y
		ADC	valPepTmp3
		STA	$D727, X
		INY
		LDA	(ptrPepTmpA), Y
		ADC	valPepTmp3 + 1
		STA	$D728, X

;	Enable DMA channel loop
		LDA	#$C2
		STA	$D720, X

		JMP	@finish

@noloop:
;	Sample start
		LDY	#PEP_INSDATA::ptrSmpD
		LDA	(ptrPepTmpA), Y
		STA	valPepTmp3
		STA	$D721, X
		INY
		LDA	(ptrPepTmpA), Y
		STA	valPepTmp3 + 1
		STA	$D722, X
		INY
		LDA	(ptrPepTmpA), Y
		STA	valPepTmp3 + 2
		STA	$D723, X
		INY

;	Sample play offset
		LDY	#PEP_CHNDATA::valSmpOffs
		CLC
		LDA	(ptrPepChan), Y
		ADC	valPepTmp3
		STA	$D72A, X
		INY
		LDA (ptrPepChan), Y
		ADC	valPepTmp3 + 1
		STA	$D72B, X
		LDA	#$00
		ADC	valPepTmp3 + 2
		STA	$D72C, X

;	Sample end
		LDY	#PEP_INSDATA::valSLen
		CLC
		LDA	(ptrPepTmpA), Y
		ADC	valPepTmp3
		STA	$D727, X
		INY
		LDA	(ptrPepTmpA), Y
		ADC	valPepTmp3 + 1
		STA	$D728, X

;	Enable DMA channel no loop
		LDA	#$82
		STA	$D720, X

@finish:
;	Volume
		LDY	#PEP_CHNDATA::valVol
		LDA	(ptrPepChan), Y
		STA	$D729, X
		LDA	#$00
		LDX	valPepChan
		STA	$D71C, X

		RTS

