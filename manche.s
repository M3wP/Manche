;===========================================================
;Manche MOD Replayer
;
;Version 0.20A
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

	.setcpu		"4510"

	.feature	leading_dot_in_identifiers, loose_string_term

;	Determine whether MOD loading is from D81 or SDC.
	.define		DEF_MNCH_USEMINI	0


numConvLEAD0	=	$C2
numConvDIGIT	=	$C3
numConvVALUE	=	$C4
numConvHeapPtr	=	$C6

ptrScreen		=	$C8
ptrModule		=	$CC

ptrTempA		=	$F0
ptrTempB		=	$F2
ptrTempC		=	$F4
ptrTempD		=	$F6
valTempA		=	$F8

ADDR_SCREEN		=	$4000

AD32_MODFILE	=	$00020000
AD32_COLOUR		=	$0FF80000

ADDR_DATABUF	=	$C000
ADDR_INPBUF		=	$0300

ADDR_DIRFPTRS	=	$8000
ADDR_DIRFNAMS	=	$8400
ADDR_DIRFNTOP	=	$C000
ADDR_SHUFPTRS	=	$C400


	.macro	.defPStr Arg
	.byte	.strlen(Arg), Arg
	.endmacro

	.macro	__MNCH_ASL_MEM16	mem
		CLC
		LDA	mem
		ASL
		STA	mem
		LDA	mem + 1
		ROL
		STA	mem + 1
	.endmacro

;-----------------------------------------------------------
;BASIC interface
;-----------------------------------------------------------
	.code
;start 2 before load address so
;we can inject it into the binary
	.org		$07FF			
						
	.byte		$01, $08		;load address
	
;BASIC next addr and this line #
	.word		_basNext, $000A		
	.byte		$9E			;SYS command
	.asciiz		"2061"			;2061 and line end
_basNext:
	.word		$0000			;BASIC prog terminator
	.assert		* = $080D, error, "BASIC Loader incorrect!"
;-----------------------------------------------------------

	.define KEY_ASC_A	$41
	.define KEY_ASC_B	$42
	.define KEY_ASC_C	$43
	.define KEY_ASC_D	$44
	.define KEY_ASC_E	$45
	.define KEY_ASC_F	$46
	.define KEY_ASC_L_A	$61
	.define KEY_ASC_L_B	$62
	.define KEY_ASC_L_C	$63
	.define KEY_ASC_L_D	$64
	.define KEY_ASC_L_E	$65
	.define KEY_ASC_L_F	$66
	.define KEY_ASC_BSLASH	$5C		;!!Needs screen code xlat
	.define KEY_ASC_CARET	$5E		;!!Needs screen code xlat
	.define KEY_ASC_USCORE	$5F		;!!Needs screen code xlat
	.define KEY_ASC_BQUOTE	$60		;!!Needs screen code xlat. !!Not C64
	.define KEY_ASC_OCRLYB	$7B		;!!Needs screen code xlat. !!Not C64
	.define KEY_ASC_LCRLYB	$7B		;Alternate
	.define KEY_ASC_PIPE	$7C		;!!Needs screen code xlat
	.define KEY_ASC_CCRLYB	$7D		;!!Needs screen code xlat. !!Not C64
	.define KEY_ASC_RCRLYB	$7D		;Alternate
	.define KEY_ASC_TILDE	$7E		;!!Needs screen code xlat

bootstrap:
		JMP	init


PEPPITO:
	.include	"peppito.s"


filename:
;	.defPStr	""
	.byte		$00
	.res		64, $20
	.byte		$00

errStr:
	.defPStr	"ERROR OPENING FILE!"


flgMnchDirty:
	.byte		$00
flgMnchPlay:
	.byte		$00
flgMnchLoad:
	.byte		$00

cntMnchTick:
	.word		$0000
cntMnchNTSC:
	.byte		$00

valMnchInst:
	.byte		$01

valMnchMsVol:
	.word		$FFDC
valMnchLLVol:
	.word		$B31A
valMnchLRVol:
	.word		$332C
valMnchRLVol:
	.word		$332C
valMnchRRVol:
	.word		$B31A

valMnchMsVPc:
	.byte		100
valMnchRRVPc:
	.byte		70
valMnchRLVPc:
	.byte		20

valMnchChVIt:
	.byte		$01, $01, $01, $01
valMnchChVFd:
	.byte		$00, $00, $00, $00


flgMnchJukeB:
	.byte		$00
flgMnchJNext:
	.byte		$00
flgMnchDirDn:
	.byte		$00
valMnchJFIdx:
	.word		$0000
valMnchJFCnt:
	.word		$0000


valMnchDummy:
	.byte		$00

valMnchTemp0:
	.dword		$00000000


valHexDigit0:
		.byte	"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
		.byte	KEY_ASC_A, KEY_ASC_B, KEY_ASC_C
		.byte	KEY_ASC_D, KEY_ASC_E, KEY_ASC_F

valVolSteps0:
		.byte	4, 4, 4, 4, 5, 5, 5, 10, 10, 12, 1

valMixSteps0:
		.byte	18, 9, 9, 9, 9, 9, 9, 9, 9, 9, 1

valVolColrs0:
		.byte	5, 5, 5, 5, 7, 7, 7, 8, 8, 10, 4



screenASCIIXLAT:
	.byte	KEY_ASC_BSLASH, KEY_ASC_CARET, KEY_ASC_USCORE, KEY_ASC_BQUOTE
	.byte	KEY_ASC_OCRLYB, KEY_ASC_PIPE, KEY_ASC_CCRLYB, KEY_ASC_TILDE, $00
screenASCIIXLATSub:
	.byte	$4D, $71, $64, $4A ,$55, $5D, $49, $45, $00
screenTemp0:
	.byte	$00


;lstDMATest:
;	.byte $0A				; Request format is F018A
;	.byte $80,$00			; Source MB 
;	.byte $81,$00			; Destination MB 
;	.byte $00				; No more options
;	.byte $00				; copy +  chained
;	.word $8000				; size of copy
;	.word $0000
;	.byte $01 				; source bank
;	.word $0000
;	.byte $01				; dest bank
;	.word $0000				; modulo


;-----------------------------------------------------------
init:
;-----------------------------------------------------------
;	disable standard CIA irqs
		LDA	#$7F			
		STA	$DC0D		;CIA IRQ control
		
;	No decimal mode, no interrupts
		CLD
		SEI

;	Setup Mega65 features and speed
		JSR	initM65IOFast

;	Disable ROM write-protect
		LDA	#$70
		STA	$D640
;	This is required as part of the hypervisor interface
		NOP

;	Clear the input buffer
		LDX	#$00
@loop0:
		LDA	$D610
		STX	$D610
		BNE	@loop0

	.if	.not	DEF_MNCH_USEMINI
		LDA	#>ADDR_DATABUF
		STA	Z:ptrBigglesBufHi
		LDA	#>ADDR_INPBUF
		STA	Z:ptrBigglesXfrHi
	.endif


;	Make the memory area pretty for debugging
		LDA	#$00
		LDX	#$00
@loop1:
		STA	ADDR_DIRFPTRS, X
		STA	ADDR_DIRFPTRS + $0100, X
		STA	ADDR_DIRFPTRS + $0200, X
		STA	ADDR_DIRFPTRS + $0300, X
		STA	ADDR_DIRFPTRS + $0400, X
		STA	ADDR_DIRFPTRS + $0500, X
		STA	ADDR_DIRFPTRS + $0600, X
		INX
		BNE	@loop1


		JSR	initState
		JSR	initScreen
		JSR	initAudio

		JSR	initIRQ

		SEI
		LDA	#$01
		STA	flgMnchDirty
		CLI

main:
;		LDA	#$03
;		STA	$D020
;
;		LDA #>lstDMATest
;		STA $D701
;		LDA #<lstDMATest
;		STA $D705
;
;		LDA	#$00
;		STA	$D020
		LDA	flgMnchJukeB
		BEQ	@getinput

		SEI
		LDA	flgMnchJNext
		CLI

		BEQ	@getinput

		SEI
		LDA	#$00
		STA	flgMnchJNext
		CLI

		JSR	jukeboxPlayNext

@getinput:
		LDA	$D610
		BEQ	main

		LDX	#$00
		STX	$D610

		CMP	#$F7
		BNE	@tstInsDn

		INC	valMnchInst
		LDA	valMnchInst
		CMP	valPepMaxI
		LBNE	@update

		LDX	valMnchInst
		DEX
		STX valMnchInst

		JMP	@update

@tstInsDn:
		CMP	#$F8
		BNE	@testf1

		DEC	valMnchInst
		LDA	valMnchInst
		CMP	#$00
		LBNE	@update

		LDA	#$01
		STA	valMnchInst
		JMP	@update

@testf1:
		CMP	#$F1
		BNE	@testspc

		LDA	#$00
		STA	flgMnchJukeB

		JSR	inputModule
		JSR	loadModule

		JMP	@update

@testspc:
		CMP	#' '
		BNE	@tstf3

		LDA	flgMnchLoad
		BEQ	@next0

		SEI
		LDA	flgMnchPlay
		EOR	#$01
		STA	flgMnchPlay
		CLI

		JMP	@update

@tstf3:
		CMP	#$F3
		BNE	@tstf4

	.if	.not	DEF_MNCH_USEMINI
		JSR	jukeboxStartDefault
		JMP	@update
	.else
		JMP	@next0
	.endif

@tstf4:
		CMP	#$F4
		BNE	@tstf5

	.if	.not	DEF_MNCH_USEMINI
		JSR	jukeboxStartShuffle
		JMP	@update
	.else
		JMP	@next0
	.endif

@tstf5:
		CMP	#$F5
		BNE	@tstf9

		LDA	flgMnchJukeB
		BEQ	@next0

		JSR	jukeboxPlayNext
		JMP	@update

@tstf9:
		CMP	#$F9
		BNE	@tstf10

		JSR	mixerIncMaster
		JMP	@update

@tstf10:
		CMP	#$FA
		BNE	@tstf11

		JSR	mixerDecMaster
		JMP	@update

@tstf11:
		CMP	#$FB
		BNE	@tstf12

		JSR	mixerIncLeftLeft
		JSR	mixerIncRightRight
		JMP	@update

@tstf12:
		CMP	#$FC
		BNE	@tstf13

		JSR	mixerDecLeftLeft
		JSR	mixerDecRightRight
		JMP	@update

@tstf13:
		CMP	#$FD
		BNE	@tstf14

		JSR	mixerIncLeftRight
		JSR	mixerIncRightLeft
		JMP	@update

@tstf14:
		CMP	#$FE
		BNE	@next0

		JSR	mixerDecLeftRight
		JSR	mixerDecRightLeft
;		JMP	@update

@update:
		SEI
		LDA	#$01
		STA	flgMnchDirty
		CLI

@next0:
		JMP	main



;-----------------------------------------------------------
jukeboxPlayNext:
;-----------------------------------------------------------

		SEI
		LDA	#$00
		STA	flgMnchLoad

		LDA	flgMnchPlay
		BEQ	@cont0

		JSR	PEPPITO + $0C

@cont0:
		LDA	#$00
		STA	flgMnchPlay

		CLI


		LDA	valMnchJFIdx + 1
		CMP	valMnchJFCnt + 1
		BCC	@begin
		BEQ	@tstlo0

		JMP	@fail

@tstlo0:
		LDA	valMnchJFIdx
		CMP	valMnchJFCnt
		BCC	@begin

		JMP	@fail


@begin:


;@halt0:
;		INC	$D020
;		JMP	@halt0

		LDA	valMnchJFIdx
		STA	valMnchTemp0
		LDA	valMnchJFIdx + 1
		STA	valMnchTemp0 + 1

		__MNCH_ASL_MEM16	valMnchTemp0

		LDA	flgMnchJukeB
		BMI	@shuffle

		CLC
		LDA	valMnchTemp0
		ADC	#<ADDR_DIRFPTRS
		STA	ptrTempB
		LDA	valMnchTemp0 + 1
		ADC	#>ADDR_DIRFPTRS
		STA	ptrTempB + 1

		JMP	@copyfn

@shuffle:
		CLC
		LDA	valMnchTemp0
		ADC	#<ADDR_SHUFPTRS
		STA	ptrTempB
		LDA	valMnchTemp0 + 1
		ADC	#>ADDR_SHUFPTRS
		STA	ptrTempB + 1

@copyfn:
		LDY	#$00
		LDA	(ptrTempB), Y
		STA	ptrTempC
		INY
		LDA	(ptrTempB), Y
		STA	ptrTempC + 1

		LDA	#$00
		STA	filename

		LDX	#$01
		LDY	#$00
@loop0:
		LDA	(ptrTempC), Y
		BEQ	@done0

		STA	filename, X
		INY
		INX
		JMP	@loop0

@done0:
		DEX
		STX	filename

		CLC
		LDA	valMnchJFIdx
		ADC	#$01
		STA	valMnchJFIdx
		LDA	valMnchJFIdx + 1
		ADC	#$00
		STA	valMnchJFIdx + 1

		JSR	loadModule

		LDA	flgMnchLoad
		BEQ	@fail

		SEI
;		LDA	flgMnchPlay
		LDA	#$01
		STA	flgMnchPlay
		CLI

		RTS

@fail:
		LDA	#$00
		STA	flgMnchJukeB

		RTS


	.if	.not	DEF_MNCH_USEMINI
;-----------------------------------------------------------
jukeboxStartShuffle:
;-----------------------------------------------------------
		LDA	flgMnchJukeB
		BEQ	@begin
		BPL	@begin

@nostart:
		RTS

@begin:
		LDA	flgMnchDirDn
		BNE	@cont0

		JSR	dirListLoad

@cont0:
		LDA	#$00
		STA	valMnchJFIdx
		STA	valMnchJFIdx + 1

		LDA	valMnchJFCnt
		ORA	valMnchJFCnt + 1
		BEQ	@nostart

		JSR	jukeboxRandomise

		LDA	#$81
		STA	flgMnchJukeB

		JSR	jukeboxPlayNext

		RTS


;-----------------------------------------------------------
getRandomByte:
;-----------------------------------------------------------
;	get parity of 256 reads of lowest bit of FPGA temperature
;	for each bit. Then add to raster number.
;	Should probably have more than enough entropy, even if the
;	temperature is biased, as we are looking only at parity of
;	a large number of reads.
;	(Whether it is cryptographically secure is a separate issue.
;	but it should be good enough for random MAC address generation).
		LDA #$00
		LDX #8
		LDY #0
@bitLoop:
		EOR $D6DE
		DEY
		BNE @bitLoop
;	low bit into C
		LSR
;	then into A
		ROR
		DEX
		BPL @bitLoop
		CLC
		ADC $D012

		RTS


;-----------------------------------------------------------
jukeboxRandomise:
;-----------------------------------------------------------
		LDA	#<ADDR_DIRFPTRS
		STA	ptrTempB
		LDA	#>ADDR_DIRFPTRS
		STA	ptrTempB + 1

		LDA	#<ADDR_SHUFPTRS
		STA	ptrTempC
		LDA	#>ADDR_SHUFPTRS
		STA	ptrTempC + 1

		LDA	valMnchJFCnt
		STA	valTempA
		LDA	valMnchJFCnt + 1
		STA	valTempA + 1

		__MNCH_ASL_MEM16 valTempA

		LDY	#$00
@loop0:
		LDA	(ptrTempB), Y
		STA	(ptrTempC), Y

		INW	ptrTempB
		INW	ptrTempC

		DEW	valTempA

		LDA	valTempA + 1
		CMP	#$FF
		BNE	@loop0


		LDA	valMnchJFCnt
		STA	valTempA
		STA	valMnchTemp0 + 2
		LDA	valMnchJFCnt + 1
		STA	valTempA + 1
		STA	valMnchTemp0 + 3

		SEC
		LDA	valMnchTemp0 + 2
		SBC	#$01
		STA	valMnchTemp0 + 2
		LDA	valMnchTemp0 + 3
		SBC	#$00
		STA	valMnchTemp0 + 3

		LDA	#<ADDR_SHUFPTRS
		STA	ptrTempC
		LDA	#>ADDR_SHUFPTRS
		STA	ptrTempC + 1

@loop1:

;	Get low byte of swap pos
@loop2:
		JSR	getRandomByte
		STA	ptrTempB
		LDA valMnchTemp0 + 2
		CMP	ptrTempB
		BCC	@loop2

;	Skip high byte if not so many
		LDA	#$00
		STA	ptrTempB + 1
		LDA	valMnchTemp0 + 3
		BEQ	@cont0

;	Get high byte of swap pos
@loop3:
		JSR	getRandomByte
		AND	#$03
		STA	ptrTempB + 1
		LDA	valMnchTemp0 + 3
		CMP	ptrTempB + 1
		BCC	@loop3

@cont0:
		__MNCH_ASL_MEM16 ptrTempB

		CLC
		LDA	ptrTempB
		ADC	#<ADDR_SHUFPTRS
		STA	ptrTempB
		LDA	ptrTempB + 1
		ADC	#>ADDR_SHUFPTRS
		STA	ptrTempB + 1

		LDY	#$00

		LDA	(ptrTempB), Y
		STA	valMnchTemp0
		LDA	(ptrTempC), Y
		STA	(ptrTempB), Y
		LDA	valMnchTemp0
		STA	(ptrTempC), Y

		INY

		LDA	(ptrTempB), Y
		STA	valMnchTemp0
		LDA	(ptrTempC), Y
		STA	(ptrTempB), Y
		LDA	valMnchTemp0
		STA	(ptrTempC), Y

		INW	ptrTempC
		INW	ptrTempC

		DEW	valTempA
		BNE	@loop1

		RTS


;-----------------------------------------------------------
jukeboxStartDefault:
;-----------------------------------------------------------
		LDA	flgMnchJukeB
		BEQ	@begin
		BMI	@begin

@nostart:
		RTS

@begin:
		LDA	flgMnchDirDn
		BNE	@cont0

		JSR	dirListLoad

@cont0:
		LDA	#$00
		STA	valMnchJFIdx
		STA	valMnchJFIdx + 1

		LDA	valMnchJFCnt
		ORA	valMnchJFCnt + 1

		BEQ	@nostart

		LDA	#$01
		STA	flgMnchJukeB

		JSR	jukeboxPlayNext

		RTS


;sdcounter:
;		.dword		$00000000

;;-------------------------------------------------------------------------------
;sdwaitawhile:
;;-------------------------------------------------------------------------------
;		JSR	sdtimeoutreset
;
;@sw1:
;		INC	sdcounter + 0
;		BNE	@sw1
;		INC	sdcounter + 1
;		BNE	@sw1
;		INC	sdcounter + 2
;		BNE @sw1
;
;		RTS
;
;;-------------------------------------------------------------------------------
;sdtimeoutreset:
;;-------------------------------------------------------------------------------
;;	count to timeout value when trying to read from SD card
;;	(if it is too short, the SD card won't reset)
;
;		LDA	#$00
;		STA	sdcounter + 0
;		STA	sdcounter + 1
;		LDA	#$F3
;		STA	sdcounter + 2
;
;		RTS

;-----------------------------------------------------------
dirListLoad:
;-----------------------------------------------------------
		JSR	bigglesCloseFile

;		LDA	#$81
;		STA	$D680
;
;		JSR	sdwaitawhile
;
;;	Work out if we are using primary or secondard SD card
;
;;	First try resetting card 1 (external)
;;	so that if you have an external card, it will be used in preference
;		LDA	#$C0
;		STA	$D680
;		LDA	#$00
;		STA	$D680
;		LDA	#$01
;		STA	$D680
;
;		JSR	sdwaitawhile


;*******************************************************************************
;***FIXME These calls need to be put in bigglesworth
;*******************************************************************************
;	Get default drive/partition
		LDA	#$02
		STA	$D640
		NOP

;	Set drive/partition
		TAX
		LDA	#$06
		STA	$D640
		NOP

;	Change to root directory
		LDA #$3C
		STA $D640
		NOP
;*******************************************************************************

		LDA	#$00
		STA	valMnchJFCnt
		STA	valMnchJFCnt + 1

		STA	ptrTempB
		STA	ptrTempC
		STA	ptrTempD

		LDA	#>ADDR_DATABUF
		STA	ptrTempB + 1

		LDA	#>ADDR_DIRFNAMS
		STA	ptrTempC + 1

		LDA	#>ADDR_DIRFPTRS
		STA	ptrTempD + 1

		LDA	#$01
		STA	flgMnchDirDn

		JSR	bigglesOpenDir
		CMP	#$00
		BNE	@begin

;	Assume this means failure
@fail:
		LDA	#$02
		STA	$D020

		RTS

@begin:
		STA	valMnchDummy

;		LDA	#$38
;		STA	$D640
;		NOP
;
;		CMP	#$00
;		BEQ	@cont
;
;		PHA
;
;		LDA	#$0E
;		STA	$D020
;
;		PLA
;		SEI
;
;@halt:
;		JMP	@halt
;
;		JMP	@fail
;
;@cont:
;		LDA	valMnchDummy
		
@loop:
		JSR	bigglesReadDir
		BCS	@done

;	Not directories
		AND	#$10
		BNE	@next

		JSR	dirMatchFile
		BCC	@add

@next:
		LDA	valMnchDummy
		JMP	@loop

@add:
		JSR	dirAddFile
		JMP	@next

@done:
		LDA	valMnchDummy
		JSR	bigglesCloseDir

;@wait0:
;		INC	$D020
;		LDA	$D610
;		BEQ	@wait0
;		LDA	#$00
;		STA	$D610

		RTS


extBytes0:
		.byte	'd', 'o', 'm', '.'
extBytes4:
		.byte	'D', 'O', 'M', '.'

;-----------------------------------------------------------
dirMatchFile:
;-----------------------------------------------------------
		LDY	#$FF
@loop0:
		INY
		CPY	#64
		BEQ	@fail

		LDA	(ptrTempB), Y
		BNE	@loop0

;		CPY	#$00
;		BEQ	@fail
		CPY	#$06
		BCC	@fail

		DEY
		LDX	#$00
@loop1:
		LDA	(ptrTempB), Y
		CMP	extBytes0, X
		BEQ	@next1

		CMP	extBytes4, X
		BEQ	@next1

		JMP	@fail

@next1:
		DEY
		INX
		CPX	#$04
		BNE	@loop1

		CLC
		RTS

@fail:
		SEC
		RTS


;-----------------------------------------------------------
dirAddFile:
;-----------------------------------------------------------
		LDA	ptrTempD + 1
		CMP	#>ADDR_DIRFNAMS
		BNE	@cont

		RTS

@cont:
		LDA	ptrTempC + 1
		CMP	#>ADDR_DIRFNTOP
		BNE	@begin

		RTS

@begin:
		LDY	#$00
		LDA	ptrTempC
		STA	(ptrTempD), Y
		INY
		LDA	ptrTempC + 1
		STA	(ptrTempD), Y

		LDY	#$00
		LDZ	#$00
@loop:
		LDA	(ptrTempB), Y
		STA	(ptrTempC), Z

		PHA

		INW	ptrTempC
		INY

		LDA	ptrTempC + 1
		CMP	#>ADDR_DIRFNTOP
		BNE	@next

		PLA
		RTS

@next:
		PLA
		BNE	@loop

		INW	ptrTempD
		INW	ptrTempD

		CLC
		LDA	valMnchJFCnt
		ADC	#$01
		STA	valMnchJFCnt
		LDA	valMnchJFCnt + 1
		ADC	#$00
		STA	valMnchJFCnt + 1


		RTS


	.endif


;-----------------------------------------------------------
mixerIncRightRight:
;-----------------------------------------------------------
		INC	valMnchRRVPc

		CLC
		LDA	valMnchRRVol
		ADC	#$8F
		STA	valMnchRRVol
		LDA	valMnchRRVol + 1
		ADC	#$02
		STA	valMnchRRVol + 1
		LDA	#$00
		ADC	#$00

		BNE	@max

		LDA	valMnchRRVol + 1
		CMP	#$FF
		BCC	@exit
		BNE	@max
		LDA	valMnchRRVol
		CMP	#$DC
		BCC	@exit
@max:
		LDA	#$DC
		STA	valMnchRRVol
		LDA	#$FF
		STA	valMnchRRVol + 1

		LDA	#100
		STA	valMnchRRVPc

@exit:
		JSR	setRightRVolume

		RTS


;-----------------------------------------------------------
mixerDecRightRight:
;-----------------------------------------------------------
		DEC	valMnchRRVPc

		SEC
		LDA	valMnchRRVol
		SBC	#$8F
		STA	valMnchRRVol
		LDA	valMnchRRVol + 1
		SBC	#$02
		STA	valMnchRRVol + 1
		LDA	#$00
		SBC	#$00

		BPL	@exit

		LDA	#$00
		STA	valMnchRRVol
		STA	valMnchRRVol + 1

		STA	valMnchRRVPc

@exit:
		JSR	setRightRVolume

		RTS


;-----------------------------------------------------------
mixerIncRightLeft:
;-----------------------------------------------------------
		INC	valMnchRLVPc

		CLC
		LDA	valMnchRLVol
		ADC	#$8F
		STA	valMnchRLVol
		LDA	valMnchRLVol + 1
		ADC	#$02
		STA	valMnchRLVol + 1
		LDA	#$00
		ADC	#$00

		BNE	@max

		LDA	valMnchRLVol + 1
		CMP	#$FF
		BCC	@exit
		BNE	@max
		LDA	valMnchRLVol
		CMP	#$DC
		BCC	@exit
@max:
		LDA	#$DC
		STA	valMnchRLVol
		LDA	#$FF
		STA	valMnchRLVol + 1

		LDA	#100
		STA	valMnchRLVPc

@exit:
		JSR	setRightLVolume

		RTS


;-----------------------------------------------------------
mixerDecRightLeft:
;-----------------------------------------------------------
		DEC	valMnchRLVPc

		SEC
		LDA	valMnchRLVol
		SBC	#$8F
		STA	valMnchRLVol
		LDA	valMnchRLVol + 1
		SBC	#$02
		STA	valMnchRLVol + 1
		LDA	#$00
		SBC	#$00

		BPL	@exit

		LDA	#$00
		STA	valMnchRLVol
		STA	valMnchRLVol + 1

		STA	valMnchRLVPc


@exit:
		JSR	setRightLVolume

		RTS


;-----------------------------------------------------------
mixerIncLeftRight:
;-----------------------------------------------------------
		LDA	$D629
		AND	#$40
		BEQ	@nonnexys

		RTS
		
@nonnexys:
		CLC
		LDA	valMnchLRVol
		ADC	#$8F
		STA	valMnchLRVol
		LDA	valMnchLRVol + 1
		ADC	#$02
		STA	valMnchLRVol + 1
		LDA	#$00
		ADC	#$00

		BNE	@max

		LDA	valMnchLRVol + 1
		CMP	#$FF
		BCC	@exit
		BNE	@max
		LDA	valMnchLRVol
		CMP	#$DC
		BCC	@exit
@max:
		LDA	#$DC
		STA	valMnchLRVol
		LDA	#$FF
		STA	valMnchLRVol + 1

@exit:
		JSR	setLeftRVolume

		RTS


;-----------------------------------------------------------
mixerDecLeftRight:
;-----------------------------------------------------------
		LDA	$D629
		AND	#$40
		BEQ	@nonnexys

		RTS
		
@nonnexys:
		SEC
		LDA	valMnchLRVol
		SBC	#$8F
		STA	valMnchLRVol
		LDA	valMnchLRVol + 1
		SBC	#$02
		STA	valMnchLRVol + 1
		LDA	#$00
		SBC	#$00

		BPL	@exit

		LDA	#$00
		STA	valMnchLRVol
		STA	valMnchLRVol + 1

@exit:
		JSR	setLeftRVolume

		RTS


;-----------------------------------------------------------
mixerIncLeftLeft:
;-----------------------------------------------------------
		LDA	$D629
		AND	#$40
		BEQ	@nonnexys

		RTS
		
@nonnexys:
		CLC
		LDA	valMnchLLVol
		ADC	#$8F
		STA	valMnchLLVol
		LDA	valMnchLLVol + 1
		ADC	#$02
		STA	valMnchLLVol + 1
		LDA	#$00
		ADC	#$00

		BNE	@max

		LDA	valMnchLLVol + 1
		CMP	#$FF
		BCC	@exit
		BNE	@max
		LDA	valMnchLLVol
		CMP	#$DC
		BCC	@exit
@max:
		LDA	#$DC
		STA	valMnchLLVol
		LDA	#$FF
		STA	valMnchLLVol + 1

@exit:
		JSR	setLeftLVolume

		RTS


;-----------------------------------------------------------
mixerDecLeftLeft:
;-----------------------------------------------------------
		LDA	$D629
		AND	#$40
		BEQ	@nonnexys

		RTS
		
@nonnexys:
		SEC
		LDA	valMnchLLVol
		SBC	#$8F
		STA	valMnchLLVol
		LDA	valMnchLLVol + 1
		SBC	#$02
		STA	valMnchLLVol + 1
		LDA	#$00
		SBC	#$00

		BPL	@exit

		LDA	#$00
		STA	valMnchLLVol
		STA	valMnchLLVol + 1

@exit:
		JSR	setLeftLVolume

		RTS


;-----------------------------------------------------------
mixerIncMaster:
;-----------------------------------------------------------
		INC	valMnchMsVPc

		CLC
		LDA	valMnchMsVol
		ADC	#$8F
		STA	valMnchMsVol
		LDA	valMnchMsVol + 1
		ADC	#$02
		STA	valMnchMsVol + 1
		LDA	#$00
		ADC	#$00

		BNE	@max

		LDA	valMnchMsVol + 1
		CMP	#$FF
		BCC	@exit
		BNE	@max
		LDA	valMnchMsVol
		CMP	#$DC
		BCC	@exit
@max:
		LDA	#100
		STA	valMnchMsVPc

		LDA	#$DC
		STA	valMnchMsVol
		LDA	#$FF
		STA	valMnchMsVol + 1

@exit:
		JSR	setMasterVolume

		RTS


;-----------------------------------------------------------
mixerDecMaster:
;-----------------------------------------------------------
		DEC	valMnchMsVPc

		SEC
		LDA	valMnchMsVol
		SBC	#$8F
		STA	valMnchMsVol
		LDA	valMnchMsVol + 1
		SBC	#$02
		STA	valMnchMsVol + 1
		LDA	#$00
		SBC	#$00

		BPL	@exit

		LDA	#$00
		STA	valMnchMsVol
		STA	valMnchMsVol + 1

		STA	valMnchMsVPc

@exit:
		JSR	setMasterVolume

		RTS


;-----------------------------------------------------------
displayMixerInfo:
;-----------------------------------------------------------
		LDA	#<(ADDR_SCREEN + (14 * 80) + 0)
		STA	ptrScreen
		LDA	#>(ADDR_SCREEN + (14 * 80) + 0)
		STA	ptrScreen + 1

		LDA	#$00
		STA	valMnchDummy

		LDX	#$00
@loop0:
		PHX

		LDA	valMnchMsVPc, X
		STA	ptrTempA
		LDA	#$00
		STA	ptrTempA + 1

		LDY	valMnchDummy
		LDZ	#$00
		LDA	#$20
@loop1:
		STA	(ptrScreen), Y
		INY
		INZ
		CPZ	#$0B
		BNE	@loop1

		LDY	valMnchDummy
		LDX	#$00

		LDA	ptrTempA + 1
@loop2:
		BMI	@next0

		LDA	ptrTempA
		BEQ	@next0

		TAZ
		LDA	#$A0
		STA	(ptrScreen), Y
		INY
		TZA

		SEC
		LDA	ptrTempA
		SBC	valMixSteps0, X
		STA	ptrTempA
		LDA	ptrTempA + 1
		SBC	#$00
		STA	ptrTempA + 1
		
		PHA
		INX
		PLA

		JMP	@loop2

@next0:
		CLC
		LDA	valMnchDummy
		ADC	#$0C
		STA	valMnchDummy

		PLX
		INX
		CPX	#$03
		BNE	@loop0

		LDA	#<(ADDR_SCREEN + (15 * 80) + 6)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (15 * 80) + 6)
		STA	numConvHeapPtr + 1

		LDA	valMnchMsVPc
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		LDA	#<(ADDR_SCREEN + (15 * 80) + 18)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (15 * 80) + 18)
		STA	numConvHeapPtr + 1

		LDA	valMnchRRVPc
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		LDA	#<(ADDR_SCREEN + (15 * 80) + 30)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (15 * 80) + 30)
		STA	numConvHeapPtr + 1

		LDA	valMnchRLVPc
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		RTS


;-----------------------------------------------------------
displayInstInfo:
;-----------------------------------------------------------
		LDA	#<(ADDR_SCREEN + (2 * 80) + 70)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (2 * 80) + 70)
		STA	numConvHeapPtr + 1

		LDA	valMnchInst
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		LDA	valMnchInst
		ASL
		TAX

		LDA	idxPepIns0, X
		STA	ptrTempA
		LDA	idxPepIns0 + 1, X
		STA	ptrTempA + 1

		LDY	#PEP_INSDATA::ptrHdr
		LDA	(ptrTempA), Y
		STA	ptrModule
		INY
		LDA	(ptrTempA), Y
		STA	ptrModule + 1
		INY
		LDA	(ptrTempA), Y
		STA	ptrModule + 2
		INY
		LDA	(ptrTempA), Y
		STA	ptrModule + 3
		
		LDA	#<(ADDR_SCREEN + (2 * 80) + 50)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (2 * 80) + 50)
		STA	numConvHeapPtr + 1

		LDZ	#$00
@loop:
		NOP
		LDA	(ptrModule), Z

		JSR	screenASCIIToScreen

		STA	(numConvHeapPtr), Z
		INZ

		CPZ	#$16
		BNE	@loop

		LDA	#<(ADDR_SCREEN + (3 * 80) + 60)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (3 * 80) + 60)
		STA	numConvHeapPtr + 1

		LDY	#PEP_INSDATA::valVol
		LDA	(ptrTempA), Y
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1

		JSR	numConvPRTINT

		LDA	#<(ADDR_SCREEN + (3 * 80) + 70)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (3 * 80) + 70)
		STA	numConvHeapPtr + 1

		LDY	#PEP_INSDATA::valFTune
		LDA	(ptrTempA), Y
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1

		JSR	numConvPRTINT

		LDA	#<(ADDR_SCREEN + (4 * 80) + 70)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (4 * 80) + 70)
		STA	numConvHeapPtr + 1

		LDY	#PEP_INSDATA::valSLen
		LDA	(ptrTempA), Y
		STA	numConvVALUE
		INY
		LDA	(ptrTempA), Y
		STA	numConvVALUE + 1

		JSR	numConvPRTINT

		LDA	#<(ADDR_SCREEN + (5 * 80) + 70)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (5 * 80) + 70)
		STA	numConvHeapPtr + 1

		LDY	#PEP_INSDATA::valLStrt
		LDA	(ptrTempA), Y
		STA	numConvVALUE
		INY
		LDA	(ptrTempA), Y
		STA	numConvVALUE + 1

		JSR	numConvPRTINT

		LDA	#<(ADDR_SCREEN + (6 * 80) + 70)
		STA	numConvHeapPtr
		LDA	#>(ADDR_SCREEN + (6 * 80) + 70)
		STA	numConvHeapPtr + 1

		LDY	#PEP_INSDATA::valLLen
		LDA	(ptrTempA), Y
		STA	numConvVALUE
		INY
		LDA	(ptrTempA), Y
		STA	numConvVALUE + 1

		JSR	numConvPRTINT

		RTS


;-----------------------------------------------------------
displaySongInfo:
;-----------------------------------------------------------
		LDA	#<(ADDR_SCREEN + 2 * 80)
		STA	ptrScreen
		LDA	#>(ADDR_SCREEN + 2 * 80)
		STA	ptrScreen + 1

		LDA	#<.loword(AD32_MODFILE)
		STA	ptrModule
		LDA	#>.loword(AD32_MODFILE)
		STA	ptrModule + 1
		LDA	#<.hiword(AD32_MODFILE)
		STA	ptrModule + 2
		LDA	#>.hiword(AD32_MODFILE)
		STA	ptrModule + 3

		LDZ	#$00
@loop:
		NOP
		LDA	(ptrModule) , Z

		JSR	screenASCIIToScreen

		STA	(ptrScreen), Z
		INZ

		CPZ	#$14
		BNE	@loop

		RTS


;-----------------------------------------------------------
displayCurrVol:
;-----------------------------------------------------------
		LDA	#<(ADDR_SCREEN + (6 * 80) + 0)
		STA	ptrScreen
		LDA	#>(ADDR_SCREEN + (6 * 80) + 0)
		STA	ptrScreen + 1

		LDA	#$00
		STA	valMnchDummy

		LDX	#$00
@loop0:
		PHX

		TXA
		ASL
		TAX

		LDA	idxPepChn0, X
		STA	ptrTempA
		LDA	idxPepChn0 + 1, X
		STA	ptrTempA + 1

		LDY	valMnchDummy
		LDZ	#$00
		LDA	#$20
@loop1:
		STA	(ptrScreen), Y
		INY
		INZ
		CPZ	#$0C
		BNE	@loop1

		PLX
		PHX

		LDY	#PEP_NOTDATA::valKey
		LDA	(ptrTempA), Y
		INY
		ORA	(ptrTempA), Y

		BNE	@cont0

		LDA	#$02
		STA	valMnchChVIt, X

		LDY	#PEP_CHNDATA::valVol
		LDA	(ptrTempA), Y

;		LSR
		STA	valMnchChVFd, X

		JMP	@cont1

@cont0:
		LDY	#PEP_CHNDATA::valVol
		LDA	(ptrTempA), Y
		STA	valMnchChVFd, X

		LDA	valMnchChVIt, X
		BEQ	@cont1

		SEC
		LDA	valMnchChVIt, X
		SBC	#$01
		STA	valMnchChVIt, X

		LDA	valMnchChVFd, X
		LSR
		STA	valMnchChVFd, X

;		JMP	@next0

@cont1:
;		LDY	#PEP_CHNDATA::valVol
;		LDA	(ptrTempA), Y
		LDA	valMnchChVFd, X

		PHA
		LDY	valMnchDummy
		LDX	#$00
		PLA
@loop2:
		BEQ	@next0
		BMI	@next0

		TAZ
		LDA	#$A0
		STA	(ptrScreen), Y
		INY
		TZA
		SEC
		SBC	valVolSteps0, X
		
		PHA
		INX
		PLA

		JMP	@loop2

@next0:
		CLC
		LDA	valMnchDummy
		ADC	#$0C
		STA	valMnchDummy

		PLX
		INX
		CPX	#$04
		LBNE	@loop0

		RTS


;-----------------------------------------------------------
displayCurrRow:
;	Only call when IRQ can't be called (eg from IRQ handler)
;-----------------------------------------------------------
		LDA	#<(ADDR_SCREEN + (8 * 80) + 0)
		STA	ptrScreen
		LDA	#>(ADDR_SCREEN + (8 * 80) + 0)
		STA	ptrScreen + 1

		LDA	#$00
		STA	valMnchDummy

		LDX	#$00
@loop0:
		PHX

		TXA
		ASL
		TAX

		LDA	idxPepChn0, X
		STA	ptrTempA
		LDA	idxPepChn0 + 1, X
		STA	ptrTempA + 1

;	Note key
		LDY	#PEP_NOTDATA::valKey
		LDA	(ptrTempA), Y
		INY
		STA	numConvVALUE

		LDA	(ptrTempA), Y
		STA	numConvVALUE + 1

		LDY	valMnchDummy
		LDA	numConvVALUE + 1
		AND	#$0F
		JSR	outputHexNybble

		LDA	numConvVALUE
		JSR	outputHexByte

		LDA	#':'
		STA	(ptrScreen), Y
		INY

		STY	valMnchDummy

;	Note instrument
		LDY	#PEP_NOTDATA::valIns
		LDA	(ptrTempA), Y
		STA	numConvVALUE

		LDY	valMnchDummy
		LDA	numConvVALUE
		JSR	outputHexByte

		LDA	#':'
		STA	(ptrScreen), Y
		INY

		STY	valMnchDummy

;	Note effect
		LDY	#PEP_NOTDATA::valEff
		LDA	(ptrTempA), Y
		STA	numConvVALUE

		LDY	valMnchDummy
		LDA	numConvVALUE
		JSR	outputHexByte

		STY	valMnchDummy

;	Note param
		LDY	#PEP_NOTDATA::valPrm
		LDA	(ptrTempA), Y
		STA	numConvVALUE

		LDY	valMnchDummy
		LDA	numConvVALUE
		JSR	outputHexByte

		INY
		STY	valMnchDummy

		PLX
		INX
		CPX	#$04
		LBNE	@loop0


		RTS


;-----------------------------------------------------------
displayPString:
;-----------------------------------------------------------
		LDY	#$00
		LDZ	#$00

		LDA	(ptrTempA), Y
		TAX
		BEQ	@exit

		INY

@loop0:
		LDA	(ptrTempA), Y
		STA	(ptrScreen), Z

		INY
		INZ

		DEX
		BNE	@loop0

@exit:
		RTS


;-----------------------------------------------------------
error:
;-----------------------------------------------------------
		LDA	#<(ADDR_SCREEN + 2 * 80)
		STA	ptrScreen
		LDA	#>(ADDR_SCREEN + 2 * 80)
		STA	ptrScreen + 1

		LDA	#<errStr
		STA	ptrTempA
		LDA	#>errStr
		STA	ptrTempA + 1

		JSR	displayPString

;@halt:
;		JMP	@halt

		RTS


;-----------------------------------------------------------
inputModule:
;-----------------------------------------------------------
		SEI
		LDA	#$00
		STA	flgMnchLoad

		LDA	flgMnchPlay
		BEQ	@cont0

		JSR	PEPPITO + $0C

@cont0:
		LDA	#$00
		STA	flgMnchPlay

		CLI

		LDA	#$01
		STA	valMnchInst

		LDA	filename
		STA	valMnchDummy

		LDA	#$14
		STA	filename

		LDA	#<(ADDR_SCREEN + 2 * 80)
		STA	ptrScreen
		LDA	#>(ADDR_SCREEN + 2 * 80)
		STA	ptrScreen + 1

		LDA	#<filename
		STA	ptrTempA
		LDA	#>filename
		STA	ptrTempA + 1

@loop0:
		JSR	displayPString

		LDY	valMnchDummy
		LDA	(ptrScreen), Y
		ORA	#$80
		STA	(ptrScreen), Y

		INY
		LDA	#$20
		STA	(ptrScreen), Y

@loop1:
		LDA	$D610
		BEQ	@loop1

		CMP	#$14
		BEQ	@delkey

		CMP	#$0D
		BEQ	@retkey

		CMP	#$20
		BCC	@next1

		CMP	#$7B
		BCC	@acceptkey

@next1:
		LDA	#$00
		STA	$D610
		JMP	@loop1

@acceptkey:
		LDY	valMnchDummy
		CPY	#$10
		BCC	@append0

		JMP	@next1

@append0:
		CMP	#$61
		BCC	@append1

		CMP	#$7B
		BCC	@append2

@append1:
		INY
		STA	filename, Y
		STY	valMnchDummy

		JMP	@next0

@append2:
		SEC
		SBC	#$20
		JMP	@append1

@delkey:
		LDY	valMnchDummy
		BEQ	@next1

		LDA	#$20
		STA	filename, Y
		DEY
		STY	valMnchDummy

		JMP	@next0

@retkey:
		LDY	valMnchDummy
		BEQ	@next1

		STY	filename
		LDA	#$00
		STA	$D610

		JMP	@load0

@next0:
		LDA	#$00
		STA	$D610
		JMP	@loop0


@load0:
		RTS


;-----------------------------------------------------------
loadModule:
;-----------------------------------------------------------
		SEI
;@halt:
;		INC	$D020
;		JMP	@halt
;
;		LDA	filename			;Set the file name
;		LDX	#<(filename + 1)
;		LDY	#>(filename + 1)

	.if	DEF_MNCH_USEMINI
		LDA	filename
		LDX	#<(filename + 1)
		LDY	#>(filename + 1)

		JSR	miniSetFileName

		LDA	#VAL_DOSFTYPE_SEQ 
		JSR	miniSetFileType

		JSR	miniOpenFile
	.else
		LDY	filename
		INY
		LDA	#$00
		STA	filename, Y

		LDX	#<(filename + 1)
		LDY	#>(filename + 1)

		JSR	bigglesSetFileName
		
		LDY	filename
		INY
		LDA	#$20
		STA	filename, Y

		JSR	bigglesOpenFile
	.endif

		BCC	@cont1

		JSR	error
		RTS

@cont1:
		LDA	#<.loword(AD32_MODFILE)
		STA	ptrScreen
		STA	adrPepMODL
		STA	ptrModule
		LDA	#>.loword(AD32_MODFILE)
		STA	ptrScreen + 1
		STA	adrPepMODL + 1
		STA	ptrModule + 1
		LDA	#<.hiword(AD32_MODFILE)
		STA	ptrScreen + 2
		STA	adrPepMODL + 2
		STA	ptrModule + 2
		LDA	#>.hiword(AD32_MODFILE)
		STA	ptrScreen + 3
		STA	ptrModule + 3
		STA	adrPepMODL + 3

		LDZ	#$00
@loop:
	.if	DEF_MNCH_USEMINI
		JSR	miniReadByte
	.else
		JSR	bigglesReadByte
	.endif
		BCS	@done

		NOP
		STA	(ptrScreen), Z

		STA	$D020

		INZ
		BNE	@loop

		CLC
		LDA	#$00
		ADC	ptrScreen
		STA	ptrScreen
		LDA	#$01
		ADC	ptrScreen + 1
		STA	ptrScreen + 1
		LDA	#$00
		ADC	ptrScreen + 2
		STA	ptrScreen + 2
		LDA	#$00
		ADC	ptrScreen + 3
		STA	ptrScreen + 3

		JMP	@loop

@done:
	.if	DEF_MNCH_USEMINI
		JSR	miniCloseFile
	.else
		JSR	bigglesCloseFile
	.endif

		LDA	#$00
		STA	$D020

		JSR	PEPPITO

		LDA	#$01
		STA	flgMnchLoad
		STA	flgMnchDirty
		CLI

		RTS


;-----------------------------------------------------------
plyrNOP:
;-----------------------------------------------------------
		RTI


;-----------------------------------------------------------
plyrIRQ:
;-----------------------------------------------------------
		PHP				;save the initial state
		PHA
		TXA				
		PHA
		TYA
		PHA

		CLD
		
;	Is the VIC-II needing service?
		LDA	$D019		;IRQ regs
		AND	#$01
		BNE	@proc
		
;	Some other interrupt source??  Peculiar...  And a real problem!  How
;	do I acknowledge it if its not a BRK when I don't know what it would be?
		LDA	#$02
		STA	$D020

		JMP	@done
		
@proc:
		ASL	$D019
		


		LDA	#<ADDR_SCREEN
		STA	numConvHeapPtr
		LDA	#>ADDR_SCREEN
		STA	numConvHeapPtr + 1

		LDA	cntMnchTick
		STA	numConvVALUE
		LDA	cntMnchTick + 1
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		CLC
		LDA	#$01
		ADC	cntMnchTick
		STA	cntMnchTick
		LDA	#$00
		ADC	cntMnchTick + 1
		STA	cntMnchTick + 1


		LDA	flgMnchPlay
		AND	#$7F
		BNE	@chkNTSC

		LDA	flgMnchPlay
		BPL	@skip0

		JSR	PEPPITO + $0C

		LDA	#$00
		STA	flgMnchPlay
		JMP	@skip0

@chkNTSC:
		ORA	#$80
		STA	flgMnchPlay

		LDA	$D06F
		AND	#$80
		BEQ	@play1

		LDA	cntMnchNTSC
		CMP	#$05
		BCC	@play0

		LDA	#$00
		STA	cntMnchNTSC
		JMP	@skip0

@play0:
		INC	cntMnchNTSC

@play1:
		JSR	PEPPITO + 3

		LDA	flgPepRsrt
		BEQ	@skip0

		LDA	#$00
		STA	flgPepRsrt

		LDA	flgMnchJukeB
		BEQ	@skip0

		LDA	#$01
		STA	flgMnchJNext

@skip0:
;		LDA	#$03
;		STA	$D020
;
;		LDA #>lstDMATest
;		STA $D701
;		LDA #<lstDMATest
;		STA $D705
;
;		LDA	#$00
;		STA	$D020


		LDA	#<ADDR_SCREEN + 8
		STA	numConvHeapPtr 
		LDA	#>ADDR_SCREEN
		STA	numConvHeapPtr + 1

		LDA	cntPepTick
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		LDA	#<ADDR_SCREEN  + 16
		STA	numConvHeapPtr
		LDA	#>ADDR_SCREEN
		STA	numConvHeapPtr + 1

		LDA	cntPepPRow
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		LDA	#<ADDR_SCREEN + 24
		STA	numConvHeapPtr 
		LDA	#>ADDR_SCREEN
		STA	numConvHeapPtr + 1

		LDA	cntPepSeqP
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		LDA	flgMnchDirty
		BEQ	@skip1

		JSR	displayMixerInfo

@skip1:
		LDA	flgMnchLoad
		BEQ	@finish

		LDA	#<ADDR_SCREEN + 32
		STA	numConvHeapPtr 
		LDA	#>ADDR_SCREEN
		STA	numConvHeapPtr + 1

		LDA	valPepSLen
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		LDA	#<ADDR_SCREEN + 40
		STA	numConvHeapPtr 
		LDA	#>ADDR_SCREEN
		STA	numConvHeapPtr + 1

		LDA	valPepMaxP
		STA	numConvVALUE
		LDA	#$00
		STA	numConvVALUE + 1
		
		JSR	numConvPRTINT

		JSR	displayCurrVol
		JSR	displayCurrRow

		LDA	flgMnchDirty
		BEQ	@finish
		
		JSR	displayInstInfo
		JSR	displaySongInfo

@finish:
		LDA	#$00
		STA	flgMnchDirty

		LDA	#$19
		STA	$D012		;Raster pos

@done:
		PLA
		TAY
		PLA
		TAX
		PLA
		PLP

		RTI


;-----------------------------------------------------------
screenASCIIToScreen:
;-----------------------------------------------------------
		CMP	#$00
		BNE	@cont0

		LDA	#$20
		RTS

@cont0:
		PHY

		STA	screenTemp0
		LDY	#$07
@loop:
		LDA	screenASCIIXLAT, Y
		CMP	screenTemp0
		BEQ	@subst
		DEY
		BPL	@loop

		LDA	screenTemp0
		
		CMP	#$20
		BCS	@regular

@irregular:
		LDA	#$66
		
		PLY
		RTS

@regular:
		CMP	#$7F
		BCS	@irregular

		CMP	#$40
		BCC	@exit
	
		CMP	#$60
		BCC	@upper
	
		SEC
		SBC	#$60
		
		PLY
		RTS

@upper:
;		SEC
;		SBC	#$40
		
@exit:
		PLY
		RTS

@subst:
		LDA	screenASCIIXLATSub, Y
		PLY
		RTS


;-----------------------------------------------------------
outputHexNybble:
;-----------------------------------------------------------
		PHX
		TAX
		LDA	valHexDigit0, X
		JSR	screenASCIIToScreen

		STA	(ptrScreen), Y
		INY

		PLX

		RTS


;-----------------------------------------------------------
outputHexByte:
;-----------------------------------------------------------
		PHA
		
		LSR
		LSR
		LSR
		LSR

		JSR	outputHexNybble

		PLA
		AND	#$0F

		JSR	outputHexNybble

		RTS


;-----------------------------------------------------------
numConvPRTINT:  
;-----------------------------------------------------------
		PHA
		PHX
		PHY
		
		LDY	#$00

                LDX 	#$4         		;OUTPUT UP TO 5 DIGITS

;de	I'm pretty sure we have a problem below and this will help fix it
;		STX 	numConvLEAD0       	;INIT LEAD0 TO NON-NEG
		LDA	#%10000000
		STA	numConvLEAD0
		
;
@PRTI1:
		LDA 	#'0'        		;INIT DIGIT COUNTER
                STA 	numConvDIGIT
;
@PRTI2:
		SEC            	 		;BEGIN SUBTRACTION PROCESS
        LDA 	numConvVALUE
        SBC 	numConvT10L, X      	;SUBTRACT LOW ORDER BYTE
        PHA             		;AND SAVE.
        LDA 	numConvVALUE + $1    	;GET H.O BYTE
        SBC 	numConvT10H, X      	;AND SUBTRACT H.O TBL OF 10
        BCC 	@PRTI3       		;IF LESS THAN, BRANCH
;
        STA 	numConvVALUE + $1    	;IF NOT LESS THAN, SAVE IN
        PLA             		;VALUE.
        STA 	numConvVALUE
        INC 	numConvDIGIT       	;INCREMENT DIGIT COUNTER
        JMP 	@PRTI2
;
;
@PRTI3:
		PLA             		;FIX THE STACK
        LDA 	numConvDIGIT       	;GET CHARACTER TO OUTPUT
                
		CPX 	#$0         		;LAST DIGIT TO OUTPUT?
        BEQ 	@PRTI5       		;IF SO, OUTPUT REGARDLESS

		CMP 	#'0'        		;A ZERO?

;de	#$31+ is not negative so this wouldn't work??
;       BEQ 	@PRTI4       		;IF SO, SEE IF A LEADING ZERO
;		STA 	numConvLEAD0       	;FORCE LEAD0 TO NEG.
;de 	We'll do this instead
		BNE	@PRTI5
@PRTI4:   	
		BIT 	numConvLEAD0       	;SEE IF ZERO VALUES OUTPUT
;de	I need to this as well
;       BPL 	@PRTI6       		;YET.
;		BPL 	@space			;de I want spaces.
		BMI	@space

@PRTI5:
;		JSR 	numConvCOUT
;de	And this too (only l6bit here)
		CLC
		ROR	numConvLEAD0

		STA	(numConvHeapPtr), Y
		INY
		
		JMP	@PRTI6			;de This messes the routine but
						;I need spaces

@space:
		LDA	#' '
		STA	(numConvHeapPtr), Y
		INY
		
@PRTI6:
		DEX             		;THROUGH YET?
		BPL 	@PRTI1


		PLY
		PLX
		PLA
		
		RTS

numConvT10L:
	.byte 		<1
	.byte 		<10
	.byte		<100
	.byte		<1000
	.byte		<10000

numConvT10H:		
	.byte		>1
	.byte		>10
	.byte		>100
	.byte		>1000
	.byte		>10000


VEC_CPU_IRQ		= $FFFE
VEC_CPU_RESET	= $FFFC
VEC_CPU_NMI 	= $FFFA


;-----------------------------------------------------------
initIRQ:
;-----------------------------------------------------------
		LDA	#<plyrIRQ		;install our handler
		STA	VEC_CPU_IRQ
		LDA	#>plyrIRQ
		STA	VEC_CPU_IRQ + 1

		LDA	#<plyrNOP		;install our handler
		STA	VEC_CPU_RESET
		LDA	#>plyrNOP
		STA	VEC_CPU_RESET + 1

		LDA	#<plyrNOP		;install our handler
		STA	VEC_CPU_NMI
		LDA	#>plyrNOP
		STA	VEC_CPU_NMI + 1

;	make sure that the IO port is set to output
		LDA	$00
		ORA	#$07
		STA	$00
		
;	Now, exclude BASIC + KERNAL from the memory map 
		LDA	#$1D
		STA	$01

		LDA	#%01111111		;We'll always want rasters
		AND	$D011		;    less than $0100
		STA	$D011
		
		LDA	#$19
		STA	$D012
		
		LDA	#$01			;Enable raster irqs
		STA	$D01A

		CLI

		RTS


;-----------------------------------------------------------
initState:
;-----------------------------------------------------------
;	Prevent VIC-II compatibility changes
		LDA	#$80
		TRB	$D05D		
		
		LDA	#$00
		STA	$D020
		STA	$D021

;	Set the location of screen RAM
		LDA	#<ADDR_SCREEN
		STA	$D060
		LDA	#>ADDR_SCREEN
		STA	$D061
		LDA	#$00
		STA	$D062
		STA	$D063

;	lower case
		LDA	#$16
		STA	$D018

;	Normal text mode
		LDA	#$00
		STA	$D054

;	H640, fast CPU, extended attributes
		LDA	#$E0
		STA	$D031

;	Adjust D016 smooth scrolling for VIC-III H640 offset
;		LDA	#$C9
;		STA	$D016
		LDA	$D016
		AND	#$F8
		ORA	#$02
		STA	$D016


;	640x200 16bits per char, 16 pixels wide per char
;	= 640/16 x 16 bits = 80 bytes per row
		LDA	#<80
		STA	$D058
		LDA	#>80
		STA	$D059
;	Draw 80 chars per row
		LDA	#80
		STA	$D05E

		RTS


;-----------------------------------------------------------
setCoefficient:
;-----------------------------------------------------------
		STX	$D6F4

		STX	$D6F4
		STX	$D6F4
		STX	$D6F4
		STX	$D6F4

		STA	$D6F5

		RTS


;-----------------------------------------------------------
setMasterVolume:
;-----------------------------------------------------------
;	Check if on Nexys - need the amplifier on (bit 0)
		LDA	$D629
		AND	#$40
		BEQ	@nonnexys

		LDA	#$01
		STA	valMnchDummy
		JMP	@cont0

@nonnexys:
		LDA	#$00
		STA	valMnchDummy
		
@cont0:
		LDX	#$1E				;Speaker Left master 
		LDA	valMnchMsVol
		ORA	valMnchDummy

		JSR	setCoefficient

		LDX	#$1F
		LDA	valMnchMsVol + 1
		JSR	setCoefficient

		LDX	#$3E				;Speaker right master
		LDA	valMnchMsVol
		ORA	valMnchDummy

		JSR	setCoefficient

		LDX	#$3F
		LDA	valMnchMsVol + 1
		JSR	setCoefficient

		LDX	#$DE				;Headphones right? master
		LDA	valMnchMsVol
		ORA	valMnchDummy

		JSR	setCoefficient

		LDX	#$DF
		LDA	valMnchMsVol + 1
		JSR	setCoefficient

		LDX	#$FE				;Headphones left? master
		LDA	valMnchMsVol
		ORA	valMnchDummy

		JSR	setCoefficient

		LDX	#$FF
		LDA	valMnchMsVol + 1
		JSR	setCoefficient

		RTS

;-----------------------------------------------------------
setLeftLVolume:
;-----------------------------------------------------------
		LDX	#$10				;Speaker left digi left
		LDA	valMnchLLVol
		JSR	setCoefficient

		LDX	#$11
		LDA	valMnchLLVol + 1
		JSR	setCoefficient

		LDX	#$F0				;Headphones left? digi left
		LDA	valMnchLLVol
		JSR	setCoefficient

		LDX	#$F1
		LDA	valMnchLLVol + 1
		JSR	setCoefficient
		
		RTS


;-----------------------------------------------------------
setLeftRVolume:
;-----------------------------------------------------------
		LDX	#$12				;Speaker left, digi right
		LDA	valMnchLRVol
		JSR	setCoefficient

		LDX	#$13
		LDA	valMnchLRVol + 1
		JSR	setCoefficient

		LDX	#$F2				;Headphone left?, digi right
		LDA	valMnchLRVol
		JSR	setCoefficient

		LDX	#$F3
		LDA	valMnchLRVol + 1
		JSR	setCoefficient
		
		RTS


;-----------------------------------------------------------
setRightRVolume:
;-----------------------------------------------------------
		LDX	#$32				;Speaker right, digi right
		LDA	valMnchRRVol
		JSR	setCoefficient

		LDX	#$33
		LDA	valMnchRRVol + 1
		JSR	setCoefficient

		LDX	#$D2				;Headphone right?, digi right
		LDA	valMnchRRVol
		JSR	setCoefficient

		LDX	#$D3
		LDA	valMnchRRVol + 1
		JSR	setCoefficient
		
		RTS

;-----------------------------------------------------------
setRightLVolume:
;-----------------------------------------------------------
		LDX	#$30				;Speaker right, digi left
		LDA	valMnchRLVol
		JSR	setCoefficient

		LDX	#$31
		LDA	valMnchRLVol + 1
		JSR	setCoefficient

		LDX	#$D0				;Headphone right?, digi left
		LDA	valMnchRLVol
		JSR	setCoefficient

		LDX	#$D1
		LDA	valMnchRLVol + 1
		JSR	setCoefficient

		RTS


;-----------------------------------------------------------
initAudio:
;-----------------------------------------------------------
		LDX	#$00
		LDA	#$00
@loop:
		JSR	setCoefficient

		INX
		BNE	@loop

;	Check if on Nexys - mono to right side although appears
;	on left.
		LDA	$D629
		AND	#$40
		BEQ	@cont0

		LDA	#$00
		STA	valMnchLLVol
		STA	valMnchLRVol
		STA	valMnchLLVol + 1
		STA	valMnchLRVol + 1

		LDA	#$EE
		STA	valMnchRLVol
		STA	valMnchRRVol
		LDA	#$7F
		STA	valMnchRLVol + 1
		STA	valMnchRRVol + 1

@cont0:
		JSR	setMasterVolume
		JSR	setLeftLVolume
		JSR	setLeftRVolume
		JSR	setRightRVolume
		JSR	setRightLVolume

		RTS


;		LDX	#$C2
;		LDA	#$15
;		JSR	setCoefficient
;
;		LDX	#$C3
;		LDA	#$80
;		JSR	setCoefficient
;
;		LDX	#$D0
;		LDA	#$16
;		JSR	setCoefficient
;
;		LDX	#$D1
;		LDA	#$40
;		JSR	setCoefficient
;
;		LDX	#$E2
;		LDA	#$16
;		JSR	setCoefficient
;
;		LDX	#$e3
;		LDA	#$40
;		JSR	setCoefficient
;
;		LDX	#$f0
;		LDA	#$57
;		JSR	setCoefficient
;
;		LDX	#$F1
;		LDA	#$A1
;		JSR	setCoefficient
;
;		LDX	#$F2
;		LDA	#$15
;		JSR	setCoefficient
;
;		LDX	#$F3
;		LDA	#$80
;		JSR	setCoefficient
;
;		RTS


;-----------------------------------------------------------
initScreen:
;-----------------------------------------------------------
		LDA	#<ADDR_SCREEN
		STA	ptrScreen
		LDA	#>ADDR_SCREEN
		STA	ptrScreen + 1
	
		LDX	#$18
@loop0:
		LDZ	#$00
		LDA	#$20
@loop1:
		STA	(ptrScreen), Z
		INZ
		
		CPZ	#$50
		BNE	@loop1
		
		CLC
		LDA	#$50
		ADC	ptrScreen
		STA	ptrScreen
		LDA	#$00
		ADC	ptrScreen + 1
		STA	ptrScreen + 1
		
		DEX
		BPL	@loop0

		LDA	#<.loword(AD32_COLOUR)
		STA	ptrScreen
		LDA	#>.loword(AD32_COLOUR)
		STA	ptrScreen + 1
		LDA	#<.hiword(AD32_COLOUR)
		STA	ptrScreen + 2
		LDA	#>.hiword(AD32_COLOUR)
		STA	ptrScreen + 3

		LDX	#$18
@loop2:
		LDZ	#$00
		LDA	#$0F
@loop3:
		NOP
		STA	(ptrScreen), Z
		INZ

		CPZ	#$50
		BNE	@loop3
		
		CLC
		LDA	#$50
		ADC	ptrScreen
		STA	ptrScreen
		LDA	#$00
		ADC	ptrScreen + 1
		STA	ptrScreen + 1
		
		DEX
		BPL	@loop2

		LDA	#<(.loword(AD32_COLOUR) + (6 * 80))
		STA	ptrScreen
		LDA	#>(.loword(AD32_COLOUR) + (6 * 80))
		STA	ptrScreen + 1
		LDA	#<.hiword(AD32_COLOUR)
		STA	ptrScreen + 2
		LDA	#>.hiword(AD32_COLOUR)
		STA	ptrScreen + 3

		LDZ	#$00
		LDY	#$00
@loop4:
		LDX	#$00
@loop5:
		LDA	valVolColrs0, X
		NOP
		STA	(ptrScreen), Z
		INZ
		INX
		CPX	#$0B
		BNE	@loop5

		INZ
		INY
		CPY	#$04
		BNE	@loop4

		LDA	#<(.loword(AD32_COLOUR) + (14 * 80))
		STA	ptrScreen
		LDA	#>(.loword(AD32_COLOUR) + (14 * 80))
		STA	ptrScreen + 1
		LDA	#<.hiword(AD32_COLOUR)
		STA	ptrScreen + 2
		LDA	#>.hiword(AD32_COLOUR)
		STA	ptrScreen + 3

		LDZ	#$00
		LDY	#$00
@loop6:
		LDX	#$00
@loop7:
		LDA	valVolColrs0, X
		NOP
		STA	(ptrScreen), Z
		INZ
		INX
		CPX	#$0B
		BNE	@loop7

		INZ
		INY
		CPY	#$03
		BNE	@loop6


		RTS

;-----------------------------------------------------------
initM65IOFast:
;-----------------------------------------------------------
;	Go fast, first attempt
		LDA	#65
		STA	$00

;	Enable M65 enhanced registers
		LDA	#$47
		STA	$D02F
		LDA	#$53
		STA	$D02F
;	Switch to fast mode, be sure
; 	1. C65 fast-mode enable
		LDA 	$D031
		ORA 	#$40
		STA 	$D031
; 	2. MEGA65 48MHz enable (requires C65 or C128 fast mode to truly enable, 
;	hence the above)
		LDA 	#$40
		TSB 	$D054
		
		RTS

	.if	DEF_MNCH_USEMINI
	.include	"minime.s"
	.else
	.include	"bigglesworth.s"
	.endif
