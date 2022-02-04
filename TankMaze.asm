;-------------------------------------------------------------------------
;---------Tank Battalion Atari 2600 port----------------------------------
;-------------------------------------------------------------------------
;	Bob Montgomery
;	(c) 2012
;
;
;	To do:
;	Rewrite tank movement routines so that tanks (a) always move (i.e. don't
;		get stuck in corners or dead-ends) and (b) don't reverse unless that
;		is the only direction to move
;			New routine:
;				first, determine directions possible to move
;				next, choose one of those and move 
;	Figure out good AI for tank movement/firing routines
;	Add explosion graphics
;	Sound FX
;	Music?
;	Title/splash screen?
;	Power-Ups - ???
;	Maze generation routine done?  Tweaking?  Rewrite?
;	Collisions!
;	Score
;	Level objectives moving through levels
;	Difficulty settings
;	Gameplay tweaking
;	Graphics/colors
;	PAL60 version
;	2-player?  (unlikely but...)
;	Other...?

;	BUG KILLING!

;	!








	processor 6502
	include vcs.h
	include macro.h

;-------------------------Constants Below---------------------------------

TANKHEIGHT	=	7
BLOCKHEIGHT = 7
MAZEROWS	=	12
TANKAREAHEIGHT	=	BLOCKHEIGHT * MAZEROWS
MAZEAREAHEIGHT 	= 	TANKAREAHEIGHT

;--TankStatus bits:
TANKRIGHT		=	J0RIGHT
TANKLEFT		=	J0LEFT
TANKDOWN		=	J0DOWN
TANKUP			=	J0UP


TANKOFFSCREEN	=	127



;--GameStatus bits

GAMEON			=	%10000000



BALLOFFSCREEN	=	200

BULLETRIGHT		=	0
BULLETLEFT		=	1
BULLETDOWN		=	2
BULLETUP		=	3
BULLETCLEAR		=	3

TRIGGERDEBOUNCEVALUE = 20

BULLETSPEEDHOR		=		2
BULLETSPEEDVER		=		2

BASECOLOR		=		GREEN

WALLCOLOR			=		RED+6


;-------------------------COLOR CONSTANTS (NTSC)--------------------------

GRAY		=	$00
GOLD		=	$10
ORANGE		=	$20
BURNTORANGE	=	$30
RED		=	$40
PURPLE		=	$50
PURPLEBLUE	=	$60
BLUE		=	$70
BLUE2		=	$80
LIGHTBLUE	=	$90
TURQUOISE	=	$A0
GREEN		=	$B0
BROWNGREEN	=	$C0
TANGREEN	=	$D0
TAN		=	$E0
BROWN		=	$F0

;--------------------------TIA CONSTANTS----------------------------------

	;--NUSIZx CONSTANTS
	;	player:
ONECOPYNORMAL		=	$00
TWOCOPIESCLOSE		=	$01
TWOCOPIESMED		=	$02
THREECOPIESCLOSE	=	$03
TWOCOPIESWIDE		=	$04
ONECOPYDOUBLE		=	$05
THREECOPIESMED		=	$06
ONECOPYQUAD		=	$07
	;	missile:
SINGLEWIDTHMISSILE	=	$00
DOUBLEWIDTHMISSILE	=	$10
QUADWIDTHMISSILE	=	$20
OCTWIDTHMISSILE		=	$30

	;---CTRLPF CONSTANTS
	;	playfield:
REFLECTEDPF		=	%00000001
SCOREPF			=	%00000010
PRIORITYPF		=	%00000100
	;	ball:
SINGLEWIDTHBALL		=	SINGLEWIDTHMISSILE
DOUBLEWIDTHBALL		=	DOUBLEWIDTHMISSILE
QUADWIDTHBALL		=	QUADWIDTHMISSILE
OCTWIDTHBALL		=	OCTWIDTHMISSILE

	;---HMxx CONSTANTS
LEFTSEVEN		=	$70
LEFTSIX			=	$60
LEFTFIVE		=	$50
LEFTFOUR		=	$40
LEFTTHREE		=	$30
LEFTTWO			=	$20
LEFTONE			=	$10
NOMOVEMENT		=	$00
RIGHTONE		=	$F0
RIGHTTWO		=	$E0
RIGHTTHREE		=	$D0
RIGHTFOUR		=	$C0
RIGHTFIVE		=	$B0
RIGHTSIX		=	$A0
RIGHTSEVEN		=	$90
RIGHTEIGHT		=	$80

	;---AUDCx CONSTANTS (P Slocum's naming convention)
SAWSOUND		=	1
ENGINESOUND		=	3
SQUARESOUND		=	4
BASSSOUND		=	6
PITFALLSOUND		=	7
NOISESOUND		=	8
LEADSOUND		=	12
BUZZSOUND		=	15

	;---SWCHA CONSTANTS (JOYSTICK)
J0RIGHT		=	%10000000
J0LEFT		=	%01000000
J0DOWN		=	%00100000
J0UP		=	%00010000
J1RIGHT		=	%00001000
J1LEFT		=	%00000100
J1DOWN		=	%00000010
J1UP		=	%00000001

	;---SWCHB CONSTANTS (CONSOLE SWITCHES)
P1DIFF		=	%10000000
P0DIFF		=	%01000000
BWCOLOR		=	%00001000
SELECT		=	%00000010
RESET		=	%00000001

;-------------------------End Constants-----------------------------------

;---Macros

	MAC FILLER
		REPEAT {1}
		.byte {2}
		REPEND
	ENDM
	


;--------Variables To Follow-------------

	SEG.U Variables
   	org $80

FrameCounter ds 1

RandomNumber ds 1

MazeNumber ds 1

GameStatus ds 1




TriggerDebounce ds 1
ConsoleDebounce ds 1

BaseColor ds 1

TankX ds 4
TankY ds 4
TankStatus ds 4

BulletX ds 4
BulletY ds 4
BulletDirection ds 1


PlayerX ds 2
MissileX ds 2
BallX ds 1

PlayerY ds 2
MissileY ds 2
BallY ds 1   	

MissileHeight ds 2
BallHeight ds 1
MissileHeightAdjuster ds 2
BallHeightAdjuster ds 1
Player0Bottom ds 1
PlayerYTemp ds 1
MissileYTemp ds 2
BallYTemp ds 1
Player0Top ds 1


Player0Ptr ds 4
Player1Ptr ds 4


PF1Left ds MAZEROWS-1
PF2Left ds MAZEROWS-1
PF2Right ds MAZEROWS-1
PF1Right ds MAZEROWS-1


MiscPtr ds 2
Temp ds 1

   ; Display Remaining RAM
   echo "----",($100 - *) , "bytes left (ZP RAM)"
;---End Variables

	seg Bank0

	org $F000

Start
	CLEAN_START

;--Some Initial Setup


; 	jsr GenerateMazeSubroutine	
	
	lda #BALLOFFSCREEN
	sta BulletX
	sta BulletX+1
	sta BulletX+2
	sta BulletX+3
	sta BulletY
	sta BulletY+1
	sta BulletY+2
	sta BulletY+3
	
	lda #TANKOFFSCREEN
; 	lda #60
	sta TankX
	sta TankX+1
	sta TankX+2
	sta TankX+3
	sta TankY
	sta TankY+1
	sta TankY+2
	sta TankY+3
	
	
	lda #TANKUP
	sta TankStatus
	sta TankStatus+1
	sta TankStatus+2
	sta TankStatus+3

	
	lda #$FF
	sta VDELP0
	sta VDELBL

	sta RandomNumber
	
	lda #GOLD+10
	sta COLUP0
	lda #BLUE2+12
	sta COLUP1
	
	lda #WALLCOLOR
	sta COLUPF
	
	lda #QUADWIDTHBALL
	sta NUSIZ0
	sta NUSIZ1
	
	lda #REFLECTEDPF|DOUBLEWIDTHBALL
	sta CTRLPF
	
	lda #BASECOLOR
	sta BaseColor

;----------------------------------------------------------------------------
;--------------------GAME MAIN LOOP------------------------------------------
;----------------------------------------------------------------------------
MainGameLoop

	jsr VBLANKRoutine
	jsr KernelRoutineGame
	jsr OverscanRoutine
	jmp MainGameLoop

;----------------------------------------------------------------------------
;-------------------VBLANK Routine-------------------------------------------
;----------------------------------------------------------------------------
VBLANKRoutine
	lda #%00000111
VSYNCWaitLoop
	sta WSYNC
	sta VSYNC
	lsr
	bcs VSYNCWaitLoop

	lda #43
	sta TIM64T
	
	
	dec FrameCounter
	
	lda FrameCounter
	and #$0F
	sta Temp
	lda BaseColor
	and #$F0
	ora Temp
	sta BaseColor
	
	lda TriggerDebounce
	beq TriggerDebounceZero
	dec TriggerDebounce
TriggerDebounceZero	



	jsr KernelSetupSubroutine

	
	

WaitForVblankEnd
	lda INTIM
	bne WaitForVblankEnd

	sta WSYNC

	sta VBLANK


	rts

;----------------------------------------------------------------------------
;----------------------Kernel Routine----------------------------------------
;----------------------------------------------------------------------------
	align 256

Switch0
	lda Player0Bottom
	sta Player0Top
	bne BackFromSwitch0

Wait0
	nop
	nop
	bpl BackFromSwitch0
	
Switch1
	lda Player0Bottom
	sta Player0Top
	bne BackFromSwitch1

Wait1
	nop
	nop
	bpl BackFromSwitch1
	
KernelRoutineGame


	;--use stack pointer to hit ENABL
	tsx
	stx Temp
	ldx #<ENABL
	txs
	
	
	ldy #BLOCKHEIGHT
	lda #$FF
	ldx #$80
TopWallLoop
	sta WSYNC
	stx PF0
	sta PF1
	sta PF2
	dey
	bne TopWallLoop			;+13	13
	
	
	
	sta WSYNC
	lda #0
	sta PF1
	sta PF2					;+8		 8
	SLEEP 25
	ldy #TANKAREAHEIGHT	;+2		35
KernelLoop
	;draw player 0
	cpy Player0Top				
	beq Switch0
	bpl Wait0
	lda (Player0Ptr),Y
	sta GRP0				;+15	50
BackFromSwitch0	
	

	ldx BlockRowTable-BLOCKHEIGHT-1,Y	;+4		54

	;draw player 1
	lda #TANKHEIGHT*2-1
	dcp PlayerYTemp
	bcs DoDraw10
	lda #0
	.byte $2C
DoDraw10
	lda (Player1Ptr),Y
	sta GRP1				;+18	72



	;draw missile 0
	lda #TANKHEIGHT-2
	dcp MissileYTemp
	sbc #TANKHEIGHT-4
	sta ENAM0				;+12	 8
	
	
	lda #TANKHEIGHT-2
	dcp MissileYTemp+1
	sbc #TANKHEIGHT-4
	sta ENAM1				;+12	20
	

	
	
	lda PF1Left,X
	sta PF1				;+7		27
	lda PF2Left,X
	sta PF2				;+7		34
	
	lda PF1Right,X
	sta PF1				;+7		41
	lda PF2Right,X
	sta PF2				;+7		48

	tsx					;+2		50
	cpy BallY
	php
	txs					;+8		58
	
	
	cpy Player0Top				
	beq Switch1
	bpl Wait1
	lda (Player0Ptr+2),Y		
	sta GRP0			;+15	73
BackFromSwitch1
	
	
	;draw player 1
	lda #TANKHEIGHT*2-1
	dcp PlayerYTemp
	bcs DoDraw11
	lda #0
	.byte $2C
DoDraw11
	lda (Player1Ptr+2),Y
	sta GRP1			;+18	15
	
	lda #0
	sta PF1
	sta PF2				;+8		23
	
	
	dey
	cpy #BLOCKHEIGHT
	beq KernelLastRow	;+6		29
	SLEEP 3
	bne KernelLoop		;+6		35

	align 256
	
Switch0b
	lda Player0Bottom
	sta Player0Top
	bne BackFromSwitch0b

Wait0b
	nop
	nop
	bpl BackFromSwitch0b
	
Switch1b
	lda Player0Bottom
	sta Player0Top
	bne BackFromSwitch1b

Wait1b
	nop
	nop
	bpl BackFromSwitch1b
	
KernelLastRow				;		31		branch here crosses page boundary
	SLEEP 4

KernelLastRowLoop			;		35
	;draw player 0
	cpy Player0Top				
	beq Switch0b
	bpl Wait0b
	lda (Player0Ptr),Y
	sta GRP0				;+15	50
BackFromSwitch0b
	

	SLEEP 4

	;draw player 1
	lda #TANKHEIGHT*2-1
	dcp PlayerYTemp
	bcs DoDraw10b
	lda #0
	.byte $2C
DoDraw10b
	lda (Player1Ptr),Y
	sta GRP1				;+18	72



	;draw missile 0
	lda #TANKHEIGHT-2
	dcp MissileYTemp
	sbc #TANKHEIGHT-4
	sta ENAM0				;+12	 8
	
	
	lda #TANKHEIGHT-2
	dcp MissileYTemp+1
	sbc #TANKHEIGHT-4
	sta ENAM1				;+12	20
	

	
	
	lda #$80
	sta PF2				;+5		25
	SLEEP 10			;+10	35
	lda BaseColor
	sta COLUPF			;+6		41
	
	tsx
	cpy BallY
	php
	txs					;+10	51
	
	
	
	lda #WALLCOLOR		
	sta COLUPF			;+5		56
	nop
	
		
	cpy Player0Top				
	beq Switch1b
	bpl Wait1b
	lda (Player0Ptr+2),Y		
	sta GRP0			;+15	73
BackFromSwitch1b
	
	
	;draw player 1
	lda #TANKHEIGHT*2-1
	dcp PlayerYTemp
	bcs DoDraw11b
	lda #0
	.byte $2C
DoDraw11b
	lda (Player1Ptr+2),Y
	sta GRP1			;+18	15
	
	lda #0
	sta PF1
	sta PF2				;+8		23
	
	SLEEP 7
	
	dey
	bne KernelLastRowLoop		;+5		29	
	
	
	
	
	ldy #BLOCKHEIGHT
BottomWallLoop
	lda #$80
	ldx #0
	stx ENABL
	stx GRP0
	sta WSYNC
	stx GRP1
	stx ENAM0
	stx ENAM1
	sta PF0
	lda #$FF
	sta PF1
	sta PF2
	dey
	bne BottomWallLoop
	
	sta WSYNC
	sty PF0
	sty PF1
	sty PF2
	;--restore stack pointer
	ldx Temp
	txs

	
	lda #72
	ldx #0
	stx REFP0
	stx REFP1
	jsr PositionASpriteSubroutine
	lda #80
	ldx #1
	jsr PositionASpriteSubroutine
	lda #>DigitData
	sta Player0Ptr+1
	sta Player0Ptr+3
	lda MazeNumber
	and #$0F
	tax
	lda DigitDataLo,X
	sta Player0Ptr+2
	lda MazeNumber
	lsr
	lsr
	lsr
	lsr
	tax
	lda DigitDataLo,X
	sta Player0Ptr
	
	ldy #6
BottomKernelLoop
	sta WSYNC
	lda (Player0Ptr),Y
	sta GRP0
	lda (Player0Ptr+2),Y
	sta GRP1
	dey
	bpl BottomKernelLoop
	
	sta WSYNC
	iny
	sty GRP0
	sty GRP1
	
	rts

;----------------------------------------------------------------------------
;------------------------Overscan Routine------------------------------------
;----------------------------------------------------------------------------
OverscanRoutine




	ldy #2
	sty WSYNC
	sty VBLANK
	lda  #42
	sta  TIM64T
	
	
	lda GameStatus
	and #GAMEON
	beq GameNotOn	
	jsr CollisionsSubroutine
	jsr ReadControllersSubroutine
	jsr MoveBulletSubroutine
	jsr MoveEnemyTanksSubroutine
GameNotOn
	jsr ReadConsoleSwitchesSubroutine



WaitForOverscanEnd
	lda INTIM
	bne WaitForOverscanEnd
	sta WSYNC		;last line...I think?

	rts

;----------------------------------------------------------------------------
;----------------------------End Main Routines-------------------------------
;----------------------------------------------------------------------------

;****************************************************************************

;----------------------------------------------------------------------------
;----------------------Begin Functions---------------------------------------
;----------------------------------------------------------------------------

;****************************************************************************

DivideByTwoSubroutine		;come in with number in accumlator
	and #$FF		;set flags
	clc
	bpl DivideByTwoPositive
	;--else negative
	sec
DivideByTwoPositive
	ror
	rts

;****************************************************************************

;--X is index into which tank
;--X, Y position the tank is going to move towards is pushed on stack 
ChooseTankDirectionSubroutine
	lda #0
	pha						;we will save new direction on stack
	txa
	tay						;get tank index into Y so we can use X for stack ptr
	tsx
	lda $05,X				;X position we are aiming for
	cmp TankX,Y
	beq MoveTankOnYAxis		;if equal don't move tank horizontally
	lda #0
	rol						;get carry into A (carry set=tank is to the left of target, carry clear=tank to the right)
	tax
	lda TankDirection,X
	tsx
	sta $01,X				;save new horizontal direction
MoveTankOnYAxis
	lda $04,X				;Y position we are aiming for
	cmp TankY,Y
	beq NoVerticalTankMovement
	lda #0
	rol						;similar to horizontal movement, use carry as index into offset table
	tax
	lda TankDirection+2,X
	tsx
	ora $01,X				;ORA vertical direction into direction variable on stack
	sta $01,X
NoVerticalTankMovement
	tya
	tax						;get tank index back into X
	pla						;pull new tank direction off stack into A
	rts

TankDirection
	.byte TANKLEFT, TANKRIGHT, TANKDOWN, TANKUP

;****************************************************************************

MoveEnemyTanksSubroutine

; 	ldx #3
; MoveEnemiesLoop

	;--hack:
	;	can't move all 3 enemies every frame, takes too long.
	;	So instead move one per frame (moving none every 4th frame)
	;	Works well enough for now, work on optimizing this routine later

	lda FrameCounter
	and #3
	beq ReturnFromTankMovementSubroutine
	tax

	;--plan for now:  every time enemy tank hits intersection, change direction
	;push X, Y target for tank onto stack and then call ChooseTankDirectionSubroutine
	
	lda TankX,X
	and #$07
	bne NotAtIntersection
	lda TankY,X
	sec
	sbc #1
FindYIntersectionLoop
	sbc #BLOCKHEIGHT
	bcs FindYIntersectionLoop
	adc #BLOCKHEIGHT
	bne NotAtIntersection
	
	cpx #2
	bne NotTankTwo
	;--tank #2 moves towards player:
	lda TankX
	pha
	lda TankY
	pha
	jmp ChooseTankDirection
	
NotTankTwo
	cpx #1
	bne NotTankOne
	;--move tank 1 towards midpoint between player and base

	lda TankX
	sec
	sbc #80
	jsr DivideByTwoSubroutine
	clc
	adc #80
	pha
	lda TankY
	lsr
	pha
	jmp ChooseTankDirection
	
	;other tank moves somewhat randomly
NotTankOne	
	jsr UpdateRandomNumber
	pha
	jsr UpdateRandomNumber
	pha

	
ChooseTankDirection
	jsr ChooseTankDirectionSubroutine
	sta TankStatus,X					;returns with new direction in accumulator
	pla
	pla									;pull target for tank off stack and discard
	
NotAtIntersection


	;push return address onto stack
	lda #>(ReturnFromTankMovementSubroutine-1)
	pha
	lda #<(ReturnFromTankMovementSubroutine-1)
	pha
	;push movementflag (zero) onto stack
	lda #0
	pha
	lda TankStatus,X			;this holds new, desired direction for tank.
	eor #$FF					;flip all bits so that is interchangeable with reading from joystick
								;and can use same routine for both.
	jmp TankMovementSubroutine
ReturnFromTankMovementSubroutine
; 	pla
; 	tax
; 	dex
; 	bne MoveEnemiesLoop
	
	rts

;****************************************************************************

CollisionsSubroutine

	;--need to use software collision detection, at least for ball
	ldx #3
CheckBallCollisionsLoop
	;--check for collision with wall
	lda BulletX,X
	cmp #BALLOFFSCREEN
	beq BallHasNotHitBlock	;short circuit if the ball is offscreen
	sta Temp
	lda BulletY,X
	sec
	sbc #2
	sta Temp+1
	jsr IsBlockAtPosition
	;--result is in Temp
	lda Temp
	beq BallHasNotHitBlock
	;--remove block from screen!
	;--get row of block into Y and column into Temp+1

	lda BulletY,X
	ldy #0
	sec
	sbc #2
GetRowBulletIsInLoop
	sbc #BLOCKHEIGHT
	bcc FoundWhichRowBulletIsIn
	iny
	jmp GetRowBulletIsInLoop
FoundWhichRowBulletIsIn
	lda BulletX,X
	sec
	sbc #16
	lsr
	lsr
	lsr
	sta Temp+1
	txa
	pha
		
	lda #<PF1Left
	sta MiscPtr
	lda #>PF1Left
	sta MiscPtr+1
	ldx Temp+1
	lda PFMaskLookup,X
	eor #$FF
	pha
	lda PFRegisterLookup,X
	clc
	adc MiscPtr
	sta MiscPtr
	pla
	dey
	and (MiscPtr),Y
	sta (MiscPtr),Y	
	pla
	tax
	;--bullet has hit block, remove bullet from screen:
	lda #BALLOFFSCREEN
	sta BulletX,X
	sta BulletY,X
BallHasNotHitBlock
	dex
	bpl CheckBallCollisionsLoop


	rts

;****************************************************************************

ReadConsoleSwitchesSubroutine


	


	lda SWCHB
	lsr				;get RESET into carry
	bcs NoRESET
	lda ConsoleDebounce
	bne RESETNotReleased
	lda #1
	sta ConsoleDebounce
	;--start game
	lda GameStatus
	ora #GAMEON
	sta GameStatus
	
	lda #16
	sta TankX
	lda #16
	sta TankX+1
	lda #72
	sta TankX+2
	lda #136
	sta TankX+3
	
	lda #TANKHEIGHT+1
	sta TankY
	lda #MAZEAREAHEIGHT+1
	sta TankY+1
	sta TankY+2
	sta TankY+3
	
	lda #$FF
	sta TankStatus+1
	sta TankStatus+2
	sta TankStatus+3
	
	;--finally, cycle the random number
	jsr UpdateRandomNumber
	
	jsr GenerateMazeSubroutine
	
	jmp DoneWithConsoleSwitches
NoRESET
	lda #0
	sta ConsoleDebounce
RESETNotReleased
DoneWithConsoleSwitches
	rts

;****************************************************************************
	
GenerateMazeSubroutine

	;--update maze number
UpdateMazeNumber
	dec MazeNumber
	beq UpdateMazeNumber
	
	;--use MazeNumber as seed of random number generator
	;--save current random number so we can restore it when we are done.
	lda RandomNumber
	pha
	lda MazeNumber
	sta RandomNumber

	
	;--first, fill all with walls
	ldx #MAZEROWS-1
	lda #$FF
FillMazeLoop
	sta PF1Left,X
	sta PF2Left,X
	sta PF2Right,X
	sta PF1Right,X
	dex
	bpl FillMazeLoop
	
	
	lda #>PF1Left
	sta MiscPtr+1

	
	ldy #MAZEROWS-2
MakeMazeLoopRow
	ldx #15
	
MakeMazeLoopOuter	
	lda #<PF1Left
	sta MiscPtr

	lda PFMaskLookup,X
	eor #$FF
	pha
	lda PFRegisterLookup,X
	clc
	adc MiscPtr
	sta MiscPtr
	pla
	and (MiscPtr),Y
	sta (MiscPtr),Y
	stx Temp
	stx Temp+1
MakeMazeLoopInner
	lda #<PF1Left
	sta MiscPtr
	jsr UpdateRandomNumber
	lda RandomNumber
	cmp #100
	bcs NotEndOfRun
EndOfRun
	;--don't carve passage downward if we're on the bottom row
	cpy #0
	beq EndRunBeginNextRun
	;--otherwise, find a random passage to carve
	lda Temp
	sec
	sbc Temp+1
	sta Temp+1
 	beq OnlyOnePlaceToCarve
	jsr UpdateRandomNumber
	lda RandomNumber
	sec
FindRandomPassageToCarve
	sbc Temp+1
	bcs FindRandomPassageToCarve
	adc Temp+1
	sta Temp+1
OnlyOnePlaceToCarve
	lda Temp
	sec
	sbc Temp+1
	sta Temp+1
	txa
	pha
	ldx Temp+1
	lda PFMaskLookup,X
	eor #$FF
	pha
	lda PFRegisterLookup,X
	clc
	adc MiscPtr
	sta MiscPtr
	pla
	pha
	dey
	and (MiscPtr),Y
	sta (MiscPtr),Y
	pla
	dey
	bmi AtBottomRow
	and (MiscPtr),Y
	sta (MiscPtr),Y
AtBottomRow
	iny	
	iny
	pla
	tax
EndRunBeginNextRun
	stx Temp
	stx Temp+1
	dec Temp
	dec Temp+1
	lda #<PF1Left
	sta MiscPtr
	dex
; 	bpl MakeMazeLoopOuter
	bmi DoneWithRow
NotEndOfRun	
	lda PFMaskLookup,X
	eor #$FF
	pha
	lda PFRegisterLookup,X
	clc
	adc MiscPtr
	sta MiscPtr
	pla
	and (MiscPtr),Y
	sta (MiscPtr),Y
	dec Temp+1
	dex
	bpl MakeMazeLoopInner
	;--need to carve a passage downward if we reach this spot:
	bmi EndOfRun
	
	
	
DoneWithRow
	dey
	dey
	bmi DoneMakingMaze
	jmp MakeMazeLoopRow
DoneMakingMaze

	;--final touchup:
	;	open up area directly above base:
	lda PF2Left
	and #$0F
	sta PF2Left
	lda PF2Right
	and #$0F
	sta PF2Right
	;	leave room for enemy tanks to enter at upper right corners:
	lda PF1Left+MAZEROWS-2
	and #$03
	sta PF1Left+MAZEROWS-2
	lda PF1Right+MAZEROWS-2
	and #$03
	sta PF1Right+MAZEROWS-2
	lda PF2Left+MAZEROWS-2
	and #$0F
	sta PF2Left+MAZEROWS-2
	lda PF2Right+MAZEROWS-2
	and #$0F
	sta PF2Right+MAZEROWS-2
	
	
	;--restore original random number
	pla
	sta RandomNumber
	
	rts
	
PFRegisterLookup
	.byte 0, 0, 0, 0
	.byte MAZEROWS-1, MAZEROWS-1, MAZEROWS-1, MAZEROWS-1
	.byte (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2
	.byte (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3

PFMaskLookup
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0
	
;****************************************************************************
BulletDirectionMask
	.byte %11, %11<<2, %11<<4, %11<<6


	
MoveBulletSubroutine

	ldx #3
MoveBulletsLoop
	lda BulletX,X
	cmp #BALLOFFSCREEN
	beq NoBulletMovement
	lda BulletDirection
	and BulletDirectionMask,X
	tay							;save value
	cmp BulletUp,X
	bne BulletNotUp
	lda BulletY,X
	clc
	adc #BULLETSPEEDVER
	sta BulletY,X
BulletNotUp
	tya
	cmp BulletDown,X
	bne BulletNotDown
	lda BulletY,X
	sec
	sbc #BULLETSPEEDVER
	sta BulletY,X
BulletNotDown
	tya	
	cmp BulletRight,X
	bne BulletNotRight
	lda BulletX,X
	clc
	adc #BULLETSPEEDHOR
	sta BulletX,X
BulletNotRight	
	tya
	cmp BulletLeft,X
	bne NoBulletMovement
	lda BulletX,X
	sec
	sbc #BULLETSPEEDHOR
	sta BulletX,X
NoBulletMovement

	;--check for off screen:
	lda BulletX,X
	cmp #12
	bcc BulletOffscreen
	cmp #148
	bcs BulletOffscreen
	lda BulletY,X
	cmp #(MAZEAREAHEIGHT)+4
	bcc BulletOnScreen	
BulletOffscreen
	lda #BALLOFFSCREEN
	sta BulletX,X
	sta BulletY,X	
BulletOnScreen	
	dex
	bpl MoveBulletsLoop


	rts


;****************************************************************************
EliminateDiagonalSubroutine
	;--trashes Y.
	;--X is index into which tank
	;--A holds direction of joystick
	;--now get directions down to a single direction (i.e., no diagonals)
	;--first, determine if trying to move diagonally:
	;	need to count cleared bits in the top nibble
	ldy #0					;will count bits set with Y
	pha						;save allowed directions on stack
	txa
	pha						;save X index on stack
	tsx
	inx
	inx
	lda $00,X				;get allowed directions off of stack
	and #$F0				;clear lower nibble
	eor #$F0				;flip upper nibble
	beq DoneCountingBits	;zero bits set
CountSetBitsLoop
	iny
	pha
	sec
	sbc #1
	tsx
	inx
	and $00,X
	sta $00,X
	pla
	bne CountSetBitsLoop
DoneCountingBits	
	pla
	tax						;restore X
	pla						;restore directions
	
	cpy #2
	bcc DirectionsFine
	;--too many directions - must narrow down to 1
	pha						;save directions
	ora TankStatus,X		;first, eliminate previous direction
	and #$F0
	cmp #$F0				
	beq DirectionsNotFine		;if eliminated all movement, 
	tay
	pla
	tya
	jmp DirectionsFine
DirectionsNotFine
	;--otherwise, back to the drawing board:
	pla						;restore directions 
	ora #J0LEFT|J0RIGHT		;eliminate left & right
	cmp #$F0
	bne EliminateDown
	and #~J0RIGHT
	bne DirectionsFine		;branch always
EliminateDown
	ora #J0DOWN
	cmp #$F0
	bne DirectionsFine
	eor #J0DOWN
DirectionsFine

	rts

;****************************************************************************

ReadControllersSubroutine

	lda #0
	pha			;movement flag

	lda SWCHA	;A holds joystick
	ldx #0		;index into which tank
	
TankMovementSubroutine				;jump here when moving enemy tanks
	pha			;--save desired direction of tank
	jsr CheckForWallSubroutine		;--returns with allowed directions in A
	
	;--if no directions are allowed, but tank is trying to move, turn tank but do not move.
	and #$F0	;clear bottom nibble
	cmp #$F0	;no movement = $F0
	bne MovementAllowed
	;--if movement is not allowed, flag that and then restore directions:
	txa
	pha		;save index into which tank
	tsx
	lda #$FF
	sta $03,X	;movementflag
	pla
	tax			;restore tank index
	jmp ProcessDirections
MovementAllowed
	pha		;save allowed directions
	txa
	pha		;save index into which tank
	
	tsx
	lda $02,X	;load allowed directions (see above)
	sta $03,X	;overwrite desired directions (either joystick (if player) or calc direction if enemy)
	pla
	tax			;restore tank index
	pla			;pull allowed directions off
ProcessDirections
	pla			;pulls 
	jsr EliminateDiagonalSubroutine		;tanks cannot move diagonally

	tay	;--save direction in Y
	
	txa
	pha	;save tank index
	
	tsx
	asl $02,X		;get movement flag into carry flag

	pla
	tax	;restore tank index
	
	tya	;get saved directions back in A	
	and #J0UP
	bne NoUp
	bcs TurnUpward			;carry holds movement flag (carry clear=no movement)
	inc TankY,X
TurnUpward
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKUP
	sta TankStatus,X
NoUp
	tya
	and #J0DOWN
	bne NoDown
	bcs TurnDownward		;carry holds movement flag (carry clear=no movement)
	dec TankY,X
TurnDownward
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKDOWN
	sta TankStatus,X
NoDown
	tya
	and #J0RIGHT
	bne NoRight
	bcs TurnRightward		;carry holds movement flag (carry clear=no movement)
	inc TankX,X
TurnRightward	
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKRIGHT
	sta TankStatus,X
NoRight
	tya
	and #J0LEFT
	bne NoLeft
	bcs TurnLeftward		;carry holds movement flag (carry clear=no movement)
	dec TankX,X
TurnLeftward
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKLEFT
	sta TankStatus,X
NoLeft

	pla		;pop movement flag off of stack and discard

	;are any balls available for firing
	ldx #1
FindAvailableBallLoop
	lda BulletX,X
	cmp #BALLOFFSCREEN
	beq FoundAvailableBall
	dex
	bpl FindAvailableBallLoop
	bmi NoAvailableBalls
FoundAvailableBall	
	;--now read joystick trigger if debounce is zero:
	lda TriggerDebounce
	bne TriggerNotDebounced
	lda INPT4
	bmi NoTrigger
	;--bullet is fired!
	;set trigger debounce
	lda #TRIGGERDEBOUNCEVALUE
	sta TriggerDebounce
	;--set position:
	lda TankX
	clc
	adc #4
	sta BulletX,X
	lda TankY
	sec
	sbc #3
	sta BulletY,X
	
	
	;--clear old direction
	lda BulletDirectionClear,X
	eor #$FF
	and BulletDirection
	sta BulletDirection
	
	
	;--then set new direction:
	lda TankStatus
	tay	
	and #TANKUP
	beq NotFiringUp
	lda BulletDirection
	ora BulletUp,X
	sta BulletDirection
	jmp DoneFiring
NotFiringUp
	tya
	and #TANKDOWN
	beq NotFiringDown
	lda BulletDirection
	ora BulletDown,X
	sta BulletDirection
	jmp DoneFiring
NotFiringDown	
	tya
	and #TANKLEFT
	beq NotFiringLeft
	lda BulletDirection
	ora BulletLeft,X
	sta BulletDirection
	jmp DoneFiring
NotFiringLeft
	tya
	and #TANKRIGHT
	beq NotFiring
	lda BulletDirection
	ora BulletRight,X
	sta BulletDirection
NotFiring
DoneFiring
NoAvailableBalls	
NoTrigger
TriggerNotDebounced

	
	rts
	
BulletDirectionClear
BulletUp
	.byte	BULLETUP, BULLETUP<<2, BULLETUP<<4, BULLETUP<<6
BulletDown
	.byte	BULLETDOWN, BULLETDOWN<<2, BULLETDOWN<<4, BULLETDOWN<<6
BulletLeft
	.byte	BULLETLEFT, BULLETLEFT<<2, BULLETLEFT<<4, BULLETLEFT<<6
BulletRight
	.byte	BULLETRIGHT, BULLETRIGHT<<2, BULLETRIGHT<<4, BULLETRIGHT<<6

	
;****************************************************************************

	align 256

PositionASpriteSubroutine
	sec
	sta HMCLR
	sta WSYNC
DivideLoop			;				this loop can't cross a page boundary!!!
	sbc #15
	bcs DivideLoop	;+4		 4
	eor #7
	asl
	asl
	asl
	asl				;+10	14
	sta.wx HMP0,X	;+5		19
	sta RESP0,X		;+4		23
	sta WSYNC
	sta HMOVE
Return					;label for cycle-burning code
	rts




;****************************************************************************

RotationOdd
	.byte 0, 2, 1, 3
RotationEven
	.byte 2, 1, 3, 0

RotationTables
	.word RotationEven, RotationOdd	

KernelSetupSubroutine


	;rotate Tanks through Players and Missiles
	
	lda FrameCounter
	and #1
	asl
	tax
	lda RotationTables,X
	sta MiscPtr
	lda RotationTables+1,X
	sta MiscPtr+1
	ldy #3
RotationLoop
	lax (MiscPtr),Y
	lda TankX,Y
	sta PlayerX,X
	lda TankY,Y
	sta PlayerY,X
	txa
	lsr		;--only setup gfx pointer when the tank is displayed with the player
	beq PlayerNotMissile 
	jmp MissileNotPlayer
PlayerNotMissile
	;--get correct tank gfx ptr set up
	txa
	asl
	asl
	tax		;we need X * 4
	lda TankStatus,Y
	and #TANKUP
	beq TankNotFacingUp
	
	stx Temp		;save X index
	lda FrameCounter
	lsr
	and #%00000110
	tax
	lda TankUpFrame,X
	pha
	lda TankUpFrame+1,X
	ldx Temp		;restore X
	jmp SetTankGfxPtr
TankNotFacingUp
	lda TankStatus,Y
	and #TANKDOWN
	beq TankNotFacingDown
	stx Temp		;save X index
	lda FrameCounter
	lsr
	and #%00000110
	tax
	lda TankDownFrame,X
	pha
	lda TankDownFrame+1,X
	ldx Temp		;restore X
	jmp SetTankGfxPtr
TankNotFacingDown
	lda TankStatus,Y
	and #TANKRIGHT
	beq TankNotFacingRight
	stx Temp		;save X index
	lda FrameCounter
	lsr
	and #%00000110
	tax
	lda TankRightFrame,X
	pha
	lda TankRightFrame+1,X
	ldx Temp		;restore X
	jmp SetTankGfxPtr
TankNotFacingRight
	stx Temp		;save X index
	lda FrameCounter
	lsr
	and #%00000110
	tax
	lda TankRightFrame,X
	pha
	lda TankRightFrame+1,X
	ldx Temp		;restore X
SetTankGfxPtr
	sta Player0Ptr+1,X
	sta Player0Ptr+3,X
	pla
	clc
	adc #TANKHEIGHT
	sec
	sbc TankY,Y
	sta Player0Ptr,X
	clc
	adc #TANKHEIGHT
	sta Player0Ptr+2,X
	;--now set REFPx to correct value, if necessary
	lda TankStatus,Y
	and #TANKLEFT|TANKRIGHT
	beq EndRotationLoop
	lax (MiscPtr),Y		;get original X index back
	lda TankStatus,Y
	and #TANKLEFT
	bne TankFacingRight
	lda #$00
	.byte $2C
TankFacingRight
	lda #$FF
	sta REFP0,X	
	jmp EndRotationLoop
MissileNotPlayer	
	;--adjust missile width and position
MissileNotPlayer	
	;--adjust missile width and position
	txa
	and #1
	tax		;get correct index
	lda MissileX,X
	sec
	sbc #2
	sta MissileX,X
	lda #OCTWIDTHMISSILE
	sta NUSIZ0,X
	lda #TANKHEIGHT-2
	sta MissileHeight,X
EndRotationLoop
	dey
	bmi DoneWithRotationLoop
	jmp RotationLoop
DoneWithRotationLoop
	
	
	lda #TANKAREAHEIGHT
	sec
	sbc PlayerY+1
	adc #TANKHEIGHT
	asl
	sta PlayerYTemp

	lda PlayerY
	sta Player0Top
	sec
	sbc #TANKHEIGHT	
	ora #$80
	sta Player0Bottom
	lda PlayerY
	cmp #TANKAREAHEIGHT
	bcc NoSpecialAdjustmentToPlayer0Top
	lda Player0Bottom
	sta Player0Top	

NoSpecialAdjustmentToPlayer0Top



	ldx #2
PreKernelSetupLoop2
	lda #TANKAREAHEIGHT+1
	sec
	sbc MissileY,X
	adc MissileHeight,X
	sta MissileYTemp,X
	lda MissileHeight,X
	sec
	sbc #2
	sta MissileHeightAdjuster,X
	
	lda MissileX,X
	clc
	adc #3
	sta MissileX,X
	dex
	bpl PreKernelSetupLoop2
	

	;bullet flicker rotation:
	lda FrameCounter
	and #3
	tax
	lda BulletX,X
	sta BallX
	lda BulletY,X
	sta BallY
	
		
	ldx #4
PositioningLoop
	lda PlayerX,X
	jsr PositionASpriteSubroutine
	dex
	bpl PositioningLoop
	
	
	;--clear collision registers
	sta CXCLR

	rts
	
;****************************************************************************


IsBlockAtPosition		;position in Temp (x), Temp+1 (y)
	;--returns result in Temp
	;--special case for first row which is always open (except for the base, but we'll deal with that later)
	;;temp:
; 	pha
; 	lda #0
; 	sta Temp
; 	pla
; 	rts


	

	
	pha			;save A on stack
	txa
	pha			;save X on stack
	lda Temp
	sec
	sbc #16
	lsr
	lsr
	lsr
	tax
	lda PFRegisterLookup,X
	pha
	lda PFMaskLookup,X
	pha
	lda Temp+1
	;--special case: if Temp+1 < BLOCKHEIGHT then no block because bottom row is always open
	sec
	sbc #BLOCKHEIGHT
	bcs NotOnBottomRow
	lda #0
	beq FoundWhetherBlockExists
NotOnBottomRow	
	;--divide by blockheight
	ldx #0
	sec
DivideLoop2
	sbc #BLOCKHEIGHT
	bcc DoneDividing
	inx
	jmp DivideLoop2
DoneDividing
	;--now add X to 2nd element on stack
	txa
	tsx
	clc
	adc $00+2,X
	tax
	
	lda PF1Left,X
	tsx
	and $00+1,X
FoundWhetherBlockExists
	;--result is now in A --nonzero means a block is there, zero means no
	sta Temp		;result goes into Temp
	pla
	pla		;get intermediate results off of stack
	pla
	tax		;restore X from stack
	pla		;restore Accumulator from stack
	rts

	
;****************************************************************************
	
CheckForWallSubroutine
				;--X holds index into which tank, A holds direction (mask against joystick constants)
				;return A holding allowed directions
	tay			;save direction
	and #J0LEFT
	bne NotMovingLeft
	;--moving left.
	;--first, if at left edge, don't allow left movement
	lda TankX,X
	cmp #16
	beq CannotMoveLeft
	;--check for wall immediately to the left of upper left corner
	lda TankX,X
	sec
	sbc #1
	sta Temp
	lda TankY,X
	sec
	sbc #2
	sta Temp+1
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	bne CannotMoveLeft
	lda TankX,X
	sec
	sbc #1
	sta Temp
	lda TankY,X
	sec
	sbc #8
	sta Temp+1
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	beq NoWallL
CannotMoveLeft
	tya
	ora #J0LEFT
	tay
	jmp CheckVerticalMovement
NoWallL
NotMovingLeft
	tya
	and #J0RIGHT
	bne NotMovingRight
	;--moving right
	;--first, if at right edge, don't allow rightward movement
	lda TankX,X
	cmp #136
	beq CannotMoveRight
	;--check for wall immediately to the right
	lda TankX,X
	clc
	adc #8
	sta Temp
	lda TankY,X
	sec
	sbc #2
	sta Temp+1
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	bne CannotMoveRight
	lda TankX,X
	clc
	adc #8
	sta Temp
	lda TankY,X
	sec
	sbc #8
	sta Temp+1
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	beq NoWallR
	
CannotMoveRight
	tya
	ora #J0RIGHT
	tay
NoWallR	
NotMovingRight
CheckVerticalMovement
	tya
	and #J0UP
	bne NotMovingUp
	;--moving up
	;--first, if at top edge, don't allow upward movement
	lda TankY,X
	cmp #TANKAREAHEIGHT+1
	beq CannotMoveUp
	;--check wall above
	lda TankX,X
	sta Temp
	lda TankY,X
	sec
	sbc #1
	sta Temp+1
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	bne CannotMoveUp
	;--temp+1 is unchanged
	lda TankX,X
	clc
	adc #7
	sta Temp
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	beq NoWallU
CannotMoveUp
	tya
	ora #J0UP
	tay
	jmp DoneWithMovementChecks
NotAtTopEdge
NoWallU	
NotMovingUp	
	tya
	and #J0DOWN
	bne NotMovingDown
	;--moving down
	;--first, if at bottom edge, don't allow downward movement
	lda TankY,X
	cmp #8
	beq CannotMoveDown
	;--check wall below
	lda TankX,X
	sta Temp
	lda TankY,X
	sec
	sbc #9
	sta Temp+1
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	bne CannotMoveDown
	;--temp+1 is unchanged
	lda TankX,X
	clc
	adc #7
	sta Temp
	jsr IsBlockAtPosition
	;result is in Temp
	lda Temp
	beq NoWallD
CannotMoveDown
	tya
	ora #J0DOWN
	tay
	jmp DoneWithMovementChecks
NotAtBottomEdge	
NoWallD	
NotMovingDown

DoneWithMovementChecks
	tya		;get allowed directions back into A

	rts




;****************************************************************************


UpdateRandomNumber

    lda RandomNumber
    lsr
    bcc SkipEOR
    eor #$B2
SkipEOR
    sta RandomNumber

    rts

;----------------------------------------------------------------------------
;-------------------------Data Below-----------------------------------------
;----------------------------------------------------------------------------
DigitDataLo
	.byte <Zero,<One,<Two,<Three,<Four,<Five,<Six,<Seven,<Eight,<Nine
	.byte <DigitA, <DigitB, <DigitC, <DigitD, <DigitE, <DigitF


	align 256


DigitData
Zero
        .byte #%01111111;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01000001;--
        .byte #%01000001;--
;        .byte #%01111111;--
Two
        .byte #%01111111;--
        .byte #%01100000;--
        .byte #%01100000;--
        .byte #%01111111;--
        .byte #%00000001;--
        .byte #%01000001;--
;        .byte #%01111111;--
Three
        .byte #%01111111;--
        .byte #%01000011;--
        .byte #%00000011;--
        .byte #%00111111;--
        .byte #%00000010;--
        .byte #%01000010;--
        .byte #%01111110;--
Seven
        .byte #%00000011;--
        .byte #%00000011;--
        .byte #%00000011;--
        .byte #%00000011;--
        .byte #%00000001;--
        .byte #%00000001;--
;        .byte #%01111111;--

Five
        .byte #%01111111;--
        .byte #%01000011;--
        .byte #%00000011;--
        .byte #%01111111;--
        .byte #%01000000;--
        .byte #%01000001;--
;        .byte #%01111111;--
Six
        .byte #%01111111;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01111111;--
        .byte #%01000000;--
        .byte #%01000001;--
        .byte #%01111111;--

Nine
        .byte #%00000011;--
        .byte #%00000011;--
        .byte #%00000011;--
        .byte #%01111111;--
        .byte #%01000001;--
        .byte #%01000001;--
;        .byte #%01111111;--
Eight
        .byte #%01111111;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01111111;--
        .byte #%00100010;--
        .byte #%00100010;--
        .byte #%00111110;--

One
        .byte #%00001100;--
        .byte #%00001100;--
        .byte #%00001100;--
        .byte #%00001100;--
        .byte #%00000100;--
        .byte #%00000100;--
        .byte #%00000100;--

Four
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%01111111;--
        .byte #%01000010;--
        .byte #%01000010;--
        .byte #%01000010;--
DigitA
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01111111;--
        .byte #%01000010;--
        .byte #%01000010;--
        .byte #%01111110;--
DigitB
        .byte #%01111111;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01111111;--
        .byte #%01000010;--
        .byte #%01000010;--
        .byte #%01111110;--
DigitC
        .byte #%01111111;--
        .byte #%01100000;--
        .byte #%01100000;--
        .byte #%01100000;--
        .byte #%00100000;--
        .byte #%00100000;--
        .byte #%00111100;--
DigitD
        .byte #%01111111;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01000011;--
        .byte #%01000010;--
        .byte #%01000010;--
        .byte #%01111110;--
DigitE
        .byte #%01111111;--
        .byte #%01100000;--
        .byte #%01100000;--
        .byte #%01111100;--
        .byte #%01100000;--
        .byte #%00100000;--
        .byte #%00111110;--
DigitF
        .byte #%01100000;--
        .byte #%01100000;--
        .byte #%01100000;--
        .byte #%01111100;--
        .byte #%01100000;--
        .byte #%00100000;--
        .byte #%00111110;--
	
	align 256
	ds BLOCKHEIGHT+1
BlockRowTable
	ds BLOCKHEIGHT, 0
	ds BLOCKHEIGHT, 1
	ds BLOCKHEIGHT, 2
	ds BLOCKHEIGHT, 3
	ds BLOCKHEIGHT, 4
	ds BLOCKHEIGHT, 5
	ds BLOCKHEIGHT, 6
	ds BLOCKHEIGHT, 7
	ds BLOCKHEIGHT, 8
	ds BLOCKHEIGHT, 9
	ds BLOCKHEIGHT, 10
	ds BLOCKHEIGHT, 11
	ds BLOCKHEIGHT, 12
	ds BLOCKHEIGHT, 13
	ds BLOCKHEIGHT, 14
	ds BLOCKHEIGHT, 15
	ds BLOCKHEIGHT, 16
	ds BLOCKHEIGHT, 17
	
TankUpFrame
	.word TankUpAnimated4, TankUpAnimated3, TankUpAnimated2, TankUpAnimated1
TankDownFrame
	.word TankDownAnimated4, TankDownAnimated3, TankDownAnimated2, TankDownAnimated1
TankRightFrame
	.word TankRightAnimated4, TankRightAnimated3, TankRightAnimated2, TankRightAnimated1
	align 256
	
	
	ds 80
	
TankGfxVertical
   
      
        
     ;--animation for upward facing tank:   
TankUpAnimated1
		.byte 0
        .byte #%11100111;--
        .byte #%00011000;--
        .byte #%11011011;--
        .byte #%00100100;--
        .byte #%11100111;--
        .byte #%00111100;--
TankUpAnimated1b
		.byte 0
        .byte #%00111111;--
        .byte #%11011000;--
        .byte #%00011011;--
        .byte #%11100100;--
        .byte #%00100111;--
        .byte #%11100100;--
        .byte 0
TankUpAnimated2
		.byte 0
		.byte #%11100100;--
		.byte #%00011011;--
		.byte #%11011000;--
		.byte #%00100111;--
		.byte #%11100100;--
		.byte #%00111111;--
TankUpAnimated2b
		.byte 0
        .byte #%11111111;--
        .byte #%00011000;--
        .byte #%11011011;--
        .byte #%00100100;--
        .byte #%11100111;--
        .byte #%00100100;--
		.byte 0
TankUpAnimated3
		.byte 0
		.byte #%00100100;--
		.byte #%11011011;--
		.byte #%00011000;--
		.byte #%11100111;--
		.byte #%00100100;--
		.byte #%11111111;--
TankUpAnimated3b
		.byte 0
        .byte #%11111100;--
        .byte #%00011011;--
        .byte #%11011000;--
        .byte #%00100111;--
        .byte #%11100100;--
        .byte #%00100111;--
		.byte 0
TankUpAnimated4
		.byte 0
		.byte #%00100111;--
		.byte #%11011000;--
		.byte #%00011011;--
		.byte #%11100100;--
		.byte #%00100111;--
		.byte #%11111100;--
TankUpAnimated4b
		.byte 0
        .byte #%00111100;--
        .byte #%11011011;--
        .byte #%00011000;--
        .byte #%11100111;--
        .byte #%00100100;--
        .byte #%11100111;--
		.byte 0
TankDownAnimated1
        .byte 0
        .byte #%11100100;--
        .byte #%00100111;--
        .byte #%11100100;--
        .byte #%00011011;--
        .byte #%11011000;--
        .byte #%00111111;--
TankDownAnimated1b
		.byte 0
		.byte #%00111100;--
		.byte #%11100111;--
		.byte #%00100100;--
		.byte #%11011011;--
		.byte #%00011000;--
		.byte #%11100111;--
		.byte 0
TankDownAnimated2
        .byte 0
        .byte #%00100100;--
        .byte #%11100111;--
        .byte #%00100100;--
        .byte #%11011011;--
        .byte #%00011000;--
        .byte #%11111111;--
TankDownAnimated2b
		.byte 0
		.byte #%00111111;--
		.byte #%11100100;--
		.byte #%00100111;--
		.byte #%11011000;--
		.byte #%00011011;--
		.byte #%11100100;--
		.byte 0
TankDownAnimated3
        .byte 0
        .byte #%00100111;--
        .byte #%11100100;--
        .byte #%00100111;--
        .byte #%11011000;--
        .byte #%00011011;--
        .byte #%11111100;--
TankDownAnimated3b
		.byte 0
		.byte #%11111111;--
		.byte #%00100100;--
		.byte #%11100111;--
		.byte #%00011000;--
		.byte #%11011011;--
		.byte #%00100100;--
		.byte 0
TankDownAnimated4
        .byte 0
        .byte #%11100111;--
        .byte #%00100100;--
        .byte #%11100111;--
        .byte #%00011000;--
        .byte #%11011011;--
        .byte #%00111100;--       	
TankDownAnimated4b
		.byte 0
		.byte #%11111100;--
		.byte #%00100111;--
		.byte #%11100100;--
		.byte #%00011011;--
		.byte #%11011000;--
		.byte #%00100111;--
		.byte 0
		
		
		align 256
		
		ds 80
		
TankGfxHorizontal
TankRightAnimated1
        .byte 0
        .byte #%01100110;--
        .byte #%11001111;--
        .byte #%10110001;--
        .byte #%10110111;--
        .byte #%11001100;--
        .byte #%11001100;--
TankRightAnimated1b
		.byte 0
		.byte #%01100110;--
		.byte #%01100110;--
		.byte #%10110111;--
		.byte #%10110001;--
		.byte #%11001111;--
		.byte #%11001100;--
		.byte 0
TankRightAnimated2
        .byte 0
        .byte #%11001100;--
        .byte #%11001111;--
        .byte #%10110001;--
        .byte #%10110111;--
        .byte #%10011001;--
        .byte #%10011001;--
TankRightAnimated2b
		.byte 0
		.byte #%11001100;--
		.byte #%11001100;--
		.byte #%10110111;--
		.byte #%10110001;--
		.byte #%11001111;--
		.byte #%10011001;--
		.byte 0
TankRightAnimated3
        .byte 0
        .byte #%10011001;--
        .byte #%11001111;--
        .byte #%10110001;--
        .byte #%10110111;--
        .byte #%00110011;--
        .byte #%00110011;--
TankRightAnimated3b
		.byte 0
		.byte #%10011001;--
		.byte #%10011001;--
		.byte #%10110111;--
		.byte #%10110001;--
		.byte #%11001111;--
		.byte #%00110011;--
		.byte 0
TankRightAnimated4
       	.byte 0
        .byte #%00110011;--
        .byte #%11001111;--
        .byte #%10110001;--
        .byte #%10110111;--
        .byte #%01100110;--
        .byte #%01100110;--
TankRightAnimated4b
		.byte 0
		.byte #%00110011;--
		.byte #%00110011;--
		.byte #%10110111;--
		.byte #%10110001;--
		.byte #%11001111;--
		.byte #%01100110;--
		.byte 0

		
		
		
		
		
		
		
		
		
		
		
		
		
    echo "----", ($10000-*), " bytes left (ROM)"


    org $FFFC
	.word Start
	.word Start



