;-------------------------------------------------------------------------
;---------Tank Battalion Atari 2600 port----------------------------------
;-------------------------------------------------------------------------
;	Bob Montgomery
;	(c) 2012-2022
;
;	VERY loose port of Tank Battalion
;		objective: destroy enemy tanks while protecting base in a destroyable maze
;		arcade game - play for high score
;			earn points by destroying tanks and advancing levels
;		have finite # of tanks to destroy each level (same for all, or increasing?)
;		once destroyed, level is won and advance to new level
;		destroy walls by shooting them
;		enemy tanks also shoot and also can destroy walls
;		enemy tanks shoot you, when you are shot, lose one life
;		if enemy tanks shoot base, game over (or?)
;		open question: what about tank:tank collisions?
;
;	To do:
;	Figure out better AI for tank movement/firing routines 
;	Add explosion graphics
;	Sound FX
;	Music?
;	Title/splash screen?  STARTED... still needs work.
;	Power-Ups - ???
;		keep this simple: 
;			speed
;			extra life
;			"bomb" that destroys all tanks on screen (and all walls?)
;			something that slows all enemies down?
;			
;
;	Maze generation routine done?  Tweaking?  Rewrite?
;	Collisions!
;	Score
;	Level objectives moving through levels - i.e., # of tanks remaining to kill
;	Difficulty settings
;	Gameplay tweaking
;	Graphics/colors
;	PAL60 version
;	2-player?  (unlikely but...)
;	Other...?
;	
;	right now stack uses 16 bytes, so I am basically out of RAM.  Need to reduce stack usage, or find other savings.
;
;	BUG KILLING!
;		diagonal movement (player) needs to be fixed
;		scanline count is wonky, need to tighten up various subroutines that take too long





	processor 6502
	include vcs.h
	include macro.h

;-------------------------Constants Below---------------------------------

TANKHEIGHT	=	7
BLOCKHEIGHT = 	7
MAZEROWS	=	11
TANKAREAHEIGHT	=	BLOCKHEIGHT * MAZEROWS
MAZEAREAHEIGHT 	= 	TANKAREAHEIGHT
DATASPACEBEFOREGFX	=	MAZEAREAHEIGHT

;--TankStatus bits:
TANKRIGHT		=	J0RIGHT
TANKLEFT		=	J0LEFT
TANKDOWN		=	J0DOWN
TANKUP			=	J0UP


PLAYERSTARTINGX	=	16
ENEMY0STARTINGX	=	16
ENEMY1STARTINGX	=	72
ENEMY2STARTINGX	=	136

TANKOFFSCREEN	=	127

MAZEGENERATIONPASSES = MAZEROWS/2-1

;--GameStatus bits

GAMEON			=	%10000000
GENERATINGMAZE	=	%01000000


BALLOFFSCREEN	=	154

BULLETRIGHT		=	0
BULLETLEFT		=	1
BULLETDOWN		=	2
BULLETUP		=	3
BULLETCLEAR		=	3

TRIGGERDEBOUNCEVALUE = 15
CONSOLEDEBOUNCEFLAG	=	%00010000
ENEMYDEBOUNCE = 32

BULLETSPEEDHOR		=		1
BULLETSPEEDVER		=		1

BASECOLOR		=		GREEN

WALLCOLOR			=		RED+6

MAZEPATHCUTOFF	=	100

TANKSPEED1	=	1
TANKSPEED2	=	2
TANKSPEED3	=	3
TANKSPEED4	=	4
TANKSPEED5	=	5
TANKSPEED6	=	6
TANKSPEED7	=	7
TANKSPEED8	=	8
TANKSPEED9	=	9
TANKSPEED10	=	10
TANKSPEED11	=	11
TANKSPEED12	=	12
TANKSPEED13	=	13
TANKSPEED14	=	14
TANKSPEED15	=	15
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


TANKAISWITCH	=	0


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
TankMovementCounter ds 1
RandomNumber ds 1

MazeNumber ds 1
MazeGenerationPass ds 1
GameStatus ds 1
Score ds 3
TanksRemaining ds 1


Debounce ds 1

EnemyDebounce ds 1
;BaseColor ds 1

TankX ds 4
TankY ds 4
TankStatus ds 4

TankFractional ds 4
;PlayerSpeed ds 1

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
LastRowL ds 1
LastRowR ds 1
Temp ds 3
ScorePtr
MiscPtr ds 2

   ; Display Remaining RAM
   echo "----",($100 - *) , "bytes left (ZP RAM)"
;---End Variables

	seg Bank0

	org $F000

Start
	CLEAN_START

;--Some Initial Setup


	jsr InitialSetupSubroutine


		
	
;	lda #BASECOLOR
;	sta BaseColor

;----------------------------------------------------------------------------
;--------------------GAME MAIN LOOP------------------------------------------
;----------------------------------------------------------------------------
MainGameLoop

	jsr VBLANKRoutine
	jsr KernelRoutineGame
	jsr OverscanRoutine
	jmp MainGameLoop



	
;----------------------------------------------------------------------------
;----------------------Kernel Routine----------------------------------------
;----------------------------------------------------------------------------
	
KernelRoutineGame


	;--room for score up here?

	ldy #0
	sta WSYNC
	lda #$FF
	sta COLUP0
	sta COLUP1
	pha
	pla	;wait 7 cycles
	
	;--display score
	lda #THREECOPIESCLOSE|LEFTONE
	sta VDELP0
	sta VDELP1						;+8		23		turn on VDELPx (bit0==1)

	sta NUSIZ0
	sta NUSIZ1						;+6		29

	sta HMP0						  ;					 LEFTONE
	asl
	sta HMP1						  ;+8		37		LEFTTWO

	sta RESP0						 ;+8		40		positioned at 57 (move left 1)
	sta RESP1						 ;+3		43		positioned at 66 (move left 2)

	sty GRP1
	sty GRP0
	sty ENAM0						 ;+7		50


	sta WSYNC
	sta HMOVE	
	;--waste time efficiently:
	ldy #10		; 2
.wait
	dey			 ; 2
	bne .wait	 ; 3

	SLEEP 2

	ldy #6+1
ScoreKernelLoop				 ;		  59		this loop can't cross a page boundary!
									 ;				(or - if it does, adjust the timing!)
	SLEEP 3
	dey							 ;+5		64
	sty Temp					;+3		67
	lda (ScorePtr),Y
	sta GRP0					  ;+8		75
	lda (ScorePtr+2),Y
	sta GRP1					  ;+8		 7
	lda (ScorePtr+4),Y
	sta GRP0					  ;+8		15
	lda (ScorePtr+6),Y
	tax							 ;+7		22
	lda (ScorePtr+8),Y
	pha							 ;+8		30
	lda (ScorePtr+10),Y
	tay							 ;+7		37
	pla							 ;+4		41
	stx GRP1					  ;+3		44
	sta GRP0
	sty GRP1
	sty GRP0					  ;+9		53
	ldy Temp					;+3		56
	bne ScoreKernelLoop		;+3		59
									 ;		  58
	sty GRP0
	sty GRP1
	sty GRP0					  ;+9		67	
	
	
	lda #ONECOPYNORMAL|OCTWIDTHMISSILE
	sta NUSIZ0
	sta NUSIZ1
	
	;--tank colors
	lda #GOLD+10
	sta COLUP0
	lda #BLUE2+12
	sta COLUP1
	
	ldx #4
PositioningLoopVBLANK	;--excluding players
	lda PlayerX,X
	jsr PositionASpriteSubroutine
	dex
	cpx #2
	bne PositioningLoopVBLANK

	
	;--reflect P0 or P1 as necessary.   prep for flip loop below
	lda FrameCounter
	and #1
	asl
	tax
	lda RotationTables,X
	sta MiscPtr
	lda RotationTables+1,X
	sta MiscPtr+1
	
	lda #WALLCOLOR
	sta COLUPF
	ldx #$80
	lda #$FF
	sta WSYNC
	stx PF0
	sta PF1
	sta PF2
	
	;---reflect P0/P1
	;--this loop is garbage, need to rewrite so it is faster
	ldy #3
SetREFPLoop
	lda TankStatus,Y
	and #TANKLEFT
	beq EndREFPLoop
	lax (MiscPtr),Y		;get original X index back
	cpx #2
	bcs EndREFPLoop
	lda #$FF
	sta REFP0,X	
EndREFPLoop
	dey
	bpl SetREFPLoop
	
	;--clear collision registers
	sta CXCLR


	ldx #2
PositioningLoop	;--just players
	lda PlayerX,X
	jsr PositionASpriteSubroutine
	dex
	bpl PositioningLoop

	
	jmp BeginMainMazeKernel


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
	
	
	
BeginMainMazeKernel

	sta WSYNC
	lda #0
	sta PF1
	sta PF2					;+8		 8
	
	sta VDELP1
	
	lda #$FF
	sta VDELP0
	sta VDELBL

	SLEEP 5

	;--use stack pointer to hit ENABL
	tsx
	stx Temp
	ldx #<ENABL
	txs						;+9		31

	
	ldy #TANKAREAHEIGHT		;+2		35
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

;	align 256
	
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
	;lda #$80
	;sta PF2
	SLEEP 5
	;line 1 of last row, this row has BASE (not wall)
KernelLastRowLoop			;		36
	lda Temp+1
	sta COLUPF				;+6		42
	;draw player 0
	cpy Player0Top				
	beq Switch0b
	bpl Wait0b
	lda (Player0Ptr),Y
	sta GRP0				;+15	57
BackFromSwitch0b
	
	lda #WALLCOLOR
	sta COLUPF				;+5		62
	

	;draw player 1
	lda #TANKHEIGHT*2-1
	dcp PlayerYTemp
	bcs DoDraw10b
	lda #0
	.byte $2C
DoDraw10b
	lda (Player1Ptr),Y
	sta GRP1				;+18	 4


	;line 2 of last row, has WALL (not base)
	;draw missile 0
	lda #TANKHEIGHT-2
	dcp MissileYTemp
	sbc #TANKHEIGHT-4
	sta ENAM0				;+12	16
	
	
	lda #TANKHEIGHT-2
	dcp MissileYTemp+1
	sbc #TANKHEIGHT-4
	sta ENAM1				;+12	28
	
	
	lda LastRowL
	sta PF2					;+6		34
	
	SLEEP 6					;		40
	
	lda LastRowR
	sta PF2					;+6		46
	
	tsx
	cpy BallY
	php
	txs						;+10	56
	
	
	
	cpy Player0Top				
	beq Switch1b
	bpl Wait1b
	lda (Player0Ptr+2),Y		
	sta GRP0				;+15	71
BackFromSwitch1b
	
	
	;draw player 1
	lda #TANKHEIGHT*2-1
	dcp PlayerYTemp
	bcs DoDraw11b
	lda #0
	.byte $2C
DoDraw11b
	lda (Player1Ptr+2),Y
	sta GRP1			;+18	13
	
	lda LastRowL
	sta PF2				;+6		19

	
	SLEEP 7			;		31
	

	lda #$80
	sta PF2				;+5		31
	dey
	bne KernelLastRowLoop		;+5		36
	
	sty PF2		;AKSHUALLY don't display base on last row
	ldx Temp
	txs			;+8		44

	sty ENABL	;+3		47
	sty GRP0	;+3		50
	ldy #$FF	;+2		52
	ldx #0		;+2		54
	sta WSYNC
	sty PF2		;+3		57
	sty PF1		;+3		60
	stx GRP1	
	stx ENAM0
	stx ENAM1
;	sta PF0
	
	lda #104
	jsr PositionASpriteSubroutine
	
	lda #112
	inx
	jsr PositionASpriteSubroutine

	lda #60
	inx
	jsr PositionASpriteSubroutine

	lda #66
	inx
	jsr PositionASpriteSubroutine
	
	
	;--done displaying maze
	;--set up score, tanks remaining, lives remaining etc

	sta WSYNC
	ldy #0
	sty PF0
	sty PF1
	sty PF2
	sty COLUPF
	sty REFP0
	sty REFP1
	sty VDELP0
	sty VDELBL
	lda #THREECOPIESCLOSE
	sta NUSIZ0
	lda #TWOCOPIESCLOSE
	sta NUSIZ1
	
	lda #RED
	sta COLUP0
	sta COLUP1
	
	

	lda #>DigitDataMissile
	sta Player0Ptr+3
	sta Player1Ptr+1
	lda MazeNumber
	lsr
	lsr
	lsr
	lsr
	tay
	lda DigitDataMissileLo,Y
	sta Player0Ptr+2
	lda MazeNumber
	and #$0F
	tay
	lda DigitDataMissileLo,Y
	sta Player1Ptr

	lda #$FF
	sta PF0
	sta PF2		;this masks the missiles before we want them to show
				;we will use the actual mask inside the loop below
	
	lda TanksRemaining
	lsr
	tax

	lda TanksRemainingPF1Mask,X
	sta PF1
	
	lda #2
	sta ENAM0
	sta ENAM1
	ldy #8
	bne BottomKernelLoopInner	;	branch always
	
BottomKernelLoopMiddle			;		19
	dey							;+2		21
	
	lda TanksRemaining
	lsr
	adc #0						;+7		28
	
	tax							;+2		30
	lda TanksRemainingPF1Mask,X
	sta PF1
	lda TanksRemainingPF2Mask,X
	sta PF2						;14		44
	
	lda (Player0Ptr+2),Y		;+5
	sta HMM0					;+3
	asl							;+2
	asl							;+2
	ora #3						;+2
	sta NUSIZ0					;+3		61

	lda (Player1Ptr),Y			;+5		66
	sta HMM1					;+3		69	
	asl							;+2		71
	asl							;+2		73
	ora #3						;+2		75
	sta NUSIZ1					;+3		 2

	sta HMOVE					;+3		 5

	
	bne BottomKernelLoopInnerMiddle	;	 8		branch always
	
BottomKernelLoopInner			;		38
	lda (Player0Ptr+2),Y		;+5
	sta HMM0					;+3
	asl							;+2
	asl							;+2
	ora #3						;+2
	sta NUSIZ0					;+3		55

	lda (Player1Ptr),Y			;+5		
	sta HMM1					;+3			
	asl							;+2
	asl							;+2		
	ora #3						;+2
	sta NUSIZ1					;+3		72

	sta WSYNC
	
	sta HMOVE					;+3		 3
BottomKernelLoopInnerMiddle		;		 8
	

	lda TanksRemainingGfx,Y		;+4	
	sta GRP0			
	sta GRP1					;+6		13
	
	cpy #4						
	beq BottomKernelLoopMiddle	;+5		18
								;		17
	;--PF1 is set once per half loop, this is set here because we enter the loop with PF2=$FF
	;	which is necessary to mask the missiles before we are ready to display them
	lda TanksRemainingPF2Mask,X
	sta PF2
	SLEEP 4						;+10	28		MUST be 10 here so that we don't run over the scanline (>12) or change NUSIZ0 too early (<10)
	dey
	bpl BottomKernelLoopInner	;+5		33

	
BottomKernelLoopDone
	sta WSYNC
	iny
	sty ENAM0
	sty ENAM1
	sty GRP0
	sty GRP1
	sty GRP0
	sty PF0
	sty PF1
	sty PF2

	rts


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

	lda #44
	sta TIM64T
	
	
	dec FrameCounter
	
	
	
	lda GameStatus
	and #GAMEON
	beq GameNotOnVBLANK	
	jsr CollisionsSubroutine
	jsr ReadControllersSubroutine
GameNotOnVBLANK
	
	jsr UpdateRandomNumber
	
	lda Debounce
	and #$0F
	beq TriggerDebounceZero
	dec Debounce
TriggerDebounceZero	

	jsr ReadConsoleSwitchesSubroutine



	jsr KernelSetupSubroutine

	

WaitForVblankEnd
	lda INTIM
	bmi OverTimeVBLANK
	bne WaitForVblankEnd
	beq EndVBLANK
OverTimeVBLANK				;this nonsense is here just so I can trap an overtime condition in an emulator, if needed
	nop
		
EndVBLANK	

	sta WSYNC
	lda #0
	sta VBLANK


	rts

	
	
;----------------------------------------------------------------------------
;------------------------Overscan Routine------------------------------------
;----------------------------------------------------------------------------
OverscanRoutine




	ldy #2
	sty WSYNC
	sty VBLANK
	lda  #34
	sta  TIM64T
	
	
	lda GameStatus
	and #GAMEON
	beq GameNotOn	
	jsr MoveEnemyTanksSubroutine	
	jsr MoveBulletSubroutine
GameNotOn
	lda GameStatus
	and #GENERATINGMAZE
	beq NotGeneratingMaze
	jsr GenerateMazeSubroutine
NotGeneratingMaze
	



WaitForOverscanEnd
	lda INTIM
	bmi OverTimeOverscan
	bne WaitForOverscanEnd
	beq EndOverscan
OverTimeOverscan
	nop
EndOverscan	
	
	sta WSYNC		;last line...I think?

	rts
	
	
;----------------------------------------------------------------------------
;----------------------------End Main Routines-------------------------------
;----------------------------------------------------------------------------

;****************************************************************************

;----------------------------------------------------------------------------
;----------------------Begin Functions---------------------------------------
;----------------------------------------------------------------------------




InitialSetupSubroutine

	
	ldx #3
SetUpTankInitialValues
	lda #BALLOFFSCREEN
	sta BulletX,X
	sta BulletY,X
	lda #TANKOFFSCREEN
	sta TankX,X
	sta TankY,X
	dex
	bpl SetUpTankInitialValues
	
	sta TankMovementCounter	;start this at $FF (for debugging purposes, for now...)
	sta RandomNumber
	
	
; 	lda #$65
; 	sta Score
; 	lda #$43
; 	sta Score+1
; 	lda #$21
; 	sta Score+2
	
	lda #WALLCOLOR
	sta COLUPF
	
; 	lda #QUADWIDTHBALL
; 	sta NUSIZ0
; 	sta NUSIZ1
; 	
	lda #REFLECTEDPF|DOUBLEWIDTHBALL|PRIORITYPF
	sta CTRLPF
	
	
	;--TODO: interleave the gfx data so this loop can be cleaned up.... maybe.
	ldx #TitleGraphicsEnd-TitleGraphics-1
	ldy #MAZEROWS-2
PutTitleGraphicsInPlayfieldLoop
	txa
	lsr
	lsr
	tay
	lda TitleGraphics,X
	sta PF1Right,Y
	dex
	lda TitleGraphics,X
	sta PF2Right,Y
	dex
	lda TitleGraphics,X
	sta PF2Left,Y
	dex
	lda TitleGraphics,X
	sta PF1Left,Y
	dex
	bpl PutTitleGraphicsInPlayfieldLoop
	
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
	
	eor #$FF	;flip bits
	jsr EliminateDiagonalSubroutine
	eor #$FF
	
	rts


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
	bne MoveAnEnemyTank
	dec TankMovementCounter
	;--if we ain't moving a tank, let's shoot a bullet
	;--- only every ... 16 frames
	lda EnemyDebounce
	beq FireEnemyBullet
	dec EnemyDebounce
	jmp DoNotFireEnemyBullet
FireEnemyBullet
	ldx #1
FindAvailableEnemyBallLoop
	lda BulletX+2,X
	cmp #BALLOFFSCREEN
	beq FoundAvailableEnemyBall
	dex
	bpl FindAvailableEnemyBallLoop
	bmi NoAvailableEnemyBalls
FoundAvailableEnemyBall
	inx
	inx
	lda #ENEMYDEBOUNCE
	sta EnemyDebounce
	;--find random tank 
	lda RandomNumber
	and #3
	tay
	bne ShootFromTank
	iny
ShootFromTank
	
	
	
	jsr FireBulletRoutine
DoNotFireEnemyBullet	
NoAvailableEnemyBalls
	jmp ReturnFromTankMovementSubroutine
MoveAnEnemyTank
	tax

	lda TankStatus,X
	and #$F0
	bne TankOnscreenMoveIt
	;tank offscreen
	lda TankMovementCounter
	and #7
	bne DoNotBringTankOnscreenYet
	dec TankStatus,X
	bpl DoNotBringTankOnscreenYet
	lda #MAZEAREAHEIGHT+TANKHEIGHT
	sta TankY,X
	lda #J0DOWN
	ora NewTankSpeed,X
	sta TankStatus,X
DoNotBringTankOnscreenYet
	rts
TankOnscreenMoveIt
	
	;--plan for now:  every time enemy tank hits intersection, change direction
	;push X, Y target for tank onto stack and then call ChooseTankDirectionSubroutine
	
	lda TankX,X
	and #$07
	beq AtIntersectionX
	jmp NotAtIntersection
AtIntersectionX
	lda TankY,X
	sec
	sbc #1
FindYIntersectionLoop
	sbc #BLOCKHEIGHT
	bcs FindYIntersectionLoop
	adc #BLOCKHEIGHT
	beq AtIntersectionY
	jmp NotAtIntersection
AtIntersectionY	
	;--here is where we choose which way each tank will go
	;
	;--new routine: 
	;--first set TankFractional so that they move immediately
	lda #255
	sta TankFractional,X
	
	
	;	get allowable directions
	lda #0;#(J0LEFT|J0RIGHT|J0UP|J0DOWN)
	jsr CheckForWallSubroutine	;--returns with allowable directions in A

	and #$F0	;clear bottom nibble
	eor #$F0
	
	;--if single direction, then move that direction
	pha			;--save direction

	lsr
	lsr
	lsr
	lsr			;--get in bottom nibble
	tay
	lda NumberOfBitsSet,Y
	;--if can only move a single direction, go that direction
	cmp #1
	bne MoreThanOneAllowedDirection
	pla			;get direction back off stack
	sta Temp
	lda TankStatus,X
	and #~(J0UP|J0DOWN|J0LEFT|J0RIGHT)
	ora Temp
	sta TankStatus,X
	jmp DirectionChosen
MoreThanOneAllowedDirection
	;--if can only move in two directions, go whichever ISN'T back the way we came from
	cmp #2
	bne MoreThanTwoAllowedDirections
	;--find current direction, clear it's opposite, and then move the one remaining direction
	lda TankStatus,X
	lsr
	lsr
	lsr
	lsr			;index into table with opposite directions
	tay
	pla
	eor PreventReverses,Y
	sta Temp
	lda TankStatus,X
	and #~(J0UP|J0DOWN|J0LEFT|J0RIGHT)
	ora Temp
	sta TankStatus,X
	jmp DirectionChosen

			
	
MoreThanTwoAllowedDirections	
	;--find current direction, clear it's opposite, and then save the remaining allowable directions
	lda TankStatus,X
	lsr
	lsr
	lsr
	lsr			;index into table with opposite directions
	tay
	pla
	eor PreventReverses,Y
	pha
	
	;--if TankMovementCounter < 8, then move tanks towards various corners, otherwise, follow regular pattern
	lda TankMovementCounter
	cmp #TANKAISWITCH
	bcs RegularMovement
	; move to upper left corner
	lda SwitchMovementX,X
	pha
	lda SwitchMovementY,X
	pha
	jmp ChooseTankDirection
RegularMovement
	;routine: 
	; pick two targets, then take the midpoint between them
	ldy TankTargetX,X
	lda $00,Y
	ldy TankTargetAdjustX,X
	sec
	sbc $00,Y
	jsr DivideByTwoSubroutine
	clc
	adc $00,Y
	pha
	jsr UpdateRandomNumber
	ldy TankTargetY,X
	lda $00,Y
	ldy TankTargetAdjustY,X
	sec
	sbc $00,Y
	jsr DivideByTwoSubroutine
	clc
	adc $00,Y
	pha


	jmp ChooseTankDirection
	
	
ChooseTankDirection
	jsr ChooseTankDirectionSubroutine	;returns with new direction in accumulator
	sta Temp
	lda TankStatus,X
	and #~(J0UP|J0DOWN|J0LEFT|J0RIGHT)
	ora Temp
	sta TankStatus,X					
	pla
	pla									;pull target for tank off stack and discard

	;--now compare to allowable directions
	pla	;	--get allowable directions off stack
	pha	;	but leave on stack
	and TankStatus,X
	bne DirectionIsFine
	;--direction is not ok
	;	for now, randomly pick one of the allowed directions
	lda RandomNumber
	and #3
	tay
TryAnotherDirection
	pla	;--get allowed directions
	pha	;--but leave on stack
	and MovementMask,Y
	bne FoundGoodDirection
	dey
	bpl TryAnotherDirection
	ldy #3					;loop around
	bne TryAnotherDirection	;branch always -- this assumes we will always find a good direction, that the tank can never be stuck and unable to move
FoundGoodDirection
	sta Temp
	lda TankStatus,X
	and #~(J0UP|J0DOWN|J0LEFT|J0RIGHT)
	ora Temp
	sta TankStatus,X
		
DirectionIsFine
	pla
DirectionChosen
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
	
	rts

;****************************************************************************

;****************************************************************************



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



UpdateRandomNumber

    lda RandomNumber
    lsr
    bcc SkipEOR
    eor #$B2
SkipEOR
    sta RandomNumber

    rts
;****************************************************************************

	;come in with amount to increase in A
IncreaseScoreSubroutine
	clc
	sed
	adc Score+2
	sta Score+2
	lda Score+1
	adc #0
	sta Score+1
	lda Score
	adc #0
	sta Score
	cld
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
	
	;--first, add 1 to score
	lda #1
	jsr IncreaseScoreSubroutine
	
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
	;--special check for row = 0
	tya
	bne BulletNotOnBottomRow
	;--all we care is left or right of center
	lda BulletX,X
	cmp #80
	bcc BulletOnLeftLastRow
	lda #0
	sta LastRowR
	beq RemovedWallBlock
BulletOnLeftLastRow
	lda #0
	sta LastRowL
	beq RemovedWallBlock
BulletNotOnBottomRow
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
RemovedWallBlock
	;--bullet has hit block, remove bullet from screen:
	lda #BALLOFFSCREEN
	sta BulletX,X
	sta BulletY,X
BallHasNotHitBlock
	dex
	bpl CheckBallCollisionsLoop


	;--now check if bullet has hit an enemy tank
	
	ldx #3
CheckBulletTankCollisionOuterLoop
	;first check if tank is offscreen
	lda TankY,X
	cmp #MAZEAREAHEIGHT+TANKHEIGHT+4
	beq EnemyTankOffscreen
	
	;now compare ball location to tank location
	;--only care about player bullets
	ldy #3
CheckBulletTankCollisionInnerLoop
	lda BulletX,Y
	cmp #BALLOFFSCREEN
	beq BulletOffScreen
	;--compare X position
	clc
	adc #1
	cmp TankX,X
	bcc BulletDidNotHitTank 
	sbc #1
	sta Temp
	lda TankX,X
	clc
	adc #8
	cmp Temp
	bcc BulletDidNotHitTank
	;--compare Y position
	lda BulletY,Y
	sec
	sbc #1
	cmp TankY,X
	bcs BulletDidNotHitTank
	adc #1
	sta Temp
	lda TankY,X
	sec
	sbc #TANKHEIGHT
	cmp Temp
	bcs BulletDidNotHitTank
	
	
	
	;--bullet did hit tank.
	;	remove bullet and tank from screen
	lda #BALLOFFSCREEN
	sta BulletX,Y
	sta BulletY,Y
	lda StartingTankYPosition,X
	sta TankY,X
	lda StartingTankXPosition,X
	sta TankX,X
	lda StartingTankStatus,X
	sta TankStatus,X
	;--and decrease tanks remaining ONLY if enemy tank hit
	dec TanksRemaining
	
	
BulletDidNotHitTank
BulletOffScreen
	dey
	bpl CheckBulletTankCollisionInnerLoop
	
EnemyTankOffscreen
	dex
	bpl CheckBulletTankCollisionOuterLoop
	
	
	rts

;****************************************************************************

ReadConsoleSwitchesSubroutine


	


	lda SWCHB
	lsr				;get RESET into carry
	bcs NoRESET
	lda Debounce
	and #CONSOLEDEBOUNCEFLAG
	
	bne RESETNotReleased
	lda Debounce
	ora #CONSOLEDEBOUNCEFLAG
	sta Debounce
	;--start game
	lda GameStatus
	ora #GENERATINGMAZE
	sta GameStatus
	
	;--move tanks off screen, immobilize and set score to zeroes
	ldx #3
	ldy #0
MoveTanksOffscreenLoop
	lda #TANKOFFSCREEN
	sta TankX,X
	sta TankY,X
	lda #0
 	sta TankStatus,X
	sta Score,X
	dex
	bpl MoveTanksOffscreenLoop
	
	
	
	;--finally, cycle the random number
	jsr UpdateRandomNumber
	
	lda #$FF
	sta MazeGenerationPass

	
	
	jmp DoneWithConsoleSwitches
NoRESET
	lda Debounce
	and #~CONSOLEDEBOUNCEFLAG
	sta Debounce
RESETNotReleased
DoneWithConsoleSwitches
	rts

	

;****************************************************************************
	
GenerateMazeSubroutine

	;--first, save RandomNumber
	lda RandomNumber
	pha
	
	;--let's try splitting it 
	ldy MazeGenerationPass
	bpl NotFirstPass
	ldy #MAZEGENERATIONPASSES+1
	sty MazeGenerationPass
	;--on first pass, fill all with walls
	
	ldx #MAZEROWS-2
	lda #$FF
FillMazeLoop
	sta PF1Left,X
	sta PF2Left,X
	sta PF2Right,X
	sta PF1Right,X
	dex 
	bpl FillMazeLoop
	
	;	leave room for enemy tanks to enter:
	;clear upper L corner
	lda PF1Left+MAZEROWS-2
	and #$3F
	sta PF1Left+MAZEROWS-2
	
	;clear upper R corner
	lda PF1Right+MAZEROWS-2
	and #$3F
	sta PF1Right+MAZEROWS-2
	
	;clear spot directly to L of center in top row
	lda PF2Left+MAZEROWS-2
	and #$3F
	sta PF2Left+MAZEROWS-2
	
	;--update maze number on first pass also
UpdateMazeNumber
	lda MazeNumber
	sed
	clc
	adc #1
	sta MazeNumber
	cld
	
	beq UpdateMazeNumber
	
	;and set seed for maze
	
	lda MazeNumber
	sta Temp+2	
	
	;--and that's it for the first pass
	jmp DoneWithFirstPass
	
	
NotFirstPass
	lda Temp+2
	sta RandomNumber


	
	;--use MazeNumber as seed of random number generator
	;--save current random number so we can restore it when we are done.



	lda #>PF1Left
	sta MiscPtr+1

	;--now what we are doing is going through every other row
	;	starting at a random block on the right, and carving out a random-length
	;	horizontal path to the left.
	;	when we finish that, we pick a random spot to carve a block out of the row below
	;	and then we move two blocks to the left (if there is room), and repeat for another 
	;	random-length horizontal path
	;	once done with a row, we move two rows down and repeat the whole process
	;	Y holds row, X holds block within the row
	
	tya
	asl
	clc
	adc #1
	tay

	;ldx #15			;<-- this always starts on the right-most column, need to adjust this so random
	jsr UpdateRandomNumber
	;lda RandomNumber
	and #1
	eor #15
	tax
	
MakeMazeLoopOuter	
	stx Temp
	stx Temp+1
MakeMazeLoopInner
	lda #<PF1Left
	sta MiscPtr
	
	;--clear block
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
	
	lda #<PF1Left
	sta MiscPtr

	;--are we at the end of our horizontal path?  compare random number to constant
	jsr UpdateRandomNumber
	;lda RandomNumber
	cmp #MAZEPATHCUTOFF
	bcc EndOfRun
	;--not at the end of the run, so loop around
	dec Temp+1
	dex
	bpl MakeMazeLoopInner

	;--need to carve a passage downward if we reach this spot:
EndOfRun
	;--don't carve passage downward if we're on the bottom row
	cpy #0
	beq EndRunBeginNextRun
	;--otherwise, find a random passage to carve
	;--Temp holds starting block number (X)
	;--Temp+1 holds ending block number
	lda Temp
	sec
	sbc Temp+1
	sta Temp+1		;now Temp+1 holds length of horizontal passage (minus 1)
 	beq OnlyOnePlaceToCarve
	;--this routine tries to find a random place in our new passage to carve downwards
	;--picks a random number, then subtracts the length of the passage
	;	until we cross zero, then adds length back for spot to add downward passage
 	jsr UpdateRandomNumber
	;lda RandomNumber
	;--new routine:
	;	decrease length by 1, then remove random bits
	dec Temp+1
	and Temp+1
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
	iny		;restore Y to current row index
	pla		;get block index back into X
	tax
	stx Temp
	stx Temp+1
	dec Temp
	dec Temp+1
	lda #<PF1Left
	sta MiscPtr
	dex
	dex
	bmi DoneWithRow
	bpl MakeMazeLoopOuter
EndRunBeginNextRun
	;dex
	stx Temp
	stx Temp+1
	dec Temp
	dec Temp+1
	lda #<PF1Left
	sta MiscPtr
	dex
	bmi DoneWithRow
NotEndOfRun	
	
	
DoneWithRow
DoneWithFirstPass
	dec MazeGenerationPass

	bpl NotCompletelyDoneWithMaze


	;--final touchup:
	;	open up area directly above base:
	;--- change to CLOSE up area directly above base, not sure about this.
	lda PF2Left
	ora #$F0
	sta PF2Left
	lda PF2Right
	ora #$F0
	sta PF2Right

	;--add walls on bottom row surrounding base
	lda #$30
	sta LastRowL
	sta LastRowR
	
	;--and set GAMEON flag and turn off maze generation flag
	lda GameStatus
	ora #GAMEON
	and #~GENERATINGMAZE
	sta GameStatus
	

	;--set starting tank position
	
	;--move tanks off screen
	ldx #3
SetStartingEnemyTankLocationsLoop
	lda StartingTankXPosition,X
	sta TankX,X
	lda StartingTankYPosition,X
	sta TankY,X
	dex
	bpl SetStartingEnemyTankLocationsLoop
	
; 	;--set score to zero and make tanks immobile
; 	lda #0
; 	ldx #3
; GameStartLoop
;  	sta TankStatus,X
; 	sta Score,X
; 	dex
; 	bpl GameStartLoop
	
	
	
	
	lda #20
	sta TanksRemaining
	
	;--starting timers for when tanks enter the maze
	lda #15
	sta TankStatus+1
	lda #7
	sta TankStatus+2
	lda #1
	sta TankStatus+3
	
	;--set speed for player tank
	lda #TANKSPEED4
	sta TankStatus

	
NotCompletelyDoneWithMaze
	;--restore original random number
	lda RandomNumber
	sta Temp+2
	pla
	sta RandomNumber
	
	rts
	
	
	
;****************************************************************************


	
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
	cmp #16
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
	;--A holds direction of joystick (filtered to allowed directions)
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
	
	lsr
	lsr
	lsr
	lsr
	eor #$0F
	tay
	lda NumberOfBitsSet,Y
	tay
	

	pla
	tax						;restore X
	pla						;restore directions
	
	cpy #2
	bcc DirectionsFine
	;--too many directions - must narrow down to 1
	;  assumption is that an actual diagnonal is here -
	;	  i.e., L+U, or R+D, but NOT L+R or U+D nor three or four directions

	pha	;save desired directions on the stack
	; for Enemy Tanks=randomly eliminate either horizontal or vertical movement
	; for Player Tank=do previous routine which allowed tank to slide along
	txa
	bne EnemyTankEliminateDiagonal
EliminatePlayerDiagonal
	;--otherwise, back to the drawing board:
	;-- assumption for player tank is if holding at a diagonal and we come to 
	;	an intersection, player always wants to turn
	;--so if moving up/down, start by eliminating those directions

	
	;--moving at a diagonal.
	;.....not sure what to do.
	lda TankStatus
	and #$F0
	tsx
	and ($00,X)
	beq CompleteChangeOfDirection
	
	ldx #0
	txa
;	jsr CheckForWallSubroutine
;	tax
;	bne RegularDiagonalHandling
;	lda TankStatus
;	and #$F0
;	eor #$F0
;	ldx #0
;	jmp DirectionsFine
	
	
RegularDiagonalHandling
CompleteChangeOfDirection
	ldx #0
	lda TankStatus
	and #J0LEFT|J0RIGHT
	beq EliminateUpDown
	;--else eliminate left and right
	pla
	ora #J0LEFT|J0RIGHT
	bne DirectionsFine
	
EliminateUpDown
	pla						;restore directions 
	ora #J0UP|J0DOWN		;eliminate up and down
	bne DirectionsFine		;branch always

EnemyTankEliminateDiagonal
	lda RandomNumber
	lsr				;random bit
	pla ;get desired directions back into accumulator
	bcs EliminateHorizontal
	;--else eliminate vertial
	ora #J0RIGHT|J0LEFT
	bne DirectionsFine
EliminateHorizontal
	ora #J0UP|J0DOWN

	
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
	pla			;pull tank index off stack
	tax			;restore tank index
	pla			;pull allowed directions off (of original desired directions)
ProcessDirections
	pla			;pulls allowed directions off (of original desired directions)
	jsr EliminateDiagonalSubroutine		;tanks cannot move diagonally

	tay	;--save direction in Y
	
	;--if tank is turning, set fractional variable so tank moves immediately
	and #$F0
	eor #$F0
	sta Temp
	lda TankStatus,X
	and #$F0
	cmp Temp
	beq TankNotTurning
	;tank turning
	lda #$FF
	sta TankFractional,X
TankNotTurning
	txa
	pha	;save tank index
	
	tsx
	asl $02,X		;get movement flag into carry flag

	pla
	tax	;restore tank index
	
	tya						;get saved directions back in A	
	and #J0UP
	bne NoUp
	bcs TurnUpward			;carry holds movement flag (carry clear=no movement)
	lda TankStatus,X
	asl
	asl
	asl
	asl
	ora #$0F
	clc
	adc TankFractional,X
	sta TankFractional,X
	lda TankY,X
	adc #0
	sta TankY,X
	
TurnUpward
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKUP
	sta TankStatus,X
	jmp DoneMoving
NoUp
	tya
	and #J0DOWN
	bne NoDown
	bcs TurnDownward		;carry holds movement flag (carry clear=no movement)
	lda TankStatus,X
	asl
	asl
	asl
	asl
	ora #$0F
	sta Temp
	lda TankFractional,X
	clc
	adc Temp
	sta TankFractional,X
	bcc TurnDownward	;but don't move down
	dec TankY,X
TurnDownward
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKDOWN
	sta TankStatus,X
	jmp DoneMoving
NoDown
	tya
	and #J0RIGHT
	bne NoRight
	bcs TurnRightward		;carry holds movement flag (carry clear=no movement)
	lda TankStatus,X
	asl
	asl
	asl
	asl
	ora #$0F
	clc
	adc TankFractional,X
	sta TankFractional,X
	lda TankX,X
	adc #0
	sta TankX,X
TurnRightward	
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKRIGHT
	sta TankStatus,X
	jmp DoneMoving
NoRight
	tya
	and #J0LEFT
	bne NoLeft
	bcs TurnLeftward		;carry holds movement flag (carry clear=no movement)
;	txa
;	bne EnemyTankGoesLeft
	lda TankStatus,X
	asl
	asl
	asl
	asl
	ora #$0F
	sta Temp
	lda TankFractional,X
	clc
	adc Temp
	sta TankFractional,X
	bcc TurnLeftward	;but don't move
	dec TankX,X
TurnLeftward
	lda TankStatus,X
	and #~(TANKUP|TANKDOWN|TANKRIGHT|TANKLEFT)
	ora #TANKLEFT
	sta TankStatus,X
	
NoLeft
DoneMoving
	pla		;pop movement flag off of stack and discard

	;are any balls available for firing
	ldx #1
FindAvailableBallLoop
	lda BulletX,X
	cmp #BALLOFFSCREEN
	beq FoundAvailableBall
	dex
	bpl FindAvailableBallLoop
	jmp NoAvailableBalls
FoundAvailableBall	
	;--now read joystick trigger if debounce is zero:
	lda INPT4
	bpl TriggerHit
	;--if trigger not hit, set debounce to zero
	lda Debounce
	and #$F0
	sta Debounce
	jmp NotFiring
TriggerHit
	lda Debounce
	and #$0F
	beq TriggerDebounced
	jmp TriggerNotDebounced
	
TriggerDebounced
	;--bullet is fired!

	
	
	;set trigger debounce
	lda Debounce
	ora #TRIGGERDEBOUNCEVALUE
	sta Debounce
	
	ldy #0		;set index into which tank 
				;X holds index into which bullet
FireBulletRoutine
	
	
	;--set position:
	lda TankX,Y
	clc
	adc #4
	;sta BulletX,X
	sta Temp
	lda TankY,Y
	sec
	sbc #3
	;sta BulletY,X
	sta Temp+1
	
	;--clear old direction
	lda BulletDirectionClear,X
	eor #$FF
	and BulletDirection
	sta BulletDirection 

	
	;--then set new direction:
	lda TankStatus,Y
	tay	
	and #TANKUP
	beq NotFiringUp
	;--shooting up
	lda BulletDirection
	ora BulletUp,X
	sta BulletDirection
	lda Temp+1
	clc
	adc #4
	sta Temp+1
	jmp DoneFiring
NotFiringUp
	tya
	and #TANKDOWN
	beq NotFiringDown
	;--shooting down
	lda BulletDirection
	ora BulletDown,X
	sta BulletDirection
	lda Temp+1
	sec
	sbc #4
	sta Temp+1
	jmp DoneFiring
NotFiringDown	
	tya
	and #TANKLEFT
	beq NotFiringLeft
	;--shooting left
	lda BulletDirection
	ora BulletLeft,X
	sta BulletDirection
	lda Temp
	sec
	sbc #6
	sta Temp
	jmp DoneFiring
NotFiringLeft
	tya
	and #TANKRIGHT
	beq NotFiring
	;--shooting right
	lda BulletDirection
	ora BulletRight,X
	sta BulletDirection
	lda Temp
	clc
	adc #5
	sta Temp
DoneFiring
	lda Temp
	sta BulletX,X
	lda Temp+1
	sta BulletY,X


NotFiring
NoAvailableBalls	

TriggerNotDebounced

	
	rts
	

	

;****************************************************************************


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








	ldx #1
PreKernelSetupLoop2
	lda #TANKAREAHEIGHT+1
	sec
	sbc MissileY,X
	adc MissileHeight,X
	sta MissileYTemp,X
	
	lda MissileX,X
	clc
	adc #3
	sta MissileX,X
	dex
	bpl PreKernelSetupLoop2
	
	
	;--if missile Y is above the playable area, need to adjust down by ... 1 line?
	ldx #1
AdjustMissileYLoop
	lda MissileYTemp,X
	cmp #TANKHEIGHT-2
	bcs NoAdjustMissileY
	adc #1
	sta MissileYTemp,X
NoAdjustMissileY
	dex
	bpl AdjustMissileYLoop
	
	
	;bullet flicker rotation:
	lda FrameCounter
	and #3
	tax
	lda BulletX,X
	sta BallX
	lda BulletY,X
	sta BallY


	;--cycle BaseColor
	lda FrameCounter
	and #$0F
	sta Temp
	lda #BASECOLOR
	and #$F0
	ora Temp
	sta Temp+1
	
	;--set up score pointers
	
	ldx #10
SetupScorePtrsLoop
	txa
	lsr
	lsr
	tay
	lda Score,Y
	pha
	and #$0F
	tay
	lda DigitDataLo,Y
	sta ScorePtr,X
	pla
	lsr
	lsr
	lsr
	lsr
	tay
	lda DigitDataLo,Y
	sta ScorePtr-2,X
	lda #>DigitData
	sta ScorePtr+1,X
	sta ScorePtr-1,X
	dex
	dex
	dex
	dex
	bpl SetupScorePtrsLoop
	
	
	
	rts
	
;****************************************************************************


IsBlockAtPosition		;position in Temp (x), Temp+1 (y)
	;--returns result in Temp
	;--special case for first row which is mostly open (except for the base, but we'll deal with that later)
	;--except now first row has walls at two positions (maybe!) only
		
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
	;--special case: if Temp+1 < BLOCKHEIGHT then check in a different way
	sec
	sbc #BLOCKHEIGHT
	bcs NotOnBottomRow
	;--on bottom row
	;--only two blocks on bottom row
	;	at X positions (LastRowL) 64-72 and (LastRowR) 88-96
	lda LastRowL
	beq LastRowNoWallOnLeft
	lda Temp
	cmp #73
	bcs LastRowCheckRightBlock
	cmp #64
	bcs FoundWhetherBlockExists
LastRowNoWallOnLeft
LastRowCheckRightBlock
	lda LastRowR
	beq FoundWhetherBlockExists
	lda Temp
	cmp #96
	bcs LastRowNoHit
	cmp #88
	bcs FoundWhetherBlockExists
LastRowNoHit
	lda #0
	beq FoundWhetherBlockExists
NotOnBottomRow	
	;--divide by blockheight
	;maximum Y value is 77 (TANKAREAHEIGHT=77)
	;	we subtract BLOCKHEIGHT above to see if we are on the bottom row
	;	so maximum once we are here is 70
	;	BLOCKHEIGHT = 7
	; 	so maximum loops is 10


;	ldx #0
;	sec
;DivideLoop2
;	sbc #BLOCKHEIGHT		;--this is division by 7
;	bcc DoneDividing
;	inx
;	bcs DivideLoop2	;branch always		
	;		each loop is 8 cycles, last is 9
	;			so max cycles is 65
	;			don't think this can be improved much
;DoneDividing

	;got this here: https://forums.nesdev.org/viewtopic.php?f=2&t=11336
	;	post gives credit to December '84 Apple Assembly Line
	;--constant cycle count divide by 7 !!!!
	;	if BLOCKHEIGHT is changed from 7, this routine will need to be updated
	sta Temp
	lsr
	lsr
	lsr
	adc Temp
	ror
	lsr
	lsr
	adc Temp
	ror
	lsr
	lsr
	tax

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
	cpx #0
	bne NoWallL
	;--this check is only for player 0
	lda TankX
	sec
	sbc #1
	sta Temp
	lda TankY
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
	;jmp CheckVerticalMovement
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
	;--this extra check is only for player 0
	cpx #0
	bne NoWallR
	lda TankX
	clc
	adc #8
	sta Temp
	lda TankY
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
	;--this is only for player 0
	cpx #0
	bne NoWallU
	;--temp+1 is unchanged
	lda TankX
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
	;jmp DoneWithMovementChecks
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
	;--following check only for player 0
	cpx #0
	bne NoWallD
	;--temp+1 is unchanged
	lda TankX
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
	;jmp DoneWithMovementChecks
NotAtBottomEdge	
NoWallD	
NotMovingDown

DoneWithMovementChecks
	tya		;get allowed directions back into A

	rts





;----------------------------------------------------------------------------
;-------------------------Data Below-----------------------------------------
;----------------------------------------------------------------------------
	

	
	
	
;	align 256


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
    
    
DigitDataMissile
MissileZero
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)


MissileOne
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTTWO|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTONE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(DOUBLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)				
	.byte RIGHTTWO|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)







MissileTwo
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTTHREE|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTTHREE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)


MissileThree
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTTHREE|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTONE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(DOUBLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTTHREE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)


MissileFour
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTTWO|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(DOUBLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTTWO|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)

MissileFive
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTTHREE|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTTHREE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)



MissileSix
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)



MissileSeven
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTTHREE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)

MissileEight
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTONE|(DOUBLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)

MissileNine    
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
    .byte RIGHTTHREE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
   	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)


;DigitA
;       .byte #%01000011;--
;       .byte #%01000011;--
;       .byte #%01000011;--
;       .byte #%01111111;--
;       .byte #%01000010;--
;       .byte #%01000010;--
;       .byte #%01111110;--
;DigitB
;       .byte #%01111111;--
;       .byte #%01000011;--
;       .byte #%01000011;--
;       .byte #%01111111;--
;       .byte #%01000010;--
;       .byte #%01000010;--
;       .byte #%01111110;--
;DigitC
;       .byte #%01111111;--
;       .byte #%01100000;--
;       .byte #%01100000;--
;       .byte #%01100000;--
;       .byte #%00100000;--
;       .byte #%00100000;--
;       .byte #%00111100;--
;DigitD
;       .byte #%01111111;--
;       .byte #%01000011;--
;       .byte #%01000011;--
;       .byte #%01000011;--
;       .byte #%01000010;--
;       .byte #%01000010;--
;       .byte #%01111110;--
;DigitE
;       .byte #%01111111;--
;       .byte #%01100000;--
;       .byte #%01100000;--
;       .byte #%01111100;--
;       .byte #%01100000;--
;       .byte #%00100000;--
;       .byte #%00111110;--
;DigitF
;       .byte #%01100000;--
;       .byte #%01100000;--
;       .byte #%01100000;--
;       .byte #%01111100;--
;       .byte #%01100000;--
;       .byte #%00100000;--
;       .byte #%00111110;--
	


	align 256
	


	
NumberOfBitsSet
	.byte 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
MovementMask
	.byte J0UP, J0DOWN, J0LEFT, J0RIGHT

	
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

	
	

	
	
	ds DATASPACEBEFOREGFX - (* & $FF)
	
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
		
TankUpFrame
	.word TankUpAnimated4, TankUpAnimated3, TankUpAnimated2, TankUpAnimated1
TankDownFrame
	.word TankDownAnimated4, TankDownAnimated3, TankDownAnimated2, TankDownAnimated1
TankRightFrame
	.word TankRightAnimated4, TankRightAnimated3, TankRightAnimated2, TankRightAnimated1

TanksRemainingGfx
;	.byte 0
	.byte %11101110
	.byte %11101110
	.byte %11101110
	.byte %01000100		
	.byte 0
	.byte %11101110
	.byte %11101110
	.byte %11101110
	.byte %01000100			
		align 256

		
BulletDirectionClear
BulletUp
	.byte	BULLETUP, BULLETUP<<2, BULLETUP<<4, BULLETUP<<6
BulletDown
	.byte	BULLETDOWN, BULLETDOWN<<2, BULLETDOWN<<4, BULLETDOWN<<6
BulletLeft
	.byte	BULLETLEFT, BULLETLEFT<<2, BULLETLEFT<<4, BULLETLEFT<<6
BulletRight
	.byte	BULLETRIGHT, BULLETRIGHT<<2, BULLETRIGHT<<4, BULLETRIGHT<<6

	
TitleGraphics
	.byte %00001100, %11001100, %00111111, %11001100	
	.byte %00001100, %11111100, %00110011, %00111100
	.byte %00001100, %11001100, %00110011, %11001100
	.byte %11111111, %11111100, %00110011, %11111100
	.byte %11000000, %00000000, %00000000, %00000000
	.byte %11000000, %00110011, %11001100, %00111111
	.byte %11000000, %00110011, %11001100, %00110011
	.byte %11001100, %00110011, %11001100, %00110011
	.byte %11001100, %00110011, %11111100, %00111111
	.byte %11111111, %00000011, %00000000, %00000000
TitleGraphicsEnd				


DigitDataLo
	.byte <Zero,<One,<Two,<Three,<Four,<Five,<Six,<Seven,<Eight,<Nine
	;.byte <DigitA, <DigitB, <DigitC, <DigitD, <DigitE, <DigitF
	


		ds DATASPACEBEFOREGFX - (* & $FF)
		
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

		
		
		
		
		
		
StartingTankXPosition
	.byte PLAYERSTARTINGX, ENEMY0STARTINGX, ENEMY1STARTINGX, ENEMY2STARTINGX

StartingTankYPosition 
	.byte TANKHEIGHT+1, MAZEAREAHEIGHT+TANKHEIGHT+4, MAZEAREAHEIGHT+TANKHEIGHT+4, MAZEAREAHEIGHT+TANKHEIGHT+4	

	
StartingTankStatus
	.byte  TANKSPEED4, 7, 7, 7

	
RotationOdd
	.byte 0, 2, 1, 3
RotationEven
	.byte 2, 1, 3, 0

RotationTables
	.word RotationEven, RotationOdd	
		
	;tank 0 = player, so two more wasted bytes here
	;tank 1 target = player position
	;tank 2 target = random position
	;tank 3 target = player position
TankTargetX = *-1
	.byte TankX, RandomNumber, TankX
TankTargetY = *-1
	.byte TankY, RandomNumber, TankY
	
	;--following is combined with target above (see routine for details)
	;tank 0 = player, so two more wasted bytes here
	;tank 1 target = player position
	;tank 2 target = itself
	;tank 3 target = tank 1
TankTargetAdjustX = *-1
	.byte TankX, TankX+1, TankX+1
TankTargetAdjustY = *-1
	.byte TankY, TankY+1, TankY+1


BulletDirectionMask
	.byte %11, %11<<2, %11<<4, %11<<6

	align 256

	ds BLOCKHEIGHT+1
BlockRowTable
DigitBlank
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
	


DigitDataMissileLo
	.byte <MissileZero, <MissileOne, <MissileTwo, <MissileThree, <MissileFour
	.byte <MissileFive, <MissileSix, <MissileSeven, <MissileEight, <MissileNine
		
TanksRemainingPF1Mask
	.byte $FF,$7F,$3F,$1F,$0F,$07,$03,$01,$00,$00,$00

TanksRemainingPF2Mask		
	.byte $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3E,$3C


TankDirection
	.byte TANKLEFT, TANKRIGHT, TANKDOWN, TANKUP

	
NewTankSpeed = *-1	;--don't use this for player tank, so don't need initial byte
	.byte TANKSPEED7, TANKSPEED13, TANKSPEED3
	
	
PreventReverses = *-1	;--the ZEROES are wasted bytes
	.byte 	J0DOWN, J0UP, 0, J0RIGHT, 0, 0, 0, J0LEFT
	
	;tank 0 = player, so two wasted bytes here
	;tank 1 target = upper left corner
	;tank 2 target = base (bottom center)
	;tank 3 target = upper right corner
SwitchMovementX = *-1
	.byte 0, 0, 255
SwitchMovementY = *-1
	.byte 255, 80, 255
	
	
			
    echo "----", ($10000-*), " bytes left (ROM)"


    org $FFFC
	.word Start
	.word Start



