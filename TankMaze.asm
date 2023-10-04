;-------------------------------------------------------------------------
;---------Tank Battalion Atari 2600 port----------------------------------
;-------------------------------------------------------------------------
;	Bob Montgomery
;	(c) 2012-2023
;
;	VERY loose port of Tank Battalion
;		objective: destroy enemy tanks while protecting base in a destroyable maze
;		arcade game - play for high score
;			earn points by destroying tanks and advancing levels
;		have finite # of tanks to destroy each level 
;		once destroyed, level is won and advance to new level
;		destroy walls by shooting them
;		enemy tanks also shoot and also can destroy walls
;		enemy tanks shoot you, when you are shot, lose one life
;		if enemy tanks shoot base, game over (or?)
;		open question: what about tank:tank collisions?
;
;	To do:
;	Probably need to go to 8K.  Have about 1/2 a page left of ROM, and
;		could free up some space by going through things with a fine-toothed comb, but ...
;		it would be a big stretch even to get the bare minimum for a game in there.
;		anything extra, like music, or a fancier title screen, or a more-complicated enemy-tank AI routine
;		is going to require more space.  
;	Biggest thing at this point is figure out ramping difficulty
;		IDEAS:
;			faster tanks at higher levels  DONE
;			more frequent shooting by enemy tanks DONE
;			more aggressive tank movement routines IN PROGRESS
;			smarter shooting by enemy tanks HMM
;

;	Figure out better AI for tank movement/firing routines 
;	Add explosion graphics
;	Title/splash screen?  STARTED... still needs work.
;	Power-Ups - ???
;		keep this simple: 
;			speed
;			extra life
;			"bomb" that destroys all tanks on screen (and all walls?)
;			something that slows all enemies down?
;			other?
;
;	Other TODOs:
;       fix scanline count
;		tank:tank collisions	
;		OR NOT?  display player lives remaining
;		DONE: don't give points for enemy tank actions
;		DONE replace placeholder font
;		Graphics/colors (including changing colors (of tanks?  walls?) for different levels)
;		PAL60 version
;		2-player?  (unlikely but...)
;		Other...?
;		sound tweaking/improvements
;	
;	right now stack uses 16 bytes, so I am basically out of RAM.  Need to reduce stack usage, or find other savings.
;
;	BUG KILLING!
;		scanline count is wonky, need to tighten up various subroutines that take too long
;		FIXED: remove bullets from screen during level transitions
;	
;   Notes from ZeroPage livestream on 9/1:
;       FIXED KINDA: screen rolls are way more frequent than I thought, ugh.  Need to work on that.  Note: Seem to have stabilized it at 272 scanlines by increasing Overscan timer setting
;       if possible would like to speed up game play.
;       FIXED: update spawn points to force player to move from bottom of screen ...
;       FIXED: found bug that you can shoot tanks before they are on the screen (!)
;       need to make AI more aggressively chasing "base"
;       IN PROGRESS: probably need to finally implement the "game over" logic for when a tank gets the base	



DEBUGNOENEMYBULLETS = 0 ;enemies cannot shoot
DEBUGMAZE = 0           ;makes entire top row blank and 2nd row solid so tanks are confined up there.
DEBUGPFPRIORITY = 0     ;leaves objects with priority over the playfield so can see where enemies respawn

	processor 6502
	include vcs.h
	include macro.h

	
;-----------------------MY MACROS-----------------------------------------


;--this macro aligns at page boundary (only if needed) and echoes the bytes used
    MAC PAGEALIGN
        if <* != 0
        	echo "---", ((* & $FF00) + $100) - *, "bytes left at location", *
    	    align 256
        endif
    ENDM
	
	
;-------------------------Constants Below---------------------------------


VBLANK_TIMER = 48       ;--this jitters a bit
OVERSCAN_TIMER = 32     ;--this seems to be fine except during maze generation, on the last pass it takes too long
                        ;-- (maybe make all the cleanup stuff the very last pass instead of as part of the last pass)


TANKHEIGHT	=	7
BLOCKHEIGHT = 	7
MAZEROWS	=	11
TANKAREAHEIGHT	=	BLOCKHEIGHT * MAZEROWS
MAZEAREAHEIGHT 	= 	TANKAREAHEIGHT
DATASPACEBEFOREGFX	=	MAZEAREAHEIGHT

;--TankStatus bits (upper four are direction.  lower four are speed when moving, are "delay" when waiting to come onscreen.)
TANKRIGHT		=	J0RIGHT
TANKLEFT		=	J0LEFT
TANKDOWN		=	J0DOWN
TANKUP			=	J0UP

ENEMYTANK1DELAY	=	7;7     # of frames of delay is x 4
ENEMYTANK2DELAY	=	11;7
ENEMYTANK3DELAY	=	15;7

PLAYERRESPAWNDELAY = 15


STARTINGENEMYTANKCOUNT	=	 20


LEVELENDTANKSPEEDBOOST  =   4

TANKONSCREENLEFT    =   16
TANKONSCREENRIGHT   =   136
TANKONSCREENTOP     =   MAZEAREAHEIGHT

FIRSTCOLUMNX    =   16


PLAYERSTARTINGX	=	8
PLAYERSTARTINGY =   TANKHEIGHT+1

ENEMY01STARTINGX	=	24
ENEMY02STARTINGX	=	112
ENEMY11STARTINGX	=	64
ENEMY12STARTINGX	=	88
ENEMY21STARTINGX	=	128
ENEMY22STARTINGX	=	40



ENEMY0STARTINGX2	=	144
ENEMY1STARTINGX2	=	144
ENEMY2STARTINGX2	=	144
ENEMY0STARTINGX3	=	8
ENEMY1STARTINGX3	=	8
ENEMY2STARTINGX3	=	8

TANKOFFSCREEN	=	127

ENEMYSTARTINGYTOP = MAZEAREAHEIGHT+TANKHEIGHT
ENEMYSTARTINGYHIGHROW = 9
ENEMYSTARTINGYHIGH = (BLOCKHEIGHT * ENEMYSTARTINGYHIGHROW) + 1
ENEMYSTARTINGYHIGHMIDROW = 8
ENEMYSTARTINGYHIGHMID = (BLOCKHEIGHT * ENEMYSTARTINGYHIGHMIDROW) + 1
ENEMYSTARTINGYMIDHIGHROW = 7
ENEMYSTARTINGYMIDHIGH = (BLOCKHEIGHT * ENEMYSTARTINGYMIDHIGHROW) + 1
ENEMYSTARTINGYMIDROW = 5
ENEMYSTARTINGYMID = (BLOCKHEIGHT * ENEMYSTARTINGYMIDROW) + 1
ENEMYSTARTINGYMIDLOWROW = 4
ENEMYSTARTINGYMIDLOW = (BLOCKHEIGHT * ENEMYSTARTINGYMIDLOWROW) + 1
ENEMYSTARTINGYLOWROW = 3
ENEMYSTARTINGYLOW	=	(BLOCKHEIGHT * ENEMYSTARTINGYLOWROW) + 1

MAZEGENERATIONPASSES = MAZEROWS/2-1

PLAYERTANKSPEED	=	TANKSPEED3
ENEMYTANKBASESPEED0	=   TANKSPEED7
ENEMYTANKBASESPEED1 = 	TANKSPEED5
ENEMYTANKBASESPEED2	=	TANKSPEED3


PLAYERTANKVOLUME	=	8
BULLETTONE		=	3
BULLETFREQ		=	15

BRICKSOUND	=	0
BULLETSOUND = 1
ENEMYTANKSOUND	=	2
SHORTBRICKSOUND = 3
LONGEXPLOSIONSOUND	=	4

PLAYERTANKENGINEFREQ	=	8;31
PLAYERTANKENGINETONE	=	2;ENGINESOUND
PLAYERTANKENGINEVOLUME	=	3

BULLETSOUNDTONE		=	3
BULLETSOUNDFREQ		=	13
BULLETSOUNDLENGTH	=	12
BRICKSOUNDTONE		=	8
BRICKSOUNDFREQ		=	20
BRICKSOUNDLENGTH	=	25
ENEMYTANKSOUNDTONE	=	8
ENEMYTANKSOUNDFREQ	=	30
ENEMYTANKSOUNDLENGTH	=	50
SHORTBRICKSOUNDTONE	=	BRICKSOUNDTONE
SHORTBRICKSOUNDFREQ	=	BRICKSOUNDFREQ
SHORTBRICKSOUNDLENGTH	=	7
LONGEXPLOSIONTONE	=	ENEMYTANKSOUNDTONE
LONGEXPLOSIONFREQ	=	18
LONGEXPLOSIONLENGTH	=	200

WALLDESTRUCTIONSCORE    =   $0005
KILLTANKSCORE           =   $0100

;--GameStatus flags

GAMEOFF			=	%10000000
GENERATINGMAZE	=	%01000000
LEVELCOMPLETE	=	%00100000
TITLESCREEN		=	%00010000
GAMEOVER		=	%00001000
DRAWBASE        =   %00000100

LEVELCOMPLETETIMER	=	$03


;--end GameStatus flags

BALLOFFSCREEN	=	0


BULLETRIGHT		=	0
BULLETLEFT		=	1
BULLETDOWN		=	2
BULLETUP		=	3
BULLETCLEAR		=	3

TRIGGERDEBOUNCEVALUE = 15
TRIGGERDEBOUNCEFLAG =   %10000000
CONSOLEDEBOUNCEFLAG	=	%01000000
ENEMYDEBOUNCEBITS =     %00011111


BULLETSPEEDHOR		=		1
BULLETSPEEDVER		=		1
BULLETFRACTIONALPERCENT =   85
BULLETFRACTIONALSPEED   =   256/100*BULLETFRACTIONALPERCENT  ;slowing bullets down slightly so the collision detection works better

BASECOLOR		    =		GOLD
SCORECOLOR          =       GRAY|$C
WALLCOLOR			=		RED|$6
TANKSREMAININGCOLOR =   TURQUOISE|$A
TANKCOLOR1          =    GOLD|$A
TANKCOLOR2          =    BLUE2|$C

;--used by maze generation algorithm
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


TANKAISWITCH	=	64


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
Lives ds 1
TanksRemaining ds 1

Channel1Decay ds 1

Debounce ds 1

;EnemyDebounce ds 1

TankX ds 4
TankY ds 4
TankStatus ds 4

TankFractional ds 4

BulletX ds 4
BulletY ds 4
BulletDirection ds 1
BulletFractional ds 1

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
MiscPtr 
ScorePtr ds 6

   ; Display Remaining RAM
   echo "----",($100 - *) , "bytes left (ZP RAM)"
;---End Variables

	seg Bank0

	org $1000
	rorg $1000

Start
	sta $1FF8
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

	lda #RIGHTEIGHT
	sta HMM1
	
	ldy #0
	sta WSYNC
	lda #SCORECOLOR
	sta COLUP0
	sta COLUP1						;+8		 8
		
	
	;--display score
	lda #THREECOPIESCLOSE|LEFTONE
	sta VDELP0
	sta VDELP1						;+8		16		turn on VDELPx (bit0==1)

	sta NUSIZ0
	sta NUSIZ1						;+6		29

	SLEEP 7
	
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
	SUBROUTINE
	ldy #10		; 2
.wait
	dey			 ; 2
	bne .wait	 ; 3

	SLEEP 3
	
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
	lda #TANKCOLOR1 ;GOLD+10
	sta COLUP0
	lda #TANKCOLOR2 ;BLUE2+12
	sta COLUP1
	
	;--reflect P0 or P1 as necessary.   prep for flip loop below
	lda FrameCounter
	and #1
	asl
	tax
	lda RotationTablesBank1,X
	sta MiscPtr
	lda RotationTablesBank1+1,X
	sta MiscPtr+1
	lda #WALLCOLOR
	sta COLUPF
	;--do this above...the rest we do during the wall below.  
	ldx #4
; PositioningLoopVBLANK	;--excluding players (and M1)
	lda PlayerX,X
	jsr PositionASpriteSubroutine   ;        9
; 	dex
; 	cpx #3
; 	bne PositioningLoopVBLANK
	

	
	ldx #$C0
	lda #$FF                        ;+4     13
; 	sta WSYNC
; 	sta HMCLR
	stx PF0
	sta PF1
	sta PF2                         ;+9     22
	
	;---reflect P0/P1
	;--this loop is garbage, need to rewrite so it is faster.... though not sure how, actually.
	ldy #3
SetREFPLoop
	lda TankStatus,Y
	and #TANKLEFT
	beq EndREFPLoop     ;--only if facing left do we reflect
	lax (MiscPtr),Y		;get X index into graphics registers
	cpx #2
	bcs EndREFPLoop
	lda #$FF            ;--1 or 0 (players) get reflected, 2 and 3 (missiles) do not
	sta REFP0,X	
EndREFPLoop
	dey
	bpl SetREFPLoop

	
	sty VDELP0  ;Y is 255 following loop above
	sty VDELBL    

	iny         ;Y = 0
	sty VDELP1
	
	tsx
	stx Temp       ;save stack pointer
		

	ldx #3
PositioningLoop	;--just players
	lda PlayerX,X
	jsr PositionASpriteSubroutine
	dex
	bpl PositioningLoop     ;       13 cycles last time through
    
	sty PF1                 ; Y is zero
	sty PF2					;
	sta CXCLR               ;+9     22

	;--use stack pointer to hit ENABL
	ldx #<ENABL
	txs						;+4		26
	
	nop
    nop
    
	ldy #TANKAREAHEIGHT		;+2		30

		
	jmp BeginMainMazeKernel ;+3     33

    PAGEALIGN
	
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

    ;--don't draw base sometimes	
    lda GameStatus
    and #DRAWBASE
    beq DoNotDrawBase   ;7 if branch not taken.  8 if taken.
	
	lda #$80
	sta PF2				;+5		31
	dey
	bne KernelLastRowLoop		;+5		36
	beq DoneWithKernelLastRowLoop   ;is this necessary?
DoNotDrawBase
    sta.w PF2           ;+4
    dey
    bne KernelLastRowLoop
DoneWithKernelLastRowLoop
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
	
;     sta HMCLR
	lda #104
; 	jsr PositionASpriteNoHMOVESubroutine
    jsr PositionASpriteSubroutine	

	lda #112
	inx
; 	jsr PositionASpriteNoHMOVEOrHMCLRSubroutine
    jsr PositionASpriteSubroutine	
    
	lda #60
	inx
; 	jsr PositionASpriteNoHMOVEOrHMCLRSubroutine
    jsr PositionASpriteSubroutine	

	lda #66
	inx
; 	jsr PositionASpriteNoHMOVEOrHMCLRSubroutine
    jsr PositionASpriteSubroutine	

	
	;--done displaying maze
	;--set up score, tanks remaining, lives remaining etc

	sta WSYNC
; 	sta HMOVE
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
	
	lda #TANKSREMAININGCOLOR
	sta COLUP0
	sta COLUP1
	sta HMCLR
	

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

	lda #VBLANK_TIMER
	sta TIM64T
	
	dec FrameCounter
	
	lda GameStatus
	and #GAMEOFF|LEVELCOMPLETE
	beq GameOnVBLANK	
	and #GAMEOFF
	beq InBetweenLevels
	;--if game not on (and ONLY IF) read joystick button to start game
	lda INPT4
	bmi NoTriggerToStartGameClearTriggerDebounce
	;--trigger IS pressed, now check debounce
	lda Debounce
	and #TRIGGERDEBOUNCEFLAG
	bne TriggerNotDebouncedYet
    ;--set trigger debounce and then start new game
    lda Debounce
    ora #TRIGGERDEBOUNCEFLAG
    sta Debounce
	
	lda #0
	sta MazeNumber
	jsr StartNewGame
	jmp DoneStartingNewGameWithTrigger
NoTriggerToStartGameClearTriggerDebounce
    lda Debounce
    and #~TRIGGERDEBOUNCEFLAG
    sta Debounce
TriggerNotDebouncedYet
DoneStartingNewGameWithTrigger

	jmp GameNotOnVBLANK
GameOnVBLANK
	jsr MoveEnemyTanksSubroutine
	brk
	.word MoveBulletSubroutine
	
GameNotOnVBLANK
InBetweenLevels
	;--keep playing sound even when game not on
	jsr SoundSubroutine
	
	jsr UpdateRandomNumber
	


	jsr ReadConsoleSwitchesSubroutine

	brk
	.word KernelSetupSubroutine


WaitForVblankEnd
	lda INTIM
; 	bpl WaitForVblankEnd
	bmi OverTimeVBLANK
	cmp #1
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
	lda #OVERSCAN_TIMER
	sta TIM64T
	
	
	lda GameStatus
	and #GAMEOFF|LEVELCOMPLETE|GENERATINGMAZE
	bne GameNotOn	
	brk
	.word CollisionsSubroutine
	;--need to recheck, since a collision could have modified these flags
	lda GameStatus
	and #GAMEOFF|LEVELCOMPLETE|GENERATINGMAZE
	bne GameNotOn	
	jsr PlayerTankMovementRoutine


	jmp WaitForOverscanEnd
GameNotOn
	;--A still holds GameStatus AND #GAMEOFF|LEVELCOMPLETE|GENERATINGMAZE
	and #LEVELCOMPLETE
	beq .LevelNotComplete
	;--level complete
	lda FrameCounter
	and #$F
	bne .LevelNotComplete
	lda GameStatus
	and #LEVELCOMPLETETIMER
	tax
	dex
	bmi LevelCompleteNow
	lda GameStatus
	and #~LEVELCOMPLETETIMER
	sta GameStatus
	txa
	ora GameStatus
	sta GameStatus
	bne .LevelNotComplete
	
LevelCompleteNow
	lda GameStatus
	and #~(LEVELCOMPLETE|LEVELCOMPLETETIMER)
	sta GameStatus
	jsr StartNewLevel
.LevelNotComplete
	


	lda GameStatus
	and #GENERATINGMAZE
	beq NotGeneratingMaze
	lda FrameCounter
	and #7
	bne NotGeneratingMaze
	brk
	.word GenerateMazeSubroutine
NotGeneratingMaze
	lda GameStatus
	and #TITLESCREEN
	beq NotOnTitleScreen
	;--on title screen
	lda MazeGenerationPass
	bmi GameNotOnVBLANK
	lda FrameCounter
	and #$7
	bne WaitToDrawTitleScreen
	jsr DrawTitleScreen
WaitToDrawTitleScreen
NotOnTitleScreen


WaitForOverscanEnd
	lda INTIM
; 	bpl WaitForOverscanEnd
	bmi OverTimeOverscan
	cmp #1
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


DrawTitleScreen

	;--TODO: interleave the gfx data so this loop can be cleaned up.... maybe.
	ldx MazeGenerationPass
PutTitleGraphicsInPlayfieldLoop
	txa
	and #3
	asl
	tay
	lda PFPointer,Y
	sta MiscPtr
	lda PFPointer+1,Y
	sta MiscPtr+1
	
	txa
	cmp #((TitleGraphicsEnd-TitleGraphics-1)/2)+(TitleGraphicsEnd-TitleGraphics-1)/4)
	bcc DrawTitleScreenModerately
	;--else slower
	lda FrameCounter
	and #$1F
	bne SkipDrawingTitleScreenThisFrame
DrawTitleScreenModerately	
	cmp #((TitleGraphicsEnd-TitleGraphics-1)/2)
	bcc DrawTitleScreenFast
	lda FrameCounter
	and #$F
	bne SkipDrawingTitleScreenThisFrame
DrawTitleScreenFast
	txa
	lsr
	lsr
	tay
	lda TitleGraphics,X
	
	sta (MiscPtr),Y

	
	dex
	stx MazeGenerationPass
	bmi DoneDrawingTitleScreen
	tay	;set flags based on what we wrote
	beq PutTitleGraphicsInPlayfieldLoop	;--this only works if last value is NOT zero
DoneDrawingTitleScreen
	;--play sound
	txa	;reset flags
	bmi PlayLongSound
	ldy #SHORTBRICKSOUND
	bne PlayShortSound      ;branch always
PlayLongSound
	lda #%11111100
	sta LastRowL
	sta LastRowR	
	lda GameStatus
	ora #DRAWBASE
	sta GameStatus
	ldy #LONGEXPLOSIONSOUND
PlayShortSound
	jsr StartSoundSubroutine

SkipDrawingTitleScreenThisFrame	
	
	rts

	
PFPointer
	.word PF1Left, PF2Left, PF2Right, PF1Right
	
	
;----------------------------------------------------------------------------


ReadConsoleSwitchesSubroutine

	lda SWCHB
	lsr				;get RESET into carry
	bcc RESETPressed
	lsr
	bcc SELECTPressed
	;--switch not pressed, so reset debounce flag
	lda Debounce
	and #~CONSOLEDEBOUNCEFLAG
	sta Debounce
RESETNotReleased
DoneWithConsoleSwitches
	rts

 
	
RESETPressed
	lda Debounce
	and #CONSOLEDEBOUNCEFLAG
	bne RESETNotReleased
    lda #0
    sta MazeNumber
SELECTPressed
	lda Debounce
	and #CONSOLEDEBOUNCEFLAG
	bne RESETNotReleased
	lda Debounce
	ora #CONSOLEDEBOUNCEFLAG
	sta Debounce
	
StartNewGame	
	;--reset score here 
	lda #0
	ldx #3
	bne NewGameZeroLoopEntryPoint
NewGameZeroLoop
	sta Score,X
NewGameZeroLoopEntryPoint
    sta BulletX,X
    sta BulletY,X
    dex
    bpl NewGameZeroLoop
	
StartNewLevel
	;--start game
	lda GameStatus
	and #GENERATINGMAZE
	bne AlreadyStartingNewLevel
	lda GameStatus
	ora #GENERATINGMAZE	;--start generating maze
	and #~TITLESCREEN	;--turn off title screen
	sta GameStatus
	
	;--move tanks off screen, immobilize 
	ldx #3
	lda #TANKOFFSCREEN
MoveTanksOffscreenLoop
	sta TankX,X
	sta TankY,X
 	sty TankStatus,X    ;--what does Y hold here?
	dex
	bpl MoveTanksOffscreenLoop
	
	stx MazeGenerationPass
AlreadyStartingNewLevel	
	;--finally, cycle the random number
	jmp UpdateRandomNumber				;--return from subroutine there
	


	
;*******************************************************************

InitialSetupSubroutine

	lda #TANKOFFSCREEN
	ldx #3
SetUpTankInitialValues
	sta TankX,X
	sta TankY,X
	dex
	bpl SetUpTankInitialValues
	
	sta RandomNumber
	
	lda #GAMEOFF|TITLESCREEN
	sta GameStatus

	lda #PLAYERTANKENGINETONE
	sta AUDC0
	
	lda #PLAYERTANKENGINEFREQ
	sta AUDF0
	
	lda #WALLCOLOR
	sta COLUPF
	
	lda #REFLECTEDPF|DOUBLEWIDTHBALL|PRIORITYPF
	if DEBUGPFPRIORITY = 1
	    and #~PRIORITYPF
	endif    
	sta CTRLPF
	
	lda #TitleGraphicsEnd-TitleGraphics-1
	sta MazeGenerationPass
	
	
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

FireEnemyBulletRoutine

    if DEBUGNOENEMYBULLETS = 1

    rts ;eliminate enemy firing for now

    endif    
    
; 	;--- only every ... variable # of frames
	lda Debounce    ;was EnemyDebounce
	and #ENEMYDEBOUNCEBITS
	beq FireEnemyBullet
	dec Debounce    ;was EnemyDebounce
	
	rts					;--and we're done
FireEnemyBullet
	ldx #1
FindAvailableEnemyBallLoop
	lda BulletX+2,X             ;enemy bullets are indexed at 2 & 3 (player bullets are indexed at 0 and 1)
; 	cmp #BALLOFFSCREEN          ;<--as of 9/27 this value is zero so we can skip the comparison
	beq FoundAvailableEnemyBall
	dex
	bpl FindAvailableEnemyBallLoop
	;bmi NoAvailableEnemyBalls  ;<--remove since we just exit immediately.  save 3 cycles (and 1 byte)
	rts
FoundAvailableEnemyBall
	inx
	inx
	;--shoot more often as levels increase
	ldy MazeNumber
    cpy #16
    bcc MazeNumberSixteenOrLess
    ldy #16
MazeNumberSixteenOrLess    
	lda Debounce    ;was EnemyDebounce
	and #~ENEMYDEBOUNCEBITS ;this probably isn't necessary, but being safe for now
    ora EnemyBulletDebounce-1,Y ;minus one because maze number starts at 1 (not zero)
	sta Debounce    ;was EnemyDebounce
	;--find random tank 
	lda RandomNumber
	and #3
	tay
	bne ShootFromTank   ;zero is player, so if non-zero we are fine, otherwise shoot from tank 1
	iny				;this routine shoots from tank 1 half the time and tanks 2 and 3 a quarter of the time each
ShootFromTank
    ;--if tank offscreen, don't shoot from it
	lda TankStatus,Y
	and #$F0
	beq TankOffscreenCannotShoot
	jsr FireBulletRoutine
TankOffscreenCannotShoot
NoAvailableEnemyBalls
	;--then we're done
	rts	
	
;****************************************************************************

EnemyBulletDebounce ;these values * 4 is number of frames between enemy bullet firing
    .byte 30, 28, 25, 22
    .byte 20, 19, 18, 17
    .byte 15, 13, 12, 11
    .byte 10, 7, 2, 1	

    
;****************************************************************************

TanksRemainingSpeedBoost ;--just realized the first three values in this table have no effect.  not sure
                        ;   yet if I want to shift the values or not.
    .byte 15, 10, 8, 6, 4, 2, 1, 0, 0, 0
    .byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0    
    	
SetInitialEnemyTankSpeedRoutine
    ;--come in here with X pointing to tank # (1-3)
    ;--and A holding new direction (in upper four bits)
	sta TankStatus,X	;--first write new direction
	;--tank starting speed and add level, capped at 15-tank # (0, 1, 2)
	lda NewTankSpeed,X
	clc
	adc MazeNumber
	;--increase speed depending on tanks remaining
;	sta Temp
; 	lda Temp
    ldy TanksRemaining
    adc TanksRemainingSpeedBoost,Y
	cmp #15
	bcc NewSpeedNotTooHigh
	lda #TANKSPEED15
	bne SetNewEnemyTankSpeed
	
NewSpeedNotTooHigh
	sta Temp
	;convert X's range (1-3) to (0-2)
	txa
	sec
	sbc #1
	;make X negative
	eor #$FF
	clc
	adc #1
	;add -(X-1) to the new speed and set.
	clc
	adc Temp
SetNewEnemyTankSpeed
	ora TankStatus,X
	sta TankStatus,X
	
	rts
	
	
;****************************************************************************
	
	


	
;****************************************************************************

MoveEnemyTanksSubroutine

	;	can't move all 3 enemies every frame, takes too long.
	;	So instead move one per frame (moving none every 4th frame - this is when we fire a bullet)
	;	Works well enough for now, work on optimizing this routine later

	lda FrameCounter
	and #3
	bne MoveAnEnemyTank
	dec TankMovementCounter     ;tank movement counter decremented every four frames
	;--if we ain't moving a tank, let's shoot a bullet
	jmp FireEnemyBulletRoutine

MoveAnEnemyTank
	tax

	lda TankStatus,X
	and #$F0
	bne TankOnscreenMoveIt
	;tank offscreen
	;--only bring tank onscreen if there are enemy tanks remaining
	;--how many tanks onscreen?
	;lda #0  ;---A is zero already following BNE above
	sta Temp    ;using this to count how many tanks onscreen
	ldy #3
CountTanksOnScreenLoop
	lda TankStatus,Y
	and #$F0    ;--if tank has no direction set, it is offscreen and waiting to be respawned, and lower 4 bits hold wait counter until it is brought back onscreen
	beq TankNotOnscreen
	inc Temp
TankNotOnscreen
	dey
	bne CountTanksOnScreenLoop
	;--now compare with tanks remaining	
	lda Temp
	cmp TanksRemaining
	bcs DoNotBringTankOnscreenYet
	lda TankMovementCounter         ;tank movement counter .... this has effect of multiplying the delay (ranging from 4-60 frames) by 4.
	and #3
	bne DoNotBringTankOnscreenYet
	dec TankStatus,X
	bpl DoNotBringTankOnscreenYet
	; if tank off left edge of screen, move right.  if off right edge of screen, move left.  otherwise, move down
	lda TankX,X
	cmp #16
	bpl TankNotOffLeftEdge
	lda #J0RIGHT
	bne SetInitialEnemyTankSpeed
TankNotOffLeftEdge
	cmp #140
	bmi TankNotOffRightEdge
	lda #J0LEFT
	bne SetInitialEnemyTankSpeed	;branch always
TankNotOffRightEdge
	lda #J0DOWN

SetInitialEnemyTankSpeed
	jsr SetInitialEnemyTankSpeedRoutine
	
	;--set tank fractional so it moves immediately and doesn't turn before it gets onscreen
 	lda #255
 	sta TankFractional,X
DoNotBringTankOnscreenYet
	rts

PlayerTankMovementRoutine
	;--if tank off left edge of screen, we don't read joystick, we just wait (??) and then move it on to the maze from the left
	lda TankX
	cmp #16
	bcs PlayerTankOnscreenMoveIt
	;--wait only if TankX == 8
	cmp #8
	bne DoneWaitingBringPlayerTankOnScreen
	lda TankStatus
	and #$0F
	beq AllFinishedWaitingStartPlayerRespawn
	;--else still waiting
	;--update counter every 8 frames
	lda FrameCounter
	and #7
	bne WaitToUpdateRespawnCounter
	dec TankStatus
WaitToUpdateRespawnCounter
	rts
AllFinishedWaitingStartPlayerRespawn
	;--set player to move immediately and reset speed correctly
	lda #255
	sta TankFractional
	lda #PLAYERTANKSPEED
	ora TankStatus
	sta TankStatus
DoneWaitingBringPlayerTankOnScreen
 	lda #0		
	pha			;movement flag
	lda #~J0RIGHT
	jmp SkipReadingControllers
PlayerTankOnscreenMoveIt
    ldx #0	
TankOnscreenMoveIt
	;--plan for now:  every time enemy tank hits intersection, change direction
	;push X, Y target for tank onto stack and then call ChooseTankDirectionSubroutine
	;--intersection means X mod 8 = 0 and Y mod 7 = 0
	lda TankX,X
	and #$07
	beq AtIntersectionX
	jmp NotAtIntersection
AtIntersectionX
	lda TankY,X
	sec
	sbc #1
	
	
	;--could use giant table instead.... 
	;got this here: https://forums.nesdev.org/viewtopic.php?f=2&t=11336
	;	post gives credit to December '84 Apple Assembly Line
	;--constant cycle count divide by 7 !!!!
	;	if BLOCKHEIGHT is changed from 7, this routine will need to be updated
	sta Temp+1 ;save for below --- necessary?  yes.
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
	
	;--need modulo BLOCKHEIGHT (=7)
	;	so now multiply by 7 
    ;       new routine x * 7 = x * 4 + x + x + x
    sta Temp
    asl
    asl
    adc Temp
    adc Temp
    adc Temp
    sta Temp    ;+19 uses one byte RAM
	;--now subtract from original number (all we care is if modulo = 0 or not)
	sec
	sbc Temp+1
	beq AtIntersectionY
	jmp NotAtIntersection
AtIntersectionY	
	;--here is where we choose which way each tank will go

	;--if player tank, read joystick
	txa             ;quick test to see if X = 0 (i.e., player tank we are moving)
	bne EnemyTankDirectionRoutine
	jmp ReadControllersSubroutine
EnemyTankDirectionRoutine
	;--new thought.  if tank is turning, wait until it is actually going to move (fractional overflow) before calculating new direction.
	;--should fix reversals and also should prevent tanks from turning through solid barriers (oops)
	lda TankStatus,X
	asl
	asl
	asl
	asl
	ora #$0F
	clc
	adc TankFractional,X
	bcs TimeForEnemyTankToTurn
	sta TankFractional,X
	rts
TimeForEnemyTankToTurn
	
	;--check if tank is offscreen (respawned and waiting to move)
	lda TankY,X
	cmp #MAZEAREAHEIGHT+2
	bcc TankNotAboveMaze
	lda #J0DOWN
	bne SetNewTankDirection	;branch always
TankNotAboveMaze	
	lda TankX,X
	cmp #FIRSTCOLUMNX
	bcs TankNotLeftOfMaze
	lda #J0RIGHT
	bne SetNewTankDirection	;branch always
TankNotLeftOfMaze
	cmp #ENEMY2STARTINGX2-1
	bcc TankInMaze
	lda #J0LEFT
	bne SetNewTankDirection
TankInMaze


    ;--overall routine:
    ;   get allowable directions.
    ;       if there is only one, turn in that direction.
    ;       if there is more than one allowable direction:
    ;           save those directions, then push a target (X and Y) onto the stack, and then turn towards the direction that best goes to the target.
	lda #0      ;#(J0LEFT|J0RIGHT|J0UP|J0DOWN)  = all directions
	jsr CheckForWallSubroutine	;--returns with allowable directions in A
	jsr CheckForEnemyTankSubroutine ;--returns with allowable directions in A
	
	and #$F0	;clear bottom nibble
	eor #$F0
	
	;--if single direction, then move that direction
	pha			;--save direction
	bne ThereAreSomeAllowedDirections ;if no allowed directions, skip all the nonsense below
	jmp NoAllowedDirections
  	
ThereAreSomeAllowedDirections
	lsr
	lsr
	lsr
	lsr			;--get in bottom nibble
	tay
	lda NumberOfBitsSet,Y   ;+14 cycles
	;--if can only move a single direction, go that direction
	cmp #1
	bne MoreThanOneAllowedDirection
	pla			;get direction back off stack
SetNewTankDirection
	sta Temp
	lda TankStatus,X
	and #~(J0UP|J0DOWN|J0LEFT|J0RIGHT)
	ora Temp
	sta TankStatus,X
	jmp DirectionChosen
MoreThanOneAllowedDirection
;----cannot use this any longer, since directions can be eliminated by the presence of another tank, so the two directions may *not* include reversing back where the tank came from
; 	;--if can only move in two directions, go whichever ISN'T back the way we came from
; 	cmp #2
; 	bne MoreThanTwoAllowedDirections
; 	;--find current direction, clear it's opposite, and then move the one remaining direction
; 	lda TankStatus,X
; 	lsr
; 	lsr
; 	lsr
; 	lsr			;index into table with opposite directions
; 	tay
; 	pla
; 	and PreventReverses,Y
; 	sta Temp
; 	lda TankStatus,X
; 	and #~(J0UP|J0DOWN|J0LEFT|J0RIGHT)
; 	ora Temp
; 	sta TankStatus,X
; 	jmp DirectionChosen

			
	
MoreThanTwoAllowedDirections	
	;--find current direction, clear it's opposite, and then save the remaining allowable directions
	lda TankStatus,X
	lsr
	lsr
	lsr
	lsr			;index into table with opposite directions
	tay
	pla
	and PreventReverses,Y
	pha                         ;25-25 cycles


	
	;--TANK AI ROUTINE ... such as it is
	;--if TankMovementCounter < TANKAISWITCH, then move tanks towards static location(s), otherwise, follow regular pattern
	;--TankMovementCounter updates every 4 frames, so goes from 0 to 255 every ... 16-ish seconds (1024 frames)
	;--at fastest tank speed it is moving ....a pixel every 4 frames.  At slowest tank speed a pixel every 32 frames.
	;--and each block is approx 8x8, so - at fastest speed, tanks will choose a new direction every 32 frames, at slowest speed will
	;       choose a new direction every 256 frames
	lda TankMovementCounter
	cmp #TANKAISWITCH
	bcs RegularMovement
	; move to static location
	lda SwitchMovementX,X
	pha
	lda SwitchMovementY,X
	pha
	bcc ChooseTankDirection ;branch always
RegularMovement
	;routine: 
	; pick two targets, then take the midpoint between them.
	;   each target is a ZP variable.  Have to update random number in between in case both targets are random numbers (like in the case of tank 3)
	ldy TankTargetX,X
	lda $00,Y
	ldy TankTargetAdjustX,X
	sec
	sbc $00,Y
	jsr DivideByTwoSubroutine   ;can't just LSR since we need to account for negative numbers
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

ChooseTankDirection
    ;have allowable directions in stack and target X and Y values in stack
	jsr ChooseTankDirectionSubroutine	;returns with new direction in accumulator
	sta Temp
	lda TankStatus,X
	and #~(J0UP|J0DOWN|J0LEFT|J0RIGHT)
	ora Temp
	sta TankStatus,X					
	pla
	pla									;pull target for tank off stack and discard

	;--now compare to allowable directions
	;   if desired direction is one of the allowable directions, then go that way.
	;       if not, pick random direction from allowable and move that way.
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
	bne TryAnotherDirection	;branch always -- this assumes we will always find a good direction
FoundGoodDirection
NoAllowedDirections
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

SoundSubroutine
	;--no sound for enemy tanks; too annoying
	
	;  tentative:
	;	channel 0 = player tank movement
	;	channel 1 = all shots and explosions (no shot sound when an explosion is happening)
	
	lda Channel1Decay
 	cmp #$07
 	bcc SetChannel1Volume
 	lda #$07
SetChannel1Volume
	sta AUDV1
	lda Channel1Decay
	beq DoNotUpdateChannel1Decay
	dec Channel1Decay
DoNotUpdateChannel1Decay	
	rts

	
;****************************************************************************

    ;--this is to make sure the "DivideLoop" doesn't cross a page boundary.
    if ((* + 5) & $FF00) != ((* + 8) & $FF00)
        echo $FF - ((* + 4) & $FF), "bytes left at location", *
        ds $FF - ((* + 4) & $FF)
    endif

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
	rts                 ;+9      9

	
PositionASpriteNoHMOVESubroutine
	sta HMCLR
PositionASpriteNoHMOVEOrHMCLRSubroutine
    sec
	sta WSYNC
DivideLoop2			;				this loop can't cross a page boundary!!!
	sbc #15
	bcs DivideLoop2	;+4		 4
	eor #7
	asl
	asl
	asl
	asl				;+10	14
	sta.wx HMP0,X	;+5		19
	sta RESP0,X		;+4		23
; 	sta WSYNC
; 	sta HMOVE
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
	lda $02,X				;get allowed directions off of stack
	
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
	txa ;set zero flag based on value in X (zero = player tank, non-zero = enemy tanks)
	bne EnemyTankEliminateDiagonal
    ;--player tank.  routine here basically tries to pick which direction to move in.
    ;   routine is basically, if we were moving horizontally, have tank move vertically, and vice versa.	
CompleteChangeOfDirection
; 	ldx #0      ;X is already zero
	lda TankStatus
	and #J0LEFT|J0RIGHT
	beq EliminateUpDown
	;--else eliminate left and right
	pla
	ora #J0LEFT|J0RIGHT
; 	bne DirectionsFine
	rts
EliminateUpDown
	pla						;restore directions 
	ora #J0UP|J0DOWN		;eliminate up and down
; 	bne DirectionsFine		;branch always
    rts
EnemyTankEliminateDiagonal
	lda RandomNumber
	lsr				;random bit
	pla ;get desired directions back into accumulator
	bcs EliminateHorizontal
	;--else eliminate vertial
	ora #J0RIGHT|J0LEFT
; 	bne DirectionsFine
    rts
EliminateHorizontal
	ora #J0UP|J0DOWN
DirectionsFine

	rts

;****************************************************************************

TankFractionalAddition	
	lda TankStatus,X
	asl
	asl
	asl
	asl
	ora #$0F
    clc
    adc TankFractional,X
	sta TankFractional,X
    rts
	
	
;****************************************************************************

ReadControllersSubroutine

	lda #0
	pha			;movement flag
	
	lda SWCHA	;A holds joystick
	and #$F0	;clear bottom nibble
	cmp #$F0
    bne TryingToMove
    ;--not trying to move, so just ... exit?
    ;--also clear TankStatus bottom nibble so we know it isn't trying to move.
    lda TankStatus
    and #$F0
    sta TankStatus
    ;--stop engine sound
    pla ;--pop movement flag (=0) off stack
    sta AUDV0
    rts
TryingToMove
SkipReadingControllers
	ldx #PLAYERTANKENGINEVOLUME
; NotTryingToMove
	stx AUDV0	
	ldx #0		;index into which tank	


	sta Temp			;--save desired direction of tank
    ;--and set player speed which we set to zero when he stopped
    lda TankStatus
    and #$F0
    ora #PLAYERTANKSPEED
    sta TankStatus 
    
	lda Temp
	pha
	
	jsr CheckForWallSubroutine		;--returns with allowed directions in A
	.byte $24	;--skip next byte
TankMovementSubroutine				;jump here when moving enemy tanks
	pha			;--save desired direction of tank (skipped when player tank)
	
	;--if no directions are allowed, but tank is trying to move, turn tank but do not move.
	and #$F0	;clear bottom nibble
	cmp #$F0	;no movement = $F0
	bne MovementAllowed
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
; 	jsr EliminateDiagonalSubroutine		;tanks cannot move diagonally  is this necessary?

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
	jsr TankFractionalAddition
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
	jsr TankFractionalAddition
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
	jsr TankFractionalAddition
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
	jsr TankFractionalAddition
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

	;--don't allow player to fire when offscreen
	lda TankX
	cmp #16
	bcs PlayerOnScreenCanShoot
	jmp NotFiring
PlayerOnScreenCanShoot
	;are any balls available for firing
	ldx #1
FindAvailableBallLoop
	lda BulletX,X
	;cmp #BALLOFFSCREEN
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
	and #~TRIGGERDEBOUNCEFLAG
	sta Debounce
	jmp NotFiring
TriggerHit
	lda Debounce
	and #TRIGGERDEBOUNCEFLAG
	beq TriggerDebounced
	jmp TriggerNotDebounced
	
TriggerDebounced
	;--bullet is fired!

	;--bullet sound
	;--if explosion sound in early stages, do not make bullet sound
	lda Channel1Decay
	and #$F8
	bne NoBulletSound
	ldy #BULLETSOUND
	jsr StartSoundSubroutine
	
NoBulletSound
	
	;set trigger debounce
	lda Debounce
	ora #TRIGGERDEBOUNCEFLAG
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
OnBottomRow
	;--on bottom row
	;--only two blocks on bottom row
	;	at X positions (LastRowL) 64-71 and (LastRowR) 88-95
	lda LastRowL
	beq LastRowNoWallOnLeft
	lda Temp
	cmp #72
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
	

StartSoundSubroutine
	
	lda Channel1Decay
	and #$F0
	bne DoNotStartSound
	lda Tone,Y
	sta AUDC1
	lda Frequency,Y
	sta AUDF1
	lda SoundLength,Y
	sta Channel1Decay
	
DoNotStartSound
	rts

	
;****************************************************************************

SUBROUTINE

CheckForEnemyTankSubroutine
    ;--come in with X pointing at current tank
    ;   and A holds direction (mask against joystick constants)
    ;   return A holding allowed directions
    
    ;TODO: ignore tanks that are off screen

    stx Temp+1  ;--save index into current tank
    sta Temp ;save directions
    and #J0LEFT
    bne .NotMovingLeft
    ;--moving left.
    ldy #3
.CheckForEnemyTankLeftLoop
    cpy Temp+1  ;--don't bother comparing tank to itself
    beq .NoCompareTankToItselfLeft
    ;--see if enemy tank is offscreen
    lda TankStatus,Y
    and #$F0
    beq .TankOffScreenIgnoreLeft
    ;see if an enemy tank within 1.5 blocks (12 pix) to the left
    lda TankX,X
    clc
    adc #1      ;--have to add one so equal values are ignored
    sec
    sbc TankX,Y
    cmp #14
    bcs .ThisTankNotToLeft
    ;if so, see if that tank is between 1.5 blocks up and 1.5 blocks down 
    lda TankY,X
    clc
    adc #10
    sec
    sbc TankY,Y
    cmp #20
    bcs .ThisTankNotToLeft
    ;--there is a tank there!  can't move left!
    lda Temp
    ora #J0LEFT
    sta Temp
    bne .CheckMovingRight   ;branch always to the next check
.ThisTankNotToLeft    
.NoCompareTankToItselfLeft
.TankOffScreenIgnoreLeft
    dey
    bne .CheckForEnemyTankLeftLoop
.NotMovingLeft
.CheckMovingRight
    lda Temp
    and #J0RIGHT
    bne .NotMovingRight
    ;--moving right.
    ldy #3
.CheckForEnemyTankRightLoop
    cpy Temp+1  ;--don't bother comparing tank to itself
    beq .NoCompareTankToItselfRight
    ;--see if enemy tank is offscreen
    lda TankStatus,Y
    and #$F0
    beq .TankOffScreenIgnoreRight
    ;see if an enemy tank is 
    lda TankX,Y
    clc
    adc #1      ;--have to add one so equal values are ignored
    sec
    sbc TankX,X
    cmp #14
    bcs .ThisTankNotToRight
    lda TankY,X
    clc
    adc #10
    sec
    sbc TankY,Y
    cmp #20
    bcs .ThisTankNotToRight
    ;--there is a tank there!  can't move right!
    lda Temp
    ora #J0RIGHT
    sta Temp
    bne .CheckMovingUp  ;branch always to the next check
.ThisTankNotToRight    
.NoCompareTankToItselfRight
.TankOffScreenIgnoreRight
    dey
    bne .CheckForEnemyTankRightLoop
.NotMovingRight
.CheckMovingUp
    lda Temp
    and #J0UP
    bne .NotMovingUp
    ;--moving up.
    ldy #3
;     stx Temp+1
.CheckForEnemyTankUpLoop
    cpy Temp+1  ;--don't bother comparing tank to itself
    beq .NoCompareTankToItselfUp
    ;--see if enemy tank is offscreen
    lda TankStatus,Y
    and #$F0
    beq .TankOffScreenIgnoreUp
    ;see if an enemy tank is above
    lda TankY,Y
    clc
    adc #1      ;--have to add one so equal values are ignored
    sec
    sbc TankY,X
    cmp #12
    bcs .ThisTankNotToUp
    lda TankX,X
    clc
    adc #12
    sec
    sbc TankX,Y
    cmp #24
    bcs .ThisTankNotToUp
    ;--there is a tank there!  can't move up!
    lda Temp
    ora #J0UP
    sta Temp
    bne .CheckMovingDown  ;branch always to the next check
.ThisTankNotToUp    
.NoCompareTankToItselfUp
.TankOffScreenIgnoreUp
    dey
    bne .CheckForEnemyTankUpLoop
.NotMovingUp

.CheckMovingDown
    lda Temp
    and #J0DOWN
    bne .NotMovingDown
    ;--moving down.
    ldy #3
;     stx Temp+1
.CheckForEnemyTankDownLoop
    cpy Temp+1  ;--don't bother comparing tank to itself
    beq .NoCompareTankToItselfDown
    ;--see if enemy tank is offscreen
    lda TankStatus,Y
    and #$F0
    beq .TankOffScreenIgnoreDown
    ;see if an enemy tank is below
    lda TankY,X
    clc
    adc #1      ;--have to add one so equal values are ignored
    sec
    sbc TankY,Y
    cmp #12
    bcs .ThisTankNotToDown
    lda TankX,X
    clc
    adc #12
    sec
    sbc TankX,Y
    cmp #24
    bcs .ThisTankNotToDown
    ;--there is a tank there!  can't move up!
    lda Temp
    ora #J0UP
    sta Temp
    bne .DoneWithEnemyTankChecks  ;branch always to the next check
.ThisTankNotToDown    
.NoCompareTankToItselfDown
.TankOffScreenIgnoreDown
    dey
    bne .CheckForEnemyTankDownLoop
.NotMovingDown
.DoneWithEnemyTankChecks



    lda Temp    ;restore allowed directions to Accumulator

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
; 	sec
; 	sbc #1
	sta Temp+1
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
	


	


	


	

	
	PAGEALIGN
	
	
    
    
DigitDataMissile




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
MissileSix
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
MissileNine    
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
    .byte RIGHTTHREE|(SINGLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;  	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)

MissileEight
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte LEFTONE|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte RIGHTONE|(DOUBLEWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)

MissileZero
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
;	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
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

	

	




	
	
	ds DATASPACEBEFOREGFX - (* & $FF)
	
TankGfxVertical
   
      
        
     ;--animation for upward facing tank:   
TankUpAnimated1
		.byte 0
        .byte #%11011011;--
        .byte #%00111100;--
        .byte #%11111111;--
        .byte #%00011000;--
        .byte #%11011011;--
        .byte #%00011000;--
TankUpAnimated1b
		.byte 0
        .byte #%00000011;--
        .byte #%11111100;--
        .byte #%00111111;--
        .byte #%11111100;--
        .byte #%00011011;--
        .byte #%11011000;--
        .byte 0
TankUpAnimated2
		.byte 0
		.byte #%11011000;--
		.byte #%00111111;--
		.byte #%11111100;--
		.byte #%00011011;--
		.byte #%11011000;--
		.byte #%00011011;--
TankUpAnimated2b
		.byte 0
        .byte #%11000011;--
        .byte #%00111100;--
        .byte #%11111111;--
        .byte #%00011000;--
        .byte #%11011011;--
        .byte #%00011000;--
		.byte 0
TankUpAnimated3
		.byte 0
		.byte #%00011000;--
		.byte #%11111111;--
		.byte #%00111100;--
		.byte #%11011011;--
		.byte #%00011000;--
		.byte #%11011011;--
TankUpAnimated3b
		.byte 0
        .byte #%11000000;--
        .byte #%00111111;--
        .byte #%11111100;--
        .byte #%00011011;--
        .byte #%11011000;--
        .byte #%00011011;--
		.byte 0
TankUpAnimated4
		.byte 0
		.byte #%00011011;--
		.byte #%11111100;--
		.byte #%00111111;--
		.byte #%11011000;--
		.byte #%00011011;--
		.byte #%11011000;--
TankUpAnimated4b
		.byte 0
        .byte #%00000000;--
        .byte #%11111111;--
        .byte #%00111100;--
        .byte #%11011011;--
        .byte #%00011000;--
        .byte #%11011011;--
		.byte 0
TankDownAnimated1
        .byte 0
        .byte #%11011000;--
        .byte #%00011011;--
        .byte #%11011000;--
        .byte #%00111111;--
        .byte #%11111100;--
        .byte #%00000011;--
TankDownAnimated1b
		.byte 0
		.byte #%00011000;--
		.byte #%11011011;--
		.byte #%00011000;--
		.byte #%11111111;--
		.byte #%00111100;--
		.byte #%11011011;--
		.byte 0
TankDownAnimated2
        .byte 0
        .byte #%00011000;--
        .byte #%11011011;--
        .byte #%00011000;--
        .byte #%11111111;--
        .byte #%00111100;--
        .byte #%11000011;--
TankDownAnimated2b
		.byte 0
		.byte #%00011011;--
		.byte #%11011000;--
		.byte #%00011011;--
		.byte #%11111100;--
		.byte #%00111111;--
		.byte #%11011000;--
		.byte 0
TankDownAnimated3
        .byte 0
        .byte #%00011011;--
        .byte #%11011000;--
        .byte #%00011011;--
        .byte #%11111100;--
        .byte #%00111111;--
        .byte #%11000000;--
TankDownAnimated3b
		.byte 0
		.byte #%11011011;--
		.byte #%00011000;--
		.byte #%11011011;--
		.byte #%00111100;--
		.byte #%11111111;--
		.byte #%00011000;--
		.byte 0
TankDownAnimated4
        .byte 0
        .byte #%11011011;--
        .byte #%00011000;--
        .byte #%11011011;--
        .byte #%00111100;--
        .byte #%11111111;--
        .byte #%00000000;--       	
TankDownAnimated4b
		.byte 0
		.byte #%11011000;--
		.byte #%00011011;--
		.byte #%11011000;--
		.byte #%00111111;--
		.byte #%11111100;--
		.byte #%00011011;--
		.byte 0
		
TitleGraphics
; 	.byte %00001100, %11001100, %00111111, %11001100	
; 	.byte %00001100, %11111100, %00110011, %00111100
; 	.byte %00001100, %11001100, %00110011, %11001100
; 	.byte %11111111, %11111100, %00110011, %11111100
; 	.byte %11000000, %00000000, %00000000, %00000000
; 	.byte %11000000, %00110011, %11001100, %00111111
; 	.byte %11000000, %00110011, %11001100, %00110011
; 	.byte %11001100, %00110011, %11001100, %00110011
; 	.byte %11001100, %00110011, %11111100, %00111111
; 	.byte %11111111, %00000011, %00000000, %00000000
    ;-----PF1--------PF2--------PF2--------PF1
    .byte %00001000, %11111100, %11111100, %00001000
    .byte %00001000, %10001100, %10001100, %00001000
    .byte %00001111, %11101111, %11101111, %00001111
    .byte %00000111, %11111111, %11111111, %00000111
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %10101010, %01110101, %01001010, %01010111
    .byte %10101010, %01010101, %01001110, %00110101
    .byte %10101010, %01010101, %01001010, %01010101
    .byte %11111010, %01110111, %11101110, %01110101
    .byte %11011010, %01110111, %11101110, %01110101


TitleGraphicsEnd				


PFRegisterLookup
	.byte 0, 0, 0, 0
	.byte MAZEROWS-1, MAZEROWS-1, MAZEROWS-1, MAZEROWS-1
	.byte (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2
	.byte (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3



		PAGEALIGN
	
DigitData
Zero
        .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--
One
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00111000;--
        .byte #%00011000;--
Two
        .byte #%11111110;--
        .byte #%11000000;--
        .byte #%01111000;--
        .byte #%00011110;--
        .byte #%00000110;--
        .byte #%11000110;--
        .byte #%01111100;--
Three
        .byte #%01111100;--
        .byte #%11000110;--
        .byte #%00000110;--
        .byte #%00001100;--
        .byte #%00000110;--
        .byte #%11000110;--
        .byte #%01111100;--
Four
        .byte #%00001100;--
        .byte #%00001100;--
        .byte #%11101110;--
        .byte #%11001100;--
        .byte #%01101100;--
        .byte #%00101100;--
        .byte #%00001100;--
Five
        .byte #%01111100;--
        .byte #%11000110;--
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%11111100;--
        .byte #%11000000;--
        .byte #%11111110;--
Six
        .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11101100;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%01101100;--
Seven
        .byte #%00110000;--
        .byte #%00110000;--
        .byte #%00011000;--
        .byte #%00001100;--
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%11111110;--
Eight
        .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--
Nine
        .byte #%01101100;--
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%01101110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--

	


		ds DATASPACEBEFOREGFX - (* & $FF)
		
TankGfxHorizontal
TankRightAnimated1
        .byte 0
        .byte #%01100110;--
        .byte #%00110000;--
        .byte #%01111111;--
        .byte #%01111000;--
        .byte #%11001100;--
        .byte #%11001100;--
TankRightAnimated1b
		.byte 0
		.byte #%01100110;--
		.byte #%01100110;--
		.byte #%01111000;--
		.byte #%01111111;--
		.byte #%00110000;--
		.byte #%11001100;--
		.byte 0
TankRightAnimated2
        .byte 0
        .byte #%11001100;--
        .byte #%00110000;--
        .byte #%01111111;--
        .byte #%01111000;--
        .byte #%10011001;--
        .byte #%10011001;--
TankRightAnimated2b
		.byte 0
		.byte #%11001100;--
		.byte #%11001100;--
		.byte #%01111000;--
		.byte #%01111111;--
		.byte #%00110000;--
		.byte #%10011001;--
		.byte 0
TankRightAnimated3
        .byte 0
        .byte #%10011001;--
        .byte #%00110000;--
        .byte #%01111111;--
        .byte #%01111000;--
        .byte #%00110011;--
        .byte #%00110011;--
TankRightAnimated3b
		.byte 0
		.byte #%10011001;--
		.byte #%10011001;--
		.byte #%01111000;--
		.byte #%01111111;--
		.byte #%00110000;--
		.byte #%00110011;--
		.byte 0
TankRightAnimated4
       	.byte 0
        .byte #%00110011;--
        .byte #%00110000;--
        .byte #%01111111;--
        .byte #%01111000;--
        .byte #%01100110;--
        .byte #%01100110;--
TankRightAnimated4b
		.byte 0
		.byte #%00110011;--
		.byte #%00110011;--
		.byte #%01111000;--
		.byte #%01111111;--
		.byte #%00110000;--
		.byte #%01100110;--
		.byte 0

		
		
		
		

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
			

	

		
	
;--if we are aiming at RAM locations, how do we aim at the base?  Base X = 80, Y = 0

	;

	;tank 0 = player, so don't need entry for that
	;tank 1 target = player position
	;tank 2 target = random position
	;tank 3 target = player position
TankTargetX = *-1
	.byte TankX, RandomNumber, TankX
TankTargetY = *-1
	.byte TankY, RandomNumber, TankY
	
	;--following is combined with target above (see routine for details)
	;tank 0 = player, so don't need entry for that
	;tank 1 target = player position
	;tank 2 target = itself
	;tank 3 target = tank 1
TankTargetAdjustX = *-1
	.byte TankX, TankX+1, TankX+1
TankTargetAdjustY = *-1
	.byte TankY, TankY+1, TankY+1
	

	
BulletDirectionClear
BulletUp
	.byte	BULLETUP, BULLETUP<<2, BULLETUP<<4, BULLETUP<<6
BulletDown
	.byte	BULLETDOWN, BULLETDOWN<<2, BULLETDOWN<<4, BULLETDOWN<<6
BulletLeft
	.byte	BULLETLEFT, BULLETLEFT<<2, BULLETLEFT<<4, BULLETLEFT<<6
BulletRight
	.byte	BULLETRIGHT, BULLETRIGHT<<2, BULLETRIGHT<<4, BULLETRIGHT<<6

	
	
PFMaskLookup
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0

		
	PAGEALIGN




	
	ds BLOCKHEIGHT+1 - (* & $FF)
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
	.byte ENEMYTANKBASESPEED0, ENEMYTANKBASESPEED1, ENEMYTANKBASESPEED2
	
	
PreventReverses = *-1	;--the FF are wasted bytes
	.byte 	~J0DOWN, ~J0UP, $FF, ~J0RIGHT, $FF, $FF, $FF, ~J0LEFT
	
	;tank 0 = player, so don't need initial byte
	;tank 1 target = upper left corner
	;tank 2 target = base (bottom center)
	;tank 3 target = upper right corner
SwitchMovementX = *-1
	.byte 0, 80, 255
SwitchMovementY = *-1
	.byte 255, 0, 255
	
Tone
	.byte BRICKSOUNDTONE, BULLETSOUNDTONE, ENEMYTANKSOUNDTONE, SHORTBRICKSOUNDTONE, LONGEXPLOSIONTONE

Frequency
	.byte BRICKSOUNDFREQ, BULLETSOUNDFREQ, ENEMYTANKSOUNDFREQ, SHORTBRICKSOUNDFREQ, LONGEXPLOSIONFREQ

SoundLength
	.byte BRICKSOUNDLENGTH, BULLETSOUNDLENGTH, ENEMYTANKSOUNDLENGTH, SHORTBRICKSOUNDLENGTH, LONGEXPLOSIONLENGTH
	
NumberOfBitsSet
	.byte 0, 1, 1, 2
	.byte 1, 2, 2, 3
	.byte 1, 2, 2, 3
	.byte 2, 3, 3, 4
MovementMask
	.byte J0UP, J0DOWN, J0LEFT, J0RIGHT
	
RotationTablesBank1
	.word RotationEvenBank1, RotationOddBank1	
	
RotationOddBank1
	.byte 0	;and first 3 bytes of next table
RotationEvenBank1
	.byte 2, 1, 3, 0
; RotationOdd	=	* - 1	;uses last byte (zero) of data immediately preceding
; 	.byte 2, 1, 3	

		
    echo "----", ($1F00-*), " bytes left (ROM) at end of Bank 1"

	org $1F00
	rorg $1F00
	
BankSwitchSubroutine1
	plp
	tsx
	dec $01,X
	lda ($01,X)
	sta MiscPtr
	inc $01,X
	lda ($01,X)
	sta MiscPtr+1
	lsr
	lsr
	lsr
	lsr
	lsr
	tax
	nop $1FF8,X
	
	jmp (MiscPtr)
	
ReturnFromBSSubroutine1
	tsx
	inx
	inx
	lda $00,X      ;get high byte of return address
	lsr
	lsr
	lsr
	lsr
	lsr
	tax
	nop $1FF8,X
	
	rts    

	org $1FFC
    rorg $1FFC
    
	.word Start
	.word BankSwitchSubroutine1

;----------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------
;---------------------------------        BANK TWO           ----------------------------------------
;----------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------


	org $2000
	rorg $3000

Start2
	sta $1FF8

	


	



	
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
	lsr		;--only setup gfx pointer when the tank is displayed with the player (i.e., X = 0 or 1)
	bne MissileNotPlayer   
PlayerNotMissile
	;--get correct tank gfx ptr set up
	txa
	asl
	asl
; 	tax		;we need X * 4
	sta Temp		;save X index
	;--if Y = 0 then we are doing player tank.  Check if not moving.
	tya ;set flags based on Y
	bne NotPlayerTankSkipNotMovingCheck
	;--how do we know if tank is not moving?  
	lda TankStatus
	and #$0F
	bne PlayerTankMovingSetRegularGraphicsPointers
; 	lda #0
	.byte $2C   ;--skip two bytes 
	;--end
PlayerTankMovingSetRegularGraphicsPointers
NotPlayerTankSkipNotMovingCheck
	;--get index into tank graphics image
	lda FrameCounter
	lsr
	and #%00000110
	tax


	lda TankStatus,Y
	and #TANKUP
	beq TankNotFacingUp
	
	lda TankUpFrame,X
	pha
	lda TankUpFrame+1,X
	jmp SetTankGfxPtr
TankNotFacingUp
	lda TankStatus,Y
	and #TANKDOWN
	beq TankNotFacingDown
	lda TankDownFrame,X
	pha
	lda TankDownFrame+1,X
	jmp SetTankGfxPtr
TankNotFacingDown
	lda TankStatus,Y
	and #TANKRIGHT
	beq TankNotFacingRight
	lda TankRightFrame,X
	pha
	lda TankRightFrame+1,X
	jmp SetTankGfxPtr
TankNotFacingRight
	lda TankRightFrame,X
	pha
	lda TankRightFrame+1,X
SetTankGfxPtr
    ldx Temp
	sta Player0Ptr+1,X
	sta Player0Ptr+3,X
	pla
	sec
	sbc TankY,Y
	sta Player0Ptr,X
	clc
	adc #TANKHEIGHT
	sta Player0Ptr+2,X
	jmp EndRotationLoop
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
	
	
	lda FrameCounter
	and #$0F
	sta Temp        ;used below for cycling base color
	;bullet flicker rotation:
	and #3
	tax
	lda BulletX,X
	sta BallX
	lda BulletY,X
	sta BallY


	;--cycle BaseColor
	lda #(BASECOLOR)&($F0)
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
	
	
	
	jmp ReturnFromBSSubroutine2

	
	
;****************************************************************************


	
MoveBulletSubroutine
    ;--if used only hardware collision registers, could just move one bullet every 4 frames.
    ;   but would probably need to redesign tank graphic for that to work.

;     lda #BULLETFRACTIONALSPEED
;     clc
;     adc BulletFractional
;     sta BulletFractional
;     bcs MoveBulletsThisFrame
;     jmp ReturnFromBSSubroutine2
; MoveBulletsThisFrame    
; 	ldx #3
    lda FrameCounter
    and #3
    tax
MoveBulletsLoop
    lda #0
    sta Temp
    lda BulletFractional
    clc
    ;--apply update four times (because only once every four frames)
    adc #BULLETFRACTIONALSPEED
    rol Temp    ;get carry into Temp
    adc #BULLETFRACTIONALSPEED
    rol Temp    ;get carry into Temp
    adc #BULLETFRACTIONALSPEED
    rol Temp    ;get carry into Temp
    adc #BULLETFRACTIONALSPEED
    rol Temp    ;get carry into Temp
    cpx #0
    bne NoUpdateBulletFractionalThisFrame
    sta BulletFractional
NoUpdateBulletFractionalThisFrame
    ldy Temp
;     bne MoveBulletsThisFrame    ;this is in effect a branch always....unless we make bullets really really slow (<1 pixel per FOUR frames)
;     jmp ReturnFromBSSubroutine2
; MoveBulletsThisFrame
;     ldy Temp
    lda NumberOfBitsSetBank2,Y       ;this is how many pixels to move
    sta Temp
	lda BulletY,X
	;cmp #BALLOFFSCREEN
	beq BulletOffScreen;NoBulletMovement
	lda BulletDirection
	and BulletDirectionMask,X
	tay							;save value
	cmp BulletUpBank2,X
	bne BulletNotUp
	;--move bullet up
	lda BulletY,X
	clc
	adc Temp    ;was #BULLETSPEEDVER
	sta BulletY,X
	bcc CheckBulletIsOffScreenVertical      ;branch always
BulletNotUp
	tya
	cmp BulletDownBank2,X
	bne BulletNotDown
	;--move bullet down
	lda BulletY,X
	sec
	sbc Temp    ;was #BULLETSPEEDVER
	sta BulletY,X
	bcs BulletOnScreen                      ;branch always
BulletNotDown
	tya	
	cmp BulletRightBank2,X
	bne BulletNotRight
	;--move bullet right
	lda BulletX,X
	clc
	adc Temp    ;was #BULLETSPEEDHOR
	sta BulletX,X
	bcc CheckBulletIsOffScreenHorizontal    ;branch always
BulletNotRight	
	tya
	cmp BulletLeftBank2,X
	bne NoBulletMovement
	;--move bullet left
	lda BulletX,X
	sec
	sbc Temp    ;was #BULLETSPEEDHOR
	sta BulletX,X

	;--check for off screen:
CheckBulletIsOffScreenHorizontal
	lda BulletX,X
	cmp #17
	bcc BulletOffScreen
	cmp #144
	bcs BulletOffScreen
	bcc BulletOnScreen
CheckBulletIsOffScreenVertical
	lda BulletY,X
	cmp #(MAZEAREAHEIGHT)+4
	bcc BulletOnScreen	
BulletOffScreen
	lda #BALLOFFSCREEN
	sta BulletX,X       ;<--is this necessary?
	sta BulletY,X	
NoBulletMovement
BulletOnScreen
; 	dex
; 	bpl MoveBulletsLoop

	jmp ReturnFromBSSubroutine2

	
	
	
	
;****************************************************************************
	
GenerateMazeSubroutine

	;--first, save RandomNumber
	lda RandomNumber
	pha
	
	;--let's try splitting it 
	ldy MazeGenerationPass
	bpl NotFirstPass
	ldy #MAZEGENERATIONPASSES+1+1
	sty MazeGenerationPass
	;--on first pass, make base disappear and fill all with walls
	lda GameStatus
	and #~DRAWBASE
	sta GameStatus
	
	ldx #MAZEROWS-2
	lda #$FF
FillMazeLoop
	sta PF1Left,X
	sta PF2Left,X
	sta PF2Right,X
	sta PF1Right,X
	dex 
	bpl FillMazeLoop
	

	
	
	;--update maze number on first pass also
	sed
	lda MazeNumber
UpdateMazeNumber
	clc
	adc #$01
	sta MazeNumber
	beq UpdateMazeNumber
	cld

	;and set seed for maze
	sta RandomNumber	
	
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
	dey
	bpl GenerateMazePasses
	jmp LastPassThroughMazeGeneration
GenerateMazePasses
	tya
	asl
	clc
	adc #1
	tay

	jsr UpdateRandomNumberBank2
	and #1
	eor #15
	;--start on random number between 14 and 15
	tax
	dex ;--now start on random number between 13 and 14 
	
MakeMazeLoopOuter	
	stx Temp
	stx Temp+1
MakeMazeLoopInner
	lda #<PF1Left
	sta MiscPtr
	
	;--clear block
	lda PFMaskLookupBank2,X
	eor #$FF
	pha
	lda PFRegisterLookupBank2,X
	clc
	adc MiscPtr
	sta MiscPtr
	pla
	and (MiscPtr),Y	
	sta (MiscPtr),Y
	
	lda #<PF1Left
	sta MiscPtr

	;--are we at the end of our horizontal path?  compare random number to constant
	jsr UpdateRandomNumberBank2
	;lda RandomNumber
	cmp #MAZEPATHCUTOFF
	bcc EndOfRun
	;--not at the end of the run, so loop around
	dec Temp+1
	dex
; 	bpl MakeMazeLoopInner
    bne MakeMazeLoopInner  ;<--don't go all the way to the left edge
    

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
;  	jsr UpdateRandomNumberBank2
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
	cpx #1
	beq NoCarveDownInColumn1
	cpx #14
	beq NoCarveDownInColumn14
	lda PFMaskLookupBank2,X
	eor #$FF
	pha
	lda PFRegisterLookupBank2,X
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
NoCarveDownInColumn1
NoCarveDownInColumn14
	pla		;get block index back into X
	tax
	dex
	stx Temp
	stx Temp+1
	lda #<PF1Left
	sta MiscPtr
	dex
	bmi DoneWithRow
; 	bpl MakeMazeLoopOuter
    bne MakeMazeLoopOuter ;<--don't carve all the way to the left edge
EndRunBeginNextRun
	dex
	stx Temp
	stx Temp+1
; 	lda #<PF1Left
; 	sta MiscPtr         ;<--is this necessary?

	
DoneWithRow
DoneWithFirstPass
	dec MazeGenerationPass

; 	bpl NotCompletelyDoneWithMaze
    jmp NotCompletelyDoneWithMaze
LastPassThroughMazeGeneration

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
	
	;	leave room for enemy tanks to enter:
	IF DEBUGMAZE = 1
	
    	ldx #0
    	stx PF1Left+MAZEROWS-2
    	stx PF1Right+MAZEROWS-2
    	stx PF2Left+MAZEROWS-2
    	stx PF2Right+MAZEROWS-2
    	ldx #$FF
    	stx PF1Left+MAZEROWS-3
    	stx PF1Right+MAZEROWS-3
    	stx PF2Left+MAZEROWS-3
    	stx PF2Right+MAZEROWS-3

   	ELSE
	    
    	ldx #$0C
    	txa
    	;clear blocks in top row that are entry points
    	and PF1Left+MAZEROWS-2
    	sta PF1Left+MAZEROWS-2
    	
    	;clear upper R corner
    	txa
    	and PF1Right+MAZEROWS-2
    	sta PF1Right+MAZEROWS-2
	
;     	ldx #$0C
    	txa
    	and PF2Left+MAZEROWS-2
    	sta PF2Left+MAZEROWS-2
    	txa
    	and PF2Right+MAZEROWS-2
    	sta PF2Right+MAZEROWS-2
	
    ENDIF

	;--clear left and right blocks on the rows where tanks enter when player is near top of screen
	ldy #5
ClearSideEntryPointsLoop
    lda EntryPointRowOffsetTable,Y
    tax
    lda #$3F
    and PF1Left,X
    sta PF1Left,X
    lda #$3F
    and PF1Right,X
    sta PF1Right,X
	dey
	bpl ClearSideEntryPointsLoop


	
	;--make base reappear
	lda GameStatus
	ora #DRAWBASE
	sta GameStatus
	
	;--and clear GAMEOFF flag and turn off maze generation flag
	lda GameStatus
	and #~(GENERATINGMAZE|GAMEOFF)
	sta GameStatus
	

	;--set starting tank position
	
	;--move tanks and bullets off screen
	lda #255		;--loop until X = 255 (-1)
	jsr MoveEnemyTanksOffScreen	
	
	lda MazeNumber
	asl
	clc
	adc #6
	;lda #STARTINGENEMYTANKCOUNT
	cmp #20
	bcc SetInitialTanksRemaining
    lda #20
SetInitialTanksRemaining
	sta TanksRemaining
	
	;--starting timers for when tanks enter the maze
	;--rewrite this:
	ldx #3
SetNewLevelTankDelay
    lda StartingTankStatus,X
    sta TankStatus,X
    dex
    bpl SetNewLevelTankDelay
	
NotCompletelyDoneWithMaze
	;--restore original random number
	lda RandomNumber
	sta Temp+2
	pla
	sta RandomNumber
	
	jmp ReturnFromBSSubroutine2
	
		
;****************************************************************************

EntryPointRowOffsetTable
    .byte ENEMYSTARTINGYHIGHROW - 2, ENEMYSTARTINGYHIGHMIDROW - 2, ENEMYSTARTINGYMIDHIGHROW - 2
    .byte ENEMYSTARTINGYMIDROW - 2, ENEMYSTARTINGYMIDLOWROW - 2, ENEMYSTARTINGYLOWROW - 2

;****************************************************************************

UpdateRandomNumberBank2
    lda RandomNumber
    lsr
    bcc .SkipEOR
    eor #$B2
.SkipEOR
    sta RandomNumber
    rts

;****************************************************************************


	
	
MoveEnemyTanksOffScreen
	sta Temp
	ldx #3
	ldy #6
SetStartingEnemyTankLocationsLoop
	lda StartingTankXPosition,Y
	sta TankX,X
	lda StartingTankYPosition,Y
	sta TankY,X
	dey
	dey
	dex
	cpx Temp                ;6, 4, 2, 0, 254
	bne SetStartingEnemyTankLocationsLoop
	
	rts	

	
;****************************************************************************
	
MoveBulletsOffScreen
	ldx #3
	lda #BALLOFFSCREEN
RemoveBulletsFromScreenLoop
	sta BulletX,X
	sta BulletY,X
	dex
	bpl RemoveBulletsFromScreenLoop
	rts

;****************************************************************************

	;come in with amount to increase in A (units and tens) and Temp (100s and 1000s)
IncreaseScoreSubroutine
	clc
	sed
	adc Score+2
	sta Score+2
	lda Score+1
	adc Temp
	sta Score+1
	lda Score
	adc #0
	sta Score
	cld
	rts

;****************************************************************************
IsTankOnScreen
    ;--X holds tank number to check
    ;--return in accumulator:  0 if offscreen, 1 if onscreen
    lda TankX,X
    cmp #TANKONSCREENLEFT
    bcc TankOffScreen
    cmp #TANKONSCREENRIGHT+1
    bcs TankOffScreen
    lda TankY,X
    cmp #TANKONSCREENTOP+2
    bcc TankOnScreen    
TankOffScreen
    lda #0
    rts
TankOnScreen
    lda #1
    rts

;****************************************************************************
; 
; CollisionRotationTables
; 	.word TankCollisionRegisterEven-1, TankCollisionRegisterOdd-1 
; 	.word TankCollisionBitEven-1, TankCollisionBitOdd-1
			
	
; TankCollisionRegisterOdd
; 	.byte CXM0P, CXPPMM, CXM1P
; ; 	.byte CXM0P, CXM1P, CXPPMM
; TankCollisionBitOdd
; 	.byte $40, $80, $80
; TankCollisionRegisterEven
; 	.byte CXM0P, CXPPMM, CXM0P
; ; 	.byte CXM0P, CXM0P, CXPPMM
; TankCollisionBitEven
; 	.byte $80, $40, $40 ; $40, $40	
	
	
;byte holds
;           CXM0P   CXM1P   CXPPMM	
;           7  6    7  6    7  6
;frame odd  12 1P   3P 32   2P 31
;frame even P1 P3   23 21   13 2P

TankCollisionRotationTables
    .word TankCollisionBitsEven, TankCollisionBitsOdd

TankCollisionBitsOdd
    .byte %01101000, %11000100, %10011000, %00110100
TankCollisionBitsEven
    .byte %11000100, %10011000, %00110100, %01101000
	
	;Rotation:		Frame odd		Frame even
	;Framecounter = xxxxxxx1        xxxxxxx0
	;		P0		player			tank 3
	;		P1		tank 2			tank 1
	;		M0		tank 1			player
	;		M1		tank 3			tank 2

	;player to tank collisions:
	
	;Tank 1			M0 to P0		P1 to M0
	;Tank 2			P1 to P0		M1 to M0
	;Tank 3			M1 to P0		P0 to M0
	
P0CollisionTable
	.byte 3, 0
P1CollisionTable
	.byte 1, 2
M0CollisionTable
	.byte 0, 1
M1CollisionTable
	.byte 2, 3	
;****************************************************************************

CollisionsSubroutine


    ;--first and most important collision check: has anything at all touched the base?
    ;-check bullets first (and only)
    
    bit CXBLPF
    bpl NoBulletToBaseCollision
    ;--hit PF, now let's see if it is in the middle on the bottom row
    lda FrameCounter
    and #3
    tax
    lda BulletY,X
    cmp #BLOCKHEIGHT+2
    bcs NoBulletToBaseCollision
    lda BulletX,X
    cmp #76
    bcc NoBulletToBaseCollision
    cmp #85
    bcs NoBulletToBaseCollision
    
    ;--collision!  Game Over, man, Game Over.
    lda GameStatus
    ora #GAMEOVER|GAMEOFF
    sta GameStatus
    lda #0
    ;--kill sounds just for my own sanity
    sta AUDV0
    sta AUDV1
    
NoBulletToBaseCollision
    
    

	;--can use hardware collisions for tank to tank
	;tentative plan: use movement routine to prevent enemy tanks from colliding
	;                if enemy tank crashes into player tank, then both die
	;Rotation:		Frame odd		Frame even
	;Framecounter = xxxxxxx1        xxxxxxx0
	;		P0		player			tank 3
	;		P1		tank 2			tank 1
	;		M0		tank 1			player
	;		M1		tank 3			tank 2

	;player to tank collisions:
	
	;Tank 1			M0 to P0		P1 to M0
	;Tank 2			P1 to P0		M1 to M0
	;Tank 3			M1 to P0		P0 to M0
	
    lda CXM0P
    and #$C0
    sta Temp+1
    lda CXM1P
    and #$C0
    lsr
    lsr
    ora Temp+1
    sta Temp+1
    lda CXPPMM
    and #$C0
    lsr
    lsr
    lsr
    lsr
    ora Temp+1
    sta Temp+1	
    
    lda FrameCounter
    and #1
    asl
    tax
    lda TankCollisionRotationTables,X
    sta MiscPtr
    lda TankCollisionRotationTables+1,X
    sta MiscPtr+1
	
; 	lda FrameCounter
; 	and #1
; 	asl
; 	tax
; 	lda CollisionRotationTables,X
; 	sta MiscPtr
; 	lda CollisionRotationTables+1,X
; 	sta MiscPtr+1
; 	lda CollisionRotationTables+4,X
; 	sta MiscPtr+2
; 	lda CollisionRotationTables+5,X
; 	sta MiscPtr+3


    ldy #3
TankCollisionLoop
    lda Temp+1    ;get all collision registers
;     beq NoTankCollisionsAtAll
; YesTankCollisions
    and (MiscPtr),Y
    beq NoTankCollision
    ;--remove tank only if fully onscreen
    tya
    tax ;%%%%%
    jsr IsTankOnScreen  ;returns 1 in A if onscreen, 0 in A if not
    and #$FF    ;--sets flags
    beq TankNotOnScreenCannotDie
    ;--tank is onscreen
    tya ;--set flags based on which tank
    bne EnemyTankDied
; 	lda StartingTankYPosition+4
; 	sta TankY
; 	lda StartingTankXPosition+4
; 	sta TankX
; 	lda StartingTankStatus;+4
; 	sta TankStatus
; 	;--play tank explosion sound
; 	sty Temp+1  ;save Y register
; 	ldy #ENEMYTANKSOUND
; 	jsr StartSoundSubroutineBank2
; 	ldy Temp+1  ;restore Y register
 	;--add KILLTANKSCORE to score 
	lda #>KILLTANKSCORE
	sta Temp
	lda #<KILLTANKSCORE
	jsr IncreaseScoreSubroutine
	;--turn off player movement sound
	lda #0
	sta AUDV0
;     beq PlayerTankDead
EnemyTankDied
    ;--remove enemy tank    
    tya
    tax
    jsr PlayerHitTank   ;this routine uses X to index into which tank got hit.
TankNotOnScreenCannotDie    
NoTankCollision
PlayerTankDead
    dey
    bpl TankCollisionLoop  
NoTankCollisionsAtAll


    ;--alternate collision routine for bullet-to-wall using collision registers
    
    bit CXBLPF
    bpl BallHasNotHitBlock
    ;--bullet hit the wall, now identify which bullet.
    lda FrameCounter
    and #3
    tax
    jsr BulletHitBrickSubroutine

BallHasNotHitBlock
	;--now check if bullet has hit an enemy tank
	;--new routine using collision registers instead of loop.  <--actually, reject this because occasionally bullets pass through tanks
	;   works better with redesigned tank graphics but shots occasionally miss still when on the edge of the tank.  not sure if acceptable or not.
	
	lda FrameCounter
	and #1
	tay
	
	bit CXP0FB
	bvs BulletCollisionP0
	bit CXP1FB
	bvs BulletCollisionP1
	bit CXM0FB
	bvs BulletCollisionM0
	bit CXM1FB
	bvs BulletCollisionM1
	bvc NoBulletToTankCollision
	
BulletCollisionP0
    lda P0CollisionTable,Y
    tax ;index into which tank is hit
    bvs FoundDeadTankNowKill    ;branch always following bvs branch above
BulletCollisionP1
    lda P1CollisionTable,Y
    tax
    bvs FoundDeadTankNowKill
BulletCollisionM0
    lda M0CollisionTable,Y
    tax
    bvs FoundDeadTankNowKill
BulletCollisionM1
    lda M1CollisionTable,Y
    tax
FoundDeadTankNowKill
;     ;--if tank offscreen, cannot be killed <--bullets are removed before they go off the maze area, so this is unnecessary
;     lda TankStatus,X
;     and #$F0
;     beq TankOffScreenCannotBeKilled
	;--add KILLTANKSCORE to score if X > 0 (enemy tank) and Y >= 2 (player bullets)
	lda FrameCounter
	and #3
	tay ;get bullet # into Y so it will be removed appropriately below
	cmp #2
	bcs NoPointsForEnemyOwnGoals
	txa ;set flags based on X
	beq NoPointsForGettingShot
	lda #>KILLTANKSCORE
	sta Temp
	lda #<KILLTANKSCORE
	jsr IncreaseScoreSubroutine

NoPointsForGettingShot		
NoPointsForEnemyOwnGoals
	jsr BulletHitTank

TankOffScreenCannotBeKilled
NoBulletToTankCollision	
	
	;--OLD ROUTINE that uses software collision detection (checks every bullet every frame)
; 	;   X is index into tank (outer loop)
; 	;   Y is index into bullet (inner loop)
; 	ldx #3
; CheckBulletTankCollisionOuterLoop
; 	;first check if tank is offscreen
; 	jsr IsTankOnScreen
; 	and #$FF
; 	bne EnemyTankOnScreen
; 	jmp EnemyTankOffscreen
; EnemyTankOnScreen
; 	;now compare ball location to tank location
; 	ldy #3
; CheckBulletTankCollisionInnerLoop
; 	lda BulletX,Y
; 	cmp #BALLOFFSCREEN
; ; 	bne BulletOnScreen2
; 	beq BulletOffScreen2    
; BulletOnScreen2
; 	;--compare X position
; 	clc
; 	adc #1
; 	cmp TankX,X
; 	bcc BulletDidNotHitTank 
; 	sbc #1
; 	sta Temp
; 	lda TankX,X
; 	clc
; 	adc #8
; 	cmp Temp
; 	bcc BulletDidNotHitTank
; 	;--compare Y position
; 	lda BulletY,Y
; 	sec
; 	sbc #1
; 	cmp TankY,X
; 	bcs BulletDidNotHitTank
; 	adc #1
; 	sta Temp
; 	lda TankY,X
; 	sec
; 	sbc #TANKHEIGHT
; 	cmp Temp
; 	bcs BulletDidNotHitTank
; 
; 	;--add KILLTANKSCORE to score if X > 0 (enemy tank) and Y >= 2 (player bullets)
; 	txa ;set flags based on X
; 	beq NoPointsForGettingShot
; 	cpy #2
; 	bcs NoPointsForEnemyOwnGoals
; 	lda #>KILLTANKSCORE
; 	sta Temp
; 	lda #<KILLTANKSCORE
; 	jsr IncreaseScoreSubroutine
; 
; NoPointsForGettingShot		
; NoPointsForEnemyOwnGoals
; 	jsr BulletHitTank
; 
; BulletDidNotHitTank
; BulletOffScreen2
; 	dey
; 	bmi EnemyTankOffscreen
; 	jmp CheckBulletTankCollisionInnerLoop
; 	
; EnemyTankOffscreen
;     
; 	dex
; 	bmi DoneWithCheckBulletTankCollisionOuterLoop
; 	jmp CheckBulletTankCollisionOuterLoop
; DoneWithCheckBulletTankCollisionOuterLoop
	
	jmp ReturnFromBSSubroutine2

;****************************************************************************


BulletHitBrickSubroutine
	;--first, add to score ONLY if player bullet
	cpx #2
	bcs NoScoreForWallDestructionByEnemies
	lda #>WALLDESTRUCTIONSCORE
	sta Temp
	lda #<WALLDESTRUCTIONSCORE
	jsr IncreaseScoreSubroutine
NoScoreForWallDestructionByEnemies	
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
    lda BulletDirection
    and BulletDirectionMask,X
    cmp BulletLeftBank2,X
    beq BulletMovingLeft
    lda #-16
    .byte $2C   ;skip two bytes
BulletMovingLeft
    lda #-17
    clc            
    adc BulletX,X
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
	lda PFMaskLookupBank2,X
	eor #$FF
	pha
	lda PFRegisterLookupBank2,X
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
	
	;--play brick explosion sound
	;--only start if longer explosion not happening
	ldy #BRICKSOUND
	jsr StartSoundSubroutineBank2
	
NoSoundForBrickExplosion

    rts
    
;****************************************************************************








BulletHitTank
	;--bullet did hit tank.
	;	remove bullet and tank from screen
	lda #BALLOFFSCREEN
	sta BulletX,Y
	sta BulletY,Y
PlayerHitTank
	;--save and restore Y in this routine
	tya
	pha

	txa
	bne EnemyTankRespawnRoutine
    ;--stop player tank sound and use regular respawn for player tank
    sta AUDV0
    beq UsePlayerRespawnPosition
EnemyTankRespawnRoutine
	;--new respawn routine: randomly pick between 2 respawn spots for each tank
	;   formula is tank # times 2 + rand(0, 1)
	lda RandomNumber
	lsr
	txa
	rol
	tay	
	jmp FoundRespawnPosition

; 	lda RandomNumber
; 	and #3
; 	ora #1
; ;	beq UseRegularRespawnPosition       ;--problem is another tank could be in this spot.  hmmmm.  instead loop through?
; 	tay
; FindEnemyTankRespawnLoop
; 	lda TankStatus,Y    ;this is to see which tanks are offscreen.   In theory, if upper four bits of TankStatus are all zeroes than tank is offscreen.
; 	and #$F0
; 	bne FoundRespawnPosition
; 	dey
; 	bne FindEnemyTankRespawnLoop
; 	ldy #3
; 	bne FindEnemyTankRespawnLoop    ;endless loop, which should work...haha.  
; 	                                ;besides this not working as intended, can be endless if all four tanks were stuck (blocked by other tanks) and so couldn't move.
; ; UseRegularRespawnPosition
UsePlayerRespawnPosition
    txa
    tay    
FoundRespawnPosition

	;--if player tank is in top part of screen, use different starting positions
	lda TankY
	cmp #BLOCKHEIGHT*7
	bcc TopRowEnemyTankRespawn
	;--use alternate positions
	;--if player is on left part of screen, use right respawn locations
	lda TankX
	cmp #80
	bcc RightSideEnemyTankRespawn
    ;--else left side
    tya
    clc
    adc #8
    tay
RightSideEnemyTankRespawn
    tya
;     clc   ;not needed since carry clear after addition above and BCC branch that took us directly here
    adc #8
    tay
TopRowEnemyTankRespawn
    lda StartingTankYPosition,Y
	sta TankY,X
	lda StartingTankXPosition,Y
	sta TankX,X
	lda StartingTankStatus,X    ;%%%
	sta TankStatus,X
; 	lda #$FF
; 	sta TankFractional,X

	ldy #ENEMYTANKSOUND
	jsr StartSoundSubroutineBank2
	
	txa
	beq PlayerTankHit
	
	;--decrease tanks remaining ONLY if enemy tank hit

	dec TanksRemaining
	bne LevelNotComplete
	;--set levelcomplete flag
	lda GameStatus
	ora #LEVELCOMPLETE|LEVELCOMPLETETIMER
	sta GameStatus
	;--stop player tank from moving
	lda TankStatus
	and #$F0
	sta TankStatus
	;--remove remaining enemy tanks from screen
	lda #0		
	sta AUDV0
	sta AUDV1
	jsr MoveEnemyTanksOffScreen
	jsr MoveBulletsOffScreen	;--returns with X=255
	;--TODO stop sounds and/or play "level end" sound
		
	
	;--increase score by some amount ...
	;---what I'd like to do is give like 10 points or something for every brick remaining
	;	but that could be complicated
	;	for now, just add level * 100
	lda MazeNumber
	sta Temp
	lda #0
	jsr IncreaseScoreSubroutine	
PlayerTankHit	
LevelNotComplete
    ;--what I want to do here is IF the # of tanks remaining <= 3, 
    ;   increase all enemy tank speeds by .... 4?  constant value.
    lda TanksRemaining
    cmp #4
    bcs MoreThanFourTanksRemainingStill
    ;--loop through tanks and increase speed?
    ;--use Y variable... or save X and restore?
    txa
    pha
    ldx #3
IncreaseEnemyTankSpeedLoop
    lda TankStatus,X
    and #$0F
    clc
    adc #LEVELENDTANKSPEEDBOOST
    cmp #15
    bcc NewEnemyTankSpeedNotTooHigh
    lda #$0F    
NewEnemyTankSpeedNotTooHigh
    sta Temp
    lda TankStatus,X
    and #$F0
    ora Temp
    sta TankStatus,X
    dex
    bne IncreaseEnemyTankSpeedLoop
    
    pla
    tax    
MoreThanFourTanksRemainingStill    
	;--save and restore Y in this routine
	pla
	tay

	rts

;****************************************************************************

StartSoundSubroutineBank2
	
	lda Channel1Decay
	and #$F0
	bne .DoNotStartSound
	lda ToneBank2,Y
	sta AUDC1
	lda FrequencyBank2,Y
	sta AUDF1
	lda SoundLengthBank2,Y
	sta Channel1Decay
	
.DoNotStartSound
	rts

	
;****************************************************************************


IsBlockAtPositionBank2		;position in Temp (x), Temp+1 (y)
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
	lda PFRegisterLookupBank2,X
	pha
	lda PFMaskLookupBank2,X
	pha
	lda Temp+1
	;--special case: if Temp+1 < BLOCKHEIGHT then check in a different way
	sec
	sbc #BLOCKHEIGHT
	bcs .NotOnBottomRow
	;--on bottom row
	;--only two blocks on bottom row
	;	at X positions (LastRowL) 64-72 and (LastRowR) 88-96
	lda LastRowL
	beq .LastRowNoWallOnLeft
	lda Temp
	cmp #73
	bcs .LastRowCheckRightBlock
	cmp #64
	bcs .FoundWhetherBlockExists
.LastRowNoWallOnLeft
.LastRowCheckRightBlock
	lda LastRowR
	beq .FoundWhetherBlockExists
	lda Temp
	cmp #96
	bcs .LastRowNoHit
	cmp #88
	bcs .FoundWhetherBlockExists
.LastRowNoHit
	lda #0
	beq .FoundWhetherBlockExists
.NotOnBottomRow	
	;--divide by blockheight
	;maximum Y value is 77 (TANKAREAHEIGHT=77)
	;	we subtract BLOCKHEIGHT above to see if we are on the bottom row
	;	so maximum once we are here is 70
	;	BLOCKHEIGHT = 7
	; 	so maximum loops is 10



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
.FoundWhetherBlockExists
	;--result is now in A --nonzero means a block is there, zero means no
	sta Temp		;result goes into Temp
	pla
	pla		;get intermediate results off of stack
	pla
	tax		;restore X from stack
	rts
	
	
	
;------------------------------------------------------
;---------------------DATA-----------------------------
;------------------------------------------------------
DigitDataLo
	.byte <Zero,<One,<Two,<Three,<Four,<Five,<Six,<Seven,<Eight,<Nine
	;.byte <DigitA, <DigitB, <DigitC, <DigitD, <DigitE, <DigitF

RotationTables
	.word RotationEven, RotationOdd	

RotationOdd
	.byte 0	;and first 3 bytes of next table
RotationEven
	.byte 2, 1, 3, 0
; RotationOdd	=	* - 1	;uses last byte (zero) of data immediately preceding
; 	.byte 2, 1, 3	

TankUpFrame
	.word TankUpAnimated4+TANKHEIGHT, TankUpAnimated3+TANKHEIGHT, TankUpAnimated2+TANKHEIGHT, TankUpAnimated1+TANKHEIGHT
TankDownFrame
	.word TankDownAnimated4+TANKHEIGHT, TankDownAnimated3+TANKHEIGHT, TankDownAnimated2+TANKHEIGHT, TankDownAnimated1+TANKHEIGHT
TankRightFrame
	.word TankRightAnimated4+TANKHEIGHT, TankRightAnimated3+TANKHEIGHT, TankRightAnimated2+TANKHEIGHT, TankRightAnimated1+TANKHEIGHT

	
BulletDirectionMask
	.byte %11, %11<<2, %11<<4, %11<<6

BulletDirectionClearBank2
BulletUpBank2
	.byte	BULLETUP, BULLETUP<<2, BULLETUP<<4, BULLETUP<<6
BulletDownBank2
	.byte	BULLETDOWN, BULLETDOWN<<2, BULLETDOWN<<4, BULLETDOWN<<6
BulletLeftBank2
	.byte	BULLETLEFT, BULLETLEFT<<2, BULLETLEFT<<4, BULLETLEFT<<6
BulletRightBank2
	.byte	BULLETRIGHT, BULLETRIGHT<<2, BULLETRIGHT<<4, BULLETRIGHT<<6
	
	
	
PFRegisterLookupBank2
	.byte 0, 0, 0, 0
	.byte MAZEROWS-1, MAZEROWS-1, MAZEROWS-1, MAZEROWS-1
	.byte (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2
	.byte (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3

	
	
PFMaskLookupBank2
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0
	
	
StartingTankXPosition
	.byte PLAYERSTARTINGX, PLAYERSTARTINGX, ENEMY01STARTINGX, ENEMY02STARTINGX, ENEMY11STARTINGX, ENEMY12STARTINGX, ENEMY21STARTINGX, ENEMY22STARTINGX
	.byte PLAYERSTARTINGX, PLAYERSTARTINGX, ENEMY0STARTINGX2, ENEMY0STARTINGX2, ENEMY1STARTINGX2, ENEMY1STARTINGX2, ENEMY2STARTINGX2, ENEMY2STARTINGX2
	.byte PLAYERSTARTINGX, PLAYERSTARTINGX, ENEMY0STARTINGX3, ENEMY0STARTINGX3, ENEMY1STARTINGX3, ENEMY1STARTINGX3, ENEMY2STARTINGX3, ENEMY2STARTINGX3

StartingTankYPosition 
	.byte PLAYERSTARTINGY, PLAYERSTARTINGY, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP 
	.byte PLAYERSTARTINGY, PLAYERSTARTINGY, ENEMYSTARTINGYLOW, ENEMYSTARTINGYMIDHIGH, ENEMYSTARTINGYMIDLOW, ENEMYSTARTINGYHIGHMID, ENEMYSTARTINGYMID, ENEMYSTARTINGYHIGH ;removed "+4" from enemy tank #s 1-3 starting Y 
	.byte PLAYERSTARTINGY, PLAYERSTARTINGY, ENEMYSTARTINGYLOW, ENEMYSTARTINGYMIDHIGH, ENEMYSTARTINGYMIDLOW, ENEMYSTARTINGYHIGHMID, ENEMYSTARTINGYMID, ENEMYSTARTINGYHIGH ;removed "+4" from enemy tank #s 1-3 starting Y 
	
StartingTankStatus
	.byte TANKRIGHT|PLAYERRESPAWNDELAY, ENEMYTANK1DELAY, ENEMYTANK2DELAY, ENEMYTANK3DELAY
; 	.byte TANKRIGHT|PLAYERRESPAWNDELAY, ENEMYTANK1DELAY, ENEMYTANK2DELAY, ENEMYTANK3DELAY

	
	
ToneBank2
	.byte BRICKSOUNDTONE, BULLETSOUNDTONE, ENEMYTANKSOUNDTONE

FrequencyBank2
	.byte BRICKSOUNDFREQ, BULLETSOUNDFREQ, ENEMYTANKSOUNDFREQ

SoundLengthBank2
	.byte BRICKSOUNDLENGTH, BULLETSOUNDLENGTH, ENEMYTANKSOUNDLENGTH
	
	
NumberOfBitsSetBank2
	.byte 0, 1, 1, 2
	.byte 1, 2, 2, 3
	.byte 1, 2, 2, 3
	.byte 2, 3, 3, 4

	
;****************************************************************************	

    echo "----", ($3F00-*), " bytes left (ROM) at end of Bank 2"

   	org $2F00
	rorg $3F00
	
BankSwitchSubroutine2
	plp
	tsx
	dec $01,X
	lda ($01,X)
	sta MiscPtr
	inc $01,X
	lda ($01,X)
	sta MiscPtr+1
	lsr
	lsr
	lsr
	lsr
	lsr
	tax
	nop $1FF8,X
	
	jmp (MiscPtr)
	
ReturnFromBSSubroutine2
	tsx
	inx
	inx
	lda $00,X      ;get high byte of return address
	lsr
	lsr
	lsr
	lsr
	lsr
	tax
	nop $1FF8,X
	
	rts


;****************************************************************************	


	org $2FFC
	rorg $3FFC
	.word Start2
	.word BankSwitchSubroutine2
