/*************************************************************************
-------------------------------------------------------------------------
---------Tank Battalion Atari 2600 port----------------------------------
-------------------------------------------------------------------------
	Bob Montgomery
	(c) 2012-2024

	VERY loose port of Tank Battalion
		objective: destroy enemy tanks while protecting base in a destroyable maze
		arcade game - play for high score
			earn points by destroying tanks and advancing levels
		have finite # of tanks to destroy each level 
		once destroyed, level is won and advance to new level
		destroy walls by shooting them
		enemy tanks also shoot and also can destroy walls
		enemy tanks shoot you, when you are shot, respawn
		if enemy tanks shoot base, game over 
  

    Overall program structure:
    Bank 1:
        startup routine
        main game loop
        VBLANK routine
            if game over: checks trigger to see if we should start a new game
            move enemy tanks
            move bullets (bank 2)
            kernel setup (moved to beginning of kernel (in bank 2))
        Overscan routine
            checks collisions (bank 2)
            move player tank
            if game not playing: draw title screen or draw maze for next level
        various subroutines: 
            ReadConsoleSwitchesSubroutine
            InitialSetupSubroutine
            ChooseTankDirectionSubroutine
            FireEnemyBulletRoutine
            SetInitialEnemyTankSpeedRoutine
            MoveEnemyTanksSubroutine
            PlaySoundSubroutine
            PositionASpriteSubroutine (no longer needed)
            PositionASpriteNoHMOVESubroutine (no longer needed)
            DivideByTwoSubroutine (no longer needed)
            UpdateRandomNumber
            EliminateDiagonalSubroutine
            TankFractionalAddition
            ReadControllersSubroutine
            
            StartSoundSubroutine
            CheckForEnemyTankSubroutine
            CheckForWallSubroutine
            KernelSetupSubroutine
        data
        bank switching logic
    Bank 2:
        kernel routine
        various subroutines:
            DrawTitleScreenSubroutine
            MoveBulletSubroutine
            GenerateMazeSubroutine
            UpdateRandomNumberBank2
            MoveEnemyTanksOffScreen
            
            IncreaseScoreSubroutine
            IsTankOnScreen
            CollisionsSubroutine
            BulletHitBrickSubroutine
            BulletHitTank / PlayerHitTank
            StartSoundSubroutineBank2
            PositionASpriteSubroutineBank2
        data
        bank switching logic



	To do:
	Highest priority:
    MAYBE? Restart title screen animation after song plays (unless SELECT pressed in last few seconds... or?)
    Lengthen title screen music?


    DECIDED NOT TO DO THIS: Give score bonus for starting at level 10?
    DONE: PAL60 version.   Need colors.
    DONE: Final color, gfx, sound fx, music tweaking
    DONE: Work on transitions when pressing SELECT/RESET and tighten up and make seamless (especially sound transitions)
	DONE: Add sound for when bullet hits tank that is not on screen (high-pitched *tink*?)    
    DONE: With changes to how bullet movement and collisions are handled, have reintroduced jitter/rolls.  Need to optimize to eliminate.
    DONE: Update standard/random maze indicator
    DONE: Allow use of joystick to change options at title screen
    DONE: Show game variation number somewhere (score?) briefly when SELECT is pressed (tie into item above)
    DONE: Change score color to be static (or change so different for every game variation?)
    DONE: Saw one time an enemy tank got stuck and program entered an endless loop (i.e., hard crash).  !!
	
    Secondary priority or duplicate of above:
	DONE: Graphics/colors (including changing colors (of tanks?  walls?) for different levels)
	DONE Turn powerups off with difficulty switch (or use select?).  Working, but look for a bit we can use to store this during a game so you can't turn it on and off during gameplay.  
	    and then change color of score (or?) to indicate.
	DONE but this is very close or maybe even done.  Difficulty ramping.  Two things:
	    1. Increase initial difficulty - potentially increase firing rate for level 1
	    2. Tweak difficulty ramping.  Currently -
	        tanks reach max speed at level 32 (and 1 or 2 tanks are faster every 4 levels)
                could we increase the maximum tank speed?  We can't move tanks more than once every 4 frames, but could we move >1 pixel at a time?
                    could be possible except that we have to make sure they don't overrun intersections.  Which is an issue since bricks are 7 lines tall.
                        what if we made them 8 lines tall
	        tanks reach maximum fire rate at level 21
	DONE Some kind of indicator that player tank is respawning (countdown something or other?)
	DONE: Last tank shot in a level should not turn into a powerup (should always explode).
	DONE: 
	    IN PROGRESS Harmonized music at title screen.  - the functionality is built, need to finish composing music
	    NOT SURE IF NECESSARY And longer?  Probably have a free byte somewhere we can use at the title screen 
	DECIDED NOT TO DO THIS: Change level-end score-for-bricks routine to remove bricks in a more aesthetic pattern?
    Attract mode?
	Other...?
	sound tweaking/improvements
	DONE: Possibly only decrement powerup countdown counter when player shoots tank?
	    Note: for score bonus for killing consecutive tanks, before dying (on a level) the player gets credit for every tank that dies.
	        After dying, only credit for tanks he kills.  In other words, this sequence gives these points (assuming base score of 50 pts and 25-pt bonus per tank killed/died)
	            1. Player kills tank:    50 points
	            2. Tank kills tank:       0 points (credited as a kill for score bonus purposes)
	            3. Player kills tank:   100 points (base points + bonus x2)
	            4. Tank kills player:     0 points
	            5. Player kills tank:    50 points
	            6. Tank kills tank:       0 points (not credited as a kill for score bonus purposes)
	            7. Player kills tank:    75 points (base points + bonus x1)
	DONE Stuck enemies do not fire.  FIX: They always fire to the right, which would be a problem if they are stuck at the right edge.  Not sure if I should care.  Maybe it's fine?
	    I've only ever seen this once, I think only way is in scenario outlined a little below.
	DONE Add explosion graphics  or ????.  STILL TO DO: Player tank also should explode.  And make it so explosion graphic doesn't kill other tanks.
    DONE Respawn routine needs work.  I am wondering if should use location of other tanks rather than player tank to determine where to respawn.
	DONE Power-Ups - ???
		keep this simple: 
			DONE speed
			NIX - extra life
			DONE "bomb" that destroys all tanks on screen (and all walls?)
			??? something that slows all enemies down?
			??? other?
			DONE Sound effect for when you get power up that gives speed boost.  A nice ascending lick would be cool but current sound FX code doesn't allow change in frequency so.
	NOT GOING TO DO THIS: Turn music off with B&W switch (or pause on 7800?).  abandoned this, music is unobtrusive enough that I don't think it is a big deal.
	DONE Adjust scoring:
	    DONE 50 points for each tank killed plus combo system: for each tank killed since last death in the level, add 25 points.  So first tank is 50, second is 75, third is 100, etc.
	        Bonus maxes out at 375 points
	    DONE Powerup that kills all tanks on screen - give 1000 points per tank killed 
	        STILL CONSIDERING (plus 500?)
	    Shooting powerup - points?
	    DONE Bricks are worth 5 points during level, all remaining bricks are worth 10 pts each at end of level
	    DONE Level end worth 100xlevel
    DONE fix scanline count
	G'NORE Logic for when levels wrap .... not worried about this.
    MOSTLY DONE Tank shooting algorithm needs to be improved drastically, especially obvious at higher levels.
    DONE Tank movement algorithm could use tweaking, tanks still run into each other too often.
	DONE: Music?  Have a free channel but basically zero RAM left.  Could probably squeeze one byte of RAM out.
	DONE: Probably need to go to 8K.  Have about 1/2 a page left of ROM, and
		could free up some space by going through things with a fine-toothed comb, but ...
		it would be a big stretch even to get the bare minimum for a game in there.
		anything extra, like music, or a fancier title screen, or a more-complicated enemy-tank AI routine
		is going to require more space.  
	DONE BUT NEEDS TO BE FINE-TUNED Biggest thing at this point is figure out ramping difficulty
		IDEAS:
			DONE faster tanks at higher levels
			IN PROGRESS more frequent shooting by enemy tanks.  Possibly too frequent at higher levels.  Look at this.
			DONE more aggressive tank movement routines 
			MOSTLY DONE smarter shooting by enemy tanks 
    DONE: Have player tank use different graphic.  This is done but not sure how I feel about the actual graphic.
	DONE: Figure out better AI for tank movement/firing routines 
        Stole routines from Pac-Man.  Seems to work pretty well, want to make the player respawn delay longer, but counter bits are already maxed so may need to fiddle with code.
        Reason I have this here is because when player is dead the enemy tanks head for the base.  Maybe it's ok, at higher levels maybe they move fast enough that it will make a difference?
	DONE Title/splash screen?  
	DONE: tank:tank collisions	
	DECISION TO NOT DO THIS: OR NOT?  display player lives remaining
	DONE: don't give points for enemy tank actions
	DONE replace placeholder font
	NOT GOING TO HAPPEN: 2-player?  (unlikely but...)
	DONE: Game Over logic of some kind
	
	BUG KILLING!
	    FIXED: Game Over routine gets stuck and never finishes.... ???  Was due to pressing trigger (or RESET/SELECT) during game over routine,
	        which tried to sort of start a new game.  Fix is to not read trigger/console switches during game over routine
	    FIXED: Bullets are getting stuck off screen but not in BALLOFFSCREEN location and so enemy tanks stop shooting mid level.
	        seems to happen at higher levels, not sure of cause.  Think cause is when bullets are travelling down and hit BulletY==0 and 
	        BulletX is not set to zero also.
        HAVE NOT SEEN THIS IN A LONG TIME - ASSUME FIXED Occasionally (cause?) a tank respawns when it shouldn't.  Haven't seen this in a while, need to confirm this is an issue still.
        FIXED ENOUGH Tanks kill each other often when entering the screen.  Need to see if this can be fixed.  More urgent now that dead tank graphic stays on screen and will kill the entering tank.
		FIXED I THINK scanline count is wonky, need to tighten up various subroutines that take too long
	    FIXED: remove bullets from screen during level transitions
        FIXED: Tanks still firing when offscreen.  
            UPDATE: I think this is when tank 3 shoots downward a "ghost" shot occurs from offscreen. 
            UPDATE UPDATE: that was incorrect, it was due to out-of-bounds conditions not being checked properly when bullets move downward
        FIXED FOR REALS NOW I THINK.  !!NOT FIXED!! FIXED AGAIN I THINK: FIXED I THINK: Tanks can reverse before fully entering the maze and go back out (this happens at AISWITCH reversals)
            Final fix 11.8.2023 - had branch with opposite condition, oops - so tanks ONLY reversed if they were offscreen.
        
        FIXED I THINK: Also not exactly a bug (??) but tanks still run into each other a lot.  This is fixed EXCEPT for when tanks enter the screen.
        FIXED: Also not exactly a bug (??) but tank shooting needs to be improved.
        FIXED: Tanks get "stuck" for too long - appears to happen when tank movement is not allowed (correctly) due to presence of other tanks,
            but when other tank dies, movement doesn't happen as quickly as it seems like it should.
        MAYBE FIXED: Possible for tanks to get stuck in this situation: (W=wall, T=tank):
            WW W
            WT W
            WWTW
            WWWW
            and neither tank can move because a tank is in the way.
                preferred solution: force tanks that don't move for more than 1/2 second to shoot (in a random direction?) to blast out a wall.
                alternative solution: have tanks that don't move for more than 1/2 second to die.
                problem is that either of those solutions require some kind of counter for "tank stuck"  
                I do have probably one free byte of RAM I could use but ... 
                IMPLEMENTED THIS: Actually, it probably wouldn't be too difficult to fire a bullet from a tank that isn't moving.  They fire immediately,
                    and just whichever direction they are facing.  Depending, this may not suffice to free them.  
	
  Notes from ZeroPage livestream on 9/1:
      FIXED KINDA: screen rolls are way more frequent than I thought, ugh.  Need to work on that.  Note: Seem to have stabilized it at 272 scanlines by increasing Overscan timer setting
      DONE if possible would like to speed up game play.
      FIXED: update spawn points to force player to move from bottom of screen ...
      FIXED: found bug that you can shoot tanks before they are on the screen (!)
      FIXED need to make AI more aggressively chasing "base"
      DONE: probably need to finally implement the "game over" logic for when a tank gets the base	

*/




DEBUGNOENEMYBULLETS = 0 ;enemies cannot shoot
DEBUGMAZE = 0           ;makes entire top row blank and 2nd row solid so tanks are confined up there.
DEBUGPFPRIORITY = 0     ;leaves objects with priority over the playfield so can see where enemies respawn
DEBUGTANKAICOUNTER = 0  ;if this is set, the top 4 bits of TankMovementCounter are set to the brightness of the maze walls
POWERUPTESTING  =   0   ;if this is set, the powerup countdown default and reset/restart are all set to 1 
 
	processor 6502
	include vcs.h
	include macro.h

;-----------------------MY MACROS-----------------------------------------


;--this macro aligns at page boundary (only if needed) and echoes the bytes used
    MAC PAGEALIGN
        if <* != 0
        	echo "---", ((* & $FF00) + $100) - *, "bytes left at PAGEALIGN", [{1}]d, "at location", *
    	    align 256
    	else
    	    echo "--- $0 bytes left at PAGEALIGN", [{1}]d, "at location", *
        endif
    ENDM
	
    MAC ALIGNGFXDATA
.marker set *
        IF DATASPACEBEFOREGFX > (* & $FF)
            ds DATASPACEBEFOREGFX - (* & $FF)
        ENDIF
        ECHO "---", (*-.marker), "bytes left at ALIGNGFXDATA", [{1}]d, "at location", .marker
    ENDM
    
    
    
    ;--pass in 1- to 4-digit decimal number and will spit out it as hex BCD number
    MACRO DecimalToBCD
.Ones SET {1}%10
.Tens Set ({1}/10)%10
.Hundreds Set ({1}/100)%10
.Thousands Set ({1}/1000)%10
        .word [.Ones]+[.Tens<<4]+[.Hundreds<<8]+[.Thousands<<12]
    ENDM
     
    
        
;-------------------------Constants Below---------------------------------

; TV System
PAL         = 1
NTSC        = 0
PAL60       = 2
SECAM       = 3

SYSTEM      = NTSC

    IF SYSTEM = NTSC || SYSTEM = PAL60
SYSTEMSPEED =   60
    ELSE
SYSTEMSPEED =   50
    ENDIF

    IF SYSTEMSPEED = 50
;--scanline count constants
VBLANK_TIMER = 76
OVERSCAN_TIMER = 52
    ELSE
    ;for NTSC or PAL60
VBLANK_TIMER = 33       ;--I think this is fine
OVERSCAN_TIMER = 36     ;--I think this is fine also, with the skipping the sound subroutine if we don't have time (see below)
    ENDIF

SOUNDTIMEBUFFER     =   (8*76)/64
;--scanline count constants end

TANKHEIGHT	=	7
BLOCKHEIGHT = 	7
ROWHEIGHT = 7
COLUMNWIDTH = 8
MAZEROWS	=	11
TANKAREAHEIGHT	=	BLOCKHEIGHT * MAZEROWS
MAZEAREAHEIGHT 	= 	TANKAREAHEIGHT
DATASPACEBEFOREGFX	=	MAZEAREAHEIGHT

;--MazeGenerationPass bits during a level (used to count player deaths and track powerups)
PLAYERDEATHCOUNTBITS        =   %00000011
PLAYERDEATHCOUNTMAX         =   3
LASTTANKTHATFIRED           =   %11000000       ;track last tank that fired a bullet
POWERUPACTIVATEDBITS        =   %00100000       ;4 possible powerups?
POWERUPCOUNTDOWNBITS        =   %00011100       ;number of enemies to be killed without any deaths to activate a powerup
POWERUPCOUNTDOWNMAX         =   %00011100
POWERUPCOUNTDOWNDECREMENT   =   %00000100       ;subtract this to decrement the powerup countdown 
    IF POWERUPTESTING == 1
POWERUPCOUNTDOWNRESTART     =   %00001000       ;this is what the counter gets restarted at after gaining or shooting a powerup
POWERUPCOUNTDOWNRESET       =   %00001000       ;this is what the counter gets reset to after player dies
POWERUPCOUNTDOWNDEFAULT     =   %00001000       ;this is default if random start is zero
    ELSE
POWERUPCOUNTDOWNRESTART     =   POWERUPCOUNTDOWNMAX         ;this is what the counter gets restarted at after gaining a powerup
POWERUPCOUNTDOWNRESET       =   POWERUPCOUNTDOWNMAX         ;this is what the counter gets reset to after player dies
POWERUPCOUNTDOWNDEFAULT     =   POWERUPCOUNTDOWNMAX         ;this is default if random start is zero
    ENDIF

    
SPEEDBONUS          =   2       ;this is applied to player when power up activated    
        
;--TankStatus bits
TANKDIRECTION   =   %11110000       ;these 4 bits are used similarly to the joystick bits from INPT4
TANKSPEED       =   %00001110       ;ASL ASL ASL ASL ORA #$1F and then added to TankFractional
TANKRESPAWNWAIT =   %00001110       ;shares bits with speed
TANKINPLAY      =   %00000001       ;this is set when tank has respawned and is coming on screen or fully on screen.  cleared when tank is dead and/or still waiting to be respawned.


    ;--TANKINPLAY cleared and direction = TANKUP means explosion graphic
    ;--TANKINPLAY cleared and direction != TANKUP means offscreen waiting to respawn
     

TANKRIGHT		=	J0RIGHT         ;%10000000
TANKLEFT		=	J0LEFT          ;%01000000
TANKDOWN		=	J0DOWN          ;%00100000
TANKUP			=	J0UP            ;%00010000




    IF SYSTEMSPEED = 60
FIRSTDEATHDELAY     =   0
SECONDDEATHDELAY    =   4
THIRDDEATHDELAY     =   8
FOURTHDEATHDELAY    =   14
PLAYERRESPAWNDELAY =    14
PLAYERINITIALDELAY  =   0
TANKDEADWAIT        =   2
TANKDEADWAITPLAYER  =   8

    ELSE
FIRSTDEATHDELAY     =   0
SECONDDEATHDELAY    =   4
THIRDDEATHDELAY     =   6
FOURTHDEATHDELAY    =   12
PLAYERRESPAWNDELAY =    12
PLAYERINITIALDELAY  =   0
TANKDEADWAIT        =   2
TANKDEADWAITPLAYER  =   6

    ENDIF    

MAXTANKSREMAINING   =   20

    ;this isn't actually used any longer
LEVELENDTANKSPEEDBOOST  =   4   ;when <= 3 tanks remaining, all on-screen tanks have speed boost applied immediately.

TANKONSCREENLEFT    =   16
TANKONSCREENRIGHT   =   136
TANKONSCREENTOP     =   MAZEAREAHEIGHT

FIRSTCOLUMNX    =   16

;--these are used for the kill zone when a powerup is shot.  They were set at 1.5 bricks, but increasing to 2 brick-widths/-heights
HORIZONTALBLASTDISTANCE = 16
VERTICALBLASTDISTANCE = (TANKHEIGHT+TANKHEIGHT)

;--constants for game over routine
;--TankMovementCounter is reused for this routine
WALLDONEFLASHING = %10000000
GAMEOVERWAITBITS = %01111111

    IF SYSTEMSPEED = 60
GAMEOVERWAIT    =   65 ;number of frames/4, can only use bottom 7 bits (0-127)
    ELSE
GAMEOVERWAIT    =   54 ;number of frames/4, can only use bottom 7 bits (0-127)
    ENDIF
GAMEOVERSOUND   =   BUZZSOUND
GAMEOVERVOLUME  =   7
GAMEOVERSTARTFREQ   =   5
    
;--some constants used by the tank AI routine
    ;should I change this value for 50 Hz (PAL/SECAM)?   It only would change the ratio of the two AI routines.
TANKAISWITCH	=	64              ;--when TankMovementCounter (updated every 4 frames) is less than this number, the enemy tanks go into "scatter" mode and head for static locations
                                    ;set to zero for no scatter mode (tanks will still reverse direction when TankMovementCounter hits zero)
CLYDEDISTANCE   =   30              ;8 tiles in Pac-Man, equivalent in Minotaur is 4 tiles.  In pixels (with tile-width approx 7.5 pixels) is 30 (note that distance from player is calculated using manhattan distance (delta X + delta Y)
PINKYADJUSTMENTX    =   16          ;4 tiles in Pac-Man, equivalent in Minotaur is 2 tiles... I think.  In pixels X = 16.
PINKYADJUSTMENTY    =   14          ; in pixels Y = 14

BLINKYTANK  =   3
PINKYTANK   =   1
;clyde tank is whichever isn't specified above    
    
;delay uses TANKRESPAWNWAIT bits (%00001110) so these values have to be even numbers between 2 and 14
    IF SYSTEMSPEED = 60
ENEMYTANK1DELAY	=	14       ;# of frames of delay is x 4
ENEMYTANK2DELAY	=	8
ENEMYTANK3DELAY	=	2
    ELSE
ENEMYTANK1DELAY	=	12       ;# of frames of delay is x 4
ENEMYTANK2DELAY	=	6
ENEMYTANK3DELAY	=	2
    ENDIF
;--tank speed constants
;--the way these are used:
;   tank speed is in lower nibble (these bits: %00001110)
;   the speed is shifted into the upper nibble (<<<<), ORAd with %00011111,
;   and then added to TankFractional,X.  When TankFractional,X wraps, Tank 
;   position (TankX,X or TankY,X) is updated.
;   So, speed of 14 (%00001110) which is the highest, will *almost* wrap every time
;   and the Tank will basically move 1 pixel per update.  Which brings us to...
;   Player tank is moved every frame, but enemy tanks are moved every four frames.
;   Which means approximate speed ranges are as follows:
;               ------------------------------NTSC (60Hz)---------------------- -------------PAL (50Hz)------------------------
;               Player tank                     Enemy tanks                     Player tank         Enemy tanks
;               frames/pixel    pixel/sec       frames/pixel    pixel/sec       pixel/sec           pixel/sec
;   TANKSPEED0  9               7.5             32              1.9             6.3                 1.6
;   TANKSPEED2  4               15              16              3.8             12.5                3.1 
;   TANKSPEED4  2.7             22.2            10.7            5.6             18.5                4.7 
;   TANKSPEED6  2               30              8               7.5             25                  6.3
;   TANKSPEED8  1.6             37.5            6.4             9.4             31.3                7.8
;   TANKSPEED14 1               60              4               15              50                  12.5
    IF SYSTEMSPEED = 60
PLAYERTANKSPEED	    =   TANKSPEED2|1    
TANKSPEEDADDER      =   $1F 
    ELSE
PLAYERTANKSPEED	    =   TANKSPEED4|1   
TANKSPEEDADDER      =   $0D
    ENDIF
;--so actual level-starting speeds are what is below + (the level # / 4) - the tank number (0-indexed).  truncated to an even number.
;       or: ([base speed] + [level #] - [tank number - 1]) && $FE
;       for example, at level 1 the initial tank speeds are as follows:
;           Tank 1 = 10 +  (1 / 4) - 0 && $FE = 10 && $FE = 10
;           Tank 2 = 8  +  (1 / 4) - 1 && $FE =  7 && $FE =  6
;           Tank 3 = 6  +  (1 / 4) - 2 && $FE =  4 && $FE =  4
;       the effect is to widen the gap between tank 1 and 3 (from 4 to 6)
;       Tank 2 will alternate between being closer to tank 1 and tank 2 depending on whether the level # is odd or even.
    IF SYSTEMSPEED = 60
ENEMYTANKBASESPEED0 =   TANKSPEED8
ENEMYTANKBASESPEED1 = 	TANKSPEED7
ENEMYTANKBASESPEED2 =   TANKSPEED7      ;I know odd numbers don't work, but it affects when they switch from one speed to the next
    ELSE
ENEMYTANKBASESPEED0 =   TANKSPEED10
ENEMYTANKBASESPEED1 = 	TANKSPEED9
ENEMYTANKBASESPEED2 =   TANKSPEED9      ;I know odd numbers don't work, but it affects when they switch from one speed to the next
    ENDIF
SHOOTATBASELEFTCOLUMNBOUNDARY   =   72                  ; this is left-most pixel tank can be in when it will fire vertically at the base
SHOOTATBASERIGHTCOLUMNBOUNDARY  =   81                  ; this is the right-most pixel (+1) tank can be in when it will fire vertically at the base
SHOOTATBASEUPPERBOUNDARY        =   (6*ROWHEIGHT)+1+1   ; this is the highest the enemy tank can be (+1) when it will fire vertically at the base
SHOOTATBASEROWBOUNDARY          =   ROWHEIGHT+2         ;this is the highest the enemy tank can be when it will fire horizontally at the base

;--tank starting locations
PLAYERSTARTINGX	=	8
PLAYERSTARTINGY =   TANKHEIGHT+1

ENEMY1TOPLEFTSTARTINGX1	    =	24      ;column=1
ENEMY1TOPLEFTSTARTINGX2     =   56      ;column=5
ENEMY1TOPRIGHTSTARTINGX1	=	112     ;column=12
ENEMY1TOPRIGHTSTARTINGX2    =   136     ;column=15
ENEMY2TOPLEFTSTARTINGX1	    =	64      ;column=6
ENEMY2TOPLEFTSTARTINGX2     =   32      ;column=2
ENEMY2TOPRIGHTSTARTINGX1	=	88      ;column=9
ENEMY2TOPRIGHTSTARTINGX2    =   120     ;column=13
ENEMY3TOPLEFTSTARTINGX1	    =	40      ;column=3
ENEMY3TOPLEFTSTARTINGX2     =   16      ;column=0
ENEMY3TOPRIGHTSTARTINGX1	=	128     ;column=14
ENEMY3TOPRIGHTSTARTINGX2    =   96      ;column=10

ENEMY1BOTTOMRIGHTSTARTINGX	=	144
ENEMY2BOTTOMRIGHTSTARTINGX	=	144
ENEMY3BOTTOMRIGHTSTARTINGX	=	144
ENEMY1BOTTOMLEFTSTARTINGX	=	8
ENEMY2BOTTOMLEFTSTARTINGX	=	8
ENEMY3BOTTOMLEFTSTARTINGX	=	8

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


;--SOUND CONSTANTS
;   indexes into lookup table for sound effects
BRICKSOUND              =   0
BULLETSOUND             =   1
ENEMYTANKSOUND          =   2
SHORTBRICKSOUND         =   3
LONGEXPLOSIONSOUND      =   4
ENEMYBULLETSOUND        =   5
WALLSOUND               =   6
PLAYERTANKENGINESOUND   =   7
SCORESOUND              =   8
POWERUPEXPLOSIONSOUND   =   9
SPEEDBOOSTSOUND         =   10
INVINCIBLETANKSOUND     =   11

PLAYERTANKVOLUME	=	8


    IF SYSTEM = NTSC || SYSTEM = PAL60
PLAYERTANKENGINELENGTH      =   4
BULLETSOUNDLENGTH	        =	12+1
ENEMYBULLETSOUNDLENGTH      =   BULLETSOUNDLENGTH
BRICKSOUNDLENGTH	        =	25+1
ENEMYTANKSOUNDLENGTH	    =	50+1
SHORTBRICKSOUNDLENGTH	    =	7+1
LONGEXPLOSIONLENGTH	        =	127
WALLSOUNDLENGTH             =   8+1
SCORESOUNDLENGTH            =   4+1
POWERUPEXPLOSIONSOUNDLENGTH =   63
SPEEDBOOSTSOUNDLENGTH       =   20
INVINCIBLETANKSOUNDLENGTH   =   9
    ELSE
PLAYERTANKENGINELENGTH      =   4
BULLETSOUNDLENGTH	        =	11
ENEMYBULLETSOUNDLENGTH      =   BULLETSOUNDLENGTH
BRICKSOUNDLENGTH	        =	22
ENEMYTANKSOUNDLENGTH	    =	42
SHORTBRICKSOUNDLENGTH	    =	7
LONGEXPLOSIONLENGTH	        =	105
WALLSOUNDLENGTH             =   7
SCORESOUNDLENGTH            =   4
POWERUPEXPLOSIONSOUNDLENGTH =   52
SPEEDBOOSTSOUNDLENGTH       =   17
INVINCIBLETANKSOUNDLENGTH   =   7
    ENDIF

    
PLAYERTANKENGINEFREQ	=	8;31
PLAYERTANKENGINETONE	=	2;ENGINESOUND
PLAYERTANKENGINEVOLUME	=	3
    
BULLETSOUNDTONE		=	ENGINESOUND
BULLETSOUNDFREQ		=	13
ENEMYBULLETSOUNDTONE    =   BULLETSOUNDTONE
ENEMYBULLETSOUNDFREQ    =   17
BRICKSOUNDTONE		=	NOISESOUND
BRICKSOUNDFREQ		=	20
ENEMYTANKSOUNDTONE	=	NOISESOUND
ENEMYTANKSOUNDFREQ	=	30
SHORTBRICKSOUNDTONE	=	BRICKSOUNDTONE
SHORTBRICKSOUNDFREQ	=	BRICKSOUNDFREQ
LONGEXPLOSIONTONE	=	ENEMYTANKSOUNDTONE
LONGEXPLOSIONFREQ	=	18
WALLSOUNDTONE       =   BUZZSOUND;SQUARESOUND
WALLSOUNDFREQ       =   10;8
SCORESOUNDTONE      =   SQUARESOUND
SCORESOUNDFREQ      =   20
POWERUPEXPLOSIONSOUNDTONE   =   NOISESOUND   
POWERUPEXPLOSIONSOUNDFREQ   =   15
SPEEDBOOSTSOUNDTONE     =   PITFALLSOUND		
SPEEDBOOSTSOUNDFREQ     =   30
INVINCIBLETANKSOUNDTONE     =   SQUARESOUND
INVINCIBLETANKSOUNDFREQ     =   15

;--music constants

;-note bits:
ARTICULATION_BIT	=	%00000001
VOLUME_BITS         =   %00001110
FREQUENCY_BITS      =   %11110000       ;--lookup into a table, probably.

VOLUME0     =   0
VOLUME2     =   2
VOLUME4     =   4
VOLUME6     =   6
VOLUME8     =   8
VOLUME10    =   10
VOLUME12    =   12
VOLUME14    =   14


G5      =   $00
Fs5     =   $10
E5      =   $20
D5      =   $30
B4      =   $40
E4      =   $50
D4      =   $60
A4      =   $70
Fs4     =   $80
A3      =   $90
G3      =   $A0
C5      =   $B0
A5      =   $C0


PATTERNEND  =   255

PERCUSSIONCUTOFF    =   3
PERCUSSIONVOLUME    =   VOLUME4

ARTICULATE      =   ARTICULATION_BIT

SNARESOUND	=	NOISESOUND
KICKSOUND	=	BUZZSOUND
HIHATSOUND	=	NOISESOUND
SNARE2SOUND	=	NOISESOUND

SNAREPITCH	=	6
KICKPITCH	=	30
HIHATPITCH	=	1
SNARE2PITCH	=	12



;--end SOUND CONSTANTS

WALLDESTRUCTIONSCORE    =   $0005
KILLTANKSCORE           =   $0100
LEVELENDBRICKSCORE      =   $0010
CONSECUTIVEKILLBONUS    =   75
POWERUPSCOREBONUS       =   $1000



;--GameStatus flags

GAMEOFF			=	%10000000
GENERATINGMAZE	=	%01000000
LEVELCOMPLETE	=	%00100000
TITLESCREEN		=	%00010000
GAMEOVER		=	%00001000
DRAWBASE        =   %00000100

LEVELCOMPLETETIMER	=	$03


;--end GameStatus flags


;--GameStatus2 flags


TANKSKILLEDBITS     =   %11110000
PLAYERKILLED        =   %10000000
TANK1KILLED         =   %01000000
TANK2KILLED         =   %00100000
TANK3KILLED         =   %00010000

GAMEVARIATIONBITS   =   %00000111
RANDOMMAZES         =   %00000100
HARDSTARTINGLEVEL   =   %00000010
POWERUPSDISABLED    =   %00000001


;--end GameStatus2 flags

;--GameStatus3 flags

CONSECUTIVEKILLBITS =   %00011111

MAXIMUMCONSECUTIVEKILLS =   20


;--end GameStatus3 flags



BALLOFFSCREEN	=	0


BULLETRIGHT		=	0
BULLETLEFT		=	1
BULLETDOWN		=	2
BULLETUP		=	3
BULLETCLEAR		=	3

TRIGGERDEBOUNCEVALUE = 15
TRIGGERDEBOUNCEFLAG     =   %10000000
CONSOLEDEBOUNCEFLAG	    =	%01000000
JOYSTICKDEBOUNCEFLAG    =   %00100000
ENEMYDEBOUNCEBITS       =   %00011111

    IF SYSTEMSPEED = 60
BULLETHORIZONTALSPEED   =   4
BULLETVERTICALSPEED = 3	
    ELSE
BULLETHORIZONTALSPEED   =   5
BULLETVERTICALSPEED = 4
    ENDIF
	
;--note that bullet speeds are no longer fractional, so these constants aren't used AND ALSO the ZP RAM location BulletFractional also isn't used.
BULLETFRACTIONALPERCENT =   99   
BULLETFRACTIONALSPEED   =   256*BULLETFRACTIONALPERCENT/100  ;slowing bullets down so the collision detection works better


/*              even frame          odd frame
player tank     TANKCOLOR1 (M0)     TANKCOLOR3 (P0)
enemy tank 1    TANKCOLOR2 (P1)     TANKCOLOR3 (M0)
enemy tank 2    TANKCOLOR2 (M1)     TANKCOLOR4 (P1)
enemy tank 3    TANKCOLOR1 (P0)     TANKCOLOR4 (M1)
*/
    IF SYSTEM = NTSC
BASECOLOR		            =		GOLD
SCORECOLOR_POWERUPSDISABLED =       GRAY|$C
WALLCOLOR			        =		RED|$8
TANKSREMAININGCOLOR         =       TURQUOISE|$A
    
TANKCOLOR1          =    GOLD|$8       
TANKCOLOR2          =    GREEN|$8
TANKCOLOR3          =    BURNTORANGE|$A
TANKCOLOR4          =    LIGHTBLUE|$C
    ELSE
    
        IF SYSTEM = PAL || SYSTEM = PAL60
BASECOLOR		            =		GOLD
SCORECOLOR_POWERUPSDISABLED =       GRAY|$E
WALLCOLOR			        =		RED|$8
TANKSREMAININGCOLOR         =       TURQUOISE|$A

TANKCOLOR1          =    GOLD|$8       
TANKCOLOR2          =    GREEN|$8
TANKCOLOR3          =    BURNTORANGE|$A
TANKCOLOR4          =    LIGHTBLUE|$A
    
        ELSE    ;SECAM
BASECOLOR		            =		BLACK
SCORECOLOR_POWERUPSDISABLED =       WHITE
WALLCOLOR			        =		RED
TANKSREMAININGCOLOR         =       CYAN

TANKCOLOR1          =    YELLOW    
TANKCOLOR2          =    GREEN
TANKCOLOR3          =    RED
TANKCOLOR4          =    BLUE
        ENDIF
    ENDIF

;--used by maze generation algorithm
MAZEPATHCUTOFF	=	90;100

TANKSPEED0  =   0
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
    IF SYSTEM = NTSC
;-------------------------COLOR CONSTANTS (NTSC)--------------------------
GRAY		=	$00
GOLD		=	$10
ORANGE		=	$20
BURNTORANGE	=	$30
RED		    =	$40
PURPLE		=	$50
PURPLEBLUE	=	$60
BLUE		=	$70
BLUE2		=	$80
LIGHTBLUE	=	$90
TURQUOISE	=	$A0
GREEN		=	$B0
BROWNGREEN	=	$C0
TANGREEN	=	$D0
TAN		    =	$E0
BROWN		=	$F0
    ELSE
        IF SYSTEM = PAL || SYSTEM = PAL60


;-------------------------COLOR CONSTANTS (PAL)--------------------------
GRAY		=	$00
GOLD		=	$20
ORANGE		=	$20
BURNTORANGE	=	$40
RED		    =	$60
PURPLE		=	$80
PURPLEBLUE	=	$A0
BLUE		=	$C0
BLUE2		=	$D0
LIGHTBLUE	=	$B0
TURQUOISE	=	$90
GREEN		=	$70
BROWNGREEN	=	$50
TANGREEN	=	$30
TAN		    =	$20
BROWN		=	$20
        ELSE    ;secam
;-------------------------COLOR CONSTANTS (SECAM)--------------------------
BLACK   =   $00
BLUE    =   $02
RED     =   $04
MAGENTA =   $06
GREEN   =   $08
CYAN    =   $0A
YELLOW  =   $0C
WHITE   =   $0F
        ENDIF

    ENDIF
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
ONECOPYQUAD		    =	$07
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
TankMovementCounter ds 1
RandomNumber ds 1
RandomNumberSaved ds 1  ;used to save the random number for more than one frame

MazeNumber ds 1
MazeGenerationPass ds 1
GameStatus ds 1
GameStatus2 ds 1
GameStatus3 ds 1
Score ds 3
TanksRemaining ds 1     ;this holds a value from 0 to 20.  Wondering if we can use the top 3 bits for something else?

Channel1Decay ds 1

Debounce ds 1


TankX ds 4
TankY ds 4
TankStatus ds 4

TankFractional ds 4

BulletX ds 4
BulletY ds 4
BulletDirection ds 1
; BulletFractional ds 1

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

SongIndex ds 1

Temp ds 3  
MiscPtr 
ScorePtr ds 12


   ; Display Remaining RAM
   echo "----",($100 - *) , "bytes left (ZP RAM)"
;---End Variables

	seg Bank0

	org $1000
	rorg $1000

Start
;	sta $1FF8
    nop
    nop
    nop
    CLEAN_START

;--Some Initial Setup


	jsr InitialSetupSubroutine

		
	
;	lda #BASECOLOR
;	sta BaseColor

;----------------------------------------------------------------------------
;--------------------GAME MAIN LOOP------------------------------------------
;----------------------------------------------------------------------------
; MainGameLoop
; 
; 	jsr VBLANKRoutine
; 	brk
; 	.word KernelRoutineGame
; 	jsr OverscanRoutine
; 	jmp MainGameLoop



	


;----------------------------------------------------------------------------
;-------------------VBLANK Routine-------------------------------------------
;----------------------------------------------------------------------------
    SUBROUTINE
VBLANKRoutine
    lda #%00001111
VSYNCWaitLoop2
    sta WSYNC
    sta VSYNC
    lsr
    bne VSYNCWaitLoop2      ;different version of this loop
    
    

	lda #VBLANK_TIMER
	sta TIM64T
	
    dec FrameCounter

	lda GameStatus
	and #GAMEOFF|LEVELCOMPLETE|GAMEOVER|GENERATINGMAZE
	beq GameOnVBLANK	
	and #GAMEOFF
	beq InBetweenLevels
	lda GameStatus
	and #GENERATINGMAZE|GAMEOVER
	bne GameAlreadyStarted

	;--if game not on and NOT generating a level and NOT in game over routine already read joystick button to start game
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
	
	jsr StartNewGame
	jmp DoneStartingNewGameWithTrigger
NoTriggerToStartGameClearTriggerDebounce
    lda Debounce
    and #~TRIGGERDEBOUNCEFLAG
    sta Debounce
TriggerNotDebouncedYet
    ;--here is where to check for changing game variation with joystick
    ;--can increase variation by pressing U or decrease by pressing D (w/ wrap in both directions)
    lda SWCHA
    and #J0UP|J0DOWN
    cmp #J0UP|J0DOWN
    beq NoJoystickForGameVariations
    ;--yes joystick so check debounce
    lda Debounce
    and #JOYSTICKDEBOUNCEFLAG
    bne JoystickNotDebounced
    ;--first set debounce
    lda Debounce
    ora #JOYSTICKDEBOUNCEFLAG
    sta Debounce
    ;--then if title screen not completely drawn, draw it
    jsr GoImmediatelyToCompleteTitleScreen
    ;--and then update game variation
    lda SWCHA
    asl
    asl
    asl
    bcc DecreaseGameVariation
    ;--if here, up must be pressed so increase the game variation
    lda #1
    jsr UpdateGameVariationSubroutine
    jmp DoneChangingGameVariations
DecreaseGameVariation
    lda #-1
    jsr UpdateGameVariationSubroutine
    jmp DoneChangingGameVariations
NoJoystickForGameVariations
    ;--so reset joystick debounce
    lda Debounce
    and #~JOYSTICKDEBOUNCEFLAG
    sta Debounce    
JoystickNotDebounced   
DoneChangingGameVariations    
DoneStartingNewGameWithTrigger

	jmp GameNotOnVBLANK
GameOnVBLANK
CallingMoveEnemyTank
	jsr MoveEnemyTanksSubroutine
CallingMoveBulletSubroutine	

    ;--this is faster than the BRK routine.   25 cycles to switch vs 52
;     lda #<MoveBulletSubroutine
;     sta MiscPtr
;     lda #>MoveBulletSubroutine
;     sta MiscPtr+1
;     jsr BankSwitchAltRoutine1
    jsr MoveBulletSubroutine
FinishedWithMoveBulletSubroutine	
    ;nop
    
GameAlreadyStarted
GameNotOnVBLANK
InBetweenLevels

    lda GameStatus
    and #GENERATINGMAZE|GAMEOVER
    bne DoNotReadConsoleSwitchesWhileGeneratingMaze ;also not while game over routine is running
	jsr ReadConsoleSwitchesSubroutine
DoNotReadConsoleSwitchesWhileGeneratingMaze

    ;--UpdateRandomNumber
    lda RandomNumber
    lsr
    bcc .SkipEOR
    eor #$B2
.SkipEOR
    sta RandomNumber
   

    ;--this is faster than the BRK routine.   25 cycles vs 52 (using BRK)
    lda #<KernelRoutineGame
    sta MiscPtr
    lda #>KernelRoutineGame
    sta MiscPtr+1
    jsr BankSwitchAltRoutine1

	
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
	and #GAMEOVER
	beq GameNotOver
	;--if game is over, do some stuff
	lda TankMovementCounter
	and #WALLDONEFLASHING
	bne DoneMakingWallsFlash
	;--play some sound?
	lda TankMovementCounter
	lsr
	lsr
	sta AUDF1
	lda #GAMEOVERSOUND
	sta AUDC1
	lda #GAMEOVERVOLUME
	sta AUDV1
	inc TankMovementCounter ;used to make walls flash
	;--now need to see if we have just finished flashing
	lda TankMovementCounter
	and #WALLDONEFLASHING
	beq WallsStillFlashing  
    ;--walls done flashing:
    ;   remove tanks from screen
    ;   wait a bit, and then go back to title screen routine
	jsr MoveAllTanksOffScreenSubroutine
    ldx #3
    lda #0
RemoveBulletsFromScreenGameOverLoop
	sta BulletX,X
	sta BulletY,X
	dex
	bpl RemoveBulletsFromScreenGameOverLoop
	;   reuse TankMovementCounter as a wait counter
	lda #WALLDONEFLASHING|GAMEOVERWAIT
	sta TankMovementCounter
	lda #30
	sta AUDF1   ;--set freq back to 30 not 31
DoneMakingWallsFlash
    lda FrameCounter
    and #3
    bne GameOverRoutineStillGoing
    dec TankMovementCounter ;--decrement wait
	lda TankMovementCounter
	and #GAMEOVERWAITBITS
	beq GameOverRoutineDone
	;--still ongoing, so a little fiddling with the sound
	cmp #(GAMEOVERWAIT/4)*3
	bcs NoChangeToGameOverSound
	ldx #31
	stx AUDF1
	cmp #32
	bcs NoChangeToGameOverVolume
	cmp #24
	bcc NoChangeToGameOverVolume
	and #7
	sta AUDV1
	
	bcs GameOverRoutineStillGoing   ;branch always
	
GameOverRoutineDone
    ;to go back to title screen routine, need to:
    ;   update GameStatus bits
    ;   set MazeGenerationPass appropriately
    ;   clear the screen
	lda GameStatus
	and #~(GAMEOVER|DRAWBASE)      ;clear game over and draw base bits
	ora #TITLESCREEN    ;set title screen bit
	sta GameStatus
	lda #TitleGraphicsEnd-TitleGraphics-1
	sta MazeGenerationPass
	ldx #MAZEROWS-2
	lda #0
ClearMazeLoop
	sta PF1Left,X
	sta PF2Left,X
	sta PF2Right,X
	sta PF1Right,X
	dex 
	bpl ClearMazeLoop
	sta LastRowL
	sta LastRowR
	;--also turn off sound
; 	sta AUDV0
	sta TanksRemaining
	lda #$00
	sta MazeNumber
GameOverRoutineStillGoing
NoChangeToGameOverVolume
NoChangeToGameOverSound
WallsStillFlashing
GameNotOver
	
	lda GameStatus
	and #GAMEOFF|LEVELCOMPLETE|GENERATINGMAZE
	bne GameNotOn	
    ;--this is faster than the BRK routine.   25 cycles vs 52 (using BRK)
    lda #<CollisionsSubroutine
    sta MiscPtr
    lda #>CollisionsSubroutine
    sta MiscPtr+1
    jsr BankSwitchAltRoutine1
;  	brk
; 	.word CollisionsSubroutine
BackFromCollisionsSubroutine	
	;--need to recheck, since a collision could have modified these flags
	lda GameStatus
	and #GAMEOFF|LEVELCOMPLETE|GENERATINGMAZE
	bne GameNotOn	
	jsr PlayerTankMovementRoutine

	jsr PowerUpBonusRoutine

	
	jmp CheckTimeForSound
GameNotOn
	;--A still holds GameStatus AND #GAMEOFF|LEVELCOMPLETE|GENERATINGMAZE
	and #LEVELCOMPLETE
	bne .LevelComplete
	jmp .LevelNotComplete
.LevelComplete
	;--level complete
	;--wait until level-ending explosion sound is mostly died down
	lda Channel1Decay
	cmp #SCORESOUNDLENGTH
	bcc .LevelExplosionComplete
	jmp .LevelNotComplete
.LevelExplosionComplete
	;--move all tanks (and explosion gfx) offscreen
	;--move tanks offscreen
	ldx #3
	ldy #0
	lda #ENEMYSTARTINGYTOP
MoveTanksOffScreenLoop
	sta TankX,X
	sty TankY,X
	dex
	bpl MoveTanksOffScreenLoop
    ;--and then remove one brick every 4 frames and give points for it, while playing a "bling" sound
	lda FrameCounter
	and #$3
	beq .RemoveBrick 
	jmp .LevelNotComplete
.RemoveBrick
	lda GameStatus
	and #LEVELCOMPLETETIMER
	cmp #LEVELCOMPLETETIMER
	bne ShortWaitUntilNextLevel
	;--but first!  give points for all bricks remaining.
	;--use RandomNumberSaved as counter.
	ldx RandomNumberSaved
	lda FrameCounter
	and #$30
	lsr
	lsr
	lsr
	lsr
	tay
FindPFWithBricks
	lda PF1Left,X
	bne FoundPFWithBricks
	inx
	cpx #(MAZEROWS-1)*4+2
	beq FoundAllBricks
	bne FindPFWithBricks    ;branch always
FoundPFWithBricks
	and PFMaskLookup,Y
	bne FoundBrick
	dey
	bpl FindPFWithBricks
	ldy #3
	bne FindPFWithBricks    ;branch always
FoundBrick
	;--clear the brick we just got points for
	lda PFMaskLookup,Y
	eor #$FF
	and PF1Left,X
	sta PF1Left,X
	;--play bling sound
    ldy #SCORESOUND
	jsr StartSoundSubroutine
	;--and get points
	lda #>LEVELENDBRICKSCORE
	sta Temp
	lda #<LEVELENDBRICKSCORE
	jsr IncreaseScoreSubroutineBank0
	jmp .LevelNotComplete
FoundAllBricks
    ;--remove base and give score for level
	;--play bling sound
    ldy #SCORESOUND
	jsr StartSoundSubroutine
	;--add levelx100 to score
	lda MazeNumber
	sta Temp
	lda #0
	jsr IncreaseScoreSubroutineBank0 

	;--remove base
    lda GameStatus
    and #~DRAWBASE
    sta GameStatus
	and #LEVELCOMPLETETIMER
ShortWaitUntilNextLevel
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
	cmp #1
	bne NotGeneratingMaze
	brk
	.word GenerateMazeSubroutine
	ldy #SHORTBRICKSOUND
	jsr StartSoundSubroutine
NotGeneratingMaze
	lda GameStatus
	and #TITLESCREEN
	beq NotOnTitleScreen
	;--on title screen
	lda MazeGenerationPass
	bmi NoLongerDrawingTitleScreen
	lda FrameCounter
	and #$7
	cmp #1
	bne WaitToDrawTitleScreen
	brk
	.word DrawTitleScreenSubroutine       ;moved this to bank 2
	jmp PutTanksOnTitleScreenNotYet
NoLongerDrawingTitleScreen
    ;--let's put some stuff on the screen
    ;--change maze number to whatever game variation is set
    lda GameStatus2
    and #HARDSTARTINGLEVEL
    beq MazeNumberOne
    lda #$10
    .byte $2C
MazeNumberOne
    lda #$01    
    sta MazeNumber
    
    
    
    lda TankStatus
    lsr
    bcs TanksAlreadyOnScreen
    lda SongIndex
;     and #$F0
    cmp #$20
    bne PutTanksOnTitleScreenNotYet
    ;--reset TankMovementCounter so animation of powerup icon is correct
    lda FrameCounter
    and #$7F
    sec
    sbc #1
    lsr
    lsr
    sta Temp
    lda #7
    sec
    sbc Temp    
    sta TankMovementCounter
    ldx #3
PutTanksOnTitleScreenLoop
    lda TankTitleScreenX,X
    sta TankX,X
    lda TankTitleScreenY,X
    sta TankY,X
    lda TankTitleScreenStatus,X
    sta TankStatus,X
    dex
    bpl PutTanksOnTitleScreenLoop
TanksAlreadyOnScreen
    ;--right here let's check to see if left difficulty switch = A.  If so, display third tank (not powerup)
    lda GameStatus2
    and #POWERUPSDISABLED
    bne DisplayTankInsteadOfPowerUp
    and #P0DIFF
    lda #TANKUP|TANKDOWN
    .byte $2C
DisplayTankInsteadOfPowerUp
    lda #TANKLEFT|TANKINPLAY|TANKSPEED2
    sta TankStatus+3

;--at this point we need to update TankMovementCounter so the power up icon animates correctly (like it does during the game)
	lda FrameCounter
	and #3
	bne DoNotUpdateTankMovementCounter
	inc TankMovementCounter     ;tank movement counter incremented every four frames
DoNotUpdateTankMovementCounter
PutTanksOnTitleScreenNotYet
WaitToDrawTitleScreen

NotOnTitleScreen

CheckTimeForSound

    ;we need .... some time for this.
    ;--I *believe* we only end up skipping this when you shoot a powerup, which causes a loud, long explosion, 
    ;   so skipping the sound routine for a frame is unnoticeable.  More testing to confirm.
    lda INTIM
    cmp #SOUNDTIMEBUFFER
    bcc NoTimeForSoundThisFrame


	;--keep playing sound even when game not on
	ldx #0              ;choose channel (0 or 1) for music
	jsr PlaySoundSubroutine
	;--if no sound effects, play music in channel 1
    ldx #1
    jsr PlaySoundSubroutine
    jmp WaitForOverscanEnd      ;--for testing
NoTimeForSoundThisFrame
    nop                         ;--for testing
WaitForOverscanEnd
	lda INTIM
 	bpl WaitForOverscanEnd

 	jmp VBLANKRoutine
	
;----------------------------------------------------------------------------
;----------------------------End Main Routines-------------------------------
;----------------------------------------------------------------------------

   



;****************************************************************************

;----------------------------------------------------------------------------
;----------------------Begin Functions---------------------------------------
;----------------------------------------------------------------------------

    ;--inlining this (in two places!)
; UpdateRandomNumber
;     lda RandomNumber
;     lsr
;     bcc SkipEOR
;     eor #$B2
; SkipEOR
;     sta RandomNumber
;     rts
	
	
	
;----------------------------------------------------------------------------

    SUBROUTINE
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
StartNewGame	
	;--if RESET pressed, set MazeNumber to $01 if not on the title screen.  Else leave it as-is.
	lda GameStatus2
	and #HARDSTARTINGLEVEL
	beq SetMazeNumberToOne
	lda #$10
	sta MazeNumber
; 	
; 	lda MazeNumber
; 	beq SetMazeNumberToOne
; 	lda GameStatus
; 	and #TITLESCREEN
	bne ResetConsoleSwitchDebounce
SetMazeNumberToOne	
    lda #$01
    sta MazeNumber
    bne ResetConsoleSwitchDebounce  ;branch always
SELECTPressed
	lda Debounce
	and #CONSOLEDEBOUNCEFLAG
	bne RESETNotReleased
    ;--new: always go to title screen when SELECT is pressed (if not there already)
    lda GameStatus
    and #TITLESCREEN
    beq NotOnTitleScreenTakeUsThere
NotOnTitleScreenTakeUsThere
    jsr GoImmediatelyToCompleteTitleScreen
    
    ;--new: pressing select cycles through 8 game variations
    lda #1
    jsr UpdateGameVariationSubroutine
    
 	lda Debounce
	ora #CONSOLEDEBOUNCEFLAG
	sta Debounce
    rts    
    
ResetConsoleSwitchDebounce
	lda Debounce
	ora #CONSOLEDEBOUNCEFLAG
	sta Debounce
	
	
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
	and #~(TITLESCREEN|LEVELCOMPLETE)	;--turn off title screen and turn off "level complete" 
	sta GameStatus
	
	;--reset consecutive kill count to zero
	lda GameStatus3
	and #~CONSECUTIVEKILLBITS
	sta GameStatus3
	
MoveAllTanksOffScreenSubroutine     ;called from gameover routine
	;--move tanks off screen, immobilize 
	ldx #3
	ldy #0  ;no direction, no speed, and TANKINPLAY = 0
	lda #TANKOFFSCREEN
MoveTanksOffscreenLoop
	sta TankX,X
	sta TankY,X
 	sty TankStatus,X   
	dex
	bpl MoveTanksOffscreenLoop
	
	
	
	stx MazeGenerationPass
AlreadyStartingNewLevel	
	;--finally, cycle the random number
; 	jmp UpdateRandomNumber				;--return from subroutine there
    lda RandomNumber
    lsr
    bcc .SkipEOR
    eor #$B2
.SkipEOR
    sta RandomNumber
    rts

	
;*******************************************************************
    
    
GoImmediatelyToCompleteTitleScreen
    ;--if we are already there, then we don't need to do anything
    lda GameStatus
    and #GAMEOFF
    beq GameOnGoingJumpToTitleScreen
    ;--this check works if we are on the title screen and we need to know if we are in the middle of drawing it.
    lda MazeGenerationPass
    bpl OnTitleScreenButImmediatelyDraw
    bmi AlreadyOnTitleScreenWeAreFine
GameOnGoingJumpToTitleScreen
    lda #BALLOFFSCREEN      ;<-- this is zero
    sta BulletY
    sta BulletY+1
    sta BulletY+2
    sta BulletY+3
    sta TanksRemaining
    jsr MoveAllTanksOffScreenSubroutine
    
OnTitleScreenButImmediatelyDraw
    ;--reset GameStatus flags
    lda GameStatus
    ora #TITLESCREEN|DRAWBASE|GAMEOFF
    and #~(GENERATINGMAZE|LEVELCOMPLETE)
    sta GameStatus
    ;--draw title screen:
    ldx #TitleGraphicsEnd-TitleGraphics-1
DrawTitleScreenWhenSELECTPressed
	txa
	lsr
	lsr
	tay
    lda TitleGraphicsBank0,X
    sta PF1Right,Y
    dex
    lda TitleGraphicsBank0,X
    sta PF2Right,Y
    dex
    lda TitleGraphicsBank0,X
    sta PF2Left,Y
    dex
    lda TitleGraphicsBank0,X
    sta PF1Left,Y
    dex
    bpl DrawTitleScreenWhenSELECTPressed
    stx MazeGenerationPass
	lda #%11111100
	sta LastRowL
	sta LastRowR	    
    ;--put song at spot where tanks show on screen:
    lda SongIndex
    cmp #$FF
    beq SetSongIndexTo20
    cmp #$20
    bcs NoChangeNecessaryToSongIndex
SetSongIndexTo20    
    lda #$20
    sta SongIndex
NoChangeNecessaryToSongIndex    
AlreadyOnTitleScreenWeAreFine
    rts    
    
;*******************************************************************

InitialSetupSubroutine

	lda #TANKOFFSCREEN
	ldx #3
SetUpTankInitialValues
	sta TankX,X
	sta TankY,X
	dex
	bpl SetUpTankInitialValues
	
	stx SongIndex       ;just need to set top bit here so music starts OFF
	stx VDELBL          ;bit 0 is set so VDELBL is on
	
	sta RandomNumber        ;seed with non-zero number, using 127 (TANKOFFSCREEN) for now
	
	lda #GAMEOFF|TITLESCREEN
	sta GameStatus

	if DEBUGPFPRIORITY = 1
	    lda #REFLECTEDPF|DOUBLEWIDTHBALL
	else
	    lda #REFLECTEDPF|DOUBLEWIDTHBALL|PRIORITYPF
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
    ;fix X index (enemy bullets indexed at 2 & 3)
	inx
	inx
	
	;--first, look for tank that is stuck (i.e., in play but not moving)
	;--this doesn't work right now because tank speed is not set to zero when tanks are stuck (!)
	;--but direction is set to zero.  let's check that.  which works but obviously we don't know what way to shoot!
	ldy #3
FindStuckTank
    lda TankStatus,Y
    and #TANKDIRECTION|TANKINPLAY
    cmp #TANKINPLAY
    beq FireEnemyBulletNow
    dey
    bne FindStuckTank
    ;--if no stuck tank, find random tank 
	lda RandomNumber
	and #3
	tay
	bne ShootFromTank
	jmp DoNotShoot
;     beq DoNotShoot  ;change so evenly random between tanks, fire less often (by 25%)
; 	bne ShootFromTank   ;zero is player, so if non-zero we are fine, otherwise shoot from tank 1
; 	iny				;this routine shoots from tank 1 half the time and tanks 2 and 3 a quarter of the time each
ShootFromTank
    ;--if tank is not in play, don't shoot from it
    lda TankStatus,Y
    and #TANKINPLAY
    beq TankNotInPlayCannotShoot
    ;--if tank offscreen, don't shoot from it
    lda TankX,Y
    cmp #16
    bcc TankOffscreenCannotShoot
    cmp #137
    bcs TankOffscreenCannotShoot
    lda TankY,Y
    cmp #MAZEAREAHEIGHT+2
    bcs TankOffscreenCannotShoot

    
    ;--what kind of logic can we use for shooting?
    ;-- how about - only allow horizontal firing if on bottom row (at base) or if player tank is within ... 3 rows?
    ;               and only allow vertical firing if in center two columns (at base) or if player tank is within ...3 columns?
    lda TankStatus,Y
    asl
    bcs EnemyShootingHorizontal
    asl
    bcs EnemyShootingHorizontal
    ;--else shooting vertical
    ;--is tank in center two columns? (X between 72 and 80)
TankShootingVertically
    lda TankX,Y
    cmp #SHOOTATBASELEFTCOLUMNBOUNDARY    ;72
    bcc EnemyNotInCenterColumns    
    cmp #SHOOTATBASERIGHTCOLUMNBOUNDARY   ;81
;     bcc FireEnemyBulletNow
    bcs EnemyNotInCenterColumns
    ;--now check if enemy is in lower half of screen
    lda TankY,Y
    cmp #SHOOTATBASEUPPERBOUNDARY   ;44     this is in the lower 6 rows
    bcc FireEnemyBulletNow
EnemyNotInCenterColumns
    ;--now see if close to player tank
;     clc
    adc #COLUMNWIDTH*2      ;carry is set, so this adds an additional 1 here.  But that's ok, because then the carry is cleared for the subtraction below, so it will subtract an additional 1.
;     sec
    sbc TankX
    cmp #(COLUMNWIDTH*4)+1
    bcc FireEnemyBulletNow
    bcs EnemyNotAllowedToShootVertically    ;branch always
EnemyShootingHorizontal    
    lda TankY,Y
    cmp #SHOOTATBASEROWBOUNDARY         ;ROWHEIGHT+1
    bcc FireEnemyBulletNow
    clc
    adc #(ROWHEIGHT*2)+1    ;add an additional 1 because the subtraction below has carry clear
;     sec
    sbc TankY
    cmp #(ROWHEIGHT*4)+1
    bcs EnemyNotAllowedToShootHorizontally

    ;%
;     lda TankStatus,Y
;     lsr     ;--get TANKINPLAY flag into carry
;     bcc TankOffscreenCannotShoot
FireEnemyBulletNow
    ;--store which enemy is shooting in top 2 bits of MazeGenerationPass
    tya
    lsr
    ror
    ror
    sta Temp
    lda MazeGenerationPass
    and #LASTTANKTHATFIRED
    cmp Temp
    bne IsNotSameTankGoAheadAndShoot
    ;--if same tank, clear this.
    lda MazeGenerationPass
    and #~LASTTANKTHATFIRED
    sta MazeGenerationPass
    jmp SameTankCannotShootTwiceInARow
IsNotSameTankGoAheadAndShoot
    ;store tank that is shooting
    lda MazeGenerationPass
    and #~LASTTANKTHATFIRED
    ora Temp
    sta MazeGenerationPass

	jsr FireBulletRoutine
	;--start enemy bullet sound
	ldy #ENEMYBULLETSOUND
	jsr StartSoundSubroutine
	
	;--reset debounce.  
	;--shoot more often as levels increase
	ldy MazeNumber
    cpy #(EnemyBulletDebounceEnd-EnemyBulletDebounce)
    bcc MazeNumberLessThanBulletDebounceLimit
    ldy #(EnemyBulletDebounceEnd-EnemyBulletDebounce)
MazeNumberLessThanBulletDebounceLimit    
	lda Debounce    ;was EnemyDebounce
	and #~ENEMYDEBOUNCEBITS ;this probably isn't necessary, but being safe for now
    ora EnemyBulletDebounce-1,Y ;minus one because maze number starts at 1 (not zero)
	sta Debounce    ;was EnemyDebounce
	
SameTankCannotShootTwiceInARow
TankNotInPlayCannotShoot
TankOffscreenCannotShoot
NoAvailableEnemyBalls
EnemyNotAllowedToShootVertically
EnemyNotAllowedToShootHorizontally
DoNotShoot
	;--then we're done
	rts	
	
;****************************************************************************

UpdateGameVariationSubroutine
    clc
    adc GameStatus2
    and #GAMEVARIATIONBITS
    sta Temp
    lda GameStatus2
    and #~GAMEVARIATIONBITS
    ora Temp
    sta GameStatus2
    ;--and display the game variation briefly in the score
    lda #$AA        ;"A" is blank digit
    sta Score
    sta Score+1
    lda GameStatus2
    and #GAMEVARIATIONBITS
    clc
    adc #1                  ;display game variations on scale of 1-8 instead of 0-7
    ora #$A0
    sta Score+2
;     ;--and put a wait into GameStatus2
;     
;     lda GameStatus2
;     ora #GAMEVARIATIONDISPLAYTIMER
;     sta GameStatus2
    rts

;****************************************************************************

    SUBROUTINE
SetInitialEnemyTankSpeedRoutine
    ;--come in here with X pointing to tank # (1-3)
    ;--and A holding new direction (in upper four bits)
	sta TankStatus,X	;--first write new direction
	;--tank starting speed and add level, add speed boost based on how many tanks are left, then subtract tank number (0-2).  Whole thing is capped at 14
	;--first, convert level (MazeNumber) from BCD to binary so we can add it below
	lda MazeNumber
	and #$F0
	lsr
	lsr
	lsr
	lsr
	sta Temp
	asl
	asl
	asl         ;this clears carry
	adc Temp
	adc Temp
	sec
	sbc #1      ;get to be zero-indexed instead of 1-indexed
	sta Temp
	lda MazeNumber
	and #$0F
	adc Temp    ;now A holds binary conversion of MazeNumber
	lsr
	lsr         ;divide by four for slower ramping of enemy tank speed (maximum speed for all four tanks not reached until level 32)
	clc
	adc NewTankSpeed,X
	;--increase speed depending on tanks killed so far
;     ldy TanksRemaining         
;     adc TanksRemainingSpeedBoost,Y
	sta Temp
    lda MazeNumber
    asl
    asl
    clc
    adc #4
    cmp #MAXTANKSREMAINING
    bcc .HaveInitialTanksRemaining
    lda #MAXTANKSREMAINING
.HaveInitialTanksRemaining
    sec
    sbc TanksRemaining
    tay
    lda Temp
    clc    
    adc TanksKilledSpeedBoost,Y
	cmp #15
	bcc NewSpeedNotTooHigh
	lda #TANKSPEED14
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
    and #TANKSPEED      ;make sure only the bits we want  --- shouldn't need to do this, since in theory the value should be <=15 and we set the lower bit just below anyway.
    ora #TANKINPLAY     ;tank in action now so set bit
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
	inc TankMovementCounter     ;tank movement counter incremented every four frames
	;--if we ain't moving a tank, let's shoot a bullet
	jmp FireEnemyBulletRoutine

MoveAnEnemyTank
	tax ;index into which tank

	lda TankStatus,X
	and #TANKINPLAY	
	beq TankNotInPlay
	jmp TankOnscreenMoveIt 
TankNotInPlay
	;--first - is the tank dead and onscreen?  if so, check it's wait and move it offscreen
	lda TankStatus,X
	and #TANKUP|TANKDOWN  
	cmp #TANKUP|TANKDOWN    ;tank up AND tank down means powerup displaying on screen
	beq TankIsActuallyPowerup   ;--actually, any direction means still onscreen
	;--not a powerup, so is an explosion - question is it in onscreen or not:
	lda TankStatus,X
	and #TANKDIRECTION
	cmp #TANKUP             ;tank up only means explosion displaying on screen
	bne TankOffScreen

	;--tank is onscreen but dead and sitting there.
	;   Decrement the wait, once it is done then move it offscreen
	lda TankMovementCounter         ;tank movement counter .... this has effect of multiplying the delay (ranging from 4-60 frames) by 8
	and #$07
	bne DoNotMoveTankOffscreenYet
	lda TankStatus,X
	and #$0F
; 	sec         ;I think this is set already after CMP and not-taken BNE branch above above
	sbc #2
	ora #TANKUP
	sta TankStatus,X
	bpl DoNotMoveTankOffscreenYet	
	;--move tank offscreen
    ;--this is faster than the BRK routine.   25 cycles vs 52 (using BRK)
    lda #<TankRespawnRoutineWrapperEnemy
    sta MiscPtr
    lda #>TankRespawnRoutineWrapperEnemy
    sta MiscPtr+1
    jsr BankSwitchAltRoutine1
; 	brk 
; 	.word TankRespawnRoutineWrapperEnemy
DoNotMoveTankOffscreenYet
TankIsActuallyPowerup       ;if tank is powerup, we don't do anything.  nothing happens until it is gathered or shot.  (at least for now, maybe later we wait and then remove)
    rts
TankOffScreen
	;tank offscreen
	;--only bring tank onscreen if there are enemy tanks remaining
	;--how many tanks onscreen?
    
	ldy #0			    ;counting number of onscreen tanks in Y
	lda TankStatus+3	
	lsr
	bcc Tank3OffScreen	
	iny			;+9/8	10-11
Tank3OffScreen
	lda TankStatus+2
	lsr
	bcc Tank2OffScreen
	iny			;+9/8	18-20
Tank2OffScreen
	lda TankStatus+1
	lsr
	bcc Tank1OffScreen
	iny			;+9/8	26-29
Tank1OffScreen
	tya			;+2	28-31 cycles 


	cmp TanksRemaining
	bcs DoNotBringTankOnscreenYet
	lda TankMovementCounter         ;tank movement counter .... this has effect of multiplying the delay (ranging from 4-60 frames) by 8
	and #7;
	bne DoNotBringTankOnscreenYet
	lda TankStatus,X
; 	sec             ;carry clear after not-taken BCS branch above
	sbc #1          ;so we are actually subtracting 2 here
	sta TankStatus,X
	bpl DoNotBringTankOnscreenYet
	;--wait counter has expired, so bring this tank on screen
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
	jsr SetInitialEnemyTankSpeedRoutine     ;this also sets TANKINPLAY bit
	
	;--set tank fractional so it moves immediately and doesn't turn before it gets onscreen
 	lda #255
 	sta TankFractional,X
 	bne TankOnscreenMoveIt          ;branch always.  so it moves on screen immediately
DoNotBringTankOnscreenYet
	rts

PlayerTankMovementRoutine
    ;--first, if TANKINPLAY is cleared, we wait
    lda TankStatus
    lsr     ;get TANKINPLAY into carry
    bcc WaitForPlayerExplosion

	;--if tank off left edge of screen, we don't read joystick, we just wait (??) and then move it on to the maze from the left
	lda TankX
	cmp #16
	bcs PlayerTankOnscreenMoveIt
	;--wait only if TankX == 8
	cmp #8
	bne NotWaitingBringPlayerTankOnScreen
WaitForPlayerExplosion
	lda TankStatus
	and #TANKRESPAWNWAIT
	beq FinishedWaitingPlayerDelay
	;--else still waiting
	;--update counter every 8 frames
	lda FrameCounter
	and #15         ;--make wait longer for tank to respawn
	bne WaitToUpdateRespawnCounter
	lda TankStatus
	sec
	sbc #2
	sta TankStatus
WaitToUpdateRespawnCounter
	rts
FinishedWaitingPlayerDelay
    ;--now figure out what we are waiting for.  if explosion, call respawn routine.
    ;   if not, we start moving tank.
    lda TankStatus
    and #TANKUP ;if tank is facing up, then is explosion and still onscreen
    beq DoneWaitingBringPlayerTankOnScreen
    ;--this is faster than the BRK routine.   25 cycles vs 52 (using BRK)
    lda #<TankRespawnRoutineWrapperPlayer
    sta MiscPtr
    lda #>TankRespawnRoutineWrapperPlayer
    sta MiscPtr+1
    jsr BankSwitchAltRoutine1
; 	brk 
; 	.word TankRespawnRoutineWrapperPlayer
	rts
DoneWaitingBringPlayerTankOnScreen
	;--set player to move immediately 
	lda #255
	sta TankFractional
NotWaitingBringPlayerTankOnScreen	
 	lda #0		
	pha			;movement flag
	lda #~J0RIGHT
	jmp SkipReadingControllers
PlayerTankOnscreenMoveIt
    ldx #0
	lda TankX
	and #$07
	beq AtIntersectionX	    ;+10	10
	jmp NotAtIntersection	;+3	    14
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

    ;--new new routine is a binary search.  Worst case is 27 cycles, best is 5.
;     cmp #ROWHEIGHT*6        ;middle value
;     beq AtIntersectionY     ;                   best case, 5 cycles
;     bcc Lower1
;     cmp #ROWHEIGHT*9
;     beq AtIntersectionY
;     bcc Lower2
;     cmp #ROWHEIGHT*11
;     beq AtIntersectionY
;     cmp #ROWHEIGHT*10
;     beq AtIntersectionY
;     cmp #ROWHEIGHT*12
;     beq AtIntersectionY
;     bne NotAtIntersectionY      ;       27  branch always, worst case
; Lower2                          ;       13
;     cmp #ROWHEIGHT*8
;     beq AtIntersectionY
;     cmp #ROWHEIGHT*7
;     beq AtIntersectionY
;     bne NotAtIntersectionY      ;+11    24  branch always
; Lower1                          ;        7
;     cmp #ROWHEIGHT*3
;     beq AtIntersectionY
;     bcc Lower3
;     cmp #ROWHEIGHT*5
;     beq AtIntersectionY
;     cmp #ROWHEIGHT*4    
;     beq AtIntersectionY
;     bne NotAtIntersectionY      ;+17    24  branch always
; Lower3                          ;       14
;     cmp #ROWHEIGHT*2
;     beq AtIntersectionY
;     cmp #ROWHEIGHT*1
;     beq AtIntersectionY         ;+9     23
    ;--else not at an intersection      22
    ;--new new new routine uses the divide by seven table:
    tay
    lda DivideBySevenBank0,Y	
    dey
    cmp DivideBySevenBank0,Y
    bne AtIntersectionY		;+15 to see if at an intersection    l
NotAtIntersectionY
EnemyTankPossibleReversal
	;--new thing.  when TankMovementCounter = 0 (i.e., switching from regular to "scatter" mode) make tanks switch direction if they are NOT at an intersection ONLY
	lda TankMovementCounter
	bne NoTankReversal
    ;--player tanks don't reverse:
    txa     ;set flags based on tank index
    beq PlayerTankDoesNotReverse
    ;--only reverse if tank is fully on screen
    jsr IsTankOnScreenBank0
    beq NoTankReversal
	lda TankStatus,X
	lsr
	lsr
	lsr
	lsr
	tay
	lda ReverseDirection,Y
	sta Temp
	lda TankStatus,X
	and #~TANKDIRECTION
	ora Temp
	sta TankStatus,X
PlayerTankDoesNotReverse
NoTankReversal	
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
	ora #TANKSPEEDADDER
	clc
	adc TankFractional,X
;     jsr TankFractionalAddition	
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
	cmp #ENEMY1BOTTOMRIGHTSTARTINGX-1
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
CallingCheckForWallSubroutine
	jsr CheckForWallSubroutine	;--returns with allowable directions in A
AboutToCheckForEnemyTank
	jsr CheckForEnemyTankSubroutine ;--returns with allowable directions in A
	
	and #$F0	;clear bottom nibble
	eor #$F0
	
	;--if single direction, then move that direction
	pha			;--save direction BUT DOES NOT SET FLAGS
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
	and #~TANKDIRECTION
	ora Temp
	sta TankStatus,X
	jmp DirectionChosen
MoreThanOneAllowedDirection
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
	;---revamp this with three separate routines, "borrowing" from Pac-Man routines (steal from the best, I guess)
	;       Tank 3 uses the Blinky chase routine (always aiming at player position)
	;       Tank 1 uses the Pinky chase routine (always aiming X "tiles" in front of player)
	;       Tank 2 uses the Clyde chase routine (aiming for player if X tiles away or more, but when close aiming for base)
	;--if player is dead, aim for base.
	lda TankX
	cmp #16
	bcs PlayerOnScreen
	bcc AimForBase  ;branch always
	
PlayerOnScreen
	cpx #BLINKYTANK
	beq BlinkyRoutine
	cpx #PINKYTANK
	beq PinkyRoutine
ClydeRoutine
    ;--if further than N tank-lengths from player (using Manhattan distance formula)
    ;   then aim for player
    ;   else aim for base
    lda TankX
    sec
    sbc TankX,X
    bcs FoundAbsoluteXDifference
    ;--negative, so multiply by -1
    eor #$FF
    adc #1
FoundAbsoluteXDifference
    sta Temp
    lda TankY
    sec
    sbc TankY,X
    bcs FoundAbsoluteYDifference
    ;--negative, so multiply by -1
    eor #$FF
    adc #1    
FoundAbsoluteYDifference
    clc
    adc Temp
    cmp #CLYDEDISTANCE
    bcs EnemyTankIsFarAimForPlayer
    ;--else aim for base
AimForBase
    lda #80
    pha
    lda #0
    pha
    beq ChooseTankDirection ;branch always
PinkyRoutine
    ;--aims for spot three tank-lengths in front of player tank
    lda TankStatus
    lsr
    lsr
    lsr
    lsr
    tay
    lda TankX
    clc
    adc TankTargetAdjustmentX,Y
    pha
    lda TankY
    clc
    adc TankTargetAdjustmentY,Y
    pha
    jmp ChooseTankDirection
    
    
EnemyTankIsFarAimForPlayer
BlinkyRoutine
    ;tank 1 aims for player tank
    lda TankX
    pha
    lda TankY
    pha
ChooseTankDirection
    ;have allowable directions in stack and target X and Y values in stack
	jsr ChooseTankDirectionSubroutine	;returns with new direction in accumulator
	;takes desired direction and sticks it in Temp
	sta Temp
	pla
	pla									;pull target for tank off stack and discard

	;--now compare to allowable directions
	;   if desired direction is one of the allowable directions, then go that way.
	;       if not, pick random direction from allowable and move that way.
	pla	;	--get allowable directions off stack
	pha	;	but leave on stack
    and Temp
    beq DesiredDirectionsNotAllowed
    ;--we may have more than one desired direction, and both may be allowed.
	eor #$FF	;flip bits
	jsr EliminateDiagonalSubroutine
	eor #$FF
	;--now stick single direction in TankStatus
	sta Temp
	lda TankStatus,X
	and #~TANKDIRECTION
	ora Temp
	sta TankStatus,X					
	bne DirectionIsFine     ;branch always
DesiredDirectionsNotAllowed
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

NoAllowedDirections	
    ;--if no allowed directions, set direction to zero
    lda #0	
FoundGoodDirection
	sta Temp
	lda TankStatus,X
	and #~TANKDIRECTION
	ora Temp
	sta TankStatus,X
		
DirectionIsFine
	pla
DirectionChosen
NotAtIntersection



	;push return address onto stack
	;push movementflag (zero) onto stack
	lda #0
	pha
	
	lda TankStatus,X			;this holds new, desired direction for tank.
	eor #$FF					;flip all bits so that is interchangeable with reading from joystick
								;and can use same routine for both.
	jmp TankMovementSubroutine
	

;****************************************************************************




PlaySoundSubroutine
	;--no sound for enemy tanks; too annoying
	
	;  tentative:
	;	channel 0 = player tank movement
	;	channel 1 = all other sound effects (no shot sound when an explosion is happening)
	
	;--the way this works:
	;   all sound effects have a duration (1-255)
	;   this duration (stored in Channel1Decay) decrements every frame
	;       while Channel1Decay >= 7, the volume for the sound is 7
	;       when Channel1Decay < 7, the volume for the sound = Channel1Decay
	;   in other words, all sounds effects have a static volume (=7) until the last 7 frames (approx 1/10 of a second)
	;       at which point the sound volume linearly fades out to zero
	
	;--skip this if game over
	lda GameStatus
	and #GAMEOVER
	beq PlaySoundGameNotOver
	rts
	
PlaySoundGameNotOver
    ;--check for music channel
    txa
    beq Channel0PlayMusicOnly
Channel1PlaySoundFX
	lda Channel1Decay
	and #$7F
	beq NoSoundEffectHappening
	dec Channel1Decay
	
        
    
    
    lda Channel1Decay
    asl
    bcc NoFrequencyChange  
    ;--so use decay to index?
    lsr
    tay   
    lda PowerUpFrequencyTable,Y
    sta AUDF1
NoFrequencyChange
	lda Channel1Decay
	and #$7F
 	cmp #$07
 	bcc SetChannel1Volume
 	lda #$07
SetChannel1Volume
	sta AUDV1       
    rts
NoSoundEffectHappening
Channel0PlayMusicOnly


    ;--now play music.  
MusicRoutine  
    ;--come in with X pointing to channel (0 or 1)
    ;   save in Temp+10
    stx Temp+10
    ;--no music while maze is generating.  which besides being aesthetically what I want, also keeps the 
    ;   maze-generation routine from being screwed up, since it uses RandomNumberSaved, which is also used by
    ;   maze-generation routine, to save the random seed *across frames*
    ;--save RandomNumber every measure
    lda GameStatus
    and #GENERATINGMAZE|LEVELCOMPLETE
    beq MusicIsPlaying
MusicIsNotPlaying
    lda #0
    ldx Temp+10
    sta AUDV0,X
    rts
MusicIsPlaying
    txa
    asl
    tax     ;get channel index x2 into X
    ;First, determine which song
    ;if on title screen play title screen song
    lda GameStatus
    and #TITLESCREEN
    cmp #TITLESCREEN
    bne NotOnTitleScreenAtAll
    ;--else on title screen, but start playing drum beat once base shows
    lda GameStatus
    and #DRAWBASE
    beq MusicIsNotPlaying
    lda TitleScreenSongPointers,X
    sta MiscPtr+2
    lda #TitleScreenSongPointers+1,X
    sta MiscPtr+3
    bne PlayMusic
NotOnTitleScreenAtAll
    ;--during level start?
    lda GameStatus
    and #GENERATINGMAZE|GAMEOFF|DRAWBASE|LEVELCOMPLETE
    cmp #DRAWBASE       ;not generating maze, game is on, base is drawn, and level is not complete
    bne DoNotPlayLevelStartMusic
    ;--no channel 1 music if not on title screen
    lda Temp+10
    beq LevelStartMusicChannel0
    ;--else just return, sound FX playing in channel 1
    rts
LevelStartMusicChannel0    
    lda #<LevelStartSong
    sta MiscPtr+2
    lda #>LevelStartSong
    sta MiscPtr+3
    bne PlayMusic   ;branch always
DoNotPlayLevelStartMusic
    ;--this is default and is incorrect, need additional logic or different default here.
    lda TitleScreenSongPointers,X
    sta MiscPtr+2
    lda #TitleScreenSongPointers+1,X
    sta MiscPtr+3

    

PlayMusic
 


    lda FrameCounter
    and #7                  ;tempo = 112.5 BPM, 16th note every seven frames
    beq GetNewNote   
    ;--at this point, if songindex = 255, then we don't do anything
    ldy SongIndex
    iny
    beq MusicIsNotPlaying
           
    and #3
    bne NoPercussionDownBeat    ;percussion is all 32nds
    jmp GetNewPercussion
NoPercussionDownBeat
    ;--not on 32nd-note beat, so need to determine if we are playing percussion or playing note
    cmp #PERCUSSIONCUTOFF
    bcc PlayRegularNote
        ;
    ;--otherwise, first see if percussion is playing
;     rts ;--if above percussion cutoff, keep playing whatever we were playing
;     ;--see if percussion should be playing
    lda FrameCounter 
    pha
    ora #$3
    sta FrameCounter
    inc FrameCounter
    jsr GetPercussionSound
    tay     ;save this value
    pla     ;restore frame counter
    sta FrameCounter
    cpy #0
    beq PlayRegularNote
;     ;--else keep playing percussion
    rts
PlayRegularNote    
    ;--else play regular tone
    lda SongIndex
    and #$F0
    lsr
    lsr
    lsr
    tay
    lda (MiscPtr+2),Y
    sta MiscPtr+4
    iny
    lda (MiscPtr+2),Y
    sta MiscPtr+5
    lda SongIndex
    and #$0F
    tay
    lda (MiscPtr+4),Y
    and #VOLUME_BITS
    sta Temp
    lda (MiscPtr+4),Y
    lsr
    bcc NoArticulation
    lda FrameCounter
    and #7
    tax
    lda Temp
    sec
    sbc ArticulationTable,X
    .byte $2C   ;skip next two bytes
NoArticulation
    lda Temp
    sta Temp+1
    lda (MiscPtr+4),Y
    lsr
    lsr
    lsr
    lsr
    tay
    lda DistortionTable,Y
    sta Temp+2
    lda FrequencyTable,Y
    sta Temp+11
    jmp PlayMusicSound
GetNewNote
    ;--only do this for channel 0
    lda Temp+10
    bne SkipUpdateOfSongIndex
    inc SongIndex
SkipUpdateOfSongIndex    
BackToBeginningOfSong
    lda SongIndex
    and #$F0
    lsr
    lsr
    lsr
    tay
    lda (MiscPtr+2),Y
    cmp #255
    bne NotEndOfSong
    iny
    lda (MiscPtr+2),Y
    cmp #255
    bne NotEndOfSong
    ;--change.  If 255, next byte tells us what to change songindex to.
    iny
    lda (MiscPtr+2),Y
    sta SongIndex
    tay
    jmp BackToBeginningOfSong
NotEndOfSong
    ;--when getting new note, save new random number at beginning of every measure (i.e., when SongIndex & $F = 0)
    ;--but again, only for channnel 0
    lda Temp+10
    bne NoChangeToSavedRandomNumber
    lda SongIndex
    and #$F
    bne NoChangeToSavedRandomNumber
    lda RandomNumber
    sta RandomNumberSaved
NoChangeToSavedRandomNumber
       ;have to reload, kind of inefficient
    lda SongIndex
    and #$F0
    lsr
    lsr
    lsr
    tay
    lda (MiscPtr+2),Y
    sta MiscPtr+4
    iny
    lda (MiscPtr+2),Y
    sta MiscPtr+5
    lda SongIndex
    and #$0F
    tay
    lda (MiscPtr+4),Y    
    and #VOLUME_BITS
    sta Temp+1
    lda (MiscPtr+4),Y
    lsr
    lsr
    lsr
    lsr
    tay
    lda DistortionTable,Y
    sta Temp+2
    lda FrequencyTable,Y
    sta Temp+11
GetNewPercussion
    jsr GetPercussionSound  ;returns with A holding percussion value *AND* flags set based on what that value is
    beq NoPercussion
    tay
    lda PercussionVolumeTable,Y
    sta Temp+1
    lda PercussionDistortionTable,Y
    sta Temp+2
    lda PercussionFrequencyTable,Y
    sta Temp+11


NotDownBeat


PlayMusicSound


    ldx Temp+10     ;??legit?

    lda Temp+1
    sta AUDV0,X
    lda Temp+2
    sta AUDC0,X
    lda Temp+11
    sta AUDF0,X
DoNotEndPercussion
NoPercussion
	rts

	
;****************************************************************************
    SUBROUTINE
GetPercussionSound  ;trashes Y, A.  Returns with percussion value (0-3) in A
                    ;uses Temp, MiscPtr, MiscPtr+1
                    ;depends on MiscPtr+2 pointing at melody data
    lda Temp+10 ;index into which channel
    beq .Channel0
    ;--channel 1 has no percussion
    lda #0
    rts    
.Channel0           
    lda SongIndex
    and #$F0
    lsr
    lsr
    lsr
    tay
    lda (MiscPtr+2),Y
    sta MiscPtr+4
    iny
    lda (MiscPtr+2),Y
    sta MiscPtr+5
    lda SongIndex
    and #$0F
    tay
    lda (MiscPtr+4),Y
    ;--if non-zero value (i.e., a note), play beat with kick
    ;   if zero (i.e., no melody), play beat without kick
    beq SnareBeatsOnly
    lda #2
SnareBeatsOnly
    sta Temp
    ;now pick randomly from various related drum beats
;     lda RandomNumber
    lda RandomNumberSaved
    and #3
    tay
    lda Temp
    clc
.loop
    dey
    bmi .loopdone
    adc #4
    bne .loop
.loopdone
    tay    
    lda SongIndex
    and #$0F
    cmp #12
    bcc BeatPattern
    ;--else fill
    lda FillPatternTable,Y
    sta MiscPtr
    lda FillPatternTable+1,Y
    sta MiscPtr+1
    bne LoadDrumPattern ;branch always
BeatPattern
    lda BeatPatternTable,Y
    sta MiscPtr
    lda BeatPatternTable+1,Y
    sta MiscPtr+1
LoadDrumPattern 
    lda SongIndex   
    and #3
    lsr         ;divide SongIndex by 2, but we will use the carry bit below!
    tay
    lda (MiscPtr),Y
    bcs NoShiftToRhythmPattern  ;if SongIndex & 1 == 1 then we want right nibble, else we want left nibble
    lsr
    lsr
    lsr
    lsr
NoShiftToRhythmPattern
    sta Temp
    lda FrameCounter
    and #%00000100
    bne SixteenthUpBeat
    lsr Temp
    lsr Temp
SixteenthUpBeat
    lda #%00000011
    and Temp
    
    rts
    
    /*

    ;--this is to make sure the "DivideLoop" doesn't cross a page boundary.
    if ((* + 5) & $FF00) != ((* + 9) & $FF00)
        echo "---Aligned PositionASpriteSubroutine -", $FF - ((* + 4) & $FF), "bytes left at location", *
        ds $FF - ((* + 4) & $FF)
    else
        echo "---Aligned PositionASpriteSubroutine not necessary"
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
*/
	
/*	
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
*/	
;****************************************************************************




; DivideByTwoSubroutine		;come in with number in accumlator
; 	and #$FF		;set flags
; 	clc
; 	bpl DivideByTwoPositive
; 	;--else negative
; 	sec
; DivideByTwoPositive
; 	ror
; 	rts
	
	
;****************************************************************************

    
;****************************************************************************

EliminateDiagonalSubroutine
	;--trashes Y.
	;--X is index into which tank
	;--A holds direction of joystick (filtered to allowed directions)
	;--now get directions down to a single direction (i.e., no diagonals)
	;--first, determine if trying to move diagonally:
	;	need to count cleared bits in the top nibble
; 	ldy #0					;will count bits set with Y     UNNECESSARY
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

ReadControllersSubroutine

	lda #0
	pha			;movement flag onto stack
	
	lda SWCHA	;A holds joystick
	and #$F0	;clear bottom nibble
	cmp #$F0
    bne TryingToMove
    ;--not trying to move, so just ... exit?  no!  have to go check if we are trying to shoot!
    ;--also clear speed bits in TankStatus so we know it isn't trying to move.
    lda TankStatus
    and #~TANKSPEED
    sta TankStatus
    ;--stop engine sound
    pla ;--pop movement flag (=0) off stack
;     sta AUDV0
    jmp PlayerTankBulletFiring
TryingToMove
SkipReadingControllers
; 	ldx #PLAYERTANKENGINEVOLUME
; NotTryingToMove
; 	stx AUDV0	
;     tax ;save joystick directions
;     ldy #PLAYERTANKENGINESOUND
;     jsr StartSoundSubroutine
;     txa ;restore joystick directions


	sta Temp			;--save original desired direction of tank
    ;--and set player speed which we set to zero when he stopped
    ;--set tank speed lower if player off left edge of screen
    lda TankStatus
    and #TANKDIRECTION
    ora #PLAYERTANKSPEED
;     ldx TankX
;     cpx #16
;     bcs RegularPlayerTankSpeed
;     ;and #$F1          ;change from TANKSPEED3 to TANKSPEED1
; RegularPlayerTankSpeed    
    sta TankStatus 

	ldx #0		;index into which tank	

        
	lda Temp
	pha         ;push desired directions onto stack (player tank)
	
	jsr CheckForWallSubroutine		;--returns with allowed directions, of the desired directions, in A
	.byte $24	;--skip next byte
TankMovementSubroutine				;jump here when moving enemy tanks, with movement flag already on top of stack)
	pha			;--save desired direction of tank (enemy tank - skipped when player tank)
	
	;--if no directions are allowed, but tank is trying to move, turn tank but do not move.
	and #$F0	;clear bottom nibble
	cmp #$F0	;no movement = $F0
	bne MovementAllowed
	txa
	pha		;save index into which tank
	tsx
	lda #$FF
	sta $03,X	;update movementflag
	pla
	tax			;restore tank index
	jmp ProcessDirections
MovementAllowed
	pha		;save allowed directions (stack holds [top down]: allowed directions, desired directions, movement flag, return address)
	
	
; 	txa
; 	pha		;save index into which tank
    stx Temp    ;save index into which tank (Temp used above and below, but unused here)
	tsx
	lda $01,X	;load allowed directions (see above)
	sta $02,X	;overwrite desired directions (either joystick (if player) or calc direction if enemy)
; 	pla			;pull tank index off stack
; 	tax			;restore tank index
    ldx Temp    ;restore tank index
	pla			;pulls allowed directions off as it is duplicative
ProcessDirections
	pla			;pulls final directions off stack (directions that are both allowed and desired) 
	            ;stack at this point holds (top down): movement flag, return address

	tay	;--save direction in Y
	
    pla             ;get movement flag into accumulator
    sta Temp
    bit Temp        ;movement flag in overflow
	tya
	pha             ;push Y (direction(s) tank wants to go) onto stack
	;--if player tank, see if tank is turning and update TankFractional accordingly, then play tank engine sound
	cpx #0
	bne NoTankSoundForEnemyTanks
	;--if tank is turning, set fractional variable so tank moves immediately -- I think this might be superfluous, since
	;   we only actually turn when TankFractional overflows (but we ... aren't updating when we turn?).  Ok, not superfluous but only for tank 0 (player)
	and #TANKDIRECTION
	eor #$F0        ;flip bits
	sta Temp
	lda TankStatus,X
	and #TANKDIRECTION
	cmp Temp
	beq TankNotTurning
TankIsTurning	
	;tank turning
	lda #$FF
	sta TankFractional,X    ;+24        (shorter if tank isn't turning)
TankNotTurning
	
	lda Channel1Decay
	and #$7F
	cmp #PLAYERTANKENGINEVOLUME
	bcs DoNotStartPlayerTankSound
	ldy #PLAYERTANKENGINESOUND
	jsr StartSoundSubroutine
DoNotStartPlayerTankSound
	
NoTankSoundForEnemyTanks
	
	pla						;get saved directions back in A	
	asl
	bcs NoRight
	bvs TurnRightward		;overflow holds movement flag (set=no movement (just turn tank))
	jsr TankFractionalAddition
	lda TankX,X
	adc #0
	sta TankX,X
TurnRightward	
	lda TankStatus,X
	and #~TANKDIRECTION
	ora #TANKRIGHT
	sta TankStatus,X
	bne DoneMoving  ;branch always 
NoRight
	asl
	bcs NoLeft
	bvs TurnLeftward		;overflow holds movement flag (set=no movement)
	jsr TankFractionalAddition
	bcc TurnLeftward	;but don't move -- since TankFractional always adds, we can't then SBC #0 and use the carry flag here
	dec TankX,X
TurnLeftward
	lda TankStatus,X
	and #~TANKDIRECTION
	ora #TANKLEFT
	sta TankStatus,X
	bne DoneMoving  ;branch always 
NoLeft
    asl
    bcs NoDown
	bvs TurnDownward		;overflow holds movement flag (clear=no movement)
	jsr TankFractionalAddition
	bcc TurnDownward	;but don't move down
	dec TankY,X
TurnDownward
	lda TankStatus,X
	and #~TANKDIRECTION
	ora #TANKDOWN
	sta TankStatus,X
	bne DoneMoving  ;branch always
NoDown
    asl
    bcs NoUp
	bvs TurnUpward			;overflow holds movement flag (clear=no movement)
	jsr TankFractionalAddition
	lda TankY,X
	adc #0
	sta TankY,X
TurnUpward
	lda TankStatus,X
	and #~TANKDIRECTION
	ora #TANKUP
	sta TankStatus,X
NoUp
DoneMoving
    ;--if enemy tank we exit now... I think.  right?  this bullet-firing routine is specific to the player, enemy bullets are fired elsewhere
    txa ;set flags based on X
    beq PlayerTankBulletFiring
    rts
PlayerTankBulletFiring    
    
    
	;--don't allow player to fire when offscreen or when not in play (i.e., when an explosion lol)
	lda TankStatus
	and #TANKINPLAY
	beq PlayerExplodingCannotShoot
	lda TankX
	cmp #16
	bcs PlayerOnScreenCanShoot
PlayerExplodingCannotShoot	
	jmp NotFiring
PlayerOnScreenCanShoot
	;are any balls available for firing
	ldx #1
FindAvailableBallLoop
	lda BulletX,X
	;cmp #BALLOFFSCREEN ;<--this value is zero
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
	and #$78
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
	
	
	;--set initial position at approx center of tank (will be adjusted below depending on which way the tank is facing)
	lda TankX,Y
	clc
	adc #4
	sta Temp
	lda TankY,Y
	sec
	sbc #3
	sta Temp+1
	
	;--clear old direction
	lda BulletDirectionClear,X
	eor #$FF
	and BulletDirection
	sta BulletDirection 

	
	;--then set new direction:
	lda TankStatus,Y
	asl ;--get J0RIGHT into carry
	bcc NotFiringRight
FiringRight	
	;--shooting right
	lda BulletDirection
	ora BulletRight,X
	sta BulletDirection
	lda Temp
	clc
	adc #6
	sta Temp
    jmp DoneFiring
NotFiringRight
    asl ;--get J0LEFT into carry
    bcc NotFiringLeft
	;--shooting left
	lda BulletDirection
	ora BulletLeft,X
	sta BulletDirection
	lda Temp
	sec
	sbc #7
	sta Temp
	jmp DoneFiring
NotFiringLeft
    asl ;--get J0DOWN into carry
    bcc NotFiringDown
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
    asl ;--get J0UP into carry
;     bcc NoDirectionNotFiringAtAll   ;is this necessary?  
    bcc FiringRight         ;if we are shooting and no directions, then we are stuck and facing right.
	;--shooting up
	lda BulletDirection
	ora BulletUp,X
	sta BulletDirection
	lda Temp+1
	clc
	adc #5
	sta Temp+1
	jmp DoneFiring
DoneFiring
	lda Temp
	sta BulletX,X
	lda Temp+1
	sta BulletY,X
NoDirectionNotFiringAtAll
NotFiring
NoAvailableBalls	
TriggerNotDebounced

	rts
	

	

;****************************************************************************

    SUBROUTINE
    
IsTankOnScreenBank0
    ;--X holds tank number to check
    ;--return in accumulator:  0 if offscreen, 1 if onscreen
    lda TankX,X
    cmp #TANKONSCREENLEFT
    bcc .TankOffScreen
    cmp #TANKONSCREENRIGHT+1
    bcs .TankOffScreen
    lda TankY,X
    cmp #TANKONSCREENTOP+2
    bcc .TankOnScreen    
.TankOffScreen
    lda #0
    rts
.TankOnScreen
    lda #1
    rts
;****************************************************************************


;     SUBROUTINE
;     
; IsTankPartiallyOnScreenBank0
;     ;--X holds tank number to check
;     ;--return in accumulator:  0 if offscreen, 1 if onscreen
;     lda TankX,X
;     cmp #TANKONSCREENLEFT-7
;     bcc .TankOffScreen
;     cmp #TANKONSCREENRIGHT+1+7
;     bcs .TankOffScreen
;     lda TankY,X
;     cmp #TANKONSCREENTOP+2+TANKHEIGHT
;     bcc .TankOnScreen    
; .TankOffScreen
;     lda #0
;     rts
; .TankOnScreen
;     lda #1
;     rts
;****************************************************************************



	
	
;****************************************************************************
	

StartSoundSubroutine
	
	lda Channel1Decay
	and #$70
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
    
    ;--possible optimization: 
    ;   right now, it checks the same directions for all on-screen tanks.
    ;   could short-circuit that, e.g., if tank 3 is preventing upward movement, skip upward check for tank 2.
    ;   if cleverly coded, could shave some cycles off the routine in certain cases, 
    ;   at the expense of additional code length, and likely making it take longer in other cases
    ;   If timing gets really tight could investigate.

    stx Temp+1  ;--save index into current tank
    sta Temp ;save directions
    ;--moving left.
    ldy #3
.CheckForEnemyTankLoop
    cpy Temp+1  ;--don't bother comparing tank to itself
    bne .NotComparingTankToItself
    jmp .NoCompareTankToItself
.NotComparingTankToItself
    ;--see if enemy tank is offscreen
    lda TankStatus,Y
    lsr                 ;get TANKINPLAY bit into carry
    bcs .TankOnScreenComparePositions
    jmp .TankOffScreenIgnore
.TankOnScreenComparePositions 
    lda Temp
    and #J0LEFT
    bne .NotMovingLeft
    ;--moving left:  CHECK FOR TANK TO THE LEFT
    ;see if an enemy tank within 2 blocks (16 pix) to the left
    lda TankX,X
;     sec       ;carry set following not-taken BCC branch above
    sbc #1      ;--have to subtract one so equal values are ignored
    sbc TankX,Y
    cmp #15
    bcs .ThisTankNotToLeft
    ;if so, see if that tank is between just under 2 blocks up and 2 blocks down.
    ;   block height = 7.  So add 13, subtract other tank Y, then compare to 26
    lda TankY,X
;     clc       ;carry clear following not-taken BCS branch above
    adc #14     ;actual value is +13 but carry-subtraction below subtracts one more than the TankY
;     sec       ;skip because we adjust above
    sbc TankY,Y
    cmp #26
    bcs .ThisTankNotToLeft
    ;--there is a tank there!  can't move left!
    lda Temp
    ora #J0LEFT
    sta Temp
.ThisTankNotToLeft    
.NotMovingLeft
    lda Temp
    and #J0RIGHT
    bne .NotMovingRight
    ;--moving right:  CHECK FOR TANK TO THE RIGHT
    ;see if an enemy tank within 2 blocks (16 pix) to the right
    lda TankX,Y
    sec     
    sbc #1      ;--have to add one so equal values are ignored
    sbc TankX,X
    cmp #15
    bcs .ThisTankNotToRight
    ;if so, see if that tank is between just under 2 blocks up and 2 blocks down.
    ;   block height = 7.  So add 13 (2-times block height minus 1), subtract other tank Y, then compare to 26
    lda TankY,X
;     clc       ;carry clear following not-taken BCS branch above
    adc #14     ;actual value is +13 but carry-subtraction below subtracts one more than the TankY
;     sec       ;skip because we adjust above
    sbc TankY,Y
    cmp #26
    bcs .ThisTankNotToRight
    ;--there is a tank there!  can't move right!
    lda Temp
    ora #J0RIGHT
    sta Temp
.ThisTankNotToRight    
.NotMovingRight
    lda Temp
    and #J0UP
    bne .NotMovingUp
    ;--moving up:  CHECK FOR TANK ABOVE
    ;see if an enemy tank within 2 blocks (14 pix) to the top
    lda TankY,Y
    sec       
    sbc #1      ;--have to add one so equal values are ignored
    sbc TankY,X
    cmp #13     ;we want to see if Y2-Y1<=14.  Since we subtracted one above, we want to see if (Y2-1)-Y1<=13
    bcs .ThisTankNotToUp
    ;if so, see if that tank is between just under 2 blocks left and 2 blocks right.
    ;   block width = 8.  So add 15 (2-times block width minus 1), subtract other tank Y, then compare to 30
    lda TankX,X
;     clc       ;carry clear following not-taken BCS branch above
    adc #16     ;actual value is +15 but carry-subtraction below subtracts one more than the TankY
;     sec       ;skip because we adjust above
    sbc TankX,Y
    cmp #30
    bcs .ThisTankNotToUp
    ;--there is a tank there!  can't move up!
    lda Temp
    ora #J0UP
    sta Temp
.ThisTankNotToUp    
.NotMovingUp

.CheckMovingDown
    lda Temp
    and #J0DOWN
    bne .NotMovingDown
    ;--moving down:  CHECK FOR TANK BELOW
    ;see if an enemy tank within 2 blocks (14 pix) to the bottom
    lda TankY,X
    sec       
    sbc #1      ;--have to add one so equal values are ignored
    sbc TankY,Y
    cmp #13     ;we want to see if Y2-Y1<=14.  Since we subtracted one above, we want to see if (Y2-1)-Y1<=13
    bcs .ThisTankNotToDown
    lda TankX,X
;     clc       ;carry clear following not-taken BCS branch above
    adc #16     ;actual value is +15 but carry-subtraction below subtracts one more than the TankY
;     sec       ;skip because we adjust above
    sbc TankX,Y
    cmp #30
    bcs .ThisTankNotToDown
    ;--there is a tank there!  can't move up!
    lda Temp
    ora #J0DOWN
    sta Temp
.ThisTankNotToDown    
.NoCompareTankToItself
.TankOffScreenIgnore
.NotMovingDown
    dey
    beq .DoneWithEnemyTankChecks
    jmp .CheckForEnemyTankLoop
.DoneWithEnemyTankChecks

    lda Temp    ;restore allowed directions to Accumulator

    rts


;****************************************************************************


    SUBROUTINE
CheckForWallSubroutine
				;--X holds index into which tank, A holds direction (mask against joystick constants)
				;return A holding allowed directions
				
		;I wonder if this would be faster to just check all four directions every time, but loop through block positions
		;rather than looping through pixel positions and calling "IsBlockAtPosition" every time.
	
	sta Temp        ;save directions
	
	;--divide by 7
	lda TankY,X ;technically should subtract 1 first but the divide by 7 will drop the remainder so we're cool
	tay
	lda DivideBySevenBank0,Y
	
; 	sec
; 	sbc #1      ;adjust so zero indexed  -- remove this because initial adjustment is to add 1 here.          
    sta Temp+6  ;block Y (row)

    txa
    pha ;save tank index
       
    lda #>PF1Left           ;<--this value is zero
    sta MiscPtr+1
	;--get brick X and Y coordinates of Tank
	lda TankX,X
    lsr
    lsr
    lsr
	sec
	sbc #2
	sta Temp+2  ;block X (column)
    ;A needs to hold Temp+2 (column) when we enter loop
    
    ldx #3
    clc     ;need to clear ahead of ADC after branch
    bcc .EntryPoint ;branch always.  first time through, the column shift is zero and the row shift is +1, but we do that already (see above)
.LookForBrickLoop
    ;move our brick coords to the next brick to check
    lda Temp+6
    clc
    adc CheckForBrickRowShift,X
    sta Temp+6
    lda Temp+2
    clc
    adc CheckForBrickColumnShift,X      
    and #$1F
    sta Temp+2
    ;--check if column > 15 (means off left or right side of maze)
    cmp #16
    bcs .YesBrick
.EntryPoint
    ;--now brick to check is at coords (Temp+2, Temp+6)
    ;--adjust MiscPtr now even though we don't need it if on row zero.
    tay         ;A holds Temp+2 (column), get into Y
    lda PFRegisterLookup,Y
    ;--carry clear following not-taken BCS above
    adc #<PF1Left
    sta MiscPtr
        
    ;--check if we are checking the bottom row
    lda Temp+6
    bne .NotRowZero
.RowZero
    ;--on row zero, so just check the two specific bricks in that row:
    lda Temp+2
    cmp #6
    beq .CheckBrick6_0
    cmp #9
    bne .NoBrick
    ;--else check brick (9, 0)
    lda LastRowR
    beq .NoBrick
    bne .YesBrick   ;branch always
.CheckBrick6_0
    lda LastRowL
    beq .NoBrick
    bne .YesBrick   ;branch always
.NotRowZero
    ;--check if we are off top or bottom of maze
    cmp #MAZEROWS
    bcs .YesBrick
    ;--ok, we are in the maze.  Check specific brick:
    ;--Y still holds the column at this point
    lda PFMaskLookup,Y
    ldy Temp+6  ;row
.NotRowZeroSecondCheck
    dey         ;this is necessary because the indexed PF data starts at row 1 (row zero is special case)
    and (MiscPtr),Y
    beq .NoBrick
.YesBrick
    lda Temp        ;get directions back in accumulator
    ora DirectionBlock,X
    sta Temp
.NoBrick   
    ;--if column is unchanged (i.e., first time through the loop), just adjust Y and check again
    cpx #3
    bne .SkipSecondCheck
    ldy Temp+2  ;reload column
    lda PFMaskLookup,Y
    ldy Temp+6  ;reload row
    dex
    dey
    dey     ;adjust again to tile below player
    bmi .YesBrick   ;off bottom of maze
    beq .RowZero
    bne .NotRowZeroSecondCheck  ;branch always
.SkipSecondCheck
    dex
    bpl .LookForBrickLoop

    pla
    tax             ;restore tank index 
    
    lda Temp        ;get updated directions back in A
    
    

	rts


;****************************************************************************

	;come in with amount to increase in A (units and tens) and Temp (100s and 1000s)
IncreaseScoreSubroutineBank0
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
	SUBROUTINE
PowerUpBonusRoutine

    ;--if player tank not in play, immediately return
    lda TankStatus
    lsr
    bcc .PlayerNotInPlayNoBonus

    ;--routine for now - if player kills half the tanks on the level (or the tanks die) without dying, player gets speed boost.
    ;--new routine: if powerup activated, player gets speed boost 
    lda MazeGenerationPass
    and #POWERUPACTIVATEDBITS
    beq .PowerUpNotActivated
;     cmp #%10000000
;     bne .OnlySpeedBonus
;     
;     ;--kill all tanks (also gives score bonus there)
;     brk
;     .word KillAllTanksBonus
;     ;--also play some kind of sound effect
;     
;     
;     lda MazeGenerationPass
;     and #~POWERUPACTIVATEDBITS
;     ora #%01000000
;     sta MazeGenerationPass
; .OnlySpeedBonus    
    ;--don't apply powerup if tank isn't moving!
    lda TankStatus
    and #TANKSPEED
    beq .PlayerNotMovingNoSpeedBoost   
    lda TankStatus
    and #~TANKSPEED
    ora #PLAYERTANKSPEED+SPEEDBONUS
    sta TankStatus
.PlayerHasDiedNoBonus    
.PowerUpNotActivated
.PlayerNotInPlayNoBonus
.PlayerNotMovingNoSpeedBoost
    rts

    
;****************************************************************************

TankFractionalAddition	
	lda TankStatus,X
	asl
	asl
	asl
	asl
	ora #TANKSPEEDADDER
    clc
    adc TankFractional,X
	sta TankFractional,X
    rts
    
;****************************************************************************
	
MoveBulletSubroutine
    
;     lda BulletFractional
;     clc
;     adc #BULLETFRACTIONALSPEED
;     sta BulletFractional
;     ;if carry clear, we didn't overflow BulletFractional and we aren't moving any bullets this frame so just return early
;     bcc DoNotMoveBulletsThisFrame
    ;--new hotness, move bullets 1 pixel every frame.  no fractional bullet speeds 
    
    
    ;--new new hotness, move only the bullet we will be displaying
    
    lda FrameCounter
    and #3
    tax
    
; 	ldx #3
; MoveBulletsLoop
	lda BulletX,X
	;cmp #BALLOFFSCREEN     == 0
	beq NoBulletMovement;BulletOffScreen;
	lda BulletDirection
	and BulletDirectionMaskBank0,X
	cmp BulletUp,X
	bne BulletNotUp
	;--move bullet up
	lda BulletY,X
	clc
	adc #BULLETVERTICALSPEED
	sta BulletY,X
; 	inc BulletY,X
    bne CheckBulletIsOffScreenVertical      ;branch always
BulletNotUp
	cmp BulletDown,X
	bne BulletNotDown
	;--move bullet down
;     dec BulletY,X
    lda BulletY,X
    sec
    sbc #BULLETVERTICALSPEED
    sta BulletY,X
;     lda BulletY,X
    ;--Y values range from 0 to about 80, so negative means we went off screen low
    bmi BulletOffScreen
	beq BulletOffScreen                     ;if Y value is zero, then bullet is also offscreen
    bne BulletOnScreen                      ;branch always
BulletNotDown
	cmp BulletRight,X
	bne BulletNotRight
	;--move bullet right
; 	inc BulletX,X
    lda BulletX,X
    clc
    adc #BULLETHORIZONTALSPEED
    sta BulletX,X
    bne CheckBulletIsOffScreenHorizontal    ;branch always
BulletNotRight	
	cmp BulletLeft,X
	bne NoBulletMovement                    ;is this branch ever taken?  it isn't.  bullets are always moving, and if we are here the bullet is onscreen and will be moved.
	;--move bullet left
;     dec BulletX,X
    lda BulletX,X
    sec
    sbc #BULLETHORIZONTALSPEED
    sta  BulletX,X
	;--check for off screen:
CheckBulletIsOffScreenHorizontal
	lda BulletX,X
	cmp #17
	bcc BulletOffScreen
	cmp #144
	bcs BulletOffScreen
	bcc BulletOnScreen      ;branch always
CheckBulletIsOffScreenVertical
	lda BulletY,X
	cmp #(MAZEAREAHEIGHT)+4
	bcc BulletOnScreen	
BulletOffScreen
    lda #BALLOFFSCREEN
    sta BulletX,X
    sta BulletY,X

; 	lda #BALLOFFSCREEN
; 	sta BulletX,X       ;<--is this necessary?
; 	sta BulletY,X	
	;--make noise for when bullet hits outer wall
	ldy #WALLSOUND
	jsr StartSoundSubroutine
NoBulletMovement
BulletOnScreen
; 	dex
; 	bpl MoveBulletsLoop
DoNotMoveBulletsThisFrame
; 	jmp ReturnFromBSSubroutine2
    rts

	
	
;****************************************************************************
    
;----------------------------------------------------------------------------
;-------------------------Data Below-----------------------------------------
;----------------------------------------------------------------------------
	
PreventReverses ;= *-1	;--the FF are wasted bytes EXCEPT for the first!
    .byte $FF
	.byte 	~J0DOWN, ~J0UP, $FF, ~J0RIGHT, $FF, $FF, $FF, ~J0LEFT
ReverseDirection = *-1	;--the FF are wasted bytes
	.byte 	J0DOWN, J0UP, $FF, J0RIGHT, $FF, $FF, $FF, J0LEFT

	
BulletDirectionMaskBank0
	.byte %11, %11<<2, %11<<4, %11<<6
	
			
	PAGEALIGN 1
	
	
BulletRight
	.byte	BULLETRIGHT, BULLETRIGHT<<2, BULLETRIGHT<<4, BULLETRIGHT<<6 ;00 00 00 00

TankTargetAdjustmentX = * - 4 ; uses 4 bytes of previous table
;     .byte 0, 0, 0, 0
    .byte -PINKYADJUSTMENTX, 0, 0, 0
    .byte PINKYADJUSTMENTX
TankTargetAdjustmentY 
    .byte 0, PINKYADJUSTMENTY, -PINKYADJUSTMENTY, 0
    .byte 0, 0, 0, 0
    .byte 0
PFRegisterLookup = * - 4
; 	.byte 0, 0, 0, 0
	.byte MAZEROWS-1, MAZEROWS-1, MAZEROWS-1, MAZEROWS-1
	.byte (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2
	.byte (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3

Tone
	.byte BRICKSOUNDTONE, BULLETSOUNDTONE, ENEMYTANKSOUNDTONE, SHORTBRICKSOUNDTONE
	.byte LONGEXPLOSIONTONE, ENEMYBULLETSOUNDTONE, WALLSOUNDTONE, PLAYERTANKENGINETONE
	.byte SCORESOUNDTONE, POWERUPEXPLOSIONSOUNDTONE, SPEEDBOOSTSOUNDTONE, INVINCIBLETANKSOUNDTONE

Frequency
	.byte BRICKSOUNDFREQ, BULLETSOUNDFREQ, ENEMYTANKSOUNDFREQ, SHORTBRICKSOUNDFREQ
	.byte LONGEXPLOSIONFREQ, ENEMYBULLETSOUNDFREQ, WALLSOUNDFREQ, PLAYERTANKENGINEFREQ
	.byte SCORESOUNDFREQ, POWERUPEXPLOSIONSOUNDFREQ, SPEEDBOOSTSOUNDFREQ, INVINCIBLETANKSOUNDFREQ

SoundLength
	.byte BRICKSOUNDLENGTH, BULLETSOUNDLENGTH, ENEMYTANKSOUNDLENGTH, SHORTBRICKSOUNDLENGTH
	.byte LONGEXPLOSIONLENGTH, ENEMYBULLETSOUNDLENGTH, WALLSOUNDLENGTH, PLAYERTANKENGINELENGTH
	.byte SCORESOUNDLENGTH, POWERUPEXPLOSIONSOUNDLENGTH, SPEEDBOOSTSOUNDLENGTH|$80, INVINCIBLETANKSOUNDLENGTH

	

;****************************************************************************
	
TankTitleScreenX
    .byte 20, 132, 132, 132
TankTitleScreenY
    .byte 20, 40, 30, 20
TankTitleScreenStatus
    .byte TANKRIGHT|TANKINPLAY|TANKSPEED2, TANKLEFT|TANKINPLAY|TANKSPEED2, TANKRIGHT|TANKINPLAY|TANKSPEED2, TANKUP|TANKDOWN ;83 43 83 30


	
BulletDirectionClear
BulletUp
	.byte	BULLETUP, BULLETUP<<2, BULLETUP<<4, BULLETUP<<6             ;03 0c 30 c0
BulletDown
	.byte	BULLETDOWN, BULLETDOWN<<2, BULLETDOWN<<4, BULLETDOWN<<6     ;02 08 20 80
BulletLeft
	.byte	BULLETLEFT, BULLETLEFT<<2, BULLETLEFT<<4;, BULLETLEFT<<6     ;01 04 10 40
DirectionBlock
;     .byte TANKLEFT, TANKRIGHT, TANKDOWN, TANKUP    ;=40, 80, 20, 10
TankDirection
	.byte TANKLEFT, TANKRIGHT, TANKDOWN, TANKUP ;=40, 80, 20, 10

CheckForBrickColumnShift    ;we'll go around top, bottom, right, left.   indexing in from last to first.  
                            ;First check (4th value) no adjustment needed; second check (third value) is adjusted manually in code.
                            ;So only two values are needed, and adjustments are from the top (not bottom!)
    .byte -2, 1
CheckForBrickRowShift
    .byte 0, -1  

	
PFMaskLookup
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0

PowerUpFrequencyTable
    .byte 15, 15, 15, 15
    .byte 15, 16, 17, 18
    .byte 19, 20, 21, 22
    .byte 23, 24, 25, 26
    .byte 27, 28, 29, 30
	
/*
    Notes on ramping difficulty.
    Basically three variables we can control, maybe four.
    1. Number of tanks player has to kill in a level
    2. Speed of enemy tanks
    3. Frequency of enemy tanks firing
    4. Shape of maze (more vertical paths = harder)

    1. starts at 8 and increases by 2 every level until it reaches maximum of 20 (on level 7)
        (Note: considering changing this to start at 8 and increase by 4 every level until it reaches maximum at level 4
    2. Enemy tanks have low base rates of speed that gradually increase, until all tanks begin the level at maximum speed starting at level 32 (or close to it)
        Also enemy tanks get speed boosts when XX tanks have been killed in a level (see table just below)
    3. This is increased by a table (immediately below) until it reaches a maximum.  
    4. Can control this kind of but issue is the change isn't noticeable except over large values.
    
    Overall an issue is the possible range of levels is 1-99, but the effective range of levels (at least right now) is 1-15, maybe.  I don't think I've ever gotten past level 13.
        
    


*/	

		
EnemyBulletDebounce ;these (value + 1) * 4 is number of frames between enemy bullet firing
;   (in other words: when value = 0, a bullet fires every 4 frames.  When value = 30, a bullet fires every 124 frames)
;a tank's initial speed changes on level plus 1 #s evenly divisible by 4  (i.e., 5, 9, 13, etc)
;so we'll reduce the bullet firing frequency on those levels, but DO increase it on the other levels.
;
;   30 is equivalent to firing  0.48 bullets / sec.  Or 1 bullet every 2.07 seconds
;   0 is equivalent to firing 15 bullets / sec  Or 1 bullet every 0.07 seconds
;   values below chosen to start at 30 and go to 0 in steps of as close to 5% as possible.
;   so for example, the change from 30 to 28 is a 6.90% increase in firing rate, and
;   18 to 17 is a 5.56% increase, and so on.  Obviously, near the end the increase is much greater.
;   E.g., going from 2 to 1 is a 50% increase.
    .byte 30, 30, 30, 30, 30    ;levels 1-5
    .byte 28, 26, 24, 26        ;levels 6-9
    .byte 24, 22, 21, 22        ;levels 10-13
    .byte 21, 20, 19, 20        ;levels 14-17
    .byte 19, 18, 17, 18        ;levels 18-21
    .byte 17, 16, 15, 16        ;levels 22-25
    .byte 15, 14, 13, 14        ;levels 26-29
    .byte 13, 12, 11, 12        ;levels 30-33
    .byte 11, 10, 9, 10         ;levels 34-37
    .byte 9, 8, 7, 8            ;levels 38-41
    .byte 7, 6, 5, 6            ;levels 42-45
    .byte 5, 4, 3, 4            ;levels 46-49
    .byte 3, 2, 1, 2            ;levels 50-53
    .byte 1, 0                  ;levels 54-55 and higher
EnemyBulletDebounceEnd
    
    ;--boost at 10 tanks killed
    ;--boost at 16, 17, 18, 19
TanksKilledSpeedBoost
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 2, 2
    .byte 2, 2, 2, 2
    .byte 6, 8, 10, 15



	
NewTankSpeed = *-1	;--don't use this for player tank, so don't need initial byte
	.byte ENEMYTANKBASESPEED0, ENEMYTANKBASESPEED1, ENEMYTANKBASESPEED2
	
	
	
	
	
	;tank 0 = player, so don't need initial byte
	;tank 1 (pinky) target = base (center bottom)
	;tank 2 (blinky) target = upper left corner
	;tank 3 (clyde) target = upper right corner
SwitchMovementX = * - 1
	.byte 80, 0, 255
SwitchMovementY = * - 3
	.byte /*0, 255, */255
	
	
NumberOfBitsSet
	.byte 0, 1, 1, 2
	.byte 1, 2, 2, 3
	.byte 1, 2, 2, 3
	.byte 2, 3, 3, 4
MovementMask
	.byte J0UP, J0DOWN, J0LEFT, J0RIGHT ;=10, 20, 40, 80
	
; RotationTablesBank1
; 	.word RotationEvenBank1, RotationOddBank1	
; 	
; RotationOddBank1
; 	.byte 0	;and first 3 bytes of next table
; RotationEvenBank1
; 	.byte 2, 1, 3, 0
; 
TitleScreenSongPointers
    .word TitleScreenSong, TitleScreenSongChannel1    

TitleScreenSongChannel1      ; ;--must be same length as matching channel 0  pattern
    .word SilencePattern
    .word SilencePattern
    
;     .word SilencePattern
;     .word Fanfare2PatternC1
;     .word Fanfare4Pattern
;     .word SilencePattern
    
    .word Fanfare4Pattern
    .word Fanfare2Pattern
    .word Fanfare4Pattern
    .word Fanfare3Pattern
    
    .word Fanfare4Pattern
    .word Fanfare5Pattern
    
    ;--don't need end-of-song bytes for channel 1

    
TitleScreenSong    
    .word SilencePattern
    .word SilencePattern
    
;     .word Fanfare1PatternC1    
;     .word Fanfare1PatternC1    
;     .word Fanfare2PatternC1    
;     .word SilencePattern    

    .word Fanfare3PatternC1    
    .word Fanfare4PatternC1   
    .word Fanfare3PatternC1   
    .word Fanfare5PatternC1   
    
    .word Fanfare2PatternC1
    .word Fanfare4Pattern
    ;silence
    ;1
    ;2
    ;3
    
    ;4
    ;5
    ;6
    ;7
    
    ;8
    
    
    .word $FFFF
    .byte 0<<4

    
/*

2 bars of silence (for explosion start)
throw out current 4-bar "intro"
keep 6-bars as "intro"
1 bar of silence
7 bars of something OR
    4 bars of something, 2 bar of silence and then repeat this forever

| silence | silence |
| intro | intro | intro | intro |
| intro | intro | silence |







*/    
    
    

SilencePattern
    .byte VOLUME0, VOLUME0, VOLUME0, VOLUME0
    .byte VOLUME0, VOLUME0, VOLUME0, VOLUME0
    .byte VOLUME0, VOLUME0, VOLUME0, VOLUME0
    .byte VOLUME0, VOLUME0, VOLUME0, VOLUME0
 
  
LevelStartFanfarePattern
    .byte VOLUME6|E4
    .byte VOLUME6|E4|ARTICULATE
    .byte VOLUME6|E4|ARTICULATE
    .byte VOLUME6|E4|ARTICULATE
    .byte VOLUME6|E4
    .byte VOLUME6|E4|ARTICULATE
    .byte VOLUME6|D4
    .byte VOLUME6|D4|ARTICULATE
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4|ARTICULATE
    
 
    
    
Fanfare1PatternC1
    .byte VOLUME4|A3
    .byte VOLUME4|A3|ARTICULATE
    .byte VOLUME4|A3|ARTICULATE
    .byte VOLUME4|A3|ARTICULATE
    .byte VOLUME4|A3
    .byte VOLUME4|A3|ARTICULATE
    .byte VOLUME4|G3
    .byte VOLUME4|G3|ARTICULATE
    .byte VOLUME4|A3
    .byte VOLUME4|A3
    .byte VOLUME4|A3
    .byte VOLUME4|A3
    .byte VOLUME4|A3
    .byte VOLUME4|A3
    .byte VOLUME4|A3
    .byte VOLUME4|A3|ARTICULATE
    
Fanfare2PatternC1
    .byte VOLUME4|E4
    .byte VOLUME4|E4|ARTICULATE
    .byte VOLUME4|E4|ARTICULATE
    .byte VOLUME4|E4|ARTICULATE
    .byte VOLUME4|E4
    .byte VOLUME4|E4|ARTICULATE
    .byte VOLUME4|D4
    .byte VOLUME4|D4|ARTICULATE
    .byte VOLUME4|E4
    .byte VOLUME4|E4
    .byte VOLUME4|E4
    .byte VOLUME4|E4
    .byte VOLUME4|E4
    .byte VOLUME4|E4
    .byte VOLUME4|E4
    .byte VOLUME4|E4|ARTICULATE
    
Fanfare3PatternC1
    .byte VOLUME4|A4
    .byte VOLUME4|A4|ARTICULATE
    .byte VOLUME4|A4|ARTICULATE
    .byte VOLUME4|A4|ARTICULATE
    .byte VOLUME4|A4
    .byte VOLUME4|A4|ARTICULATE
    .byte VOLUME4|A4
    .byte VOLUME4|A4|ARTICULATE
    .byte VOLUME4|A4
    .byte VOLUME4|A4
    .byte VOLUME4|A4
    .byte VOLUME4|A4
    .byte VOLUME4|A4
    .byte VOLUME4|A4
    .byte VOLUME4|A4
    .byte VOLUME4|A4|ARTICULATE
Fanfare4PatternC1
    .byte VOLUME6|A4
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|C5
    .byte VOLUME6|C5
    .byte VOLUME6|C5
    .byte VOLUME6|C5|ARTICULATE
    .byte VOLUME6|D5
    .byte VOLUME6|D5
    .byte VOLUME6|D5
    .byte VOLUME6|D5|ARTICULATE
Fanfare5PatternC1
    .byte VOLUME6|A4
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|A4
    .byte VOLUME6|A4|ARTICULATE
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4
    .byte VOLUME6|E4|ARTICULATE
    .byte VOLUME6|D4
    .byte VOLUME6|D4
    .byte VOLUME6|D4
    .byte VOLUME6|D4|ARTICULATE

         
Fanfare1Pattern
    .byte VOLUME6|A3
    .byte VOLUME6|A3|ARTICULATE
    .byte VOLUME6|A3|ARTICULATE
    .byte VOLUME6|A3|ARTICULATE
    .byte VOLUME6|A3
    .byte VOLUME6|A3|ARTICULATE
    .byte VOLUME6|D5
    .byte VOLUME6|D5|ARTICULATE
    .byte VOLUME6|A3
    .byte VOLUME6|A3
    .byte VOLUME6|A3
    .byte VOLUME6|A3
    .byte VOLUME6|A3
    .byte VOLUME6|A3
    .byte VOLUME6|A3
    .byte VOLUME6|A3|ARTICULATE
Fanfare2Pattern
    .byte VOLUME6|E5
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|D5
    .byte VOLUME6|D5|ARTICULATE
    .byte VOLUME6|G5
    .byte VOLUME6|G5
    .byte VOLUME6|G5
    .byte VOLUME6|G5|ARTICULATE
    .byte VOLUME6|Fs5
    .byte VOLUME6|Fs5
    .byte VOLUME6|Fs5
    .byte VOLUME6|Fs5|ARTICULATE

Fanfare3Pattern
    .byte VOLUME6|E5
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|D5
    .byte VOLUME6|D5|ARTICULATE
    .byte VOLUME6|B4
    .byte VOLUME6|B4
    .byte VOLUME6|B4
    .byte VOLUME6|B4|ARTICULATE
    .byte VOLUME6|D5
    .byte VOLUME6|D5
    .byte VOLUME6|D5
    .byte VOLUME6|D5|ARTICULATE
       
Fanfare4Pattern
    .byte VOLUME6|E5
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|E5
    .byte VOLUME6|E5|ARTICULATE
    .byte VOLUME6|D5
    .byte VOLUME6|D5|ARTICULATE
    .byte VOLUME6|E5
    .byte VOLUME6|E5
    .byte VOLUME6|E5
    .byte VOLUME6|E5
    .byte VOLUME6|E5
    .byte VOLUME6|E5
    .byte VOLUME6|E5
    .byte VOLUME6|E5|ARTICULATE

Fanfare5Pattern
    .byte VOLUME6|A5
    .byte VOLUME6|A5|ARTICULATE
    .byte VOLUME6|A5|ARTICULATE
    .byte VOLUME6|A5|ARTICULATE
    .byte VOLUME6|A5
    .byte VOLUME6|A5|ARTICULATE
    .byte VOLUME6|G5
    .byte VOLUME6|G5|ARTICULATE
    .byte VOLUME6|A5
    .byte VOLUME6|A5
    .byte VOLUME6|A5
    .byte VOLUME6|A5
    .byte VOLUME6|A5
    .byte VOLUME6|A5
    .byte VOLUME6|A5
    .byte VOLUME6|A5|ARTICULATE
    
    
        
LevelStartSong
    .word LevelStartFanfarePattern
    .word SilencePattern
    .word $FFFF
    .byte 1<<4
    
    
    
     
FillPatternTable
    .word DrumFillSnaresOnly1, DrumFillWithKick1
    .word DrumFillSnaresOnly2, DrumFillWithKick2
    .word DrumFillSnaresOnly3, DrumFillWithKick3
    .word DrumFillSnaresOnly1, DrumFillWithKick1
BeatPatternTable
    .word DrumBeatSnaresOnly1, DrumBeatWithKick1
    .word DrumBeatSnaresOnly1, DrumBeatWithKick1
    .word DrumBeatSnaresOnly1, DrumBeatWithKick1
    .word DrumBeatSnaresOnly1, DrumBeatWithKick1

 ;bits: each byte is four 32nds, reading left to right, 2-bit groups.
 ;  two-byte groups are each one beat
    ;00 = no percussion
    ;01 = snare 1
    ;02 = snare 2
    ;03 = kick
DrumBeatSnaresOnly1 
    .byte %10000000, %10000100

DrumFillSnaresOnly1
    .byte %10000100, %10000100
DrumFillSnaresOnly2
    .byte %10001001, %10000100
DrumFillSnaresOnly3
    .byte %10011001, %10000100

DrumBeatWithKick1 
    .byte %11000000, %10000100
DrumFillWithKick1
    .byte %11000100, %10000100
DrumFillWithKick2
    .byte %11001001, %10000100
DrumFillWithKick3
    .byte %11011001, %10000100
    

    ;for the following three tables, zero is not used as in index into them
PercussionVolumeTable = * - 1
    .byte PERCUSSIONVOLUME, PERCUSSIONVOLUME, PERCUSSIONVOLUME
PercussionDistortionTable = * - 1
    .byte SNARESOUND, SNARE2SOUND, KICKSOUND
PercussionFrequencyTable = * - 1
    .byte SNAREPITCH, SNARE2PITCH, KICKPITCH
    
    
	
	
	;--notes: 
	;   G5, F#5, E5, D5
	;   B4, E4, D4, A4
	;   F#4, A3, G3, C5
	;   A5
DistortionTable
    .byte SQUARESOUND, SQUARESOUND, SQUARESOUND, SQUARESOUND
    .byte SQUARESOUND, LEADSOUND, LEADSOUND, LEADSOUND
    .byte LEADSOUND, LEADSOUND, LEADSOUND, SQUARESOUND
    .byte SQUARESOUND

FrequencyTable
    .byte 19, 20, 23, 26
    .byte 31, 15, 17, 11
    .byte 13, 23, 26, 29
    .byte 17

    
TankDeadStatusBank0
    .byte TANKUP|TANKDEADWAITPLAYER, TANKUP|TANKDEADWAIT, TANKUP|TANKDEADWAIT, TANKUP|TANKDEADWAIT
    
ArticulationTable   ;--routine as currently written will never read the first and fourth values
	.byte 0,4, 2;, 0
	;.byte 0,0,0,0      ;--uses 5 bytes of next table
DivideBySevenBank0    ;blockheight = 7
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
    
	
TitleGraphicsBank0
    ;-----PF1--------PF2--------PF2--------PF1------
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
				
;------------------------------------------------------------------------------------------
	
    echo "----", ($1FE0-*), " bytes left (ROM) at end of Bank 1"

	org $1FE0
	rorg $1FE0
	
	;BRK vector comes here, call with:
	;   BRK
	;   .word AddressOfRoutine
	;routines can be moved to any bank (even the same bank as the calling routine) 
	;without changing how they are called.  
	;Banks must be RORGed to different (odd-numbered) banks ($1000, $3000, $5000, etc)
	;and first bank must be at $1000, second at $3000, etc.
	;Routines must return with:
	;   JMP ReturnFromBSSubroutine1 ;see below
	;Advantages:
	;   no jump tables to maintain
	;   routines can be moved from one bank to another with changing bank switching code
	;   bank switching code is fairly compact, approx 38 bytes.
	;   code to call a routine that may (or may not!) be in another bank is extemely compact,
	;       takes no more room than a regular JSR
	;   code to call a routine that may be in another bank is very readable, almost as readable
	;       as a regular JSR
	;   uses no RAM
	;Disadvantages
	;   38 bytes per bank can add up, hand-coded bank switching logic could potentially be more compact, at the expense of some clarity/flexibility
	;   bank switch code uses (briefly) 3 bytes of stack space, more than a JSR call, and trashes accumulator and X register.
	;   very slow:
	;       regular subroutine call: 6 cycles for JSR, 6 cycles for RTS for 12 total to call and return
	;       this bank-switching code:   65 cycles for call (BRK + code below), 29 to return for 94 cycles total
	;       admittedly, the proper comparison isn't to a JSR/RTS combination, but rather to some other bank-switching code, and
	;           there the comparison isn't quite as awful.  But still, the flexibility and clarity comes with a price.
BankSwitchSubroutine1 
	plp             ;+4         BRK pushed flags onto stack, pull off and discard
	tsx             ;+2     6
	dec $01,X       ;+6    
	lda ($01,X)     ;+6         low byte of routine we are jumping to
	sta MiscPtr     ;+3     21
	inc $01,X       ;+6
	lda ($01,X)     ;+6         high byte of routine we are jumping to
	sta MiscPtr+1   ;+3     36
;    	lsr
; 	lsr
; 	lsr
; 	lsr
; 	lsr
; 	tax             ;+12    48
; 	nop $1FF8,X     ;+5     53  uses top 3 bits of address to determine which bank to switch to
BankSwitchAltRoutine1
    nop $1FF9
	jmp (MiscPtr)   ;+9     45
	
ReturnFromBSSubroutine1
; 	tsx             ;+2
; 	lda $02,X       ;+4      6  get high byte of return address
; 	lsr
; 	lsr
; 	lsr
; 	lsr
; 	lsr
; 	tax             ;+12    18
; 	nop $1FF8,X     ;+5     23  uses top 3 bits of address to determine which bank to switch to
    nop $1FF9
	rts             ;+6     29
EndBankSwitchRoutine1


	;--reserve space for hot spots

    org $1FF8
    rorg $1FF8
    
    ds 2


	org $1FFC
    rorg $1FFC
    
	.word Start+3
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

	

;----------------------------------------------------------------------------
;----------------------Kernel Routine----------------------------------------
;----------------------------------------------------------------------------
	
KernelRoutineGame

; KernelSetupSubroutine




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
    ;Y is index into Tanks (0 is player tank, 1-3 are enemy tanks)
	lax (MiscPtr),Y     ;load X with index into which graphic object (0=P0, 1=P1, 2=M0, 3=M1)
	lda TankX,Y
	sta PlayerX,X
	lda TankY,Y
	sta PlayerY,X
	txa
	lsr		;--only setup gfx pointer when the tank is displayed with the player (i.e., X = 0 or 1)
	beq PlayerNotMissile
    jmp MissileNotPlayer	


PlayerNotMissile
	;--get correct tank gfx ptr set up
	txa
	asl
	asl     ;this clears carry
; 	tax		;we need X * 4
	sta Temp		;save X index
	;--if Y = 0 then we are doing player tank.  Check if not moving.
	tya ;set flags based on Y
	bne NotPlayerTankSkipNotMovingCheck
	;--If speed = 0, then player tank not moving (doesn't apply to enemy tanks!)
	lda TankStatus
	and #TANKSPEED
    beq SetPlayerTankNotMovingGraphic
    lda FrameCounter
    and #%00001100
    lsr             ;this clears carry
SetPlayerTankNotMovingGraphic           ;we branch here with carry clear and A=0
    adc #8
    bne TankGfxIndexSet ;branch always

NotPlayerTankSkipNotMovingCheck
	;--if title screen and player 2, then display maze graphic
	lda GameStatus
	and #TITLESCREEN
	beq NotOnTitleScreenDisplayRegularEnemyTanks
	cpy #2
	bne DisplayRegularTanks
	;--standard mazes or random mazes?
	lda GameStatus2
	and #RANDOMMAZES
	beq StandardMazeIcon
	lda FrameCounter
	lsr
	lsr
; 	lsr
	and #%00001110
	tax
	lda RandomMazeFrame,X
    pha
    lda RandomMazeFrame+1,X
    bne SetTankGfxPtr
; 	lda #<(RandomMazeImage+TANKHEIGHT)
; 	pha
; 	lda #>(RandomMazeImage+TANKHEIGHT)
; 	bne SetTankGfxPtr
StandardMazeIcon	
	lda #<(StandardMazeImage+TANKHEIGHT)
	pha
	lda #>(StandardMazeImage+TANKHEIGHT)
	bne SetTankGfxPtr                   ;branch always
DisplayRegularTanks
NotOnTitleScreenDisplayRegularEnemyTanks	
	;--get index into tank graphics image
	lda FrameCounter
    and #%00001100
    lsr
TankGfxIndexSet
	tax

	
	;--if tank is dead, use one specific gfx
	lda TankStatus,Y
	lsr     ;get TANKINPLAY bit into carry
	bcs TankInPlayUsualGraphics
	;--if direction = TANKDOWN use power up symbol
	and #TANKDOWN>>1        ;shifted over since we LSR above
	bne UsePowerUpGraphic
	;--else use explosion graphic
	lda #<(TankKilledImage+TANKHEIGHT)
	pha
	lda #>(TankKilledImage+TANKHEIGHT)
    bne SetTankGfxPtr   ;branch always
UsePowerUpGraphic
    ldx #0
    ;--set/clear TANKLEFT based on bit 4 of framecounter
    lda FrameCounter
    and #%00010000
    beq ClearTANKLEFT
    lda TankStatus,Y
    ora #TANKLEFT
;     sta TankStatus,Y
    bne SetPowerUpFrame ;branch always
ClearTANKLEFT
    lda TankStatus,Y
    and #~TANKLEFT
SetPowerUpFrame   
    sta TankStatus,Y
    
    lda FrameCounter
    and #%11100000         
    bne StaticPowerUpIcon
    lda FrameCounter
    and #%00001100
    lsr
    tax
StaticPowerUpIcon    
    lda PowerUpIconFrame,X
    pha
    lda PowerUpIconFrame+1,X
    bne SetTankGfxPtr   ;branch always

	
TankInPlayUsualGraphics
	lda TankStatus,Y
	and #TANKUP
	beq TankNotFacingUp
	
	lda TankUpFrame,X
	pha
	lda TankUpFrame+1,X
	bne SetTankGfxPtr   ;branch always
TankNotFacingUp
	lda TankStatus,Y
	and #TANKDOWN
	beq TankNotFacingDown
	lda TankDownFrame,X
	pha
	lda TankDownFrame+1,X
	bne SetTankGfxPtr   ;branch always
TankNotFacingDown
    ;--so tank is facing right or left, either way graphic image is the same
	lda TankRightFrame,X
	pha
	lda TankRightFrame+1,X
SetTankGfxPtr
    ldx Temp        ;reload index into which graphics object
	sta Player0Ptr+1,X
	sta Player0Ptr+3,X
	pla             ;pull lower byte of graphics address off of stack
	                ;and adjust for object Y value
	sec
	sbc TankY,Y
	sta Player0Ptr,X
	clc
	adc #TANKHEIGHT ;all graphics are stored, interlaced, in consecutive tables so we just add constant value to get the second ptr
	sta Player0Ptr+2,X
	bne EndRotationLoop ;branch always, none of the second image's location's lower byte is or could be zero.
MissileNotPlayer
    	
	;--adjust missile width and position
	;for code below, X index is into graphics objects (0=P0, 1=P1, 2=M0, 3=M1) so - 
	;   we subtract two from base address to just get missile RAM locations to update
    ;--if tank not in play, don't display missile
    lda TankStatus,Y
    and #TANKINPLAY|TANKDOWN|TANKUP
    cmp #TANKUP
    bne MissileOnScreen
    lda #TANKAREAHEIGHT+20
    sta MissileY-2,X
MissileOnScreen
  	lda MissileX-2,X
    clc
    adc #1
	sta MissileX-2,X
	lda #OCTWIDTHMISSILE
	sta NUSIZ0-2,X
	lda #TANKHEIGHT-2
	sta MissileHeight-2,X
	
	;--other missile-specific adjustments
	lda #TANKAREAHEIGHT+1
	sec
	sbc MissileY-2,X
	adc MissileHeight-2,X
	sta MissileYTemp-2,X
	
	lda MissileYTemp-2,X
	cmp #TANKHEIGHT-2
	bcs NoAdjustMissileY 
	adc #1
	sta MissileYTemp-2,X
NoAdjustMissileY	

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

	
	;--cycle BaseColor
	lda FrameCounter
	and #$0F
    ora #(BASECOLOR)&($F0)
	sta Temp+1

	;bullet flicker rotation:
	and #3  ;A still holds bottom 3 bits of FrameCounter
	tax
	lda BulletX,X
	sta BallX
	lda BulletY,X
	sta BallY
	
	;--set up score pointers
	
	ldx #10
	txa
SetupScorePtrsLoop
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
    txa
    sec
    sbc #4
    tax
	bpl SetupScorePtrsLoop
	;--room for score up here?

    
	;--no longer change color of score depending on game variations
;     lda GameStatus2
;     and #POWERUPSDISABLED
;     bne ScoreColorNoPowerUps
;     ldx #SCORECOLOR_POWERUPSENABLED
;     .byte $2C
; ScoreColorNoPowerUps
    ldx #SCORECOLOR_POWERUPSDISABLED
    
    
PreWaitForVblankEnd    
    nop
    

WaitForVblankEnd
	lda INTIM
	bpl WaitForVblankEnd
; 	bmi OverTimeVBLANK
; 	cmp #1
; 	bne WaitForVblankEnd
; 	beq EndVBLANK
; OverTimeVBLANK				;this nonsense is here just so I can trap an overtime condition in an emulator, if needed
; 	nop
; EndVBLANK	


	
	lda #0
	sta WSYNC
	sta VBLANK
; 	lda #SCORECOLOR
    nop
	stx COLUP0
	stx COLUP1						;+11    11
		
	

	
	;--display score
	lda #THREECOPIESCLOSE|LEFTONE
	sta VDELP0
	sta VDELP1						;+8		19		turn on VDELPx (bit0==1)

	sta NUSIZ0
	sta NUSIZ1						;+6		25

	SLEEP 4                         ;+4     29
	
	sta HMP0						  ;					 LEFTONE
	asl
	sta HMP1						 ;+8		37		LEFTTWO

	sta RESP0						 ;+8		40		positioned at 57 (move left 1)
	sta RESP1						 ;+3		43		positioned at 66 (move left 2)

    

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
	
	
	;--wall colors
	
    lda GameStatus          ;+3
    and #GAMEOVER           ;+2
;     beq RegularWallColor    ;+2/3   7/8
    bne FlashWallColor      ;+2/3   7/8
    lda #WALLCOLOR          ;+2     9
    bne SetWallColor        ;+3     12      branch always
FlashWallColor
    lda TankMovementCounter ;+3     11
    asl                     ;+2     13      get WALLDONEFLASHING bit into carry
    bcc WallFlashingColor   ;+2/3   15/16
    lda #0                  ;+2     17
WallFlashingColor           ;       16/17
    lsr                     ;+2     18/19
    and #$0F                ;+2     20/21
    ora #(WALLCOLOR&$F0)    ;+2     22/23
;     .byte $2C               ;+4     25/26   skip next two bytes
; RegularWallColor            ;       8       
;     lda #WALLCOLOR          ;+2     10
SetWallColor                ;       12/22/23 

    sta Temp+7              ;+3     26      Temp+7 overwrites the ScorePtr+4 and MiscPtr+4
    sta COLUPF              ;+3	    29
	;--reflect P0 or P1 as necessary.   prep for flip loop below

    lda FrameCounter
    and #1
    asl
    tay
	lda RotationTables,Y        ;RotationTablesBank1 if moved back to bank 1
	sta MiscPtr
	lda RotationTables+1,Y
	sta MiscPtr+1               ;+14    43

	;---reflect P0/P1
	;--this loop is garbage, need to rewrite so it is faster.... though not sure how, actually.
	ldy #3              ;+2     45
SetREFPLoop
	lax (MiscPtr),Y		;+5      5      get X index into graphics registers
	lda TankStatus,Y    ;+4      9
	and #TANKLEFT       ;+2     11
	beq EndREFPLoop     ;+2/3   13/14    only if facing left do we reflect
	cpx #2              ;+2     15
	bcs EndREFPLoop     ;+2/3   17/18
	lda #$FF            ;+2     19      1 or 0 (players) get reflected, 2 and 3 (missiles) do not
	sta REFP0,X	        ;+4     23
EndREFPLoop             ;       14/18/23
	dey                 
	bpl SetREFPLoop     ;+5     19/23/28
    ;loop minimum time is 76 cycles.  maximum is 101
    ;                   ;+76    121 (or 45)
    ;                   ;+101   146 (or 70)     cannot hit this point past cycle 73

	
	;Y is 255 here
    
    sta WSYNC   ;upper wall starts here
	
	ldx #$C0
	stx VDELP1                      ;               bit zero is clear, so clearing this
	stx PF0
	sty PF1
	sty PF2                         ;+14    14
	lda #ONECOPYNORMAL|OCTWIDTHMISSILE
	sta NUSIZ0
	sta NUSIZ1                      ;+8     22
	
SetTankColors	
	lda FrameCounter
	and #1
    tax
    lda P0M0Color,X
    sta COLUP0
    lda P1M1Color,X
    sta COLUP1	                    ;+21    43
	
	
	;save stack pointer
	tsx
	stx Temp                        ;+5     48
		
	ldx #4                          ;+2     50
; PositioningLoopVBLANK	
; 	lda PlayerX+4;,X
; 	jsr PositionASpriteSubroutineBank2     ;+9     59       need to reach subroutine 8 cycles before end of scanline (cycle 68)    
	                                       ;        9       subr returns at 9th cycle of scanline
	
    ;--VDELP0 is already on 
;     dex                 ;now X holds 3 which is what we want for loop below
PositioningLoop	
	lda PlayerX,X
	jsr PositionASpriteSubroutineBank2  ;this needs 14 cycles before end of scanline or it will wrap around and give an extra scanline
	dex
	bpl PositioningLoop     ;       13 cycles last time through
    
	
	
	inx                     ;X is now zero
	stx PF1                 ; Y is zero
	stx PF2					;
	sta CXCLR               ;+11    24

	;--use stack pointer to hit ENABL
	ldx #<ENABL
	txs						;+4		28
	
    nop                     ;+2     30
    
	ldy #TANKAREAHEIGHT		;+2		32

		
	jmp BeginMainMazeKernel ;+3     35

	
;----------------Some Data Stuck Here	
	
TankUpFrame
	.word TankUpAnimated4+TANKHEIGHT, TankUpAnimated3+TANKHEIGHT, TankUpAnimated2+TANKHEIGHT, TankUpAnimated1+TANKHEIGHT
PlayerTankUpFrame
	.word PlayerTankUp4+TANKHEIGHT, PlayerTankUp3+TANKHEIGHT, PlayerTankUp2+TANKHEIGHT, PlayerTankUp1+TANKHEIGHT

TankDownFrame
	.word TankDownAnimated4+TANKHEIGHT, TankDownAnimated3+TANKHEIGHT, TankDownAnimated2+TANKHEIGHT, TankDownAnimated1+TANKHEIGHT
PlayerTankDownFrame
	.word PlayerTankDown4+TANKHEIGHT, PlayerTankDown3+TANKHEIGHT, PlayerTankDown2+TANKHEIGHT, PlayerTankDown1+TANKHEIGHT
TankRightFrame
	.word TankRightAnimated4+TANKHEIGHT, TankRightAnimated3+TANKHEIGHT, TankRightAnimated2+TANKHEIGHT, TankRightAnimated1+TANKHEIGHT
PlayerTankRightFrame
	.word PlayerTankRight4+TANKHEIGHT, PlayerTankRight3+TANKHEIGHT, PlayerTankRight2+TANKHEIGHT, PlayerTankRight1+TANKHEIGHT
   
PowerUpIconFrame
    .word PowerUpImage1+TANKHEIGHT, PowerUpImage3+TANKHEIGHT, PowerUpImage2+TANKHEIGHT, PowerUpImage3+TANKHEIGHT
    
TanksRemainingPF1Mask
	.byte $FF,$7F,$3F,$1F,$0F,$07,$03,$01,$00,$00,$00

TanksRemainingPF2Mask		
	.byte $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3E,$3C
    


	
RespawnTankXPosition
	.byte /*PLAYERSTARTINGX, PLAYERSTARTINGX, */ENEMY1TOPLEFTSTARTINGX1, ENEMY1TOPLEFTSTARTINGX2, ENEMY2TOPLEFTSTARTINGX1, ENEMY2TOPLEFTSTARTINGX2, ENEMY3TOPLEFTSTARTINGX1, ENEMY3TOPLEFTSTARTINGX2
	.byte /*PLAYERSTARTINGX, PLAYERSTARTINGX, */ENEMY1TOPRIGHTSTARTINGX1, ENEMY1TOPRIGHTSTARTINGX2, ENEMY2TOPRIGHTSTARTINGX1, ENEMY2TOPRIGHTSTARTINGX2, ENEMY3TOPRIGHTSTARTINGX1, ENEMY3TOPRIGHTSTARTINGX2
	.byte /*PLAYERSTARTINGX, PLAYERSTARTINGX, */ENEMY1BOTTOMLEFTSTARTINGX, ENEMY1BOTTOMLEFTSTARTINGX, ENEMY2BOTTOMLEFTSTARTINGX, ENEMY2BOTTOMLEFTSTARTINGX, ENEMY3BOTTOMLEFTSTARTINGX, ENEMY3BOTTOMLEFTSTARTINGX
	.byte /*PLAYERSTARTINGX, PLAYERSTARTINGX, */ENEMY1BOTTOMRIGHTSTARTINGX, ENEMY1BOTTOMRIGHTSTARTINGX, ENEMY2BOTTOMRIGHTSTARTINGX, ENEMY2BOTTOMRIGHTSTARTINGX, ENEMY3BOTTOMRIGHTSTARTINGX, ENEMY3BOTTOMRIGHTSTARTINGX

RespawnTankYPosition 
	.byte /*PLAYERSTARTINGY, PLAYERSTARTINGY, */ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP 
	.byte /*PLAYERSTARTINGY, PLAYERSTARTINGY, */ENEMYSTARTINGYLOW, ENEMYSTARTINGYMIDHIGH, ENEMYSTARTINGYMIDLOW, ENEMYSTARTINGYHIGHMID, ENEMYSTARTINGYMID, ENEMYSTARTINGYHIGH ;removed "+4" from enemy tank #s 1-3 starting Y 
	
P0M0Color
    .byte TANKCOLOR1, TANKCOLOR3
    
P1M1Color
    .byte TANKCOLOR2, TANKCOLOR4
    
BottomHUDSpritePositions
    .byte 104, 112, 60, 66
    

ConsecutiveKillBonusTable
    DecimalToBCD CONSECUTIVEKILLBONUS*0
    DecimalToBCD CONSECUTIVEKILLBONUS*1
    DecimalToBCD CONSECUTIVEKILLBONUS*2
    DecimalToBCD CONSECUTIVEKILLBONUS*3
    DecimalToBCD CONSECUTIVEKILLBONUS*4
    DecimalToBCD CONSECUTIVEKILLBONUS*5
    DecimalToBCD CONSECUTIVEKILLBONUS*6
    DecimalToBCD CONSECUTIVEKILLBONUS*7
    DecimalToBCD CONSECUTIVEKILLBONUS*8
    DecimalToBCD CONSECUTIVEKILLBONUS*9
    DecimalToBCD CONSECUTIVEKILLBONUS*10
    DecimalToBCD CONSECUTIVEKILLBONUS*11
    DecimalToBCD CONSECUTIVEKILLBONUS*12
    DecimalToBCD CONSECUTIVEKILLBONUS*13
    DecimalToBCD CONSECUTIVEKILLBONUS*14
    DecimalToBCD CONSECUTIVEKILLBONUS*15
    DecimalToBCD CONSECUTIVEKILLBONUS*16
    DecimalToBCD CONSECUTIVEKILLBONUS*17
    DecimalToBCD CONSECUTIVEKILLBONUS*18
    DecimalToBCD CONSECUTIVEKILLBONUS*19
    DecimalToBCD CONSECUTIVEKILLBONUS*20

;----------------End Some Data Stuck Here	
	
	 
    PAGEALIGN 2
	
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
	
	
	
BeginMainMazeKernel         ;       35


	

	
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
	
KernelLastRow				;		30		branch here crosses page boundary
	;lda #$80
	;sta PF2
	SLEEP 4
	;line 1 of last row, this row has BASE (not wall)
KernelLastRowLoop			;		35
	lda Temp+1
	sta COLUPF				;+6		41
	;draw player 0
	cpy Player0Top				
	beq Switch0b
	bpl Wait0b
	lda (Player0Ptr),Y
	sta GRP0				;+15	56
BackFromSwitch0b
	
	lda Temp+7;#WALLCOLOR
	sta COLUPF				;+6		62
	

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
	sta ENAM1				;+12	28      ;--one cycle too late
	
	
	lda LastRowL
	sta PF2					;+6		34      
	
	SLEEP 5					;		40

	tsx                     ;--move this up to push the PF2 write a little later

	lda LastRowR
	sta PF2					;+6		46      ;--can't be any sooner than cycle 45
	
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

	sty ENABL	;+3		47      Y is zero
	sty GRP0	;+3		50
	ldx #$FF	;+2		54
	sta WSYNC
	stx PF2		;+3		
	stx PF1		;+3		
	sty GRP1	
	sty ENAM0
	sty ENAM1

	
	
	ldx #3
PositionLoopBottom
    lda BottomHUDSpritePositions,X
    jsr PositionASpriteSubroutineBank2
    dex
    bpl PositionLoopBottom
	


	sta WSYNC       ;extra line so bottom wall is same height (10 lines) as top wall
	
	;--done displaying maze
	;--set up score, tanks remaining, lives remaining etc
	;Y is still zero      
	sty REFP0
	sty REFP1
	sty VDELP0
; 	sty VDELBL             ;not necessary, ball is only used for bullets in the maze so we can leave this set always
 	sta WSYNC
	sty PF0               
	sty PF1
 	sty PF2                 ;+9      9
	lda #THREECOPIESCLOSE
	sta NUSIZ0
; 	lda #TWOCOPIESCLOSE
    lsr                     ;               THREECOPIESCLOSE=3, TWOCOPIESCLOSE=1
	sta NUSIZ1              ;+10    19
	
	lda #TANKSREMAININGCOLOR
	sta COLUP0
	sta COLUP1
	sta HMCLR               ;+11    30
	

	lda #>DigitDataMissile
	sta Player0Ptr+3
	sta Player1Ptr+1        ;+8     38
	lda MazeNumber
	lsr
	lsr
	lsr
	lsr
	tax                     ;+13    51
	lda DigitDataMissileLo,X
	sta Player0Ptr+2        ;+7     58
	lda MazeNumber
	and #$0F
	tax                     ;+7     65
	lda DigitDataMissileLo,X
	sta Player1Ptr          ;+7     72
	
    ;--this displays a graphic showing how long until the player tank respawns after it dies
	;--this makes the graphic indicator the same color(s) as the player tank
	lda FrameCounter
	and #1
    tax
    lda PlayerTankColor,X
    sta COLUPF              ;+14    10

	;--if tank is EITHER an explosion (TANKUP bit=1 and TANKINPLAY bit =0) OR in play (TANKINPLAY bit=1), don't show indicator
	;--in other words, only if TANKUP=0 and TANKINPLAY=0 do we show this graphic
    lda TankStatus
    and #TANKUP|TANKINPLAY
    bne NoRespawnCounterGraphic ;+7/8    17/18
    lda TankStatus
    and #$0F
    lsr
    tax                         
    lda RespawnCountdownGraphic,X
    sta PF2                     ;+16    33      <----needs to happen prior to cycle 38
NoRespawnCounterGraphic         ;               


    lda TanksRemaining          ;+3     47   
    
    
	sta WSYNC                   ;
    sty COLUPF                  ;           Y=0
    dey                         ;           make Y=$FF
    sty PF2                     ;+8      8
    sty PF0                     ;+3     11
	
	lsr
	tax                         ;+4     15

	lda TanksRemainingPF1Mask,X
	sta PF1                     ;+7     22
	
	nop
; 	lda #2
	sty ENAM0
	sty ENAM1                   ;+8     30  Y=$FF
	ldy #8
	bne BottomKernelLoopInner	;+5     35	branch always
	
BottomKernelLoopMiddle			;		18
	dey							;+2		20
	
	lda TanksRemaining
	lsr
	adc #0						;+7		27
	
	tax							;+2		29
	lda TanksRemainingPF1Mask,X
	sta PF1
	lda TanksRemainingPF2Mask,X
	sta PF2						;14		43
	
	lda (Player0Ptr+2),Y		;+5
	sta HMM0					;+3
	asl							;+2
	asl							;+2
	ora #3						;+2
	sta NUSIZ0					;+3		60

	lda (Player1Ptr),Y			;+5		65
	sta HMM1					;+3		68	
	asl							;+2		70
	asl							;+2		72
	ora #3						;+2		74
	sta NUSIZ1					;+3		 1

	sta HMOVE					;+3		 4

	lda TanksRemainingGfx,Y		;+4	     8
	sta GRP0			
	sta GRP1					;+6		14

	bne BottomKernelLoopInnerMiddle	;+3	17		branch always
	
BottomKernelLoopInner			;		35   
    SLEEP 4                     ;+4     39   
	lda (Player0Ptr+2),Y		;+5
	sta HMM0					;+3
	asl							;+2
	asl							;+2
	ora #3						;+2
	sta NUSIZ0					;+3		56      

	lda (Player1Ptr),Y			;+5		
	sta HMM1					;+3			
	asl							;+2
	asl							;+2		
	ora #3						;+2
	sta NUSIZ1					;+3		73      

	sta WSYNC                   ;+3     76      
	
	sta HMOVE					;+3		 3

	lda TanksRemainingGfx,Y		;+4	            
	sta GRP0			
	sta GRP1					;+6		13      
	
	cpy #4						
	beq BottomKernelLoopMiddle	;+5		18     
BottomKernelLoopInnerMiddle     ;       17
	;--PF1 is set once per half loop, this is set here because we enter the loop with PF2=$FF
	;	which is necessary to mask the missiles before we are ready to display them
	lda TanksRemainingPF2Mask,X
	sta PF2
	SLEEP 6						;+13	30		
	dey
	bpl BottomKernelLoopInner	;+5		35      

	
BottomKernelLoopDone
	iny
	sta WSYNC
	sty ENAM0
	sty ENAM1
	sty GRP0
	sty GRP1
	sty GRP0
	sty PF0
	sty PF1
	sty PF2

	jmp ReturnFromBSSubroutine2
	


	
;****************************************************************************	



	
	
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
	

	
	
	;--update maze number on first pass also ONLY if score != 000000 (i.e., don't do it at game start)
	lda Score
	ora Score+1
	ora Score+2
	bne NotGameStartUpdateMazeNumber
	lda MazeNumber
; 	clc
; 	bcc SetMazeSeed
    bne SetMazeSeed     ;branch always
NotGameStartUpdateMazeNumber	
	sed
	lda MazeNumber
UpdateMazeNumber
	clc
	adc #$01
	sta MazeNumber
	beq UpdateMazeNumber
	cld
SetMazeSeed	
	lda GameStatus2
	and #RANDOMMAZES
	bne RandomMazeSeed
	lda MazeNumber
	clc
    adc #99  ;--don't like the initial maze, too hard with long vertical corridor from the top to the base
	;and set seed for maze
	sta RandomNumber	
RandomMazeSeed	

	;--and that's it for the first pass
	jmp DoneWithFirstPass
	
NotFirstPass
	lda RandomNumberSaved
	sta RandomNumber
	
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

	
	lda MazeNumber
	and #$0F
	cmp #$05
	bne RegularMazeRoutine
	lda GameStatus2
	and #RANDOMMAZES
	bne RegularMazeRoutine
	;--alternate maze routine for selected mazes
	tya
	tax     ;use X here because we don't need it for another use and the writes are to ZP so the code size is smaller
	lda #%00110011
	sta PF1Right-1,X
	sta PF1Left-1,X
	lda #%11001100
	sta PF2Left-1,X
	sta PF2Right-1,X
	lda #0
	sta PF1Right,X
	sta PF1Left,X
	cpx #MAZEROWS-4
	beq PartiallyClearCenterOfRow
	cpx #MAZEROWS-8
	bne FullyClearCenterOfRow
PartiallyClearCenterOfRow	
    lda #%11000000
FullyClearCenterOfRow
	sta PF2Right,X
	sta PF2Left,X
	jmp DoneWithRow
RegularMazeRoutine
	
	
	
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

	;--are we at the end of our horizontal path?  compare random number to constant
	jsr UpdateRandomNumberBank2 ;returns with RandomNumber in A
	lda MazeNumber
	clc
	adc #MAZEPATHCUTOFF
	cmp RandomNumber
    bcs EndOfRun

	;--not at the end of the run, so loop around
	dec Temp+1
	dex
    bne MakeMazeLoopInner  ;<--don't go all the way to the left edge (again, unless we are on the first row)

	;--need to carve a passage downward if we reach this spot:
EndOfRun
	;--don't carve passage downward if we're on the bottom row
	tya
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
	lda Temp+1
	and RandomNumber
	sta Temp+1
OnlyOnePlaceToCarve
	lda Temp
	sec
	sbc Temp+1
	sta Temp+1      ;now Temp+1 holds column to carve down in
	txa
	pha             ;save X index
	ldx Temp+1
	lda AllowCarveDownTable,X
	beq NoCarveDownInThisColumn
	lda PFMaskLookupBank2,X
	eor #$FF
	pha
	lda #<PF1Left
	sta MiscPtr
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
NoCarveDownInThisColumn
	pla		;get block index back into X
	tax
	dex
	dex
	bmi DoneWithRow

    bne MakeMazeLoopOuter ;<--don't carve all the way to the left edge
EndRunBeginNextRun

	
DoneWithRow
DoneWithFirstPass
	dec MazeGenerationPass

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
	    
    	ldx #$00
;     	txa
    	;clear blocks in top row that are entry points
;     	and PF1Left+MAZEROWS-2
    	stx PF1Left+MAZEROWS-2
    	
    	;clear upper R corner
;     	txa
;     	and PF1Right+MAZEROWS-2
    	stx PF1Right+MAZEROWS-2
	
     	ldx #$C3
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
    ldx EntryPointRowOffsetTable,Y
    lda #$3F
    and PF1Left,X
    sta PF1Left,X
    lda #$3F
    and PF1Right,X
    sta PF1Right,X
	dey
	bpl ClearSideEntryPointsLoop

	
    ;--make sure tanks aren't trapped in the top row, either on the right or the left
    ;--left.  If left five blocks are filled in in the second row from the top, open the left-most block
CheckForBlockedAtTopLeft    
    lda PF1Left+MAZEROWS-3
    cmp #$FF
    bne TopLeftNotBlocked
    lda PF2Left+MAZEROWS-3
    and #%00000011
    beq TopLeftNotBlocked
    lda PF1Left+MAZEROWS-3
    and #%00111111
    sta PF1Left+MAZEROWS-3
    
TopLeftNotBlocked

    lda PF1Right+MAZEROWS-3
    cmp #$FF
    bne TopRightNotBlocked
    lda PF2Right+MAZEROWS-3
    and #%00000011
    beq TopRightNotBlocked
    lda PF1Right+MAZEROWS-3
    and #%00111111
    sta PF1Right+MAZEROWS-3
TopRightNotBlocked


	;no longer necessary since PF1 completely cleared on top rows
	;--if top corner entry points are enclosed, add escape routes downward
; 	lda PF1Left+MAZEROWS-2 ;top left
; 	and #%00001100
; 	beq TopLeftCornerNotEnclosed
; 	lda PF1Left+MAZEROWS-3
; 	and #%11001111
; 	sta PF1Left+MAZEROWS-3
; 	lda PF1Left+MAZEROWS-4
; 	and #%11001111
; 	sta PF1Left+MAZEROWS-4
; TopLeftCornerNotEnclosed
; 	lda PF1Right+MAZEROWS-2 ;top right
; 	and #%00001100
; 	beq TopRightCornerNotEnclosed
; 	lda PF1Right+MAZEROWS-3
; 	and #%11001111
; 	sta PF1Right+MAZEROWS-3
; 	lda PF1Right+MAZEROWS-4
; 	and #%11001111
; 	sta PF1Right+MAZEROWS-4
; TopRightCornerNotEnclosed

    
    ;--more escape routes, this time for the right side
    lda PF1Right+MAZEROWS-6
    and #%11001111
    sta PF1Right+MAZEROWS-6
    lda PF1Right+MAZEROWS-8
    and #%11001111
    sta PF1Right+MAZEROWS-8
    lda PF1Right+MAZEROWS-7
    and #%00001100
    beq SkipCreatingConnectingPassageRight
    lda PF1Right+MAZEROWS-7
    and #%11001111
    sta PF1Right+MAZEROWS-7
SkipCreatingConnectingPassageRight
    ;--more escape routes, this time for the left side
    lda PF1Left+MAZEROWS-6
    and #%11001111
    sta PF1Left+MAZEROWS-6
    lda PF1Left+MAZEROWS-8
    and #%11001111
    sta PF1Left+MAZEROWS-8
    lda PF1Left+MAZEROWS-7
    and #%00001100
    beq SkipCreatingConnectingPassageLeft
    lda PF1Left+MAZEROWS-7
    and #%11001111
    sta PF1Left+MAZEROWS-7
SkipCreatingConnectingPassageLeft
    
    

	
	
	;--and clear GAMEOFF flag and turn off maze generation flag, and make base reappear
	lda GameStatus
	and #~(GENERATINGMAZE|GAMEOFF)
	ora #DRAWBASE
	sta GameStatus
	
	;--set music to correct starting location
    lda #255
    sta SongIndex

	;--set starting tank position
	
	;--move tanks and bullets off screen
	lda #255		;--loop until X = 255 (-1)
	jsr MoveEnemyTanksOffScreen	
	
    lda #TANKAISWITCH+1
	sta TankMovementCounter ;enemy tanks start in regular movement mode at beginning of every level....maybe?  
	
	
	;--does this nonsense below work?  Isn't MazeNumber a BCD value?  I think it still does work, but this is sloppy and probably should be fixed at some point.
	jsr GetInitialTankCountSubroutine   ;returns with initial count in A
	sta TanksRemaining
	
	;--starting timers for when tanks enter the maze
	;--rewrite this:
	ldx #3
SetNewLevelTankDelay
    lda StartingTankStatus,X
    sta TankStatus,X
    dex
    bpl SetNewLevelTankDelay
    
    ;--cheap hack to make player show up faster
    and #~TANKRESPAWNWAIT  ;clear the wait
    ora #PLAYERINITIALDELAY
    sta TankStatus
    
    ;--and finally, set MazeGenerationPass to zero because we'll use that for powerups/bonuses during the level
    ;--set number of tanks to kill before power up appears randomly
    pla
    pha     ;get actual random number but leave on stack
    and #POWERUPCOUNTDOWNBITS
    bne SetPowerUpCountdownValue
    lda #POWERUPCOUNTDOWNDEFAULT   ;make sure it doesn't start at zero
SetPowerUpCountdownValue   
    sta MazeGenerationPass
	
NotCompletelyDoneWithMaze
	;--restore original random number
	lda RandomNumber
	sta RandomNumberSaved          ;save current random number in maze generation (this means maze generation starts random but each seed is consistent)
	pla
	sta RandomNumber
	
	jmp ReturnFromBSSubroutine2
	

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
; 	ldy #6
SetStartingEnemyTankLocationsLoop
	lda StartingTankXPosition,X
	sta TankX,X
	lda StartingTankYPosition,X
	sta TankY,X
; 	dey
; 	dey
	dex
	cpx Temp                ;6, 4, 2, 0, 254
	bne SetStartingEnemyTankLocationsLoop
	
	rts	

	
;****************************************************************************
	

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
	rts                 ;+35    35      plus 6 for JSR

;****************************************************************************
    SUBROUTINE
IsTankOnScreen
    ;--X holds tank number to check
    ;--return in accumulator:  0 if offscreen, 1 if onscreen
    lda TankX,X
    cmp #TANKONSCREENLEFT
    bcc .TankOffScreen          ;+8/9    8/9
    cmp #TANKONSCREENRIGHT+1    
    bcs .TankOffScreen          ;+4/5   12/13
    lda TankY,X
    cmp #TANKONSCREENTOP+2
    bcc .TankOnScreen           ;+8/9   20/21
.TankOffScreen                  ;       20 worst case
    lda #0
    rts
.TankOnScreen                   ;       21 worst case
    lda #1
    rts                         ;+8     29  worst case

;****************************************************************************
	
	
;byte holds
;           CXM0P   CXM1P   CXPPMM	
;           7  6    7  6    7  6
;frame odd  12 1P   3P 32   2P 31
;frame even P1 P3   23 21   13 2P

	
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
	
;****************************************************************************

/*
actual worst-case for CollisionsSubroutine
one player bullet hits wall, which is bullet that is displayed this frame. (252 cycles)
all 4 tanks all collide with each other on same frame (852 cycles)
other three bullets all hit the tanks as described above (3248 cycles)
 plus various other stuff:  

	33 cycles to see if base is hit
	55 cycles to prep for tank-to-tank collision loop
	137 cycles to update status for newly dead tanks (in scenario they are all newly dead)

TOTAL is: 4577 or 60+ scanlines.

this is extraordinarily unlikely, but .... probably need to go back to bullet-to-tank collisions handled via hardware collision.  makes 
maximum some prep (50 cycles) plus 1080 for power-up explosion + 79 for hitting another tank at the same time. total is ~1200 cycles.

then maximum for entire collision routine is:
	33 to see if base is hit
	55 to prep for tank-tank collisions
	852 for all four tanks collide same frame
	6 for bullet not hitting wall (only one bullet collision at a time!)
	1200 for bullet hits powerup and another tank at the same time
	137 to update status for all tanks newly dead
total:  2283 (30 scanlines).  still too long (overscan is 2122 cycles) but maybe close enough to optimize down.  still a lot to do, 
	moving player tank could take up to 9 scanlines (I think?  or longer) if player enters an intersection on this frame (possible with above scenario?  I don't think so.)
*/


CollisionsSubroutine


    ;zeroeth, clear the "tanks killed" bits:
    lda GameStatus2    
    and #~TANKSKILLEDBITS
    sta GameStatus2

    ;--first and most important collision check: has anything at all touched the base?
    ;-check bullets first (and only)
    
    bit CXBLPF
    bpl NoBulletToBaseCollision     ;+5/6       5
    ;--hit PF, now let's see if it is in the middle on the bottom row
    lda FrameCounter
    and #3
    tax
    lda BulletY,X
    cmp #BLOCKHEIGHT+2
    bcs NoBulletToBaseCollision     ;+15/16     20/21
    lda BulletX,X
    cmp #76
    bcc NoBulletToBaseCollision     ;+8/9       28/29
    cmp #85
    bcs NoBulletToBaseCollision     ;+4/5       32/33
    ;--if we drop through, we don't process any more collisions
    
    ;--collision!  Game Over, man, Game Over.
    lda GameStatus
    ora #GAMEOVER|GAMEOFF
    sta GameStatus
    ;--reset TankMovementCounter, (re)used to make walls flash
    lda #GAMEOVERSTARTFREQ
    sta TankMovementCounter
    ;--kill sounds just for my own sanity -this may no longer be necessary
;     lda #0
;     sta AUDV0
;     sta AUDV1
    ;--kill music (AUDV0) and sound effects (Channel1Decay variable)
    lda #0
    sta AUDV0
    sta Channel1Decay
    ;and return, so player doesn't get 5 points for shooting his own base lol
	jmp ReturnFromBSSubroutine2
NoBulletToBaseCollision             ;       33 cycles maximum
    
    

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
    sta Temp+1      ;+8      8
    lda CXM1P
    and #$C0
    lsr
    lsr
    ora Temp+1
    sta Temp+1      ;+15    23
    lda CXPPMM
    and #$C0
    lsr
    lsr
    lsr
    lsr
    ora Temp+1
    sta Temp+1	    ;+19    32
    
    lda FrameCounter
    and #1
    asl
    tax
    lda TankCollisionRotationTables,X
    sta MiscPtr
    lda TankCollisionRotationTables+1,X
    sta MiscPtr+1   ;+23    55      cycles for tank-to-tank loop prep.
    
                    ;total max cycles to this point: 55+33=88
	

    ldy #0          ;+2      2      start with player tank and count up
TankCollisionLoop
    ;--can we add a check that we ignore this tank if it is already dead?  That would eliminate half the collision loops... do we need to run through this if the tank is dead?
    lda Temp+1    ;get all collision registers
    and (MiscPtr),Y
    beq NoTankCollision     ;+10/11
YesTankCollision    
    ;--check to see if what we are colliding into is already dead:
    sta Temp
    sty Temp+2
    bne CheckForExplosionCollisionLoopEntry ;+9     19       branch always following not-taken BEQ above
CheckForExplosionCollisionLoop
    
    lda Temp
    and (MiscPtr),Y
    beq NoCollisionWithThisTank             ;+10/11     
    ;--now check if tank is dead
    lda TankStatus,Y
    lsr     ;get TANKINPLAY bit into carry
    bcs TankHitLiveTank                     ;+8/9
    ;--now see if explosion or powerup
NoCollisionWithThisTank    
CheckForExplosionCollisionLoopEntry
    dey
    bpl CheckForExplosionCollisionCheckEnd  ;+4/5
    ldy #3                                  ;+2
CheckForExplosionCollisionCheckEnd
    cpy Temp+2                              ;+3
    beq DidNotHitAnythingLive               ;+2/3   we have looped around completely and we exit this loop AND we finish with the tank and go to the next
    bne CheckForExplosionCollisionLoop      ;+3     branch always
    ;--maximum for loop above is to go through all tanks and find out we hit the last one we check...but that would mean maximum of 2 tank-to-tank collisions.
    ;if no tank collisions, loop above takes 85 cycles and skips all collision-handling immediately below and loops to check next tank.
    ;--if any collisions, will end above loop early (at either 27, 51, or 75 cycles) and drop below:
    
TankHitLiveTank                             ;       cycle counts below are relative to this spot in this loop 
    ;--if Y = 0 then tank run into by player
    sty Temp+6  ;save this we will check below  ;tank that ran into tank we are checking
    ;--restore Y index of tank we are checking
    ldy Temp+2              ;tank we are checking.
    ;--now what?
    ;--remove tank only if fully onscreen
    tya
    tax ;what is point of this?  subsequent code uses X as index into tank variables instead of Y
    ;--check if tank in play (can be on screen but not in play, if dead)
    lda TankStatus,X        
    lsr     ;get TANKINPLAY bit into carry
    bcs TankNotDeadYetKillIt                ;+18/19
    ;--tank is not in play, is it an explosion or a powerup?
    and #TANKDOWN>>1                  ;shifted due to LSR above
    beq TankAlreadyDeadCannotDie            ;+4/5       22/23       if tank already dead (i.e., an explosion) then we end loop and check next tank
    ;--else tank is powerup.  what to do exactly?
    ;--if hit by non-player tank, ignore:
    lda Temp+6  ;--this holds zero if player tank is what hit it
    bne NonPlayerTankHitPowerUp             ;+5/6       27/28       this branch, if taken, ends this loop since non-player tanks hitting powerups is ignored.

    
    ;only reach here if tank we are checking is NOT player tank and what we ran into is a power-up
    ;this is code that handles when the player tank runs into a powerup
    ;--first respawn tank to get powerup off the screen
    jsr TankRespawnRoutine                  ;+98        125         this uses X as index into tank.  and this blows away Y
    ;--always give powerup bonus when collecting power up.
    lda #>POWERUPSCOREBONUS
    sta Temp
    lda #<POWERUPSCOREBONUS
    jsr IncreaseScoreSubroutine
    ;--second actually activate powerup
    lda MazeGenerationPass  ;used for powerups during levels
    and #POWERUPACTIVATEDBITS
    beq ActivateInitialPowerUp              ;+7/8       132/133
    jsr KillAllTanksBonus                   ;+306       438         this actually blows away X
    ;--now reset powerup countdown 
    lda MazeGenerationPass
    ora #POWERUPCOUNTDOWNRESTART
    sta MazeGenerationPass  ;reset counter to the max 

    ;--I think we can skip the subsequent stuff
    ldy Temp+2      ;restore Y
    jmp TankAlreadyDeadCannotDie            ;+14        452
ActivateInitialPowerUp                      ;this is faster so for now I will ignore cycle counts of this code
    lda MazeGenerationPass
    ora #POWERUPACTIVATEDBITS|POWERUPCOUNTDOWNRESTART
    sta MazeGenerationPass    
    ldy #SPEEDBOOSTSOUND
    jsr StartSoundSubroutineBank2
;     pla ;restore Y
;     tay
    ldy Temp+2      ;restore Y
    jmp TankAlreadyDeadCannotDie        ;branch always 


    
TankNotDeadYetKillIt                    ;       19  
    jsr IsTankOnScreen                  ;+35    54      35 is worst case returns 1 in A if onscreen, 0 in A if not, and zero flag set appropriately
    beq TankNotOnScreenCannotDie        ;+2/3   56/57
    ;--tank is onscreen
    tya ;--set flags based on which tank, only get points if player runs into tank, not if tanks run into each other.
    bne EnemyTankDied                   ;+4/5   60/61
 	;--score for killing enemy tank
 	lda Temp+1      ;need to save collision results!
 	pha                                 ;+6     66
 	jsr ScoreForKillingTank             ;+128   194
 	pla
 	sta Temp+1                          ;+7     201
EnemyTankDied                           ;       201 worst case for player tank ran into enemy tank, 61 if enemy tanks collided
    tya
    tax                                 ;+4     205/65
    jsr PlayerHitTank                   ;+78    283/143     this routine uses X to index into which tank got hit.

TankAlreadyDeadCannotDie
TankNotOnScreenCannotDie    


NoTankCollision
PlayerTankDead
DidNotHitAnythingLive
NonPlayerTankHitPowerUp
    ;--looping up so player tank collisions are processed first
    iny
    cpy #4
    beq DoneWithTankCollisionLoop
    jmp TankCollisionLoop               ;+6/7   7 on first 3 loops, 6 on last (27 cycles)
DoneWithTankCollisionLoop    
    ;worst case scenario for above loop:  two enemy tanks collide and on same frame player tank runs into powerup icon when he already has first powerup, so also blows up the other tanks. 
    ;   this would take approx 852 cycles.
    
    ;                                   ;+852   940 cycles, worst case, to this point. (12+ scanlines!!!!)


    ;--bullet (ball) to wall collision check    (cycles for this collision routine counted from this point)
    bit CXBLPF
    bpl BallHasNotHitBlock              ;+5/6
    ;--bullet hit the wall, now identify which bullet.
    lda FrameCounter
    and #3
    tax                                 ;+7      12 
    jsr BulletHitBrickSubroutine        ;+252   264     worst case scenario

BallHasNotHitBlock
                                
    ;                                   ;+252   1192 cycles, worst case, to this point.
    ;--do we need to update dead tank status HERE before we process bullet-to-tank collisions?
    ;--and then again after?
    
    
	;--now check if bullet has hit an enemy tank

	;switching back to hardware collision detection
	;--we check only the bullet that was just displayed on the screen using the hardware collision registers	
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
	jmp NoBulletToTankCollision
	
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


	;   X is index into tank 
	;   Y is index into bullet
FoundDeadTankNowKill                                ;       70
    ;--now we have index of tank hit in X
    lda FrameCounter
    and #3
    tay         ;now we have index of bullet in Y
    ;--check if tank is already dead (but still on screen)
    lda TankStatus,X
    lsr     ;get TANKINPLAY bit into carry
    bcs TankAliveKillIt                             ;+8/9   78/79
    ;--so we hit a not-in-play tank.  
    ;--now see if we it is an explosion or a powerup.  if explosion, ignore
    and #(TANKUP|TANKDOWN)>>1       ;direction bits, shifted right one
    cmp #(TANKUP|TANKDOWN)>>1              ;UP and DOWN together means powerup icon
    bne TankAlreadyDeadCannotBeShot                 ;+6/7   84/85    branch taken is to end of loop
    ;--so we hit a powerup.
    ;--want to kill everything - bricks, tanks - in a one-block radius
    jsr PowerUpExplosionRemoveBricks                ;+517   601     (also removes bullet from screen)  Q: could this happen 2x (or 3x?) per frame, if power-up icon is hit by 3 separate bullets on same frame?
                                                    ;                                                  A: no, since power-up icon is "killed" as part of this routine, we would end up taking the the "BCS TankAliveKillIt" branch above
    ;--now reset powerup countdown
    lda MazeGenerationPass      ;used for all kinds of crap during game play
    ora #POWERUPCOUNTDOWNRESTART
    sta MazeGenerationPass                          ;+8     609
KillAllTanksWithinBlastRadius    
    ;--now, kill all tanks within blast radius }:-)
    ;   note: this will also as a side effect remove the powerup icon from the screen
    ;--get upper left corner of blast radius
    ;   using blast radius of 1.5 bricks
    lda TankX,X
    sec
    sbc #HORIZONTALBLASTDISTANCE
    sta Temp+4                                      ;+11    620     left boundary
    lda TankY,X
;     clc
    adc #VERTICALBLASTDISTANCE ;carry is set here so actual value being added is (TANKHEIGHT+TANKHEIGHT/2)+1
    sta Temp+1                                      ;+9     629     upper boundary+1
    lda TankX,X                                    
;     clc
    adc #HORIZONTALBLASTDISTANCE+1         ;carry clear following addition above
    sta Temp+2                                      ;+9     638     right boundary+1
    lda TankY,X
;     sec
    sbc #VERTICALBLASTDISTANCE-1     ;carry is clear here so actual value being subtracted is (TANKHEIGHT+TANKHEIGHT/2)
    sta Temp+3                                      ;+9     647     lower boundary
    ;--discarding X (powerup icon index) at this point
    lda #TANKINPLAY ;--clear all bits except tank in play so we don't screw up subsequently-processed collisions
    sta TankStatus,X                                ;               this "kills" the power-up icon
    stx Temp+5                                      ;+9     656     save X index of powerup so we can make sure not to "kill" it in loop below
    ;--save X.... except we saved it above, so .... hmmm?
;     txa
;     pha
    ldx #3                                          ;+2     658
KillAllTanksInLoop
    cpx Temp+5  ;are we killing the powerup?
    beq DoNotKillPowerUp        ;+5/6   5/6     cycle counts will be internal to the loop.  will always take this branch once and not take it the other 3 times.
                                ;we don't kill it because the overall routine (PlayerHitTank) does all kinds of stuff like decrement TanksRemaining and etc. 
                                ;just above we set it to dead so it's fine.
    ;--need to check that tanks are in play.  this isn't perfect, but will prevent us from killing:
    ;       tanks that were killed on previous frames (and are now explosions)
    ;       tanks that are offscreen and have not started coming on screen yet
    ;   we will "rekill" any tanks killed above in tank-to-tank collisions, but that's not a big deal, because we are flagging tanks killed and not re-killing (in PlayerHitTank routine)
    ;   we will also kill tanks that have begun coming on screen but aren't fully onscreen yet, which *kinda* violates the "tanks are invincible until fully onscreen" but for now I'm going to let that slide.
    lda TankStatus,X
    lsr                         ;--get TANKINPLAY into carry
    bcc DoNotKillTankAlreadyDead
    lda TankX,X
    cmp Temp+4
    bcc NotInsideBlastRadius    ;+9/10  14/15
    cmp Temp+2
    bcs NotInsideBlastRadius    ;+5/6   19/20
    lda TankY,X
    cmp Temp+3
    bcc NotInsideBlastRadius    ;+9/10  28/29
    cmp Temp+1
    bcs NotInsideBlastRadius    ;+5/6   33/34
    ;--it is KILL KILL KILL
;     jsr PlayerHitTank           ;+78    111       can hit this every time if all three non-powerup tanks are within the blast radius
        ;should we be calling KillTankViaPowerUp?  and also calling ScoreForKillingTank?
    jsr KilledTankViaPowerUp    ;+140ish            this will work for both enemy and player tanks
    jsr ScoreForKillingTank     ;+a bunch
DoNotKillPowerUp
NotInsideBlastRadius
DoNotKillTankAlreadyDead
    dex
    bpl KillAllTanksInLoop      ;+5     116     total would be 116 three times + 10 once = 358 cycles
                                                    ;+358   1016
    ldx Temp+5                                      ;+3     1019
    
    ;--third, play sound
    ;--need to save/restore Y
    sty Temp+1          ;overwrite one of the coords used for the blast radius calc above
    ldy #POWERUPEXPLOSIONSOUND                      ;+5     1024
    jsr StartSoundSubroutineBank2                   ;+40    1064
    ldy Temp+1                                      ;+3     1069
    
    jmp DoneWithBulletCollision                     ;+3     1072
TankAliveKillIt                                     ;       79
    ;--ok actually let's check that the tank is fully on the screen:
    jsr IsTankOnScreen                              ;+35    114     returns 0 if tank (index=X) is offscreen, 1 if onscreen
    bne TankIsOnScreenKillIt                        ;+2/3   116/117
    ;--if we are here, the bullet hit a tank that was partially (or completely?) off screen
    ;--we should do something else here, like play a weird noise so player knows he shot a tank that can't be killed since it is partially offscreen
    jsr MoveBulletOffScreenSubroutine2  ;uses Y index into which ball to remove.  routine loads BALLOFFSCREEN (=0) and then stores it to two RAM locations
    ldy #INVINCIBLETANKSOUND
    jsr StartSoundSubroutineBank2
        
    bne TankOffScreenCannotBeKilled     ;branch always
    
TankIsOnScreenKillIt                                ;       117
	;--add KILLTANKSCORE to score if X > 0 (enemy tank) and Y >= 2 (player bullets)
	cpy #2
	bcs NoPointsForEnemyOwnGoals                    ;+4/5   121/122
	txa ;set flags based on X
	beq NoPointsForGettingShot	                    ;+4/5   125/126   
	jsr ScoreForKillingTank                         ;+96   253
	

NoPointsForGettingShot		                        ;           126 when branching here
NoPointsForEnemyOwnGoals                            ;           122 when branching here
	jsr BulletHitTank                               ;+137   390 for player bullets hitting enemy tank
	                                                ;+113   235 for enemy bullets hitting enemy tank
	                                                ;+113   239 for enemy bullets hitting player tank
TankAlreadyDeadCannotBeShot                     ;           85 if branching to this label
TankOffScreenCannotBeKilled
NoBulletToTankCollision	
DoneWithBulletCollision                         ;           1072  if branching to this label, this is (maximum) path if bullet hit power-up icon and killed all three non-powerup tanks.
BulletDidNotHitTank                             ;       71 is longest path branching to this label
BulletOffScreen2
; 	dey
; 	bmi DoneWithBulletTankCollisionInnerLoop    ;+4/5   
; 	jmp CheckBulletTankCollisionInnerLoop       ;+3     
	    
DoneWithBulletTankCollisionInnerLoop
EnemyTankOffscreen    
; 	dex
; 	bmi DoneWithCheckBulletTankCollisionOuterLoop   ;+4/5
; 	jmp CheckBulletTankCollisionOuterLoop           ;+3
DoneWithCheckBulletTankCollisionOuterLoop

    /*in very contrived scenario:
            E1E
            BBB
          E3E E2E
            BBB
            PPP


    I think absolute worst case, maximum simultaneous bullet to tank collisions would be if two tanks were next to each other (about to collide) in exact middle of intersection, 
    and one shot the other, and player tank was directly below shooting up and hit both simultaneously and other enemy tank was directly above shooting down and also hit both simultaneously.
    And player shot up immediately before tanks got close enough so bullet passed through and hits top tank on same frame
    
    in this very contrived scenario, total time for bullet-to-tank collisions is something like 3600 cycles (calculated 3567) LMAO (46+ scanlines!)
        in this scenario, there would be no bullet-to-wall collisions.  there could be tank-to tank collisions (tank 2 & 3 could be overlapping - tank 3 is powerup)
        and player tank could have simultaneously ran into tanks 2&3 while shooting them and similarly for tank 1 running into tanks 2&3 while shooting them... except then player bullet could squeeze through to hit tank 1.
        it could be hitting a wall instead.
    
    
*/
    ;if a tank was killed immediately above, we cleared all bits except for TANKINPLAY
    ;   so that we could correctly tell if tanks if collided with it
    ;   and so now look for those tanks and set the rest of the bits correctly
    ldy #3              ;+2
SetTankStatusForDeadTanks
    lda TankStatus,Y            ;+3     3
    cmp #TANKINPLAY
    bne TankNotNewlyDead        ;+4/5   7/8
    ;--check to see if it should be a powerup
    ;   only a powerup if:
    ;       powerup countdown is zero
    ;       and TanksRemaining > 0
    ;       and no powerup on screen
    ;       and left difficulty switch = B
TankIsNewlyDead
    ;add check for tank 0 here?  or maybe change loop to BNE and inline tank zero handling below.  none of this power-up stuff matters for player tank.
    lda GameStatus2
    and #POWERUPSDISABLED
    bne PowerUpsTurnedOff       ;+7/8   14/15
    lda MazeGenerationPass
    and #POWERUPCOUNTDOWNBITS
    bne NoPowerUp               ;+7/8   21/22
    lda TanksRemaining
    beq NoPowerUp               ;+5/6   26/27
    ldx #3                      ;+2     28
CheckForPowerUpOnscreenLoop   
    lda TankStatus,X
    and #TANKUP|TANKDOWN|TANKINPLAY
    cmp #TANKUP|TANKDOWN
    beq NoPowerUp               ;+10/11 10/11   count for internal loop
    dex
    bne CheckForPowerUpOnscreenLoop ;+4/5   14/15   
        ;maximum is if no powerups on screen: 15*2+14=44
                                ;+44    72  only possible first time through (Y) loop, subsequent times internal loop above will kick out on first time through 
    
    ;--else make it a powerup!
;     lda TankDeadStatus,Y
;     ora #TANKDOWN               ;               could replace this LDA/ORA with "LDA #TANKDOWN|TANKUP|TANKDEADWAIT" and save 4 cycles and 3 bytes ROM
    lda #TANKDOWN|TANKUP|TANKDEADWAIT
    sta TankStatus,Y
    bne CheckNextTankForNewDeath;+9     81      branch always
PowerUpsTurnedOff               ;       15 if branching here
NoPowerUp                       ;       22/27 if branching here (or if branching from internal (X) loop, could be 39/54/69
    lda TankDeadStatus,Y
    sta TankStatus,Y            ;+8     30/35 47/62/77
TankNotNewlyDead
CheckNextTankForNewDeath        ;       81 if branching here
    dey
    bpl SetTankStatusForDeadTanks;+4/5  85/86
        ;maximum is we create a powerup (86 cycles) and then on subsequent loops we don't but have to check in internal loop (will find it on first internal loop every time: 53/52)
        ;   86+53+53+52=244 cycles
      
        ;maximum in frame when power-up is shot is 35*3+32=137 since powerup countdown is not updated while powerup on screen


	
	jmp ReturnFromBSSubroutine2

;****************************************************************************

    SUBROUTINE
PowerUpExplosionRemoveBricks  
        ;come in with X = tank (powerup icon) index
    txa
    pha                                 ;+5      5      save index into tank (powerup icon) that got hit
    ;--remove bullet from screen
;     lda FrameCounter
;     and #3
;     tax
    jsr MoveBulletOffScreenSubroutine2  ;+20    25      removing bullet indexed by Y.  inline to save 12 cycles
;     lda #BALLOFFSCREEN
;     sta BulletX,Y
;     sta BulletY,Y
    ;--and now we don't need bullet index any longer 
        
        
    ;--we will loop through the 8 bricks we want to blow up, starting in upper left and moving right and down
    ;first, get X position of upper left brick and convert to column
    ;--it ACTUALLY DOESN'T matter if bullet is moving left or right since we are starting from the tank (=powerup) position, not bullet position

    lda #-20
    clc
    adc TankX,X                         ;+8     33
                ;subtract 1/2 tank width (4) and also 16 pixels so we are in the center of the block to the left, 
                ;adjusted for column 0 starting at pixel 16
 
    lsr
    lsr
    lsr
    sta Temp+2                          ;+9     42      column
    
	;--now get row (fast divide by seven routine)
	lda TankY,X
    clc
    adc #TANKHEIGHT/2
    sta Temp+6   
	lsr
	lsr
	lsr
	adc Temp+6
	ror
	lsr
	lsr
	adc Temp+6
	ror
	lsr
	lsr
	sta Temp+6                          ;+38    80      store row back into Temp+6

	    ;--at this point we have position, in Temp+2 (column), Temp+6 (row), of upper left of the 9 brick square:
	    /*
	        P=powerup icon
	        X=bricks to remove
	        *=position pointed to by Temp+2, Temp+6
	        
	           *XX 
	           XPX
	           XXX
	    */

	;--save Y
	tya
	pha
	lda #>PF1Left                       ;               this value is zero
	sta MiscPtr+1
	lda #<PF1Left
	sta MiscPtr                         ;+15    95
	
	;now remove bricks in a loop.  looping through three columns to speed this up.
	lda Temp+2
	ldy #2
	sty Temp+7
	bne .LoopEntryPoint                 ;+11    106     branch always
.RemoveBricksLoop
    sty Temp+7      ;save loop index 
	lda #<PF1Left
	sta MiscPtr                         ;+8      8 (2nd)    counting from start of second loop
    ;--first, move to next row and column
    lda Temp+2  ;column
    clc
    adc RemoveBrickColumnShift,Y
    and #%00011111      ;account for "negative" bricks (which will have column = 31 / $1F)
    sta Temp+2                          ;+14    22 (2nd)    counting from start of second loop
.LoopEntryPoint
	cmp #16                             
	bcs .OffMazeGoToNext                ;+4/5   4/5 (1st)   cycle counts will be internal to this loop and the maximum possible
    ;--check if we are on row zero
	lda Temp+6
	bne .NotRowZero                     ;+5/6   9/10 (1st)
.RowZeroCheck    
	;--specific check for row zero: only care about brick 6 and 9 (if explosion hits base, should we end game?)
	lda Temp+2
	sec             ;I think carry is always clear here, whether coming from above or below.  So if we need to save 6 cycles and 1 byte, we could remove and change to SBC #5 below
	sbc #6 
	beq .ItIsBrick6Row0                 ;+9/10  18/19 (1st)
.NotBrick6Row0
    sbc #3
    bne .NotBrick9Row0	                ;+4/5   22/23 (1st)
    sta LastRowR
    beq .GoToNextBrick                  ;+6     28 (1st)    branch always following not-taken BNE above
.ItIsBrick6Row0                         ;       19 (1st)
    ;--following subtractions, A holds zero
    sta LastRowL
    beq .GoToNextBrick                  ;+6     25 (1st)    branch always, we reach .ItIsBrick6Row0 via BEQ branch
.NotRowZero                             ;       10 (1st)
    ;first, adjust MiscPtr because we will use overflow flag below and so all additions/subtractions need to be moved above
    ;--get column into X
    ldx Temp+2
    ;--get which PF register and use it to adjust our pointer
    lda PFRegisterLookupBank2,X         
;     clc   ;--not necessary following not-taken BCS branch after .LoopEntryPoint label
    adc MiscPtr
    sta MiscPtr                         ;+13    23 (1st)

	;--check for row > MAZEROWS - 1 (means we are either above or below maze)
	clv     ;we will use overflow as flag to blow up 2 or three bricks (clear means blow up 3, set means blow up 2)
	lda Temp+6
	cmp #MAZEROWS
	bcc .InMazeBlowUpBricks             ;+9/10  34/35 (1st)
	;--so if we are here we want to ignore the top row and blow up the middle and bottom row.... hm.
	;--first, set overflow
	bit Return                          ;               set overflow, which is flag so we know to blow up two bricks not three
	dec Temp+6                          ;+9     43      and decrease the row by one (will have to adjust later!)
.InMazeBlowUpBricks	                    ;       35/43 (1st) if we branch here (lower cycle count), we will blow up 3 bricks, if we fell through we will blow up 2
                                        ;                   blowing up three bricks is the longer path so we will count below for that only
    ;--if we are here, we have a legit brick location and we will remove it
    ;--get row into Y
    ldy Temp+6
    dey                                 ;+5     40 (1st)    adjust because Temp+6 is zero-indexed for entire maze but the PF data stored in RAM starts at row 1
    ;--then lookup specific brick mask
    lda PFClearLookup,X
    tax ;save for below 
    ;--then erase brick
    and (MiscPtr),Y
    sta (MiscPtr),Y                     ;+16    56 (1st)
    tya ;we decremented Y above already, this resets flags
    beq .RowZeroCheck                   ;+4/5   60/61 (1st) this branch goes UP to the row zero check, which takes 28 cycles max
    ;not on row zero, so clear the next brick down
    dey    
    txa             ;restore PFClearLookup,X
    ;--then erase brick
    and (MiscPtr),Y
    sta (MiscPtr),Y                     ;+14    74 (1st)
    ;--if overflow cleared, we do one more row
    bvc .BlowUpLastRow                  ;+2/3   84/77 (1st) will only count for taking this branch (if we don't take the branch, we are 8 cycles further along) 
    ;--if overflow set, we readjust the initial row and then start the loop again
    inc Temp+6
    bne .GoToNextBrick                  ;+8     92 (1st)    branch always
.BlowUpLastRow                          ;       77 (1st)
    tya ;we decremented Y above already, this resets flags
    beq .RowZeroCheck                   ;+4/5   81/82 (1st) or 103/104 (2nd) branch goes UP to row zero check, which takes either 23 (blow up no brick on bottom row), 25 (blew up left brick on bottom row), or 28 (blew up right brick on bottom row)
    ;not on row zero, so clear the next brick down
    dey    
    txa             ;restore PFClearLookup,X
    ;--then erase brick
    and (MiscPtr),Y
    sta (MiscPtr),Y                     ;+14    95
    
.NotBrick9Row0    
.OffMazeGoToNext  
.GoToNextBrick                          ;       110 (1st) 127 (2nd) 127 (3rd)
    ldy Temp+7
    dey
    bpl .RemoveBricksLoop               ;+7/8   118 (1st) 135 (2nd) 134 (3rd)
                                        ;       118+135+134 is "theoretical" maximum, if blowing up 6 bricks above base and the right brick on the bottom row.   
                                        ;       
                                        ;total= 387
                                        ;+387   493
    pla
    tay     ;restore Y index into bullet
    pla
    tax                                 ;+12    505     restore X index into tank
Return    
    rts                                 ;+6     511     plus six  for JSR
    

    
    
;****************************************************************************
    SUBROUTINE
GetInitialTankCountSubroutine
    lda MazeNumber
    asl
    asl
    clc
    adc #4
    cmp #MAXTANKSREMAINING
    bcc .FoundInitialTankCount  ;14/15
    lda #MAXTANKSREMAINING      ;16
.FoundInitialTankCount
    rts                         ;21/22 cycles (plus 6 for JSR)
            
;****************************************************************************

    SUBROUTINE    
ScoreForKillingTank                         ;+6      6  6 to JSR here
    txa
    pha ;save X
    
    lda GameStatus3
    and #CONSECUTIVEKILLBITS                ;+10    16
    
    
 
    asl     ;multiply by two because indexing into table of words  
    tax
    sed                                     ;                   set decimal mode, is cleared by subroutine below!
    lda #<KILLTANKSCORE                     ;+8     24
;    clc                                   ;                   carry cleared by ASL above, A always holds 0 through 20 before ASL
    adc ConsecutiveKillBonusTable,X
    sta Temp+1                              ;+7     31
    lda #>KILLTANKSCORE
    adc ConsecutiveKillBonusTable+1,X
    sta Temp                                ;+9     40
    lda Temp+1                              ;+3     43
    jsr IncreaseScoreSubroutine             ;+41    84
	pla
	tax ;--restore X
    rts                                     ;+12    96 takes about 1.33 scanlines maximum

   
    
;****************************************************************************
    
    


;****************************************************************************


BulletHitBrickSubroutine
	;--first, add to score ONLY if player bullet
	cpx #2
	bcs NoScoreForWallDestructionByEnemies  ;+4/5   4/5
	lda #>WALLDESTRUCTIONSCORE
	sta Temp
	lda #<WALLDESTRUCTIONSCORE              ;+7     11
	jsr IncreaseScoreSubroutine             ;+41    52
NoScoreForWallDestructionByEnemies	        ;       5/52    below will count maximum cycles (i.e., from 52)
	;--get row of block into Y and column into Temp+1

	lda BulletY,X
	sec
	sbc #2                                  ;+8     60
	;--use constant 27-cycle divide by 7 to get row instead of repeated-subtraction method
	
	tay
	lda BlockRowTable,Y                     ;+6     66  can use this as a divide table only in bank 1
	
	
	tay                     
	;--special check for row = 0
	bne BulletNotOnBottomRow                ;+4/5   70/71
	;--all we care is left or right of center
	lda BulletX,X   
	cmp #80
	bcc BulletOnLeftLastRow                 ;+8/9   78/79
	lda #0
	sta LastRowR
	beq RemovedWallBlock                    ;+8     86      branch always
BulletOnLeftLastRow                         ;       79
	lda #0
	sta LastRowL
	beq RemovedWallBlock                    ;+8     87      branch always
BulletNotOnBottomRow                        ;       79
    lda BulletDirection
    and BulletDirectionMask,X
    cmp BulletLeftBank2,X
    beq BulletMovingLeft                    ;+13/14 92/93
    lda #-16
    .byte $2C                               ;+6     98      skip two bytes
BulletMovingLeft                            ;       93/98   will count from maximum below
    lda #-17
    clc            
    adc BulletX,X
	lsr
	lsr
	lsr
	sta Temp+1
	txa
	pha                                     ;+22    120
		
	lda #<PF1Left
	sta MiscPtr
	lda #>PF1Left                           ;               this value is zero (zero page)
	sta MiscPtr+1                           ;+10    130
	ldx Temp+1
	lda PFMaskLookupBank2,X
	eor #$FF
	pha                                     ;+12    142
	lda PFRegisterLookupBank2,X
	clc
	adc MiscPtr
	sta MiscPtr                             ;+12    154
	pla
	dey
	and (MiscPtr),Y
	sta (MiscPtr),Y	                        ;+16    170
	pla
	tax                                     ;+6     176
RemovedWallBlock                            ;       86/87   from branches above or 176 if dropping directly through.  Will count from maximum
	;--bullet has hit block, remove bullet from screen:
    lda #BALLOFFSCREEN                      ;               this value is zero
    sta BulletX,X
    sta BulletY,X
	;--play brick explosion sound
	ldy #BRICKSOUND                         ;+2     200
	jsr StartSoundSubroutineBank2           ;+40    240
	

    rts                                     ;+6     246
    
;****************************************************************************

    
TankRespawnRoutineWrapperPlayer
    ldx #0    
    jsr TankRespawnRoutine
    jmp ReturnFromBSSubroutine2

TankRespawnRoutineWrapperEnemy
    lda FrameCounter
    and #3
    tax
    jsr TankRespawnRoutine
    jmp ReturnFromBSSubroutine2
    

TankRespawnRoutine  ;come in with X holding tank number (0 = player, 1-3 = enemy tanks)
	txa
	bne EnemyTankRespawnRoutine     ;+4/5
	;--if player tank we are here:
    ;--increase counter of number of deaths (in MazeGenerationPass) then increase respawn wait
    lda MazeGenerationPass
    and #PLAYERDEATHCOUNTBITS
    cmp #PLAYERDEATHCOUNTMAX
    beq PlayerHasDiedMaxTimes       ;+9/10  13/14
    
    inc MazeGenerationPass          
    lda MazeGenerationPass
    and #PLAYERDEATHCOUNTBITS       ;+10    23
PlayerHasDiedMaxTimes               ;       cycle counts from this point are for worst case scenario (i.e., 23)
    tay
	lda PlayerStartingStatus,Y
	sta TankStatus
	lda #PLAYERSTARTINGX
	sta TankX
	lda #PLAYERSTARTINGY
	sta TankY
	rts                             ;+25    48      maximum cycle count for player tank
EnemyTankRespawnRoutine             ;        5
	lda StartingTankStatus,X  
	sta TankStatus,X                ;+8     13

	;--new respawn routine: randomly pick between 2 respawn spots for each tank
	;   formula is tank # times 2 + rand(0, 1)
	lda RandomNumber
	lsr
	dex                             ;           change index from 1-3 to 0-2
	txa
	inx                             ;           get X back to 1-3
	rol
	tay	                            ;+15    28  at this point Y holds 0, 1, 2, 3, 4, 5

	;--if player tank is in top part of screen, use different starting positions
	lda TankY
	cmp #BLOCKHEIGHT*7
    bcs PlayerNearTop               ;+7/8   35/36
    tya
    adc TankRespawnPlayerBottom,X   ;               carry clear following not-taken BCS branch above
    tay
    bcc LeftRightRespawnAdjust      ;+11    46      branch always 
PlayerNearTop                       ;       36
    tya
    clc             ;<--could remove this CLC by adjusting the table in command below
    adc TankRespawnPlayerTop,X
    tay                             ;+10    46
LeftRightRespawnAdjust              ;       46      cycle count is same regardless of which path, coincidentally
	;--use alternate positions
	;--if player is on left part of screen, use variable respawn locations
	lda TankX
	cmp #80
	bcc PlayerLeft                  ;+7/8   53/54
    ;--else player right
    tya
    clc
    adc TankRespawnPlayerRight,X
    tay
    bcc SetEnemyRespawnPosition     ;+13    66      branch always 
PlayerLeft                          ;       54
    tya
    adc TankRespawnPlayerLeft,X    ;carry already clear following BCC 
    tay                             ;+8     62
SetEnemyRespawnPosition             ;       62 or 66 cycles here
	lda RespawnTankXPosition,Y
	sta TankX,X
	tya
	lsr
	tay
    lda RespawnTankYPosition,Y
	sta TankY,X                     ;+22    84/88

    rts                             ;+6     90/94

;****************************************************************************

    
    SUBROUTINE   
    ;--max number of tanks that can be killed is two
    ;--cycle counts below are off, K
KillAllTanksBonus
    ldx #3                              ;+2      2
.KillAllTanksLoop
    lda TankStatus,X
    lsr
    bcc .TankAlreadyDeadCannotKillAgain ;+8/9   10/11   ....we really should see if the tank is on screen also.
    jsr KilledTankViaPowerUp            ;+141   151       assuming that level is not ended, if level is ended this takes a bit longer.
    jsr ScoreForKillingTank             ;+96    247
.TankAlreadyDeadCannotKillAgain
    dex
    bne .KillAllTanksLoop               ;+4/5   252
    ;--so we loop through 3 times.  worst case: twice we blow up a tank (252 cycles) and once we don't (11 cycles).  minus 1 for the last time through the loop
                                        ;+515           worst case scenario
    
    rts                                 ;+6     521     worst case scenario (7 scanlines ooooooooooooof)

 
;****************************************************************************

BulletHitTank
    ;  Y is index into bullet
    ;  X is index into which tank
	;--bullet did hit tank.
	;	remove bullet and tank from screen
	
    lda GameStatus2
    and TanksKilled,X           ;X is index into which tank
    bne AlreadyKilledThisTankThisFrame
	
	
	
	jsr MoveBulletOffScreenSubroutine2  ;+28    28
; 	lda #BALLOFFSCREEN
; 	sta BulletX,Y
; 	sta BulletY,Y
		
    ;--first, reduce powerup tank kill countdown only if player killed the tank
    cpy #2
    bcs EnemyTankBulletsNoUpdateToPowerUpCountdown      ;+4/5   34/35   player bullets are indexed 0 & 1
KilledTankViaPowerUp    
	;--player bullets.  update power-up countdown.
    lda MazeGenerationPass  ;used for powerup stuff during levels
    and #POWERUPCOUNTDOWNBITS
    beq NoUpdateToPowerUpCountdown      ;+7/8   41/42
    sec
    sbc #POWERUPCOUNTDOWNDECREMENT
    sta Temp                            ;+7     48
    lda MazeGenerationPass
    and #~POWERUPCOUNTDOWNBITS
    ora Temp
    sta MazeGenerationPass              ;+11    59  
NoUpdateToPowerUpCountdown              
    ;--also need to increase the tank kill count
    lda GameStatus3
    and #CONSECUTIVEKILLBITS
    cmp #MAXIMUMCONSECUTIVEKILLS
    beq NoUpdateToConsecutiveKillCount
    clc
    adc #1
    sta Temp
    lda GameStatus3
    and #~CONSECUTIVEKILLBITS
    ora Temp
    sta GameStatus3                     ;+27    86  maximum cycle-count path for player bullets
                                        ;       69 maximum cycle count path for "KilledTankViaPowerUp"
NoUpdateToConsecutiveKillCount
EnemyTankBulletsNoUpdateToPowerUpCountdown	;       35 when branching here, for enemy tank bullets
	
PlayerHitTank       ;cycle counts below are relative to this point
    lda GameStatus2
    and TanksKilled,X           ;X is index into which tank
    bne AlreadyKilledThisTankThisFrame

	;--haven't killed it, so set the bit
	lda GameStatus2
    ora TanksKilled,X
    sta GameStatus2
    
    
    ;--save and restore Y in this routine
	tya
	pha                             ;+5     5

		
	ldy #ENEMYTANKSOUND
	jsr StartSoundSubroutineBank2   ;+36    41
	
	;--clear "in play" flag, set wait counter, and set direction
    lda #TANKINPLAY ;--clear all bits except tank in play so we don't screw up subsequently-processed collisions
    sta TankStatus,X                ;+6     47

    txa
	bne EnemyTankHit                ;+4/5   51/52
	;--if player hit, remove all powerups and restart the power up countdown
	lda MazeGenerationPass
	and #~(POWERUPACTIVATEDBITS|POWERUPCOUNTDOWNBITS)
	ora #POWERUPCOUNTDOWNRESET
	sta MazeGenerationPass
	;--also reset the consecutive kill count
	lda GameStatus3
	and #~CONSECUTIVEKILLBITS
	sta GameStatus3
	
	
	jmp PlayerTankHit               ;+13    64
EnemyTankHit                        ;       52
    ;--decrease tanks remaining ONLY if enemy tank hit
	dec TanksRemaining
	bne LevelNotComplete            ;+7/8   59/60
	;--killed last tank, level is complete:
	;--set levelcomplete flag
	lda GameStatus
	ora #LEVELCOMPLETE|LEVELCOMPLETETIMER
	sta GameStatus
	;--set RandomNumberSaved to zero (this is used for the points-for-bricks routine)
	lda #0
	sta RandomNumberSaved
	;--stop player tank from moving
	lda TankStatus
	and #~TANKSPEED
	sta TankStatus
	
	

	
	ldy #3
; 	lda #BALLOFFSCREEN
RemoveBulletsFromScreenLoop
; 	sta BulletX,Y
; 	sta BulletY,Y
    jsr MoveBulletOffScreenSubroutine2
	dey
	bpl RemoveBulletsFromScreenLoop
	
	
LevelNotComplete                ;       60  cycle counts below are relative to the level NOT ending
PlayerTankHit	
MoreThanFourTanksRemainingStill    

	;--save and restore Y in this routine
	pla
	tay
AlreadyKilledThisTankThisFrame
	rts                         ;+12    72
                                ;   +59          longest path for BulletHitTank (player bullets)
                                ;   +35          longest path for BulletHitTank (enemy bullets)
                                ;   +69 141      longest path for KilledTankViaPowerUp
                                
                                
                                
;****************************************************************************
    SUBROUTINE
StartSoundSubroutineBank2
	
	lda Channel1Decay
	and #$70
	bne .DoNotStartSound        ;+7/8   7/8
	lda ToneBank2,Y
	sta AUDC1
	lda FrequencyBank2,Y
	sta AUDF1
	lda SoundLengthBank2,Y
	sta Channel1Decay           ;+21    28
.DoNotStartSound
	rts                         ;+6     34      maximum cycle count is 34, minimum is 14
	

	
;****************************************************************************

DrawTitleScreenSubroutine

	;--TODO: interleave the gfx data so this loop can be cleaned up.... maybe.
	ldx MazeGenerationPass
PutTitleGraphicsInPlayfieldLoop
	txa
	and #3
	tay
	lda PFPointer,Y
	sta MiscPtr
    lda #0      ;PF is in RAM; i.e., zero page
	sta MiscPtr+1
	
	txa
; 	asl             ;is this necessary?
; 	tay             ;is this necessary
	cmp #((TitleGraphicsEnd-TitleGraphics-1)/2)+(TitleGraphicsEnd-TitleGraphics-1)/4)-1
	bcc DrawTitleScreenModerately
	;--else slower
	lda FrameCounter
	and #$18
	bne SkipDrawingTitleScreenThisFrame
DrawTitleScreenModerately	
	cmp #((TitleGraphicsEnd-TitleGraphics-1)/2)-1
	bcc DrawTitleScreenFast
	lda FrameCounter
	and #$8
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
    lda #255;(FanfarePatternEnd-FanfarePattern-1)       ;need to start on last note so we can immediately wrap to first note.
    sta SongIndex
	ldy #LONGEXPLOSIONSOUND
PlayShortSound
	jsr StartSoundSubroutineBank2

SkipDrawingTitleScreenThisFrame	
	
; 	rts
    jmp ReturnFromBSSubroutine2

;****************************************************************************


	
;****************************************************************************

    ;--this is to make sure the "DivideLoop" doesn't cross a page boundary.
    if ((* + 5) & $FF00) != ((* + 9) & $FF00)
        echo "---Aligned PositionASpriteSubroutineBank2 -", $FF - ((* + 4) & $FF), "bytes left at location", *
        ds $FF - ((* + 4) & $FF)
    else
        echo "---Aligned PositionASpriteSubroutineBank2 not necessary"
    endif
    SUBROUTINE
PositionASpriteSubroutineBank2
    sec
	sta HMCLR
PositionASpriteSubroutineBank2NoHMCLR	
	sta WSYNC
.DivideLoop			;				this loop can't cross a page boundary!!!
	sbc #15
	bcs .DivideLoop	;+4		 4
	eor #7
	asl
	asl
	asl
	asl				;+10	14      sets carry
	sta.wx HMP0,X	;+5		19
	sta RESP0,X		;+4		23
	sta WSYNC
	sta HMOVE
	rts                 ;+9      9

	
;****************************************************************************

; MoveBulletOffScreenSubroutine
;     lda #BALLOFFSCREEN
;     sta BulletX,X
;     sta BulletY,X
;     rts                 ;+16    16      plus 6 for JSR

;****************************************************************************
	
	
;------------------------------------------------------
;---------------------DATA-----------------------------
;------------------------------------------------------


    ALIGNGFXDATA 1
    
/*
new random maze gfx
Morph1
        .byte #%00111110;--2
        .byte #%00101000;--4
        .byte #%01011110;--6
        .byte #%01000001;--8
        .byte #%01011110;--10
        .byte #%00101000;--12
Morph1b
Morph2
Morph2b


*/
; 
; new maze (standard vs random) gfx:
StandardMazeImage
        .byte 0
        .byte #%01011011;--2
        .byte #%01001000;--4
        .byte #%11011111;--6
        .byte #%00000001;--8
        .byte #%01011101;--10
        .byte #%01001000;--12
StandardMazeImageb
        .byte 0
        .byte #%01011011;--1
        .byte #%01001000;--3
        .byte #%11011111;--5
        .byte #%00000001;--7
        .byte #%01011101;--9
        .byte #%01001000;--11
        .byte 0
MorphImage1 = * - 1
        .byte #%00111110;--2
        .byte #%00101000;--4
        .byte #%01011110;--6
        .byte #%01000001;--8
        .byte #%01011110;--10
        .byte #%00101000;--12
MorphImage1b
        .byte 0
        .byte #%00111110;--1
        .byte #%00101000;--3
        .byte #%01011110;--5
        .byte #%01000001;--7
        .byte #%01011110;--9
        .byte #%00101000;--11
        .byte 0


; animation runs maze-morph1-morph2-questionmark-questionmark-morph2-morph1-maze and then loop to beginning 
; (so holding on maze for 2 beats and holding on question mark for 2 beats)




	
    PAGEALIGN 3
    
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
; 	.byte NOMOVEMENT|(QUADWIDTHMISSILE>>2)|(THREECOPIESCLOSE>>2)
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

	
PlayerTankColor
    .byte TANKCOLOR1, TANKCOLOR3
    
	
	
    ALIGNGFXDATA 2
	
TankGfxVertical
     ;--animation for upward facing tank:   
TankUpAnimated1
		.byte 0
        .byte #%10111101;--2
        .byte #%00111101;--4
        .byte #%10100101;--6
        .byte #%00011011;--8
        .byte #%11011011;--10
        .byte #%00011011;--12
TankUpAnimated1b
		.byte 0
        .byte #%11000000;--1
        .byte #%10111101;--3
        .byte #%10111100;--5
        .byte #%10111101;--7
        .byte #%11011000;--9
        .byte #%11011011;--11
        .byte 0
TankUpAnimated2 = * - 1
        .byte #%10111101;--2
        .byte #%10111100;--4
        .byte #%10100101;--6
        .byte #%11011000;--8
        .byte #%11011011;--10
        .byte #%11011000;--12
TankUpAnimated2b
		.byte 0
        .byte #%11000011;--1
        .byte #%00111101;--3
        .byte #%10111101;--5
        .byte #%00111101;--7
        .byte #%11011011;--9
        .byte #%00011011;--11
		.byte 0
TankUpAnimated3 = * - 1
; 		.byte 0
        .byte #%00111101;--2
        .byte #%10111101;--4
        .byte #%00100101;--6
        .byte #%11011011;--8
        .byte #%00011011;--10
        .byte #%11011011;--12
TankUpAnimated3b
		.byte 0
        .byte #%11000011;--1
        .byte #%10111100;--3
        .byte #%10111101;--5
        .byte #%10111100;--7
        .byte #%11011011;--9
        .byte #%11011000;--11
		.byte 0
TankUpAnimated4 = * - 1
        .byte #%10111100;--2
        .byte #%10111101;--4
        .byte #%10100100;--6
        .byte #%11011011;--8
        .byte #%11011000;--10
        .byte #%11011011;--12
TankUpAnimated4b
		.byte 0
        .byte #%00000011;--1
        .byte #%10111101;--3
        .byte #%00111101;--5
        .byte #%10111101;--7
        .byte #%00011011;--9
        .byte #%11011011;--11
		.byte 0
TankDownAnimated1 = * - 1
        .byte #%11011011;--2
        .byte #%11011000;--4
        .byte #%10111101;--6
        .byte #%10111100;--8
        .byte #%10111101;--10
        .byte #%11000000;--12
TankDownAnimated1b
		.byte 0
        .byte #%00011011;--1
        .byte #%11011011;--3
        .byte #%00011011;--5
        .byte #%10100101;--7
        .byte #%00111101;--9
        .byte #%10111101;--11
		.byte 0
TankDownAnimated2 = * - 1
        .byte #%00011011;--2
        .byte #%11011011;--4
        .byte #%00111101;--6
        .byte #%10111101;--8
        .byte #%00111101;--10
        .byte #%11000011;--12
TankDownAnimated2b
		.byte 0
        .byte #%11011000;--1
        .byte #%11011011;--3
        .byte #%11011000;--5
        .byte #%10100101;--7
        .byte #%10111100;--9
        .byte #%10111101;--11
		.byte 0
TankDownAnimated3 = * - 1
        .byte #%11011000;--2
        .byte #%11011011;--4
        .byte #%10111100;--6
        .byte #%10111101;--8
        .byte #%10111100;--10
        .byte #%11000011;--12
TankDownAnimated3b
		.byte 0
        .byte #%11011011;--1
        .byte #%00011011;--3
        .byte #%11011011;--5
        .byte #%00100101;--7
        .byte #%10111101;--9
        .byte #%00111101;--11
		.byte 0
TankDownAnimated4 = * - 1
        .byte #%11011011;--2
        .byte #%00011011;--4
        .byte #%10111101;--6
        .byte #%00111101;--8
        .byte #%10111101;--10
        .byte #%00000011;--12
TankDownAnimated4b
		.byte 0
        .byte #%11011011;--1
        .byte #%11011000;--3
        .byte #%11011011;--5
        .byte #%10100100;--7
        .byte #%10111101;--9
        .byte #%10111100;--11
		.byte 0
PlayerTankUp1   = * - 1
        .byte #%11011011;--2
        .byte #%10111101;--4
        .byte #%10111101;--6
        .byte #%11011011;--8
        .byte #%11011011;--0
        .byte #%11011011;--2

PlayerTankUp1b
        .byte 0
        .byte #%11000000;--1
        .byte #%00111101;--3
        .byte #%10111100;--5
        .byte #%00011011;--7
        .byte #%11011000;--9
        .byte #%00011011;--1
        .byte 0        
PlayerTankUp2   = * - 1
        .byte #%00011011;--2
        .byte #%10111100;--4
        .byte #%00111101;--6
        .byte #%11011000;--8
        .byte #%00011011;--0
        .byte #%11011000;--2
PlayerTankUp2b
        .byte 0
        .byte #%11000011;--1
        .byte #%10111101;--3
        .byte #%10111101;--5
        .byte #%11011011;--7
        .byte #%11011011;--9
        .byte #%11011011;--1
        .byte 0
PlayerTankUp3   = * - 1
        .byte #%11011011;--2
        .byte #%10111101;--4
        .byte #%10111101;--6
        .byte #%11011011;--8
        .byte #%11011011;--0
        .byte #%11011011;--2
PlayerTankUp3b
        .byte 0
        .byte #%00000011;--1
        .byte #%10111100;--3
        .byte #%00111101;--5
        .byte #%11011000;--7
        .byte #%00011011;--9
        .byte #%11011000;--1
        .byte 0
PlayerTankUp4   = * - 1
        .byte #%11011000;--2
        .byte #%00111101;--4
        .byte #%10111100;--6
        .byte #%00011011;--8
        .byte #%11011000;--0
        .byte #%00011011;--2
PlayerTankUp4b
        .byte 0
        .byte #%11000011;--1
        .byte #%10111101;--3
        .byte #%10111101;--5
        .byte #%11011011;--7
        .byte #%11011011;--9
        .byte #%11011011;--1
        .byte 0		
        
TanksKilled
    .byte PLAYERKILLED, TANK1KILLED, TANK2KILLED, TANK3KILLED

PFPointer
	.byte <PF1Left, <PF2Left, <PF2Right, <PF1Right
    
    
            
		PAGEALIGN 4
	
DigitData
One
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00111000;--
        .byte #%00011000;--
Seven
        .byte #%00110000;--
        .byte #%00110000;--
        .byte #%00011000;--
        .byte #%00001100;--
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%11111110;--
Two = * - 1
;         .byte #%11111110;--
        .byte #%11000000;--
        .byte #%01111000;--
        .byte #%00011110;--
        .byte #%00000110;--
        .byte #%11000110;--
        .byte #%01111100;--
Three = * - 1
;         .byte #%01111100;--
        .byte #%11000110;--
        .byte #%00000110;--
        .byte #%00001100;--
        .byte #%00000110;--
        .byte #%11000110;--
        .byte #%01111100;--
Five = * - 1
;         .byte #%01111100;--
        .byte #%11000110;--
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%11111100;--
        .byte #%11000000;--
        .byte #%11111110;--
Four
        .byte #%00001100;--
        .byte #%00001100;--
        .byte #%11101110;--
        .byte #%11001100;--
        .byte #%01101100;--
        .byte #%00101100;--
        .byte #%00001100;--
Six
        .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11101100;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%01101100;--
Eight = * - 1
;         .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--
Nine = * - 1
;         .byte #%01101100;--
        .byte #%00000110;--
        .byte #%00000110;--
        .byte #%01101110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--
Zero = * - 1
;         .byte #%01101100;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%11000110;--
        .byte #%01101100;--
BlankDigit
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0        
    .byte 0
        
        
M0CollisionTable = * - 1
	;.byte 0, 1
P1CollisionTable
	.byte 1, 2
M1CollisionTable
	.byte 2, 3	
P0CollisionTable
	.byte 3, 0     




    ALIGNGFXDATA 3
    echo "If more than 0 bytes free at ALIGNGFXDATA 3 then check prior table"
TankGfxHorizontal
TankRightAnimated1
        .byte 0
        .byte #%10111011;--2
        .byte #%01111000;--4
        .byte #%01101111;--6
        .byte #%01101111;--8
        .byte #%00000111;--10
        .byte #%01110111;--12
TankRightAnimated1b
		.byte 0
        .byte #%10111011;--1
        .byte #%10000011;--3
        .byte #%01101111;--5
        .byte #%01101111;--7
        .byte #%01111000;--9
        .byte #%01110111;--11
		.byte 0
TankRightAnimated2 = * - 1
        .byte #%01110111;--2
        .byte #%01111000;--4
        .byte #%01101111;--6
        .byte #%01101111;--8
        .byte #%10000110;--10
        .byte #%11101110;--12
TankRightAnimated2b
		.byte 0
        .byte #%01110111;--1
        .byte #%00000111;--3
        .byte #%01101111;--5
        .byte #%01101111;--7
        .byte #%01111000;--9
        .byte #%11101110;--11
		.byte 0
TankRightAnimated3 = * - 1
        .byte #%11101110;--2
        .byte #%01111000;--4
        .byte #%01101111;--6
        .byte #%01101111;--8
        .byte #%10000101;--10
        .byte #%11011101;--12
TankRightAnimated3b
		.byte 0
        .byte #%11101110;--1
        .byte #%10000110;--3
        .byte #%01101111;--5
        .byte #%01101111;--7
        .byte #%01111000;--9
        .byte #%11011101;--11
		.byte 0
TankRightAnimated4 = * - 1
        .byte #%11011101;--2
        .byte #%01111000;--4
        .byte #%01101111;--6
        .byte #%01101111;--8
        .byte #%10000011;--10
        .byte #%10111011;--12
TankRightAnimated4b
		.byte 0
        .byte #%11011101;--1
        .byte #%10000101;--3
        .byte #%01101111;--5
        .byte #%01101111;--7
        .byte #%01111000;--9
        .byte #%10111011;--11
		.byte 0

PlayerTankRight1 = * - 1
        .byte #%11011101;--       
        .byte #%00110000;--        
        .byte #%01111111;--
        .byte #%01111000;--
        .byte #%00000011;--        
        .byte #%01110111;--        
PlayerTankRight1b
        .byte 0
        .byte #%11011101;--
        .byte #%10000101;--
        .byte #%01111000;--
        .byte #%01111111;--
        .byte #%00110000;--
        .byte #%01110111;--
        .byte 0
PlayerTankRight2    = *-1
        .byte #%10111011;--
        .byte #%00110000;--
        .byte #%01111111;--
        .byte #%01111000;--
        .byte #%10000110;--
        .byte #%11101110;--
PlayerTankRight2b
        .byte 0
        .byte #%10111011;--      
        .byte #%10000011;--
        .byte #%01111000;--
        .byte #%01111111;--        
        .byte #%00110000;--        
        .byte #%11101110;--
        .byte 0
PlayerTankRight3    = *-1
        .byte #%01110111;--2
        .byte #%00110000;--4
        .byte #%01111111;--6
        .byte #%01111000;--8
        .byte #%10000101;--10
        .byte #%11011101;--12
PlayerTankRight3b
        .byte 0
        .byte #%01110111;--1
        .byte #%00000111;--3
        .byte #%01111000;--5
        .byte #%01111111;--7
        .byte #%00110000;--9
        .byte #%11011101;--11
        .byte 0        
PlayerTankRight4    = *-1
        .byte #%11101110;--2
        .byte #%00110000;--4
        .byte #%01111111;--6
        .byte #%01111000;--8
        .byte #%10000011;--0
        .byte #%10111011;--2

PlayerTankRight4b
        .byte 0
        .byte #%11101110;--1
        .byte #%10000110;--3
        .byte #%01111000;--5
        .byte #%01111111;--7
        .byte #%00110000;--9
        .byte #%10111011;--1
        .byte 0        
		
PlayerTankDown1 = * - 1       ;uses .byte 0 from previous table and the 0 in the following byte
        .byte #%00011011;--2
        .byte #%11011000;--4
        .byte #%00011011;--6
        .byte #%10111100;--8
        .byte #%00111101;--0
        .byte #%11000000;--2
PlayerTankDown1b
        .byte 0
        .byte #%11011011;--1
        .byte #%11011011;--3
        .byte #%11011011;--5
        .byte #%10111101;--7
        .byte #%10111101;-9
        .byte #%11011011;--1
        .byte 0        
PlayerTankDown2 =   * - 1
        .byte #%11011011;--2
        .byte #%11011011;--4
        .byte #%11011011;--6
        .byte #%10111101;--8
        .byte #%10111101;--0
        .byte #%11000011;--2
PlayerTankDown2b
        .byte 0
        .byte #%11011000;--1
        .byte #%00011011;--3
        .byte #%11011000;--5
        .byte #%00111101;--7
        .byte #%10111100;--9
        .byte #%00011011;--1
        .byte 0        
		
TankKilledImage = * - 1
        .byte #%01010100;--2
        .byte #%00111011;--4
        .byte #%01000100;--6
        .byte #%01100101;--8
        .byte #%01011100;--10
        .byte #%10010001;--12
        
TankKilledImageb     
        .byte 0   
        .byte #%10010010;--1
        .byte #%01001000;--3
        .byte #%10100100;--5
        .byte #%00000010;--7
        .byte #%00100100;--9
        .byte #%01001010;--11
        .byte 0
        
        
        
PowerUpImage1 = * - 1
        .byte #%00111100;--2
        .byte #%00111100;--4
        .byte #%01011010;--6
        .byte #%01111110;--8
        .byte #%10111101;--10
        .byte #%01000010;--12
        
PowerUpImage1b 
        .byte 0
        .byte #%00111100;--1
        .byte #%00111100;--3
        .byte #%01111110;--5
        .byte #%01111110;--7
        .byte #%11111111;--9
        .byte #%10000001;--11
        .byte 0
        
        
        
TanksRemainingGfx
	.byte %11101110
	.byte %11101110
	.byte %11101110
	.byte %01000100		
	.byte 0
	.byte %11101110
	.byte %11101110
	.byte %11101110
	.byte %01000100		
	
	
	
	PAGEALIGN 5
	
TitleGraphics
    ;-----PF1--------PF2--------PF2--------PF1------
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
	


	
	
PFMaskLookupBank2
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0
	.byte $C0, $30, $0C, $03
	.byte $03, $0C, $30, $C0

RotationOdd
	.byte 0	;and first 3 bytes of next table
RotationEven
	.byte 2, 1, 3, 0
PFRegisterLookupBank2 = * - 1   ;uses zero from previous table
	.byte /*0, */0, 0, 0
	.byte MAZEROWS-1, MAZEROWS-1, MAZEROWS-1, MAZEROWS-1
	.byte (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2, (MAZEROWS-1)*2
	.byte (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3, (MAZEROWS-1)*3

    ALIGNGFXDATA 4
       
        
        
PlayerTankDown3 
        .byte 0
        .byte #%11011000;--2
        .byte #%00011011;--4
        .byte #%11011000;--6
        .byte #%00111101;--8
        .byte #%10111100;--0
        .byte #%00000011;--2
PlayerTankDown3b
        .byte 0
        .byte #%11011011;--1
        .byte #%11011011;--3
        .byte #%11011011;--5
        .byte #%10111101;--7
        .byte #%10111101;--9
        .byte #%11011011;--1
        .byte 0
PlayerTankDown4 = * - 1
        .byte #%11011011;--2
        .byte #%11011011;--4
        .byte #%11011011;--6
        .byte #%10111101;--8
        .byte #%10111101;--0
        .byte #%11000011;--2
PlayerTankDown4b
        .byte 0
        .byte #%00011011;--1
        .byte #%11011000;--3
        .byte #%00011011;--5
        .byte #%10111100;--7
        .byte #%00111101;--9
        .byte #%11011000;--1
        .byte 0

PowerUpImage2 = * - 1       ;straight right
        .byte #%00011110;--2
        .byte #%00111110;--4
        .byte #%01110100;--6
        .byte #%01111000;--8
        .byte #%01100000;--10
        .byte #%00011100;--12
PowerUpImage2b        
        .byte 0
        .byte #%00000110;--1
        .byte #%00111110;--3
        .byte #%01111100;--5
        .byte #%01111100;--7
        .byte #%01111000;--9
        .byte #%00110000;--11
        .byte 0
        

PowerUpImage3 = * - 1       ;angled right
        .byte #%00011111;--2
        .byte #%00111111;--4
        .byte #%01101100;--6
        .byte #%01111100;--8
        .byte #%01011100;--10
        .byte #%00011001;--12
        
PowerUpImage3b
     .byte 0
        .byte #%00001111;--1
        .byte #%00111111;--3
        .byte #%01111110;--5
        .byte #%01111110;--7
        .byte #%01111100;--9
        .byte #%00100010;--11
     .byte 0     
MorphImage2 = * - 1
        .byte #%00011100;--2
        .byte #%00101000;--4
        .byte #%00001100;--6
        .byte #%00100010;--8
        .byte #%01101110;--10
        .byte #%00111100;--12
MorphImage2b
        .byte 0
        .byte #%00011100;--1
        .byte #%00101000;--3
        .byte #%00001100;--5
        .byte #%00100010;--7
        .byte #%01101110;--9
        .byte #%00111100;--11
        .byte 0
QuestionMarkImage = * - 1
        .byte #%00011000;--2
        .byte #%00011000;--4
        .byte #%00001100;--6
        .byte #%00000110;--8
        .byte #%01100110;--10
        .byte #%00111100;--12
QuestionMarkImageb
        .byte 0
        .byte #%00011000;--1
        .byte #%00000000;--3
        .byte #%00011000;--5
        .byte #%00001110;--7
        .byte #%01100110;--9
        .byte #%00111100;--11
        .byte 0
 

        
PlayerStartingStatus
    .byte TANKRIGHT|FIRSTDEATHDELAY, TANKRIGHT|SECONDDEATHDELAY, TANKRIGHT|THIRDDEATHDELAY, TANKRIGHT|FOURTHDEATHDELAY

    
DigitDataMissileLo
	.byte <MissileZero, <MissileOne, <MissileTwo, <MissileThree
	.byte <MissileFour, <MissileFive, <MissileSix, <MissileSeven
	.byte <MissileEight, <MissileNine
		
		

RotationTables
	.word RotationEven, RotationOdd	

	
    
    

	
BulletDirectionMask
; 	.byte %11, %11<<2, %11<<4, %11<<6       ;--these values are the same as BulletUpBank2
; 
BulletDirectionClearBank2
BulletUpBank2
	.byte	BULLETUP, BULLETUP<<2, BULLETUP<<4, BULLETUP<<6
BulletDownBank2
	.byte	BULLETDOWN, BULLETDOWN<<2, BULLETDOWN<<4, BULLETDOWN<<6
BulletLeftBank2
	.byte	BULLETLEFT, BULLETLEFT<<2, BULLETLEFT<<4, BULLETLEFT<<6
BulletRightBank2
	.byte	BULLETRIGHT, BULLETRIGHT<<2, BULLETRIGHT<<4, BULLETRIGHT<<6
	
TankCollisionRotationTables
    .word TankCollisionBitsEven, TankCollisionBitsOdd

    
TankCollisionBitsOdd    ;--this is a 4-byte table, uses first 3 bytes of next table
    .byte %01101000 ;M0 to P1, M1 to P1, and P0 to P1   -player tank (odd)
TankCollisionBitsEven
    .byte %11000100 ;M0 to P0, M0 to P1, and M0 to M1   -enemy tank 1 (odd), player tank (even)
    .byte %10011000 ;M0 to P0, M1 to P0, and P0 to P1   -enemy tank 2 (odd), enemy tank 1 (even)
    .byte %00110100 ;M1 to P1, M1 to P0, and M0 to M1   -enemy tank 3 (odd), enemy tank 2 (even)
    .byte %01101000 ;M0 to P1, M1 to P1, and P0 to P1   -enemy tank 3 (even) 
    
PFClearLookup
	.byte $3F, $CF, $F3, $FC
	.byte $FC, $F3, $CF, $3F
	.byte $3F, $CF, $F3, $FC
	.byte $FC, $F3, $CF, $3F
   
	 
;--these tables are offset into RespawnTankXPosition and RespawnTankYPosition	
;--tank 1 respawns farthest from player
;--tank 2 respawns middle
;--tank 3 respawns closest to player

;for left/right: 0 = left, 6 = right
TankRespawnPlayerRight = * - 1
    .byte 0, 6, 6
TankRespawnPlayerLeft = * - 2       ;uses 6 from previous table
    .byte /*6, */0, 0
; for top/bottom: 0 = top, 12 = bottom    
TankRespawnPlayerTop = * - 1
    .byte 12, 12, 0
TankRespawnPlayerBottom = * - 2     ;uses 0 from previous table
    .byte /*0, */0, 12

DigitDataLo
	.byte <Zero,<One,<Two,<Three,<Four,<Five,<Six,<Seven,<Eight,<Nine, <BlankDigit

    
        
;****************************************************************************

MoveBulletOffScreenSubroutine2
    lda #BALLOFFSCREEN
    sta BulletX,Y
    sta BulletY,Y
    rts                 ;+22    22  plus six for JSR

;****************************************************************************
    
    
    
    PAGEALIGN 6

    
	
StartingTankYPosition	
	.byte PLAYERSTARTINGY, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP, ENEMYSTARTINGYTOP 
StartingTankXPosition
	.byte PLAYERSTARTINGX, ENEMY1TOPLEFTSTARTINGX1, ENEMY2TOPLEFTSTARTINGX1, ENEMY3TOPRIGHTSTARTINGX1
    
    
    Echo "---", BLOCKHEIGHT+1 - (* & $FF), "bytes left at BlockRowTable"
		
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
	

TankDeadStatus
    .byte TANKUP|TANKDEADWAITPLAYER, TANKUP|TANKDEADWAIT, TANKUP|TANKDEADWAIT, TANKUP|TANKDEADWAIT

    
	
	
ToneBank2
	.byte BRICKSOUNDTONE, BULLETSOUNDTONE, ENEMYTANKSOUNDTONE, SHORTBRICKSOUNDTONE
	.byte LONGEXPLOSIONTONE, ENEMYBULLETSOUNDTONE, WALLSOUNDTONE, PLAYERTANKENGINETONE
	.byte SCORESOUNDTONE, POWERUPEXPLOSIONSOUNDTONE, SPEEDBOOSTSOUNDTONE, INVINCIBLETANKSOUNDTONE

FrequencyBank2
	.byte BRICKSOUNDFREQ, BULLETSOUNDFREQ, ENEMYTANKSOUNDFREQ, SHORTBRICKSOUNDFREQ
	.byte LONGEXPLOSIONFREQ, ENEMYBULLETSOUNDFREQ, WALLSOUNDFREQ, PLAYERTANKENGINEFREQ
	.byte SCORESOUNDFREQ, POWERUPEXPLOSIONSOUNDFREQ, SPEEDBOOSTSOUNDFREQ, INVINCIBLETANKSOUNDFREQ
	
SoundLengthBank2
	.byte BRICKSOUNDLENGTH, BULLETSOUNDLENGTH, ENEMYTANKSOUNDLENGTH, SHORTBRICKSOUNDLENGTH
	.byte LONGEXPLOSIONLENGTH, ENEMYBULLETSOUNDLENGTH, WALLSOUNDLENGTH, PLAYERTANKENGINELENGTH
	.byte SCORESOUNDLENGTH, POWERUPEXPLOSIONSOUNDLENGTH, SPEEDBOOSTSOUNDLENGTH|$80, INVINCIBLETANKSOUNDLENGTH
    
	
RemoveBrickColumnShift 
    .byte 1, 1, 0
	
NumberOfBitsSetBank2 = * - 1
	.byte /*0, */1, 1, 2
	.byte 1, 2, 2, 3
	.byte 1, 2, 2, 3
	.byte 2, 3, 3, 4

AllowCarveDownTable ;can't carve downward in the outer two columns
    .byte 0, 0, 1, 1
    .byte 1, 1, 1, 1
    .byte 1, 1, 1, 1
    .byte 1, 1, 0, 0	
RespawnCountdownGraphic = * - 1
    .byte /*$00, */$80, $C0, $E0
    .byte $F0, $F8, $FC, $FE
    
    
        
EntryPointRowOffsetTable
    .byte ENEMYSTARTINGYHIGHROW - 2, ENEMYSTARTINGYHIGHMIDROW - 2, ENEMYSTARTINGYMIDHIGHROW - 2
    .byte ENEMYSTARTINGYMIDROW - 2, ENEMYSTARTINGYMIDLOWROW - 2, ENEMYSTARTINGYLOWROW - 2
    
RandomMazeFrame
    .word StandardMazeImage+TANKHEIGHT, MorphImage1+TANKHEIGHT
    .word MorphImage2+TANKHEIGHT, QuestionMarkImage+TANKHEIGHT
	.word QuestionMarkImage+TANKHEIGHT, MorphImage2+TANKHEIGHT
	.word MorphImage1+TANKHEIGHT, StandardMazeImage+TANKHEIGHT

StartingTankStatus
	.byte TANKRIGHT|PLAYERRESPAWNDELAY, ENEMYTANK1DELAY, ENEMYTANK2DELAY, ENEMYTANK3DELAY

	
;****************************************************************************	

    echo "----", ($3FF1-*), " bytes left (ROM) at end of Bank 2"

   	org $2FF1
	rorg $3FF1
	
	
BankSwitchSubroutine2 
    ;--stuff below is not needed because we never jump TO a subroutine in the other bank from this bank, 
    ;   we only return FROM a subroutine in this bank to the other bank
; 	plp             ;+4
; 	tsx             ;+2
; 	dec $01,X       ;+6
; 	lda ($01,X)     ;+6
; 	sta MiscPtr     ;+3
; 	inc $01,X       ;+6
; 	lda ($01,X)     ;+6
; 	sta MiscPtr+1   ;+3
; ; 	lsr             
; ; 	lsr
; ; 	lsr
; ; 	lsr
; ; 	lsr             ;+10
; ; 	tax             ;+2
; ; 	nop $1FF8,X     ;+4
; BankSwitchAltRoutine2
;     nop $1FF8
; 
	jmp (MiscPtr)   ;+5     57
	
ReturnFromBSSubroutine2
; 	tsx             ;+2
; 	lda $02,X       ;+4     get high byte of return address
; 	lsr
; 	lsr
; 	lsr
; 	lsr
; 	lsr             ;+10
; 	tax             ;+2
; 	nop $1FF8,X     ;+4
    nop $1FF8

	rts             ;+6     28
EndBankSwitchRoutine2
	;--reserve space for hot spots
    org $2FF8
    rorg $3FF8
    
    ds 2
	
	
;****************************************************************************	


	org $2FFC
	rorg $3FFC
	.word (Start2 & $1FFF)|$1000
	.word BankSwitchSubroutine2
