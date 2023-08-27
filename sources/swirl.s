
         .PAGE  'DOCUMENTATION, EQUATES, STORAGE'
;        SWIRL DRAWING DEMONSTRATION FOR THE MICRO TECHNOLOGY UNLIMITED
;        VISIBLE MEMORY 320 BY 200 PIXEL DISPLAY

;        ENTER AT SWIRL WITH LINES, FREQ, AND DAMP SET TO APPROPRIATE
;        VALUES TO GENERATE AN SWIRLING DISPLAY. INTERRUPT WITH RESET
;        KEY WHEN PATTERN IS COMPLETED TO DESIRED EXTENT.

;        ENTER AT RSWIRL FOR AN ENDLESS SERIES OF PATTERNS USING
;        RANDOMLY SELECTED PARAMETERS.

;        GENERAL EQUATES

KIMMON   =      X'1C22       ; RESET ENTRY INTO KIM MONITOR
NX       =      320          ; NUMBER OF BITS IN A ROW
NY       =      200          ; NUMBER OF ROWS (CHANGE FOR HALF SCREEN
                             ; OPERATION)
NPIX     =      NX*NY        ; NUMBER OF PIXELS

         .=     0            ; START PROGRAM AT ZERO

;        STORAGE FOR SWIRL GENERATOR PROGRAM

LINES:   .BYTE  1            ; CONNECTING LINES IF NON-ZERO
FREQ:    .WORD  X'7E12       ; FREQUENCY
DAMP:    .WORD  X'7E00       ; 1-(DAMPING FACTOR)
COSINT:  .WORD  X'7800       ; INITIAL COSINE VALUE
                             ; GOOD VALUE FOR GENERAL USE BUT SHOULD BE
                             ; REDUCED TO X'70 TO PREVENT OVERFLOW WITH
                             ; RANDOMLY SELECTED PARAMETERS
COS:     .=.+   2            ; COSINE VALUE
SIN:     .=.+   2            ; SINE VALUE

;        GENERAL STORAGE

VMORG:   .BYTE  X'20         ; PAGE NUMBER OF FIRST VISIBLE MEMORY
                             ; LOCATION
RANDNO:  .WORD  X'1234       ; INITIAL RANDON NUMBER, MUST NOT BE ZERO
ADP1:    .=.+   2            ; ADDRESS POINTER 1
ADP2:    .=.+   2            ; ADDRESS POINTER 2
BTPT:    .=.+   1            ; BIT NUMBER
X1CORD:  .=.+   2            ; COORDINATE PAIR 1
Y1CORD:  .=.+   2
X2CORD:  .=.+   2            ; COORDINATE PAIR 2
Y2CORD:  .=.+   2

;        STORAGE FOR ARBITRARY LINE DRAW ROUTINE

DELTAX:  .=.+   2            ; DELTA X
DELTAY:  .=.+   2            ; DELTA Y
ACC:     .=.+   2            ; ACCUMULATOR
XDIR:    .=.+   1            ; X MOVEMENT DIRECTION, ZERO=+
YDIR:    .=.+   1            ; Y MOVEMENT DIRECTION, ZERO=+
XCHFLG:  .=.+   1            ; EXCHANGE X AND Y FLAG, EXCHANGE IF NOT O
COLOR:   .=.+   1            ; COLOR OF LINE DRAWN -1=WHITE
TEMP:    .=.+   2            ; TEMPORARY STORAGE

;        STORAGE FOR THE ARITHMETIC SUBROUTINES

PROD:    .=.+   4            ; PRODUCT FOR ARITHMETIC ROUTINES
MPCD:    .=.+   2            ; MUPTIPLICAND FOR ARITHMETIC
MPLR     =      PROD         ; MULTIPLIER FOR ARITHMETIC ROUTINES
MPSAVE:  .=.+   2            ; TEMPORARY STORAGE FOR MULTIPLY

         .PAGE  'MAIN SWIRL GENERATION ROUTINE'
;        SWIRL ROUTINE FOR STRAIGHT LINES CONNECTING THE POINTS

SWIRL:   JSR    SWINIT       ; INITIALIZE COS AND SIN
SWIRL1:  JSR    SCALE        ; SCALE SIN AND COS FOR DISPLAY
         LDA    LINES        ; TEST IF LINES BETWEEN POINTS DESIRED
         BNE    SWIRL2       ; SKIP IF SO
         JSR    C2TOC1       ; IF NOT, SET LINE LENGTH TO ZERO
SWIRL2:  JSR    DRAW         ; DRAW THE LINE OR POINT
         JSR    POINT        ; COMPUTE THE NEXT POINT
         JMP    SWIRL1

;        SWIRL ROUTINE WITH RANDOM PARAMETERS

RSWIRL:  JSR    SWINIT       ; INITIALIZE COS AND SIN
RSWR1:   JSR    RAND         ; INITIALIZE FREQ RANDOMLY WITH UNIFORM
         STA    FREQ         ; DISTRIBUTION
         JSR    RAND
         STA    FREQ+1
         JSR    RNDEXP       ; INITIALIZE DAMP RANDOMLY WITH A NEGATIVE
         LSRA                ; EXPONENTIAL DISTRIBUTION
         EOR    #X'7F        ; IN THE UPPER BYTE AND UNIFORM
         STA    DAMP+1       ; DISTRIBUTION IN THE LOWER BYTE
         JSR    RAND
         STA    DAMP
         JSR    RAND         ; RANDOMLY DETERMINE PRESENCE OF
         AND    #1           ; CONNECTING LINES
         STA    LINES
         JSR    RANGCK       ; VERIFY ACCEPTABLE RANGES OF PARAMETERS
         BCS    RSWR1        ; TRY AGAIN IF NOT ACCEPTABLE
RSWR2:   JSR    SCALE        ; SCALE THE CURRENT POINT FOR PLOTTING
         LDA    LINES        ; TEST IF CONNECTING LINES SPECIFIED
         BNE    RSWR3        ; SKIP AHEAD IF SO
         JSR    C2TOC1       ; IF NOT, SET ZERO LINE LENGTH
RSWR3:   JSR    DRAW         ; ORAW A LINE FROM THE LAST POINT PLOTTED
         JSR    POINT        ; COMPUTE THE NEXT POINT
RSWR4:   LDA    SIN+1        ; TEST IF PATTERN HAS DECAYED TO NEARLY
         BEQ    RSWR5        ; ZERO
         CMP    #X'FF
         BNE    RSWR2
RSWR5:   LDA    COS+1
         BEQ    RSWIRL       ; GO START A NEW PATTERN IF SO
         CMP    #X'FF
         BEQ    RSWIRL
         BNE    RSWR2        ; GO COMPUTE NEXT POINT IF NOT

;        SWINIT - INITIALIZE COS FROM COSINT, ZERO SIN, CLEAR SCREEN

SWINIT:  LDA    COSINT       ; INITIALIZE COS
         STA    COS
         LDA    COSINT+1
         STA    COS+1
         LDA    #0           ; ZERO SIN
         STA    SIN
         STA    SIN+1
         JSR    CLEAR        ; CLEAR THE VM SCREEN
         JSR    SCALE        ; SCALE THE INITIAL POINT AND PUT INTO
         JSR    C2TOC1       ; IN BOTH SETS OF COORDINATES
         RTS                 ; RETURN

;        SCALE - TAKE VALUE OF SIN, SCALE ACCORDING TO NX, AND PUT INTO
;        X2CORD. THEN TAKE VALUE OF COS, SCALE ACCORDING TO NY, AND
;        PUT INTO Y2CORD.
;        SIN AND COS ARE ASSUMED TO BE DOUBLE LENGTH BINARY FRACTIONS
;        BETWEEN -1 AND +1.

SCALE:   LDA    COS          ; X2CORD=NX/2*SIN4NX/2
         STA    MPCD         ; TRANSFER SIN TO MULTIPLICAND
         LDA    COS+1        ; (BINARY FRACTION)
         STA    MPCD+1
         LDA    #NX/2&X'FF   ; TRANSFER NX/2 TO MULTIPLIER
         STA    MPLR         ; (INTEGER)
         LDA    #NX/2/256
         STA    MPLR+1
         JSR    SGNMPY       ; PERFORM A SIGNED MULTIPLICATION
         JSR    SLQL
         LDA    PROD+2       ; SIGNED INTEGER RESULT IN PROD+2 (LOW)
         CLC                 ; AND PROD+3 (HIGH)
         ADC    #NX/2&X'FF   ; ADD NX/2 TO PRODUCT AND PUT INTO X2CORD
         STA    X2CORD
         LDA    PROD+3
         ADC    #NX/2/256
         STA    X2CORD+1

         LDA    SIN          ; Y2CORD=NY/2*COS+NX/2
         STA    MPCD         ; TRANSFER COS TO MULTIPLICAND
         LDA    SIN+1        ; (BINARY FRACTION)
         STA    MPCD+1
         LDA    #NY/2&X'FF   ; TRANSFER NY/2 TO MULTIPLIER
         STA    MPLR         ; (INTEGER)
         LDA    #NY/2/256
         STA    MPLR+1
         JSR    SGNMPY       ; PERFORM A SIGNED MULTIPLICATION
         JSR    SLQL
         LDA    PROD+2       ; SIGNED INTEGER RESULT IN PROD+2 (LOW)
         CLC                 ; AND PROD+3 (HIGH)
         ADC    #NY/2&X'FF   ; ADD NY/2 TO PRODUCT AND PUT INTO Y2CORD
         STA    Y2CORD
         LDA    PROD+3
         ADC    #NY/2/256
         STA    Y2CORD+1
         RTS                 ; RETURN

         .PAGE  'POINT – COMPUTE NEXT POINT'
;        POINT - COMPUTE NEXT VALUE OF COS,SIN FROM CURRENT VALUE OF
;        COS,SIN ACCORDING TO FREQ AND DAMP. DIFFERENCE EQUATION FOR
;        AN ELIPSE IS USED

         .=     X'100

POINT:   LDA    SIN          ; FIRST COMPUTE DAMP*SIN AND PUT INTO SIN
         STA    MPCD
         LDA    SIN+1
         STA    MPCD+1
         LDA    DAMP
         STA    MPLR
         LDA    DAMP+1
         STA    MPLR+1
         JSR    SGNMPY
         JSR    SLQL         ; SHIFT PRODUCT LEFT ONE FOR FRACTIONAL
         LDA    PROD+2       ; RESULT
         STA    SIN          ; AND PUT BACK INTO SIN
         LDA    PROD+3
         STA    SIN+1

         LDA    COS          ; NEXT COMPUTE COS*FREQ
         STA    MPLR
         LDA    COS+1
         STA    MPLR+1
         LDA    FREQ
         STA    MPCD
         LDA    FREQ+1
         STA    MPCD+1
         JSR    SGNMPY
         JSR    SLQL
         LDA    SIN          ; ADD RESULT TO SIN AND PUT SUM BACK INTO
         CLC                 ; SIN
         ADC    PROD+2
         STA    SIN
         LDA    SIN+1
         ADC    PROD+3
         STA    SIN+1

         LDA    SIN          ; NEXT COMPUTE FREQ*SIN
         STA    MPLR
         LDA    SIN+1
         STA    MPLR+1       ; FREQ ALREADY IN MPCD
         JSR    SGNMPY
         JSR    SLQL

         LDA    COS          ; SUBSTRACT RESULT FROM COS AND PUT RESULT
         SEC                 ; IN COS
         SBC    PROD+2
         STA    COS
         LDA    COS+1
         SBC    PROD+3
         STA    COS+1
         RTS                 ; RETURN

;        SUBROUTINE TO MOVE THE CONTENTS OF COORDINATE PAIR 2 TO
;        COORDINATE PAIR 1.

C2TOC1:  LDA    X2CORD       ; DO THE MOVING
         STA    X1CORD
         LDA    X2CORD+1
         STA    X1CORD+1
         LDA    Y2CORD
         STA    Y1CORD
         LDA    Y2CORD+1
         STA    Y1CORD+1
         RTS                 ; RETURN

         .PAGE  'ABBREVIATED GRAPHICS ROUTINES'
;        PIXADR - FIND THE BYTE ADDRESS AND BIT NUMBER OF PIXEL AT
;                 X1CORD, Y1CORD
;        PUTS BYTE ADDRESS IN ADP1 AND BIT NUMBER (BIT 0 IS LEFTMOST)
;        IN BTPT.
;        DOES NOT CHECK MAGNITUDE OF COORDINATES FOR MAXIMUM SPEED
;        PRESERVES X AND Y REGISTERS, DESTROYS A
;        BYTE ADDRESS = VMORG*256+(199-Y1CORD)*40+INT(XCORD/8)
;        BIT ADDRESS = REM(XCORD/8)
;        OPTIMIZED FOR SPEED THEREFORE CALLS TO A DOUBLE SHIFT ROUTINE
;        ARE NOT DONE

PIXADR:  LDA    X1CORD       ; COMPUTE BIT ADDRESS FIRST
         STA    ADP1         ; ALSO TRANSFER X1CORD TO ADP1
         AND    #X'07        ; WHICH IS SIMPLY THE LOW 3 BITS OF X
         STA    BTPT
         LDA    X1CORD+1     ; FINISH TRANSFERRING X1CORD TO ADP1
         STA    ADP1+1
         LSR    ADP1+1       ; DOUBLE SHIFT ADP1 RIGHT 3 TO GET
         ROR    ADP1         ; INT(XCORD/8)
         LSR    ADP1+1
         ROR    ADP1
         LSR    ADP1+1
         ROR    ADP1
         LDA    #199         ; TRANSFER (199-Y1CORD) TO ADP2
         SEC                 ; AND TEMPORARY STORAGE
         SBC    Y1CORD
         STA    ADP2
         STA    TEMP
         LDA    #0
         SBC    Y1CORD+1
         STA    ADP2+1
         STA    TEMP+1
         ASL    ADP2         ; COMPUTE 40*(199-Y1CORD)
         ROL    ADP2+1       ;  2*(199-Y1CORD)
         ASL    ADP2
         ROL    ADP2+1       ;  4*(199+Y1CORD)
         LDA    ADP2         ;  ADD IN TEMPORARY SAVE OF (199-Y1CORD)
         CLC                 ;  TO MAKE 5*(199-Y1CORD)
         ADC    TEMP
         STA    ADP2
         LDA    ADP2+1
         ADC    TEMP+1
         STA    ADP2+1       ;  5*(199-Y1CORD)
         ASL    ADP2         ;  10*(199-Y1CORD)
         ROL    ADP2+1
         ASL    ADP2         ;  20*(199-Y1CORD)
         ROL    ADP2+1
         ASL    ADP2         ;  40*(199-Y1CORD)
         ROL    ADP2+1
         LDA    ADP2         ; ADD IN INT(X1CORD/8) COMPUTED EARLIER
         CLC
         ADC    ADP1
         STA    ADP1
         LDA    ADP2+1
         ADC    ADP1+1
         ADC    VMORG        ; ADD IN VMORG*256
         STA    ADP1+1       ; FINAL RESULT
         RTS                 ; RETURN

;        STPIX - SETS THE PIXEL AT X1CORD,Y1CORD TO A ONE (WHITE DOT)
;        DOES NOT ALTER X1CORD OR Y1CORD
;        PRESERVES X AND Y
;        ASSUMES IN RANGE CORRDINATES

STPIX:   JSR    PIXADR       ; GET BYTE ADDRESS AND BIT NUMBER OF PIXEL
                             ; INTO ADP1
         TYA                 ; SAVE Y
         PHA
         LDY    BTPT         ; GET BIT NUMBER IN Y
         LDA    MSKTB1,Y     ; GET A BYTE WITH THAT BIT =1, OTHERS =0
         LDY    #0           ; ZERO Y
         ORA    (ADP1),Y     ; COMBINE THE BIT WITH THE ADDRESSED VM
         STA    (ADP1),Y     ; BYTE
         PLA                 ; RESTORE Y
         TAY
         RTS                 ; AND RETURN

         .=     X'200

;        CLEAR DISPLAY MEMORY ROUTINE

CLEAR:   LDY    #0           ; INITIALIZE ADDRESS POINTER
         STY    ADP1         ; AND ZERO INDEX Y
         LDA    VMORG
         STA    ADP1+1
         CLC
         ADC    #X'20
         TAX
CLEAR1:  TYA                 ; CLEAR A BYTE
         STA    (ADP1),Y
         INC    ADP1         ; INCREMENT ADDRESS POINTER
         BNE    CLEAR1
         INC    ADP1+1
         CPX    ADP1+1       ; TEST IF DONE
         BNE    CLEAR1
         RTS                 ; RETURN

;        MASK TABLES FOR INDIVIDUAL PIXEL SUBROUTINES
;        MSKTB1 IS A TABLE OF 1 BITS CORRESPONDING TO BIT NUMBERS

MSKTB1:  .BYTE  X'80,X'40,X' 20,X'10
         .BYTE  X'08,X'04,X' 02,X'01

         .PAGE  'LINE DRAWING ROUTINES'
;        DRAW - DRAW THE BEST STRAIGHT LINE FROM X1CORD,Y1CORD TO
;        X2CORD, Y2CORD.
;        X2CORD,Y2CORD COPIED TO X1CORD,Y1CORD AFTER DRAWING
;        PRESERVES X AND Y
;        USES AN ALGORITHM THAT REQUIRES NO MULTIPLICATION OR DIVISON

DRAW:    TXA                 ; SAVE X AND Y
         PHA
         TYA
         PHA

;        COMPUTE SIGN AND MAGNITUDE OF DELTA X = X2-X1
;        PUT MAGNITUDE IN DELTAX AND SIGN IN XDIR

         LDA    #0           ; FIRST ZERO XDIR
         STA    XDIR
         LDA    X2CORD       ; NEXT COMPUTE TWOS COMPLEMENT DIFFERENCE
         SEC
         SBC    X1CORD
         STA    DELTAX
         LDA    X2CORD+1
         SBC    X1CORD+1
         STA    DELTAX+1
         BPL    DRAW2        ; SKIP AHEAD IF DIFFERENCE IS POSITIVE
         DEC    XDIR         ; SET XDIR TO -1
         SEC                 ; NEGATE DELTAX
         LDA    #0
         SBC    DELTAX
         STA    DELTAX
         LDA    #0
         SBC    DELTAX+1
         STA    DELTAX+1

;        COMPUTE SIGN AND MAGNITUDE OF DELTA Y = Y2-Y1
;        PUT MAGNITUDE IN DELTAY AND SIGN IN YDIR

DRAW2:   LDA    #0           ; FIRST ZERO YDIR
         STA    YDIR
         LDA    Y2CORD       ; NEXT COMPUTE TWOS COMPLEMENT DIFFERENCE
         SEC
         SBC    Y1CORD
         STA    DELTAY
         LDA    Y2CORD+1
         SBC    Y1CORD+1
         STA    DELTAY+1
         BPL    DRAW3        ; SKIP AHEAD IF DIFFERENCE IS POSITIVE
         DEC    YDIR         ; SET YDIR TO -1
         SEC                 ; NEGATE DELTAX
         LDA    #0
         SBC    DELTAY
         STA    DELTAY
         LDA    #0
         SBC    DELTAY+1
         STA    DELTAY+1

;        DETERMINE IF DELTAY IS LARGER-THAN DELTAX
;        IF SO, EXCHANGE DELTAY AND DELTAX AND SET XCHFLG NONZERO
;        ALSO INITIALIZE ACC TO DELTAX
;        PUT A DOT AT THE INITIAL ENDPOINT

DRAW3:   LDA    #0           ; FIRST ZERO XCHFLG
         STA    XCHFLG
         LDA    DELTAY       ; COMPARE DELTAY WITH DELTAX
         SEC
         SBC    DELTAX
         LDA    DELTAY+1
         SBC    DELTAX+1
         BCC    DRAW4        ; SKIP EXCHANGE IF DELTAX IS GREATER THAN
                             ; DELTAY
         LDX    DELTAY       ; EXCHANGE DELTAX AND DELTAY
         LDA    DELTAX
         STA    DELTAY
         STX    DELTAX
         LDX    DELTAY+1
         LDA    DELTAX+1
         STA    DELTAY+1
         STX    DELTAX+1
         DEC    XCHFLG       ; SET XCHFLG TO -1
DRAW4:   LDA    DELTAX       ; INITIALIZE ACC TO DELTAX
         STA    ACC
         LDA    DELTAX+1
         STA    ACC+1
         JSR    STPIX        ; PUT A DOT AT THE INITIAL ENDPOINT;
                             ; X1CORD, Y1CORD

;        HEAD OF MAIN DRAWING LOOP
;        TEST IF DONE

DRAW45:  LDA    XCHFLG       ; TEST IF X AND Y EXCHANGED
         BNE    DRAW5        ; JUMP AHEAD IF SO
         LDA    X1CORD       ; TEST FOR X1CORD=X2CORD
         CMP    X2CORD
         BNE    DRAW7        ; GO FOR ANOTHER ITERATION IF NOT
         LDA    X1CORD+1
         CMP    X2CORD+1
         BNE    DRAW7        ; GO FOR ANOTHER ITERATION IF NOT
         BEQ    DRAW6        ; GO RETURN IF SO
DRAW5:   LDA    Y1CORD       ; TEST FOR Y1CORD=Y2CORD
         CMP    Y2CORD
         BNE    DRAW7        ; GO FOR ANOTHER ITERATION IF NOT
         LDA    Y1CORD+1
         CMP    Y2CORD+1
         BNE    DRAW7        ; GO FOR ANOTHER ITERATION IF NOT
DRAW6:   PLA                 ; RESTORE INDEX REGISTERS
         TAY
         PLA
         TAX
         RTS                 ; AND RETURN

;        DO A CLACULATION TO DETERMINE IF ONE OR BOTH AXES ARE TO BE
;        BUMPED (INCREMENTED OR DECREMENTED ACCORDING TO XDIR AND YDIR)
;        AND DO THE BUMPING

DRAW7:   LDA    XCHFLG       ; TEST IF X AND Y EXCHANGED
         BNE    DRAW8        ; JUMP IF SO
         JSR    BMPX         ; BUMP X IF NOT
         JMP    DRAW9
DRAW8:   JSR    BMPY         ; BUMP Y IF SO
DRAW9:   JSR    SBDY         ; SUBSTRACT DY FROM ACC TWICE
         JSR    SBDY
         BPL    DRAW12       ; SKIP AHEAD IF ACC IS NOT NEGATIVE
         LDA    XCHFLG       ; TEST IF X AND Y EXCHANGED
         BNE    DRAW10       ; JUMP IF SO
         JSR    BMPY         ; BUMP Y IF NOT
         JMP    DRAW11
DRAW10:  JSR    BMPX         ; BUMP X IF SO
DRAW11:  JSR    ADDX         ; ADD DX TO ACC TWICE
         JSR    ADDX

DRAW12:  JSR    STPIX        ; OUTPUT THE NEW POINT
         JMP    DRAW45       ; GO TEST IF DONE

;        SUBROUTINES FOR DRAW

SBDY:    LDA    ACC          ; SUBSTRACT DELTAY FROM ACC AND PUT RESULT
         SEC                 ; IN ACC
         SBC    DELTAY
         STA    ACC
         LDA    ACC+1
         SBC    DELTAY+1
         STA    ACC+1
         RTS


ADDX:    LDA    ACC          ; ADD DELTAX TO ACC AND PUT RESULT IN ACC
         CLC
         ADC    DELTAX
         STA    ACC
         LDA    ACC+1
         ADC    DELTAX+1
         STA    ACC+1
         RTS


BMPX:    LDA    XDIR         ; BUMP X1CORD BY +1 OR -1 ACCORDING TO
         BNE    BMPX2        ; XDIR
         INC    X1CORD       ; DOUBLE INCREMENT X1CORD IF XDIR=0
         BNE    BMPX1
         INC    X1CORD+1
BMPX1:   RTS
BMPX2:   LDA    X1CORD       ; DOUBLE DECREMENT X1CORD IF XDIR<>0
         BNE    BMPX3
         DEC    X1CORD+1
BMPX3:   DEC    X1CORD
         RTS


BMPY:    LDA    YDIR         ; BUMP Y1CORD BY +1 OR -1 ACCORDING TO
         BNE    BMPY2        ; YDIR
         INC    Y1CORD       ; DOUBLE INCREMENT Y1CORD IF YDIR=0
         BNE    BMPY1
         INC    Y1CORD+1
BMPY1:   RTS
BMPY2:   LDA    Y1CORD       ; DOUBLE DECREMENT Y1CORD IF YDIR<>0
         BNE    BMPY3
         DEC    Y1CORD+1
BMPY3:   DEC    Y1CORD
         RTS

         .PAGE  'MULTIPLY, SHIFT, AND RANDOM NUMBER ROUTINES'
;        SIGNED MULTIPLY SUBROUTINE
;        ENTER WITH SIGNED MULTIPLIER IN PROD AND PROD+1
;        ENTER WITH SIGNED MULTIPLICAND IN MPCD AND MPCD+1
;        RETURN WITH 16 BIT SIGNED PRODUCT IN PROD (LOW) THROUGH
;        PROD+3 (HIGH)
;        A DESTROYED, X AND Y PRESERVED

SGNMPY:  LDA    PROD         ; GET MULTIPLIER
         STA    MPSAVE       ; AND SAVE IT
         LDA    PROD+1
         STA    MPSAVE+1
         JSR    UNSMPY       ; DO AN UNSIGNED MULTIPLY
         LDA    MPCD+1       ; TEST SIGN OF MULTIPLICAND
         BPL    SGNMP1       ; JUMP IF POSITIVE
         LDA    PROD+2       ; SUBTRACT MULTIPLIER FROM HIGH PRODUCT IF
         SEC                 ; NEGATIVE
         SBC    MPSAVE
         STA    PROD+2
         LDA    PROD+3
         SBC    MPSAVE+1
         STA    PROD+3
SGNMP1:  LDA    MPSAVE+1     ; TEST SIGN OF MULTIPLIER
         BPL    SGNMP2       ; GO RETURN IF POSITIVE
         LDA    PROD+2       ; SUBTRACT MULTIPLICAND FROM HIGH PRODUCT
         SEC                 ; IF NEGATIVE
         SBC    MPCD
         STA    PROD+2
         LDA    PROD+3
         SBC    MPCD+1
         STA    PROD+3
SGNMP2:  RTS                 ; RETURN

;        16 X 16 UNSIGNED MULTIPLY SUBROUTINE
;        ENTER WITH UNSIGNED MULTIPLIER IN PROD AND PROD+1
;        ENTER WITH UNSIGNED MULTIPLICAND IN MPCD AND MPCD+1
;        RETURN WITH 16 BIT UNSIGNED PRODUCT IN PROD (LOW) THROUGH
;        PROD+3 (HIGH)
;        A DESTROYED, X AND Y PRESERVED

UNSMPY:  TXA                 ; SAVE X INDEX
         PHA
         LDA    #0           ; CLEAR UPPER PRODUCT
         STA    PROD+3
         STA    PROD+2
         LDX    #17          ; SET 17 MULTIPLY CYCLE COUNT
         CLC                 ; INITIALLY CLEAR CARRY
UNSM1:   JSR    SRQL         ; SHIFT MULTIPLIER AND PRODUCT RIGHT 1
                             ; PUTTING A MULTIPLIER BIT IN CARRY
         DEX                 ; DECREMENT AND CHECK CYCLE COUNT
         BEQ    UNSM2        ; JUMP OUT IF DONE
         BCC    UNSM1        ; SKIP MULTIPLICAND ADD IF MULTIPLIER BIT
                             ; IS ZERO
         LDA    PROD+2       ; ADD MULTIPLICAND TO UPPER PRODUCT
         CLC
         ADC    MPCD
         STA    PROD+2
         LDA    PROD+3
         ADC    MPCD+1
         STA    PROD+3
         JMP    UNSM1        ; GO FOR NEXT CYCLE
UNSM2:   PLA                 ; RESTORE X
         TAX
         RTS                 ; RETURN

;        QUAD SHIFT RIGHT SUBROUTINE
;        ENTER AT SRQA FOR ALGEBRAIC SHIFT RIGHT
;        ENTER AT SRQL FOR LOGICAL SHIFT
;        ENTER WITH QUAD PRECISION VALUE TO SHIFT IN PROD THROUGH PROD+3
;        DESTROYS A, PRESERVES X AND Y, RETURNS BIT SHIFTED OUT IN CARRY

SRQA:    LDA    PROD+3       ; GET SIGN BIT OF PROD IN CARRY
         ASLA
SRQL:    ROR    PROD+3       ; LOGICAL SHIFT RIGHT ENTRY
         ROR    PROD+2
         ROR    PROD+1
         ROR    PROD
         RTS                 ; RETURN


;        QUAD SHIFT LEFT SUBROUTINE
;        ENTER AT SLQL TO SHIFT IN A ZERO BIT
;        ENTER AT RLQL TO SHIFT IN THE CARRY
;        ENTER WITH QUAD PRECISION VALUE TO SHIFT IN PROD THROUGH PROD+3
;        DESTROYS A, PRESERVES X AND Y, RETURNS BIT SHIFTED OUT IN CARRY

SLQL:    CLC                 ; SHIFT IN ZERO BIT ENTRY; CLEAR CARRY
RLQL:    ROL    PROD         ; SHIFT IN CARRY ENTRY
         ROL    PROD+1
         ROL    PROD+2
         ROL    PROD+3
         RTS                 ; RETURN

;        RANDOM NUMBER GENERATOR SUBROUTINE
;        ENTER WITH SEED IN RANDNO
;        EXIT WITH NEW RANDOM NUMBER IN RANDNO AND A
;        USES 16 BIT FEEDBACK SHIFT REGISTER METHOD
;        DESTROYS REGISTER A AND Y

RAND:    LDY    #8           ; SET COUNTER FOR 8 RANDOM BITS
RAND1:   LDA    RANDNO       ; EXCLUSIVE-OR BITS 3, 12, 14, AND 15
         LSRA                ; OF SEED
         EOR    RANDNO
         LSRA
         LSRA
         EOR    RANDNO
         LSRA
         EOR    RANDNO+1     ; RESULT IS IN BIT 3 OF A
         LSRA                ; SHIFT INTO CARRY
         LSRA
         LSRA
         LSRA
         ROL    RANDNO+1     ; SHIFT RANDNO LEFT ONE BRINGING IN CARRY
         ROL    RANDNO
         DEY                 ; TEST IF 8 NEW RANDOM BITS COMPUTED
         BNE    RAND1        ; LOOP FOR MORE IF NOT
         LDA    RANDNO
         RTS                 ; RETURN

;        EXPONENTIALLY DISTRIBUTED RANDOM NUMBER SUBROUTINE
;        RULES OF USE SAME AS RAND, 8 BIT RESULT RETURNED IN A
;        AN EXPONENTIAL DISTRIBUTION MEANS THAT THE PROBABILITY OF A
;        RESULT BETWEEN 10 AND 20 IS THE SAME AS THE PROBABILITY OF A
;        RESULT BETWEEN 100 AND 200.
;        NOTE THAT THE PROBABILITY OF A ZERO RESULT IS ZERO.

RNDEXP:  JSR    RAND         ; GET TWO NEW RANDOM BYTES
         JSR    RAND
         LDA    RANDNO       ; CONVERT ONE OF THE BYTES TO A RANDOM
         AND    #7           ; VALUE BETWEEN 0 AND 7 AND PUT IN Y AS A
         TAY                 ; SHIFT COUNT
         INY
         LDA    RANDNO+1     ; GET THE OTHER RANDOM NUMBER AND SHIFT IT
RNDXP1:  DEY                 ; RIGHT ACCORDING TO Y
         BEQ    RNDXP2
         LSRA
         JMP    RNDXP1
RNDXP2:  ORA    #0           ; TEST FOR A ZERO RESULT
         BEQ    RNDEXP       ; PROHIBIT ZERO RESULTS
         RTS                 ; RETURN

;        RANGCK - CHECK FOR ACCEPTABLE RANGE OF FREQ AND DAMP PARAMETERS
;        RETURN WITH CARRY OFF IF OK

RANGCK:  LDA    FREQ+1       ; MINIMUM ABSOLUTE VALUE FOR FREQ IS X'0100
         BEQ    RANGNK       ; GO TO FAILURE RETURN IF HIGH BYTE IS 0
         CMP    #X'FF
         BEQ    RANGNK       ; GO TO FAILURE RETURN IF HIGH BYTE IS FF
RANG2:   LDA    DAMP+1       ; CHECK THAT DAMP IS NOT GREATER THAN
         CMP    #X'7F        ; X'7EFF
         BEQ    RANGNK       ; GO TO FAILURE RETURN IF SO
RANG3:   LDA    FREQ+1       ; IF FREQ AND DAMP ARE INDIVIDUALLY OK,
         BPL    RANG4        ; VERIFY THAT DAMP IS ACCEPTABLY HIGH IF
         EOR    X'FF         ; ABSOLUTE VALUE OF FREQ IS SMALL
RANG4:   CMP    #8
         BPL    RANGOK       ; GO TO SUCCESS RETURN IF FREQ IS HIGH
         LDA    DAMP+1       ; IF FREQ IS LOW, REQUIRE DAMP TO BE HIGH
         CMP    #X'7E
         BMI    RANGNK       ; GO TO FAILURE RETURN IF DAMP NOT HIGH
                             ; ENOUGH WHEN FREQ IS LESS THAN X'10
RANGOK:  CLC                 ; CLEAR CARRY TO INDICATE SUCCESS
         RTS                 ; RETURN
RANGNK:  SEC                 ; SET CARRY TO INDICATE FAILURE
         RTS                 ; RETURN


         .END
