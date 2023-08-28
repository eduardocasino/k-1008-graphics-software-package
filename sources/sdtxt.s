
         .PAGE  'SIMPLIFIED VISABLE MEMORY TEXT DISPLAY SUBROUTINE'
;        THIS SUBROUTINE TURNS THE VISABLE MEMORY INTO A DATA DISPLAY
;        TERMINAL (GLASS TELETYPE).
;        CHARACTER SET IS 96 FULL ASCII UPPER AND LOWER CASE.
;        CHARACTER MATRIX IS 5 BY 7 SET INTO A 6 BY 9 RECTANGLE.
;        LOWER CASE IS REPRESENTED AS SMALL (5 BY 5) CAPITALS.
;        SCREEN CAPACITY IS 22 LINES OF 53 CHARACTERS FOR FULL SCREEW
;        OR 11 LINES FOR HALF SCREEN.
;        CURSOR IS A NON-BLINKING UNDERLINE.
;        CONTROL CODES RECOGNIZED:
;        CR     X'0D         SETS CURSOR TO LEFT SCREEN EDGE
;        LF     X'0A         MOVES CURSOR DOWN ONE LINE, SCROLLS
;                            DISPLAY UP ONE LINE IF ALREADY ON BOTTOM
;                            LINE
;        BS     X'08         MOVES CURSOR ONE CHARACTER LEFT, DOES
;                            NOTHING IF ALREADY AT LEFT SCREEN EDGE
;        FF     X'0C         CLEARS SCREEN AND PUTS CURSOR AT TOP LEFT
;                            OF SCREEN, SHOULD BE CALLED FOR
;                            INITIALIZATION
;        ALL OTHER CONTROL CODES IGNORED.
;        ENTER WITH CHARACTER TO BE DISPLAYED IN A.
;        X AND Y PRESERVED.
;        3 BYTES OF RAM STORAGE REQUIRED FOR KEEPING TRACK OF THE
;        CURSOR
;        4 BYTES OF TEMPORARY STORAGE IN BASE PAGE REQUIRED FOR ADDRESS
;        POINTERS. (CAN BE DESTROYED BETWEEN CALLS TO SDTXT
;        4 BYTES OF TEMPORARY STORAGE ANYWHERE (CAN BE DESTROYED
;        BETWEEN CALLS TO SDTXT)

;        * **** VMORG #MUST# BE SET TO THE PAGE NUMBER OF THE VISIBLE *
;        * MEMORY BEFORE CALLING SDTXT ****                           *

;        GENERAL EQUATES

NLOC     =      8000         ; NUMBER OF VISIBLE LOCATIONS
CHHI     =      9            ; CHARACTER WINDOW HEIGHT
CHWID    =      6            ; CHARACTER WINDOW WIDTH
NCHR     =      320/CHWID    ; NUMBER OF CHARACTERS PER LINE
NLIN     =      NLOC/40/CHHI ; NUMBER OF TEXT LINES
NSCRL    =      NLIN-1*CHHI*40 ; NUMBER OF LOCATIONS TO SCROLL
NCLR     =      NLOC-NSCRL   ; NUMBER OF LOCATIONS TO CLEAR AFTER SCROLL

;        BASE PAGE TEMPORARY STORAGE

         .=     X'EA
ADP1     .=.+   2            ; ADDRESS POINTER 1
ADP2     .=.+   2            ; ADDRESS POINTER 2

;        GENERAL TEMPORARY STORAGE

         .=     X'5B00       ; PLACE AT END OF 16K EXPANSION

BTPT:    .=.+   1            ; BIT NUMBER TEMPORARY STORAGE
DCNT1:   .=.+   2            ; DOUBLE PRECISION COUNTER
MRGT1:   .=.+   1            ; TEMPORARY STORAGE FOR MERGE

;        PERMANENT RAM STORAGE

CSRX:    .=.+   1            ; CURRENT CHARACTER NUMBER (0=LEFT CHAR)
CSRY:    .=.+   1            ; CURRENT LINE NUMBER (0=TOP LINE)
VMORG:   .=.+   1            ; FIRST PAGE NUMBER OF VISIBLE MEMORY

SDTXT:   PHA                 ; SAVE REGISTERS
         TXA
         PHA
         TYA
         PHA
         LDA    #0           ; CLEAR UPPER ADP2
         STA    ADP2+1
         TSX                 ; GET INPUT BACK
         LDA    X'103,X
         AND    #X'7F        ; INSURE 7 BIT ASCII INPUT
         SEC
         SBC    #X'20        ; TEST IF A CONTROL CHARACTER
         BMI    SDTX10       ; JUMP IF SO

;        CALCULATE TABLE ADDRESS FOR CHAR SHAPE AND PUT IT INTO ADPL

SDTXT1:  STA    ADP2         ; SAVE CHARACTER CODE IN ADP2
         JSR    SADP2L       ; COMPUTE 8*CHARACTER CODE IN ADP2
         JSR    SADP2L
         JSR    SADP2L
         EOR    #X'FF        ; NEGATE CHARACTER CODE
         SEC                 ; SUBSTRACT CHARACTER CODE FROM ADP2 AND
         ADC    ADP2         ; PUT RESULT IN ADP1 FOR A FINAL RESULT OF
         STA    ADP1         ; 7*CHARACTER CODE
         LDA    ADP2+1
         ADC    #X'FF
         STA    ADP1+1
         LDA    ADP1         ; ADD IN ORIGIN OF CHARACTER TABLE
         CLC
         ADC    #CHTB&X'FF
         STA    ADP1
         LDA    ADP1+1
         ADC    #CHTB/256
         STA    ADP1+1       ; ADP1 NOW HAS ADDRESS OF TOP ROW OF
                             ; CHARACTER SHAPE
;        COMPUTE BYTE AND BIT ADDRESS OF FIRST SCAN LINE OF
;        CHARACTER AT CURSOR POSITION

         JSR    CSRTAD       ; COMPUTE BYTE AND BIT ADDRESSES OF FIRST
                             ; SCAN LINE OF CHARACTER AT CURSOR POS.

;        SCAN OUT THE 7 CHARACTER ROWS

         LDY    #0           ; INITIALIZE Y INDEX=FONT TABLE POINTER
SDTX2:   LDA    (ADP1),Y     ; GET A DOT ROW FROM THE FONT TABLE
         JSR    MERGE        ; MERGE IT WITH GRAPHIC MEMORY AT (ADP2)
         JSR    DN1SCN       ; ADD 40 TO ADP2 TO MOVE DOWN ONE SCAN
                             ; LINE IN GRAPHIC MEMORY
         INY                 ; BUMP UP POINTER INTO FONT TABLE
         CPY    #7           ; TEST IF DONE
         BNE    SDTX2        ; GO DO NEXT SCAN LINE IF NOT
         LDA    CSRX         ; DO A CURSOR RIGHT
         CMP    #NCHR-1      ; TEST IF LAST CHARACTER ON THE LINE
         BPL    SDTX3        ; SKIP CURSOR RIGHT IF SO
         JSR    CSRCLR       ; CLEAR OLD CURSOR
         INC    CSRX         ; MOVE CURSOR ONE POSITION RIGHT
SDTX3:   JMP    SDTXRT       ; GO INSERT CURSOR, RESTORE REGISTERS,
                             ; AND RETURN

;        INTERPRET CONTROL CODES

SDTX10:  CMP    #X'0D-X'20   ; TEST IF CR
         BEQ    SDTXCR       ; JUMP IF SO
         CMP    #X'0A-X'20   ; TEST IF LF
         BEQ    SDTXLF       ; JUMP IF SO
         CMP    #X'08-X'20   ; TEST IF BS
         BEQ    SDTXCL       ; JUMP IF SO
         CMP    #X'0C-X'20   ; TEST IF FF
         BEQ    SDTXFF       ; JUMP IF SO
         JMP    SDTXRT       ; GO RETURN IF UNRECOGNIZABLE CONTROL

SDTXCR:  JSR    CSRCLR       ; CARRIAGE RETURN, FIRST CLEAR CURSOR
         LDA    #0           ; ZERO CURSOR HORIZONTAL POSITION
         STA    CSRX
         JMP    SDTXRT       ; GO SET CURSOR AND RETURN

SDTXCL:  JSR    CSRCLR       ; CURSOR LEFT, FIRST CLEAR CURSOR
         LDA    CSRX         ; GET CURSOR HORIZONTAL POSITION
         CMP    #0           ; TEST IF AGAINST LEFT EDGE
         BEQ    SDTX20       ; SKIP UPDATE IF SO
         DEC    CSRX         ; OTHERWISE DECREMENT CURSOR X POSITION
SDTX20:  JMP    SDTXRT       ; GO SET CURSOR AND RETURN

SDTXFF:  LDA    VMORG        ; FORM FEED, CLEAR SCREEN TO ZEROES
         STA    ADP2+1       ; TRANSFER VISIBLE MEMORY ORIGIN ADDRESS
         LDA    #0           ; TO ADP2
         STA    ADP2
         LDA    #NLOC&X'FF   ; SET COUNT OF LOCATIONS TO CLEAR IN DCNT1
         STA    DCNT1
         LDA    #NLOC/256
         STA    DCNT1+1
         JSR    FCLR         ; CLEAR THE SCREEN
         LDA    #0
         STA    CSRX         ; PUT CURSOR IN UPPER LEFT CORNER
         STA    CSRY
         JMP    SDTXRT       ; GO SET CURSOR AND RETURN

SDTXLF:  JSR    CSRCLR       ; LINE FEED, FIRST CLEAR CURSOR
         LDA    CSRY         ; GET CURRENT LINE POSITION
         CMP    #NLIN-1      ; TEST IF AY BOTTOM OF SCREEN
         BPL    SDTX40       ; GO SCROLL IF SO
         INC    CSRY         ; INCREMENT LINE NUMBER IF NOT AT BOTTOM
         BNE    SDTXRT       ; GO INSERT CURSOR AND RETURN
SDTX40:  LDA    #0           ; SET UP ADDRESS POINTERS FOR MOVE
         STA    ADP2         ; ADP1 - SOURCE FOR MOVE = FIRST BYTE OF
         LDA    VMORG        ; SECOND LINE OF TEXT
         STA    ADP2+1       ; ADP2 = DESTINATION FOR MOVE = FIRST BYTE
         CLC                 ; IN VISIBLE MEMORY
         ADC    #CHHI*40/256
         STA    ADP1+1
         LDA    #CHHI*40&X'FF
         STA    ADP1
         LDA    #NSCRL&X'FF  ; SET NUMBER OF LOCATIONS TO MOVE
         STA    DCNT1        ; LOW PART
         LDA    #NSCRL/256   ; HIGH PART
         STA    DCNT1+1
         JSR    FMOVE        ; EXECUTE MOVE USING AN OPTIMIZED, HIGH
                             ; SPEED MEMORY MOVE ROUTINE

                             ; CLEAR LAST LINE OF TEXT
         LDA    #NLIN-1*CHHI*40&X'FF  ; SET ADDRESS POINTER
         STA    ADP2         ; LOW BYTE
         LDA    #NLIN-1*CHHI*40/256
         CLC
         ADC    VMORG
         STA    ADP2+1       ; HIGH BYTE
         LDA    #NCLR&X'FF   ; SET LOW BYTE OF CLEAR COUNT
         STA    DCNT1
         LDA    #NCLR/256    ; SET HIGH BYTE OF CLEAR COUNT
         STA    DCNT1+1
         JSR    FCLR         ; CLEAR THE DESIGNATED AREA

;        NO EFFECTIVE CHANGE IN CURSOR POSITION

SDTXRT:  JSR    CSRSET       ; RETURN SEQUENCE, INSERT CURSOR
         PLA                 ; RESTORE REGISTERS FROM THE STACK
         TAY
         PLA
         TAX
         PLA
         RTS                 ; RETURN

         .PAGE  'SUBROUTINES FOR SDTXT'
;        COMPUTE ADDRESS OF BYTE CONTAINING LAST SCAN LINE OF
;        CHARACTER AT CURSOR POSITION
;        ADDRESS = CSRTAD+(CHHI-1)*40   SINCE CHHI IS A CONSTANT 9,
;        (CHHI-1)*40=320
;        BTPT HOLDS BIT ADDRESS, 0=LEFTMOST

CSRBAD:  JSR    CSRTAD       ; COMPUTE ADDRESS OF TOP OF CHARACTER CELL
                             ; FIRST
         LDA    ADP2         ; ADD 320 TO RESULT = 8 SCAN LINES
         CLC
         ADC    #320&X'FF
         STA    ADP2
         LDA    ADP2+1
         ADC    #320/256
         STA    ADP2+1
         RTS

;        SET CURSOR AT CURRENT POSITION

CSRSET:  JSR    CSRBAD       ; GET BYTE AND BIT ADDRESS OF CURSOR
         LDA    #X'F8        ; DATA = UNDERLINE CURSOR
CSRST1:  JMP    MERGE        ; MERGE CURSOR WITH GRAPHIC MEMORY
                             ; AND RETURN

;        CLEAR CURSOR AT CURRENT POSITION

CSRCLR:  JSR    CSRBAD       ; GET BYTE AND BIT ADDRESS OF CURSOR
         LDA    #0           ; DATA = BLANK DOT ROW
         JMP    MERGE        ; REMOVE DOT ROW FROM GRAPHIC MEMORY
                             ; AND RETURN

;        SHIFT ADP2 LEFT ONE BIT POSITION

SADP2L:  ASL    ADP2
         ROL    ADP2+1
         RTS

;        MOVE DOWN ONE SCAN LINE      DOUBLE ADDS 40 TO ADP2

DN1SCN:  LDA    ADP2         ; ADD 40 TO LOW BYTE
         CLC
         ADC    #40
         STA    ADP2
         LDA    #0           ; EXTEND CARRY TO UPPER BYTE
         ADC    ADP2+1
         STA    ADP2+1
         RTS                 ; RETURN

;        COMPUTE BYTE ADDRESS CONTAINING FIRST SCAN LINE OF
;        CHARACTER AT CURSOR POSITION AND PUT IN ADP2
;        BIT ADDRESS (BIT 0 IS LEFTMOST) AT BTPT
;        BYTE ADDRESS =VMORG*256+CHHI*40*CSRY+INT(CSRX*6/8)
;        SINCE CHHI IS A CONSTANT 9, THEN CHHI*40=360
;        BIT ADDRESS=REM(CSRX*5/8)

CSRTAD:  LDA    #0           ; AERO UPPER ADP2
         STA    ADP2+1
         LDA    CSRY         ; FIRST COMPUTE 360*CSRY
         ASLA                ;   COMPUTE 9*CSRY DIRECTLY IN A
         ASLA
         ASLA
         ADC    CSRY
         STA    ADP2         ;   STORE 9*CSRY IN LOWER ADP2
         JSR    SADP2L       ;   18*CSRY IN ADP2
         JSR    SADP2L       ;   36*CSRY IN ADP2
         ADC    ADP2         ;   ADD IN 9*CSRY TO MAKE 45*CSRY
         STA    ADP2
         LDA    #0
         ADC    ADP2+1
         STA    ADP2+1       ;   45*CSRY IN ADP2
         JSR    SADP2L       ;   90*CSRY IN ADP2
         JSR    SADP2L       ;   180*CSRY IN ADP2
         JSR    SADP2L       ;   360*CSRY IN ADP2
         LDA    CSRX         ; NEXT COMPUTE 6*CSRX WHICH IS A 9 BIT
         ASLA                ; VALUE
         ADC    CSRX
         ASLA
         STA    BTPT         ;   SAVE RESULT TEMPORARILY
         RORA                ;   DIVIDE BY 8 AND TRUNCATE FOR INT
         LSRA                ;   FUNCTION
         LSRA                ;   NOW HAVE INT(CSRX*6/8)
         CLC                 ; DOUBLE ADD TO ADP2
         ADC    ADP2
         STA    ADP2
         LDA    ADP2+1
         ADC    VMORG        ; ADD IN VMORG*256
         STA    ADP2+1       ; FINISHED WITH ADP2
         LDA    BTPT         ; COMPUTE REM(CSRX*6/8) WHICH IS LOW 3
         AND    #7           ; BITS OF CSRX*6
         STA    BTPT         ; KEEP IN BTPT
         RTS                 ; FINISHED

;        MERGE A ROW OF 5 DOTS WITH GRAPHIC MEMORY STARTING AT BYTE
;        ADDRESS AND BIT NUMBER IN ADP2 AND BTPT
;        5 DOTS TO MERGE LEFT JUSTIFIED IN A
;        PRESERVES X AND Y

MERGE:   STA    MRGT1        ; SAVE INPUT DATA
         TYA                 ; SAVE Y
         PHA
         LDY    BTPT         ; OPEN UP A 5 BIT WINDOW IN GRAPHIC MEMORY
         LDA    MERGT, Y     ; LEFT BITS
         LDY    #0           ; ZERO Y
         AND    (ADP2),Y
         STA    (ADP2),Y
         LDY    BTPT
         LDA    MERGT+8,Y    ; RIGHT BITS
         LDY    #1
         AND    (ADP2),Y
         STA    (ADP2),Y
         LDA    MRGT1        ; SHIFT DATA RIGHT TO LINE UP LEFTMOST
         LDY    BTPT         ; DATA BIT WITH LEFTMOST GRAPHIC FIELD
         BEQ    MERGE2       ; SHIFT BTPT TIMES
MERGE1:  LSRA
         DEY
         BNE    MERGE1
MERGE2:  ORA    (ADP2),Y     ; OVERLAY WITH GRAPHIC MEMORY
         STA    (ADP2),Y
         LDA    #8           ; SHIFT DATA LEFT TO LINE UP RIGHTMOST
         SEC                 ; DATA BIT WITH RIGHTMOST GRAPHIC FIELD
         SBC    BTPT         ; SHIFT (8-BTPT) TIMES
         TAY
         LDA    MRGT1
MERGE3:  ASLA
         DEY
         BNE    MERGE3
         INY
         ORA    (ADP2),Y     ; OVERLAY WITH GRAPHIC MEMORY
         STA    (ADP2),Y
         PLA                 ; RESTORE y
         TAY
         RTS                 ; RETURN

MERGT:   .BYTE  X'07,X'83,X'C1,X'E0  ; TABLE OF MASKS FOR OPENING UP
         .BYTE  X'F0,X'F8,X'FC,X'FE  ; A 5 BIT WINDOW ANYWHERE
         .BYTE  X'FF,X'FF,X'FF,X'FF  ; IN GRAPHIC MEMORY
         .BYTE  X'7F,X'3F,X'1F,X'0F

;        FAST MEMORY MOVE ROUTINE
;        ENTER WITH SOURCE ADDRESS IN ADPT1 AND DESTINATION ADDRESS IN
;        ADPT2 AND MOVE COUNT (DOUBLE PRECISION) IN DCNT1.
;        MOVE PROCEEDS FROM LOW TO HIGH ADDRESSES AT APPROXIMATELY 16US
;        PER BYTE.
;        EXIT WITH ADDRESS POINTERS AND COUNT IN UNKNOWN STATE.
;        PRESERVES X AND Y REGISTERS.

FMOVE:   TXA                 ; SAVE X AND Y ON THE STACK
         PHA
         TYA
         PHA
FMOVE1:  DEC    DCNT1+1      ; TEST IF LESS THAN 256 LEFT TO MOVE
         BMI    FMOVE3       ; JUMP TO FINAL MOVE IF SO
         LDY    #0           ; MOVE A BLOCK OF 256 BYTES QUICKLY
FMOVE2:  LDA    (ADP1),Y     ; TWO BYTES AT A TIME
         STA    (ADP2),Y
         INY
         LDA    (ADP1),Y
         STA    (ADP2),Y
         INY
         BNE    FMOVE2       ; CONTINUE UNTIL DONE
         INC    ADP1+1       ; BUMP ADDRESS POINTERS TO NEXT PAGE
         INC    ADP2+1
         JMP    FMOVE1       ; GO MOVE NEXT PAGE
FMOVE3:  LDX    DCNT1        ; GET REMAINING BYTE COUNT INTO X
FMOVE4:  LDA    (ADP1),Y     ; MOVE A BYTE
         STA    (ADP2),Y
         INY
         DEX
         BNE    FMOVE4       ; CONTINUE UNTIL DONE
         PLA                 ; RESTORE INDEX REGISTERS
         TAY
         PLA
         TAX
         RTS                 ; AND RETURN

;        FAST MEMORY CLEAR ROUTINE
;        ENTER WITH ADDRESS OF BLOCK TO CLEAR IN ADP2 AND CLEAR COUNT
;        IN DCNT1.
;        EXIT WITH ADDRESS POINTERS AND COUNT IN UNKNOWN STATE
;        PRESERVES X AND Y REGISTERS

FCLR:    TYA                 ; SAVE Y
         PHA
FCLR1:   LDY    #0
         DEC    DCNT1+1      ; TEST IF LESS THAN 256 LEFT TO MOVE
         BMI    FCLR3        ; JUMP INTO FINAL CLEAR IF SO
         TYA                 ; CLEAR A BLOCK OF 256 QUICKLY
FCLR2:   STA    (ADP2),Y     ; CLEAR A BYTE
         INY
         BNE    FCLR2
         INC    ADP2+1       ; BUMP ADDRESS POINTER TO NEXT PAGE
         JMP    FCLR1        ; GO CLEAR NEXT PAGE
FCLR3:   TYA                 ; CLEAR REMAINING PARTIAL PAGE
FCLR4:   STA    (ADP2),Y
         INY
         DEC    DCNT1
         BNE    FCLR4
         PLA                 ; RESTORE Y
         TAY
         RTS                 ; RETURN

         .PAGE    'CHARACTER FONT TABLE'
;        CHARACTER FONT TABLE
;        ENTRIES IN ORDER STARTING AT ASCII BLANK
;        96 ENTRIES
;        EACH ENTRY CONTAINS 7 BYTES
;        7 BYTES ARE CHARACTER MATRIX, TOP ROW FIRST, LEFTMOST DOT
;        IS LEFTMOST IN BYTE
;        LOWER CASE FONT IS SMALL UPPER CASE, 5 BY 5 MATRIX

CHTB:    .BYTE       X'00,X'00,X'00    ; BLANK
         .BYTE  X'00,X'00,X'00,X'00
         .BYTE       X'20,X'20,X'20    ; !
         .BYTE  X'20,X'20,X'00,X'20
         .BYTE       X'50,X'50,X'50    ; "
         .BYTE  X'00,X'00,X'00,X'00
         .BYTE       X'50,X'50,X'F8    ; #
         .BYTE  X'50,X'F8,X'50,X'50
         .BYTE       X'20,X'78,X'A0    ; X'
         .BYTE  X'70,X'28,X'F0,X'20
         .BYTE       X'C8,X'C8,X'10    ; %
         .BYTE  X'20,X'40,X'98,X'98
         .BYTE       X'40,X'A0,X'A0    ; &
         .BYTE  X'40,X'A8,X'90,X'68
         .BYTE       X'30,X'30,X'30    ; '
         .BYTE  X'00,X'00,X'00,X'00
         .BYTE       X'20,X'40,X'40    ; (
         .BYTE  X'40,X'40,X'40,X'20
         .BYTE       X'20,X'10,X'10    ; )
         .BYTE  X'10,X'10,X'10,X'20
         .BYTE       X'20,X'A8,X'70    ; *
         .BYTE  X'20,X'70,X'A8,X'20
         .BYTE       X'00,X'20,X'20    ; +
         .BYTE  X'F8,X'20,X'20,X'00
         .BYTE       X'00,X'00,X'00    ; ,
         .BYTE  X'30,X'30,X'10,X'20
         .BYTE       X'00,X'00,X'00    ; -
         .BYTE  X'F8,X'00,X'00,X'00
         .BYTE       X'00,X'00,X'00    ; .
         .BYTE  X'00,X'00,X'30,X'30
         .BYTE       X'08,X'08,X'10    ; /
         .BYTE  X'20,X'40,X'80,X'80
         .BYTE       X'60,X'90,X'90    ; 0
         .BYTE  X'90,X'90,X'90,X'60
         .BYTE       X'20,X'60,X'20    ; 1
         .BYTE  X'20,X'20,X'20,X'70
         .BYTE       X'70,X'88,X'10    ; 2
         .BYTE  X'20,X'40,X'80,X'F8
         .BYTE       X'70,X'88,X'08    ; 3
         .BYTE  X'30,X'08,X'88,X'70
         .BYTE       X'10,X'30,X'50    ; 4
         .BYTE  X'90,X'F8,X'10,X'10
         .BYTE       X'F8,X'80,X'F0    ; 5
         .BYTE  X'08,X'08,X'08,X'F0
         .BYTE       X'70,X'80,X'80    ; 6
         .BYTE  X'F0,X'88,X'88,X'70
         .BYTE       X'F8,X'08,X'10    ; 7
         .BYTE  X'20,X'40,X'80,X'80
         .BYTE       X'70,X'88,X'88    ; 8
         .BYTE  X'70,X'88,X'88,X'70
         .BYTE       X'70,X'88,X'88    ; 9
         .BYTE  X'78,X'08,X'08,X'70
         .BYTE       X'30,X'30,X'00    ; :
         .BYTE  X'00,X'00,X'30,X'30
         .BYTE       X'30,X'30,X'00    ; ;
         .BYTE  X'30,X'30,X'10,X'20
         .BYTE       X'10,X'20,X'40    ; LESS THAN
         .BYTE  X'80,X'40,X'20,X'10
         .BYTE       X'00,X'00,X'F8    ; =
         .BYTE  X'00,X'F8,X'00,X'00
         .BYTE       X'40,X'20,X'10    ; GREATER THAN
         .BYTE  X'08,X'10,X'20,X'40
         .BYTE       X'70,X'88,X'08    ; ?
         .BYTE  X'10,X'20,X'00,X'20
         .BYTE       X'70,X'88,X'08    ; @
         .BYTE  X'68,X'A8,X'A8,X'D0
         .BYTE       X'20,X'50,X'88    ; A
         .BYTE  X'88,X'F8,X'88,X'88
         .BYTE       X'F0,X'48,X'48    ; B
         .BYTE  X'70,X'48,X'48,X'F0
         .BYTE       X'70,X'88,X'80    ; C
         .BYTE  X'80,X'80,X'88,X'70
         .BYTE       X'F0,X'48,X'48    ; D
         .BYTE  X'48,X'48,X'48,X'F0
         .BYTE       X'F8,X'80,X'80    ; E
         .BYTE  X'F0,X'80,X'80,X'F8
         .BYTE       X'F8,X'80,X'80    ; F
         .BYTE  X'F0,X'80,X'80,X'80
         .BYTE       X'70,X'88,X'80    ; G
         .BYTE  X'B8,X'88,X'88,X'70
         .BYTE       X'88,X'88,X'88    ; H
         .BYTE  X'F8,X'88,X'88,X'88
         .BYTE       X'70,X'20,X'20    ; I
         .BYTE  X'20,X'20,X'20,X'70
         .BYTE       X'38,X'10,X'10    ; J
         .BYTE  X'10,X'10,X'90,X'60
         .BYTE       X'88,X'90,X'A0    ; K
         .BYTE  X'C0,X'A0,X'90,X'88
         .BYTE       X'80,X'80,X'80    ; L
         .BYTE  X'80,X'80,X'80,X'F8
         .BYTE       X'88,X'D8,X'A8    ; M
         .BYTE  X'A8,X'88,X'88,X'88
         .BYTE       X'88,X'88,X'C8    ; N
         .BYTE  X'A8,X'98,X'88,X'88
         .BYTE       X'70,X'88,X'88    ; O
         .BYTE  X'88,X'88,X'88,X'70
         .BYTE       X'F0,X'88,X'88    ; P
         .BYTE  X'F0,X'80,X'80,X'80
         .BYTE       X'70,X'88,X'88    ; Q
         .BYTE  X'88,X'A8,X'90,X'68
         .BYTE       X'F0,X'88,X'88    ; R
         .BYTE  X'F0,X'A0,X'90,X'88
         .BYTE       X'78,X'80,X'80    ; S
         .BYTE  X'70,X'08,X'08,X'F0
         .BYTE       X'F8,X'20,X'20    ; T
         .BYTE  X'20,X'20,X'20,X'20
         .BYTE       X'88,X'88,X'88    ; U
         .BYTE  X'88,X'88,X'88,X'70
         .BYTE       X'88,X'88,X'88    ; V
         .BYTE  X'50,X'50,X'20,X'20
         .BYTE       X'88,X'88,X'88    ; W
         .BYTE  X'A8,X'A8,X'D8,X'88
         .BYTE       X'88,X'88,X'50    ; X
         .BYTE  X'20,X'50,X'88,X'88
         .BYTE       X'88,X'88,X'50    ; Y
         .BYTE  X'20,X'20,X'20,X'20
         .BYTE       X'F8,X'08,X'10    ; Z
         .BYTE  X'20,X'40,X'80,X'F8
         .BYTE       X'70,X'40,X'40    ; LEFT BRACKET
         .BYTE  X'40,X'40,X'40,X'70
         .BYTE       X'80,X'80,X'40    ; BACKSLASH
         .BYTE  X'20,X'10,X'08,X'08
         .BYTE       X'70,X'10,X'10    ; RIGHT BRACKET
         .BYTE  X'10,X'10,X'10,X'70
         .BYTE       X'20,X'50,X'88    ; CARROT
         .BYTE  X'00,X'00,X'00,X'00
         .BYTE       X'00,X'00,X'00    ; UNDERLINE
         .BYTE  X'00,X'00,X'00,X'F8
         .BYTE       X'C0,X'60,X'30    ; GRAVE ACCENT
         .BYTE  X'00,X'00,X'00,X'00
         .BYTE       X'00,X'00,X'20    ; A (LC)
         .BYTE  X'50,X'88,X'F8,X'88
         .BYTE       X'00,X'00,X'F0    ; B (LC)
         .BYTE  X'48,X'70,X'48,X'F0
         .BYTE       X'00,X'00,X'78    ; C (LC)
         .BYTE  X'80,X'80,X'80,X'78
         .BYTE       X'00,X'00,X'F0    ; D (LC)
         .BYTE  X'48,X'48,X'48,X'F0
         .BYTE       X'00,X'00,X'F8    ; E (LC)
         .BYTE  X'80,X'E0,X'80,X'F8
         .BYTE       X'00,X'00,X'F8    ; F (LC)
         .BYTE  X'80,X'E0,X'80,X'80
         .BYTE       X'00,X'00,X'78    ; G (LC)
         .BYTE  X'80,X'98,X'88,X'78
         .BYTE       X'00,X'00,X'88    ; H (LC)
         .BYTE  X'88,X'F8,X'88,X'88
         .BYTE       X'00,X'00,X'70    ; I (LC)
         .BYTE  X'20,X'20,X'20,X'70
         .BYTE       X'00,X'00,X'38    ; J (LC)
         .BYTE  X'10,X'10,X'50,X'20
         .BYTE       X'00,X'00,X'90    ; K (LC)
         .BYTE  X'A0,X'C0,X'A0,X'90
         .BYTE       X'00,X'00,X'80    ; L (LC)
         .BYTE  X'80,X'80,X'80,X'F8
         .BYTE       X'00,X'00,X'88    ; M (LC)
         .BYTE  X'D8,X'A8,X'88,X'88
         .BYTE       X'00,X'00,X'88    ; N (LC)
         .BYTE  X'C8,X'A8,X'98,X'88
         .BYTE       X'00,X'00,X'70    ; O (LC)
         .BYTE  X'88,X'88,X'88,X'70
         .BYTE       X'00,X'00,X'F0    ; P (LC)
         .BYTE  X'88,X'F0,X'80,X'80
         .BYTE       X'00,X'00,X'70    ; Q (LC)
         .BYTE  X'88,X'A8,X'90,X'68
         .BYTE       X'00,X'00,X'F0    ; R (LC)
         .BYTE  X'88,X'F0,X'A0,X'90
         .BYTE       X'00,X'00,X'78    ; S (LC)
         .BYTE  X'80,X'70,X'08,X'F0
         .BYTE       X'00,X'00,X'F8    ; T (LC)
         .BYTE  X'20,X'20,X'20,X'20
         .BYTE       X'00,X'00,X'88    ; U (LC)
         .BYTE  X'88,X'88,X'88,X'70
         .BYTE       X'00,X'00,X'88    ; V (LC)
         .BYTE  X'88,X'88,X'50,X'20
         .BYTE       X'00,X'00,X'88    ; W (LC)
         .BYTE  X'88,X'A8,X'D8,X'88
         .BYTE       X'00,X'00,X'88    ; X (LC)
         .BYTE  X'50,X'20,X'50,X'88
         .BYTE       X'00,X'00,X'88    ; Y (LC)
         .BYTE  X'50,X'20,X'20,X'20
         .BYTE       X'00,X'00,X'F8    ; Z (LC)
         .BYTE  X'10,X'20,X'40,X'F8
         .BYTE       X'10,X'20,X'20    ; LEFT BRACE
         .BYTE  X'60,X'20,X'20,X'10
         .BYTE       X'20,X'20,X'20    ; VERTICAL BAR
         .BYTE  X'20,X'20,X'20,X'20
         .BYTE       X'40,X'20,X'20    ; RIGHT BRACE
         .BYTE  X'30,X'20,X'20,X'40
         .BYTE       X'10,X'A8,X'40    ; TILDA
         .BYTE  X'00,X'00,X'00,X'00
         .BYTE       X'A8,X'50,X'A8    ; RUBOUT
         .BYTE  X'50,X'A8,X'50,X'A8

         .END
