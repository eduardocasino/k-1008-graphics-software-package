        .PAGE 'SIMPLIFIED VISABLE MEMORY TEXT DISPLAY SUBROUTINE'
;       THIS SUBROUTINE TURNS THE VISABLE MEMORY INTO A DATA DISPLAY
;       TERMINAL (GLASS TELETYPE).
;       CHARACTER SET IS 96 FULL ASCII UPPER AND LOWER CASE.
;       CHARACTER MATRIX IS 5 BY 7 SET INTO A 6 BY 9 RECTANGLE.
;       LOWER CASE IS REPRESENTED AS SMALL (5 BY 5) CAPITALS.
;       SCREEN CAPACITY IS 22 LINES OF 53 CHARACTERS FOR FULL SCREEW
;       OR 11 LINES FOR HALF SCREEN.
;       CURSOR IS A NON-BLINKING UNDERLINE.
;       CONTROL CODES RECOGNIZED:
;       CR     X'0D         SETS CURSOR TO LEFT SCREEN EDGE
;       LF     X'0A         MOVES CURSOR DOWN ONE LINE, SCROLLS
;                           DISPLAY UP ONE LINE IF ALREADY ON BOTTOM
;                           LINE
;       BS     X'08         MOVES CURSOR ONE CHARACTER LEFT, DOES
;                           NOTHING IF ALREADY AT LEFT SCREEN EDGE
;       FF     X'0C         CLEARS SCREEN AND PUTS CURSOR AT TOP LEFT
;                           OF SCREEN, SHOULD BE CALLED FOR
;                           INITIALIZATION
;       ALL OTHER CONTROL CODES IGNORED.
;       ENTER WITH CHARACTER TO BE DISPLAYED IN A.
;       X AND Y PRESERVED.
;       3 BYTES OF RAM STORAGE REQUIRED FOR KEEPING TRACK OF THE
;       CURSOR
;       4 BYTES OF TEMPORARY STORAGE IN BASE PAGE REQUIRED FOR ADDRESS
;       POINTERS. (CAN BE DESTROYED BETWEEN CALLS TO SDTXT
;       4 BYTES OF TEMPORARY STORAGE ANYWHERE (CAN BE DESTROYED
;       BETWEEN CALLS TO SDTXT)

;       * **** VMORG #MUST# BE SET TO THE PAGE NUMBER OF THE VISIBLE *
;       * MEMORY BEFORE CALLING SDTXT ****                           *

;       GENERAL EQUATES

NLOC    =       8000            ; NUMBER OF VISIBLE LOCATIONS
CHHI    =       9               ; CHARACTER WINDOW HEIGHT
CHWID   =       6               ; CHARACTER WINDOW WIDTH
NCHR    =       320/CHWID       ; NUMBER OF CHARACTERS PER LINE
NLIN    =       NLOC/40/CHHI    ; NUMBER OF TEXT LINES
NSCRL   =       NLIN-1*CHHI*40  ; NUMBER OF LOCATIONS TO SCROLL
NCLR    =       NLOC-NSCRL      ; NUMBER OF LOCATIONS TO CLEAR AFTER SCROLL

;       BASE PAGE TEMPORARY STORAGE

        .=      X'EA
ADP1:   .=.+    2               ; ADDRESS POINTER 1
ADP2:   .=.+    2               ; ADDRESS POINTER 2

;       GENERAL TEMPORARY STORAGE

        .=      X'5B00          ; PLACE AT END OF 16K EXPANSION

BTPT:   .=.+    1               ; BIT NUMBER TEMPORARY STORAGE
DCNT1:  .=.+    2               ; DOUBLE PRECISION COUNTER
MRGT1:  .=.+    1               ; TEMPORARY STORAGE FOR MERGE

;       PERMANENT RAM STORAGE

CSRX:   .=.+    1               ; CURRENT CHARACTER NUMBER (0=LEFT CHAR)
CSRY:   .=.+    1               ; CURRENT LINE NUMBER (0=TOP LINE)
VMORG:  .=.+    1               ; FIRST PAGE NUMBER OF VISIBLE MEMORY

SDTXT:  PHA                     ; SAVE REGISTERS
        TXA
        PHA
        TYA
        PHA
        LDA     #0              ; CLEAR UPPER ADP2
        STA     ADP2+1
        TSX                     ; GET INPUT BACK
        LDA     X'103,X
        AND     #X'7F           ; INSURE 7 BIT ASCII INPUT
        SEC
        SBC     #X'20           ; TEST IF A CONTROL CHARACTER
        BMI     SDTXT10         ; JUMP IF SO

;       CALCULATE TABLE ADDRESS FOR CHAR SHAPE AND PUT IT INTO ADPL

SDTXT1: STA     ADP2            ; SAVE CHARACTER CODE IN ADP2
        JSR     SADP2L          ; COMPUTE 8*CHARACTER CODE IN ADP2
        JSR     SADP2L
        JSR     SADP2L
        EOR     #X'FF           ; NEGATE CHARACTER CODE
        SEC                     ; SUBSTRACT CHARACTER CODE FROM ADP2 AND
        ADC     ADP2            ; PUT RESULT IN ADP1 FOR A FINAL RESULT OF
        STA     ADP1            ; 7*CHARACTER CODE
        LDA     ADP2+1
        ADC     #X'FF
        STA     ADP1+1
        LDA     ADP1            ; ADD IN ORIGIN OF CHARACTER TABLE
        CLC
        ADC     #CHTB&X'FF
        STA     ADP1
        LDA     ADP1+1
        ADC     #CHTB/256
        STA     ADP1+1          ; ADP1 NOW HAS ADDRESS OF TOP ROW OF
                                ; CHARACTER SHAPE
;       COMPUTE BYTE AND BIT ADDRESS OF FIRST SCAN LINE OF
;       CHARACTER AT CURSOR POSITION

        JSR     CSRTAD          ; COMPUTE BYTE AND BIT ADDRESSES OF FIRST
                                ; SCAN LINE OF CHARACTER AT CURSOR POS.

;       SCAN OUT THE 7 CHARACTER ROWS

        LDY     #0              ; INITIALIZE Y INDEX=FONT TABLE POINTER
SDTX2:  LDA     (ADP1),Y        ; GET A DOT ROW FROM THE FONT TABLE
        JSR     MERGE           ; MERGE IT WITH GRAPHIC MEMORY AT (ADP2)
        JSR     DN1SCN          ; ADD 40 TO ADP2 TO MOVE DOWN ONE SCAN
                                ; LINE IN GRAPHIC MEMORY
        INY                     ; BUMP UP POINTER INTO FONT TABLE                    
        CPY     #7              ; TEST IF DONE
        BNE     SDTX2           ; GO DO NEXT SCAN LINE IF NOT
        LDA     CSRX            ; DO A CURSOR RIGHT
        CMP     #NCHR-1         ; TEST IF LAST CHARACTER ON THE LINE
        BPL     SDTX3           ; SKIP CURSOR RIGHT IF SO
        JSR     CSRCLR          ; CLEAR OLD CURSOR
        INC     CSRX            ; GO INSERT CURSOR, RESTORE REGISTERS,
                                ; AND RETURN

;       INTERPRET CONTROL CODES

SDTX10: CMP     #X'0D-X'20      ; TEST IF CR
        BEQ     SDTXCR          ; JUMP IF SO
        CMP     #X'0A-X'20      ; TEST IF LF
        BEQ     SDTXLF          ; JUMP IF SO
        CMP     #X'08-X'20      ; TEST IF BS
        BEQ     SDTXCL          ; JUMP IF SO
        CMP     #X'0C-X'20      ; TEST IF FF
        BEQ     SDTXFF          ; JUMP IF SO        
        JMP     SDTXRT          ; GO RETURN IF UNRECOGNIZABLE CONTROL

SDTXCR: JSR     CSRCLR          ; CARRIAGE RETURN, FIRST CLEAR CURSOR
        LDA     #0              ; ZERO CURSOR HORIZONTAL POSITION
        STA     CSRX
        JMP     SDTXRT          ; GO SET CURSOR AND RETURN

SDTXCL: JSR     CSRCLR          ; CURSOR LEFT, FIRST CLEAR CURSOR
        LDA     CSRX            ; GET CURSOR HORIZONTAL POSITION
        CMP     #0              ; TEST IF AGAINST LEFT EDGE
        BEQ     SDTX20          ; SKIP UPDATE IF SO
        DEC     CSRX            ; OTHERWISE DECREMENT CURSOR X POSITION
SDTX20: JMP     SDTXRT          ; GO SET CURSOR AND RETURN

SDTXFF: LDA     VMORG           ; FORM FEED, CLEAR SCREEN TO ZEROES
        STA     ADP2+1          ; TRANSFER VISIBLE MEMORY ORIGIN ADDRESS
        LDA     #0              ; TO ADP2
        STA     ADP2
        LDA     #NLOC&X'FF      ; SET COUNT OF LOCATIONS TO CLEAR IN DCNT1
        STA     DCNT1
        LDA     #NLOCK/256
        STA     DCNT1+1
        JSR     FCLR            ; CLEAR THE SCREEN
        LCD     #0
        STA     CSRX            ; PUT CURSOR IN UPPER LEFT CORNER
        STA     CSRY
        JMP     SDTXRT          ; GO SET CURSOR AND RETURN

SDTXLF: JSR     CSRCLR          ; LINE FEED, FIRST CLEAR CURSOR
        LDA     CSRY            ; GET CURRENT LINE POSITION
        CMP     #NLIN-1         ; TEST IF AY BOTTOM OF SCREEN
        BPL     SDTX40          ; GO SCROLL IF SO
        INC     CSRY            ; INCREMENT LINE NUMBER IF NOT AT BOTTOM
        BNE     SDTXRT          ; GO INSERT CURSOR AND RETURN
SDTX40: LDA     #0              ; SET UP ADDRESS POINTERS FOR MOVE
        STA     ADP2            ; ADP1 - SOURCE FOR MOVE = FIRST BYTE OF
        LDA     VMORG           ; SECOND LINE OF TEXT
        STA     ADP2+1          ; ADP2 = DESTINATION FOR MOVE = FIRST BYTE
        CLC                     ; IN VISIBLE MEMORY
        ADC     #CHHI*40/256
        STA     ADP1+1
        LDA     #CHHI*40&X'FF
        STA     ADP1
        LDA     #NSCRL&X'FF     ; SET NUMBER OF LOCATIONS TO MOVE
        STA     DCNT1           ; LOW PART
        LDA     #NSCRL/256      ; HIGH PART
        STA     DCNT1+1
        JSR     MOVE            ; EXECUTE MOVE USING AN OPTIMIZED, HIGH
                                ; SPEED MEMORY MOVE ROUTINE

                                ; CLEAR LAST LINE OF TEXT