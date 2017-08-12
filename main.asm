; MIXED VERSION 3.0
DATA SEGMENT
        SREADMSG1 DB 0DH,0AH,'PRESS O TO START THE ORGANSTOP',0DH,0AH,'$'
        SREADMSG2 DB 'PRESS M TO START THE METRONOME',0DH,0AH,'$'
        SREADMSG3 DB 'PRESS Q TO QUIT.',0DH,0AH,'$'        
		
        OTIME DB 0
        ; USED IN ORGANSTOP
        OREADMSG1 DB 0DH,0AH,'PRESS S TO START THE 440HZ STANDARD LA!',0DH,0AH,'$'
        OREADMSG2 DB 0DH,0AH,'THE 440HZ LA IS BEEPING. PRESS ANY KEY TO STOP.',0DH,0AH,'$'
        OREADMSG3 DB 'PRESS Q TO QUIT.',0DH,0AH,'$'
        
        MRHYTHM DW 60
        MHALFBEAT DW 2 DUP(0,0)
        ; MHALFBEAT DW 2 DUP(41248,7)
        ; MHALFBEAT=7*(2^16)+41248
        ; THE HALP BEAT SECONDS*1000000
        ; MHALFBEAT=30*1000000/MRHYTHM

        MTOUCH DB 0
        ; MEANS KEYBOARD TOUCH IN BEEP&EMPTY
        MTIME DW 0
        ; USED IN METRONOME
		
        BTIME DW 0
        BCOUNT DW 0
        BFLAG DB 0
        ; CASE 1,SPECIAL SOUND 659.3HZ
        ; BFLAG=0
        ; CASE 2,NORMAL SOUND 440HZ
        ; BFLAG=1
        ; CASE 3, EMPTY SOUND
        ; BFLAG=2
        ; USED IN BEEPSOUND
        BBEATNUM DB 1
        ; WHICH BEAT 
        BSUMUNIT DB 0
        BSUMDECADE DB 0
        BSUMHUNDRED DB 0
        ; HOW MANY BEATS IN TOTAL
        BSUMTOTAL DW 0
        ; [BSUMHUNDRED][BSUMDECADE][BSUMUNIT]

        READWRONG DB 0DH,0AH,'THE READ-IN RHYTHM WAS WRONG! QAQ',0DH,0AH,'$'
        READMSG1 DB 'PLEASE PUT IN THE RHYTHM IN THREE-DIGIT FORM!~FROM 020 TO 240~',0DH,0AH,'$'
        READMSG2 DB 'PRESS Q TO QUIT.',0DH,0AH,'$'
        READMSG3 DB 0DH,0AH,'THE BEEP TOTAL IS:',0DH,0AH,'$'
        RHIGHBIT DW 0
        RLOWBIT DW 0
        RQUITFLAG DB 0
        ; RQUITFLAG=1 QUIT METRONOME

        SIN DB 080H,096H,0AEH,0C5H,0D8H,0E9H,0F5H,0FDH
            DB 0FFH,0FDH,0F5H,0E9H,0D8H,0C5H,0AEH,096H
            DB 080H,066H,04EH,038H,025H,015H,009H,004H
            DB 000H,004H,009H,015H,025H,038H,04EH,066H

        LED DB 3FH,06H,5BH,4FH,66H,6DH,7DH,07H
            DB 7FH,67H,77H,7CH,39H,5EH,79H,71H
DATA ENDS

CODE SEGMENT
        ASSUME CS:CODE,DS:DATA
START:
        MOV AX,DATA
        MOV DS,AX
SREAD:        
        MOV DX,OFFSET SREADMSG1
        MOV AH,9
        INT 21H		; ORGANSTOP

        MOV DX,OFFSET SREADMSG2
        MOV AH,9
        INT 21H		; METRONOME

        MOV DX,OFFSET SREADMSG3
        MOV AH,9
        INT 21H		; QUIT

        MOV AH,1H
        INT 21H			; AL=CONTROL LETTER
        ; IF AL=O,START ORGANSTOP 
        ; IF AL=M,START METRONOME
        ; IF AL=Q,QUIT THE DISP

        CMP AL,'Q'
        JE SEND        ; EXIT		  
        CMP AL,'O'
        JE SOSTART
        CMP AL,'M'
        JE SMSTART
        JMP SREAD
SOSTART:
        CALL ORGANSTOP
        JMP SREAD
SMSTART:
        CALL METRONOME
        JMP SREAD

SEND:
        MOV AH,4CH
        INT 21H

ORGANSTOP PROC NEAR
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        ; ORGANSTOP, HELP TONE TURNING

        MOV DX,0E48BH
        MOV AL,10000001B
        OUT DX,AL       ; INITIALIZE 8255

        MOV AL,00010000B 
        MOV DX,0E483H
        OUT DX,AL       ; INITIALIZE 8253

OREAD:
        MOV DX,OFFSET OREADMSG1
        MOV AH,9
        INT 21H		; START

        MOV DX,OFFSET OREADMSG3
        MOV AH,9
        INT 21H		; QUIT

        MOV AH,1H
        INT 21H			; AL=CONTROL LETTER
        ; IF AL=S,START BEEPING LA 
        ; IF AL=Q,QUIT THE DISP

        CMP AL,'Q'
        JE OEND        ; EXIT		  
        CMP AL,'S'
        JE OSTARTBEEP        ; START BEEPING
        JMP OREAD

OSTARTBEEP:		
        MOV DX,OFFSET OREADMSG2
        MOV AH,9
        INT 21H		; START BEEPING & STOP

OLOOP:
        MOV OTIME,0
        MOV CX,32
OBEEP:
        MOV BX,OFFSET SIN
        MOV AL,OTIME
        MOV DX,0E490H
        XLAT
        OUT DX,AL
        INC OTIME
		
        MOV DX,0E480H
       ; MOV AL,71               ; 440HZ'S DELAYTIME
       ; THEORETICAL VALUE
        MOV AL,65
        OUT DX,AL		; SET 8253 DELAY NUM

ODELAY:
        MOV DX,0E48AH
        IN AL,DX
        AND AL,1
        CMP AL,0
        JZ ODELAY     ; DELAY

        MOV AH,1
        INT 16H
        JNZ OREAD               ;?
        ; IF TOUCHED,GO TO OREAD
        ; ELSE, GO TO OBEEP

        LOOP OBEEP
        JMP OLOOP
OEND:
        POP DX
        POP CX
        POP BX
        POP AX
        RET
ORGANSTOP ENDP

METRONOME PROC NEAR
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        ; METRONOME1.0 VERSION
        MOV DX,0E48BH
        MOV AL,10000001B
        OUT DX,AL       ; INITIALIZE 8255

        MOV AL,00010000B 
        MOV DX,0E483H
        OUT DX,AL       ; INITIALIZE 8253

        MOV RQUITFLAG,0
MREAD:
        CMP MTOUCH,0
        JZ MTOUCHZERO

        MOV MTOUCH,0    ; INITIALIZE

        MOV AH,02H
        MOV DL,0DH
        INT 21H
        MOV DL,0AH
        INT 21H

MTOUCHZERO:
        CALL READRHYTHM
        ; READ IN A THREE-DIGIT NUMBER: RHYTHM
        ; PUT IT IN MRHYTHM

        CMP RQUITFLAG,1
        JZ MEND

        MOV AH,1H
        INT 21H			; AL=CONTROL LETTER
        ;IF AL=S,START THE METRONOME 
        ;IF AL=Q,QUIT THE DISP

        MOV BSUMUNIT,0
        MOV BSUMDECADE,0
        MOV BSUMHUNDRED,0
        MOV BSUMTOTAL,0
        ; INITIALIZE BSUM

        CMP AL,'Q'
        JE MEND        ; EXIT  
        CMP AL,'S'
        JE MBEEPSTART  ; START OUTPUT&BEEPING
        JMP MREAD

MREADTEMP:
        JMP MREAD
        ; USED IN FINAL JMP PARTR

MBEEPSTART:
        MOV DX,OFFSET READMSG3
        MOV AH,9
        INT 21H         
MBEEP:        
        MOV BBEATNUM,0
        ; INITIALIZE

        MOV BFLAG,1
        CALL BEEPSOUND  ; SPECIAL SOUND
        CMP MTOUCH,1
        JZ MREAD
	    
        MOV BFLAG,2
        CALL BEEPSOUND  ; EMPTY SOUND
        CMP MTOUCH,1
        JZ MREAD

        MOV CX,3
MNORMAL:
        MOV BFLAG,0
        CALL BEEPSOUND  ; NORMAL SOUND
        CMP MTOUCH,1
        JZ MREADTEMP

        MOV BFLAG,2
        CALL BEEPSOUND  ; EMPTY SOUND
        CMP MTOUCH,1
        JZ MREADTEMP

        LOOP MNORMAL

        MOV AH,1
        INT 16H
        JNZ MREADTEMP               
        ; IF TOUCHED,GO TO MREADTEMP         
        ; IF NOT TOUCHED, GO TO MBEEP
        JMP MBEEP

MEND:
        POP DX
        POP CX
        POP BX
        POP AX
        RET
METRONOME ENDP

BEEPSOUND PROC NEAR
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX

        ; THE SOUND FOR 1/2 BEAT
        ; CASE 1,SPECIAL SOUND 659.3HZ
        ; BFLAG=0
        ; CASE 2,NORMAL SOUND 440HZ
        ; BFLAG=1
        ; CASE 3,EMPTY SOUND
        ; BFLAG=2

        CMP BFLAG,2
        JZ BLIGHTSTART         ; EMPTY HALF BEAT

       ; ADD BSUMTOTAL,1
       ; MOV AX,BSUMTOTAL
       ; CALL DISP              ; SHOW THE BEEPSUM ON THE SCREEM

        ADD BSUMUNIT,1
        CMP BSUMUNIT,10
        JNZ BSUMEND
        MOV BSUMUNIT,0
        ADD BSUMDECADE,1
        CMP BSUMDECADE,10
        JNZ BSUMEND
        MOV BSUMDECADE,0
        ADD BSUMHUNDRED,1

BSUMEND:
       ; MOV AH,03H
       ; INT 21H         ; READ THE CURSOR
       ; MOV DL,1
       ; MOV AH,02H
       ; INT 21H         ; OUT THE CURSOR

        MOV AX,BSUMHUNDRED
        MOV CL,12
        SHL AX,CL
        CALL DISP2              ; OUTPUT THE HUNDRED BIT
        MOV AX,BSUMDECADE
        MOV CL,12
        SHL AX,CL
        CALL DISP2              ; OUTPUT THE DECADE BIT
        MOV AX,BSUMUNIT
        MOV CL,12
        SHL AX,CL
        CALL DISP2              ; OUTPUT THE UNIT BIT

        MOV DL,0DH
        INT 21H

        ADD BBEATNUM,1
        CMP BBEATNUM,5
        JNZ BLIGHTSTART
        MOV BBEATNUM,1

BLIGHTSTART:
        ; LED LIGHT
        CMP BBEATNUM,1
        JZ BLIGHT1
        CMP BBEATNUM,3
        JZ BLIGHT3
        JMP BLIGHT2

BLIGHT1:
        MOV DX,0E488H
        MOV AL,11111111B        ; OUT LEDLIGHT 8LIGHTS
        OUT DX,AL
        JMP BDIV
BLIGHT2:
        MOV DX,0E488H
        MOV AL,00001111B        ; OUT LEDLIGHT 4LIGHTS
        OUT DX,AL
        JMP BDIV
BLIGHT3:
        MOV DX,0E488H
        MOV AL,00111111B        ; OUT LEDLIGHT 6LIGHTS
        OUT DX,AL
        JMP BDIV

BDIV:
        ; CALCULATE 
        MOV BX,OFFSET MHALFBEAT
        MOV AX,2
        XLAT
        MOV DX,AX

        MOV BX,OFFSET MHALFBEAT
        MOV AX,0
        XLAT

        CMP BFLAG,0
        JZ SBEEP1               ; SPECIAL BEEP
        MOV BX,71*32            ; NORMAL CASE & EMPTY CASE
        JMP BCONTINUE1
SBEEP1:
        MOV BX,47*32       
BCONTINUE1:
        DIV BX
        MOV BCOUNT,AX   ; DIV

BLOOP:
        MOV BTIME,0
        MOV CX,32
BBEEP:
        CMP BFLAG,2
        JZ EOUT         ; EMPTY'S OUT

        MOV BX,OFFSET SIN
        MOV AX,BTIME
        MOV DX,0E490H
        XLAT
        OUT DX,AL
EOUT:
        INC BTIME
        MOV DX,0E480H
        CMP BFLAG,0
        JZ SBEEP2
        MOV AL,71               ; NORMAL CASE & EMPTY CASE
        JMP BCONTINUE2
SBEEP2:
        MOV AL,47      
BCONTINUE2:
        OUT DX,AL       ; SET 8253 DELAY NUM
BDELAY:
        MOV AH,1
        INT 16H
        JNZ BTEND2               ;?
        ;IF TOUCHED, JUMP TO BTEND2=BTEND

        MOV DX,0E48AH   ; C PORT,INPUT  
        IN AL,DX
        AND AL,1
        CMP AL,0
        JZ BDELAY     ; DELAY
        LOOP BBEEP

        MOV AX,BCOUNT
        MOV BL,4
        DIV BL          ; REMAINDER IN AH
        CMP AH,0
        JZ BLED0
        CMP AH,1
        JZ BLED1
        CMP AH,2
        JZ BLED2
        JMP BLED3

BTEND2:
        JMP BTEND
        ; USED IN BEFORE JUMPS
BLOOP2:
        JMP BLOOP
        ; USED IN AFTER JUMPS

        ; LED OUTPUT
BLED0:
        MOV DX,0E48AH
        MOV AL,128        ; WEIMA, C PORT
        OUT DX,AL
        MOV DX,0E489H
        MOV AL,BBEATNUM
        MOV BX,OFFSET LED
        XLAT
        OUT DX,AL               ; B PORT, LED LIGHT
        JMP BLEDEND
BLED1:
        MOV DX,0E48AH
        MOV AL,16        ; WEIMA, C PORT
        OUT DX,AL
        MOV DX,0E489H
        MOV AL,BSUMUNIT
        AND AL,0FH
        MOV BX,OFFSET LED
        XLAT
        OUT DX,AL               ; B PORT, LED LIGHT
        JMP BLEDEND
BLED2:
        MOV DX,0E48AH
        MOV AL,32        ; WEIMA, C PORT
        OUT DX,AL
        MOV DX,0E489H
        MOV AL,BSUMDECADE
        AND AL,0FH
        MOV BX,OFFSET LED
        XLAT
        OUT DX,AL               ; B PORT, LED LIGHT
        JMP BLEDEND
BLED3:
        MOV DX,0E48AH
        MOV AL,64        ; WEIMA, C PORT
        OUT DX,AL
        MOV DX,0E489H
        MOV AX,BSUMHUNDRED
        AND AL,0FH
        MOV BX,OFFSET LED
        XLAT
        OUT DX,AL               ; B PORT, LED LIGHT
        JMP BLEDEND

BLEDEND:
        DEC BCOUNT
        CMP BCOUNT,0
        JNZ BLOOP2

        MOV AH,1
        INT 16H
        JZ BEND               ;?
        ;IF NOT TOUCHED, JUMP TO BEND

BTEND:
        MOV MTOUCH,1

BEND:
        POP DX
        POP CX
        POP BX
        POP AX
        RET
BEEPSOUND ENDP

READRHYTHM PROC NEAR
        ; READ IN A THREE-DIGIT NUMBER: RHYTHM
        ; PUT IT IN MRHYTHM

        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        JMP RREAD

RWRONG:
        ; IF THE READ-IN WENT WRONG,PRINT OUT
        MOV DX,OFFSET READWRONG
        MOV AH,9
        INT 21H

RREAD:
        MOV AH,0CH
        INT 21H         ; CLEAR THE KEYBOARD BUFFER

        MOV DX,OFFSET READMSG1
        MOV AH,9
        INT 21H

        MOV DX,OFFSET READMSG2
        MOV AH,9
        INT 21H

        MOV MRHYTHM,0

        MOV AH,1
        INT 21H
        CMP AL,'Q'
        JZ RQUIT
        CMP AL,'0'
        JL RWRONG
        CMP AL,'9'
        JG RWRONG
        SUB AL,'0'
        MOV AH,100
        MUL AH         ;AX=AL*100
        MOV MRHYTHM,AX

        MOV AH,1
        INT 21H
        CMP AL,'0'
        JL RWRONG
        CMP AL,'9'
        JG RWRONG
        SUB AL,'0'
        MOV AH,10
        MUL AH         ;AX=AL*10
        ADD MRHYTHM,AX

        MOV AH,1
        INT 21H
        CMP AL,'0'
        JL RWRONG
        CMP AL,'9'
        JG RWRONG
        SUB AL,'0'
        ADD MRHYTHM,AL

        CMP MRHYTHM,20
        JL RWRONG       ; ILLEGAL INPUT
        CMP MRHYTHM,240
        JG RWRONG       ; ILLEGAL INPUT
        MOV AH,1
        INT 21H
        CMP AL,0DH
        JZ REND
        ; IF THE NEXT ONE='\N',RETURN
RQUIT:
        MOV RQUITFLAG,1
        MOV MRHYTHM,233         ; PREVENT DIVIDE 0
REND:
        ; MHALFBEAT=30*1000000/MRHYTHM
        ; MHALFBEAT[0] IS THE LOWEST BIT OF RESULT
        ; THE RESULT LAST FROM MHALFBEAT[0] TO MHALFBEAT[3]
        ; http://blog.csdn.net/ljianhui/article/details/17457317
        ; SHIFTING METHOD IN DIVIDE
        MOV DX,0H
        MOV AX,01C9H         ; DIVIDEND,30*1000000=01C9C380H
        MOV CX,MRHYTHM       ; DIVISOR
        DIV CX
        MOV BX,AX
   
        MOV AX,0C380H
        DIV CX
        MOV DX,BX               ; DX, RESULT'S HIGH BIT
        ; AX,RESULT'S LOW BIT

        MOV RHIGHBIT,DX
        MOV RLOWBIT,AX

        MOV AX,RLOWBIT
        MOV MHALFBEAT[0],AL
        MOV MHALFBEAT[1],AH
        MOV DX,RHIGHBIT
        MOV MHALFBEAT[2],DL
        MOV MHALFBEAT[3],DH

       ; MOV AX,1
       ; MOV BX,OFFSET MHALFBEAT
       ; XLAT
       ; CALL DISP

        POP DX
        POP CX
        POP BX
        POP AX
        RET
READRHYTHM ENDP

DISP PROC NEAR  ;SHOW THE WORDS IN AX
        PUSH BX
        PUSH CX
        MOV BX,AX
        MOV CX,4
LLOOP1:
        MOV AX,BX
        CALL DISP2
        PUSH CX
        MOV CL,4
        SHL BX,CL
        POP CX
        LOOP LLOOP1
        POP CX
        POP BX
        RET
DISP ENDP

DISP2 PROC NEAR
        PUSH DX
        PUSH CX
        AND AH,0F0H
        MOV CL,4
        SHR AH,CL
        MOV DL,AH
        CMP DL,9
        JLE NUM
        ADD DL,7
NUM:
        ADD DL,30H
        MOV AH,02H
        INT 21H
        POP CX
        POP DX
        RET
DISP2 ENDP
CODE ENDS
END START
