(*$L'C86P1 - THE PARSER'*)

(* FORWARD REFERENCES ..................... 
 *) 
FUNCTION BASICSIZE(TYP : INTEGER):INTEGER;EXTERN; 
PROCEDURE STACKSET;EXTERN;
FUNCTION SIZEOFWORD:INTEGER;EXTERN; 
FUNCTION SIZEOFFIELD:INTEGER;EXTERN;

PROCEDURE OUTEXPRESSION;EXTERN; 
PROCEDURE OUTNAME(STRNG : ALFA);EXTERN; 

FUNCTION NEWEXPNODE(OPR : OPSYM;TOK : SYMMODE): EXPTREE;EXTERN; 
FUNCTION INDIRECT(N:INTEGER):INTEGER; EXTERN; 
PROCEDURE PREVIOUSLABEL;EXTERN; 
  FUNCTION CHECK(SYM : SYMMODE; SYSET : SYMMODESET; 
  ERRORNO : ERRORCODES) : BOOLEAN;FORWARD;
PROCEDURE DISCARD(T:EXPTREE); FORWARD;

PROCEDURE PRINTTREE(T : EXPTREE; DEPTH : INTEGER);EXTERN; 
  PROCEDURE KONSTEXP;FORWARD; 
FUNCTION STRUCTSPEC : INTEGER;FORWARD;

  FUNCTION GIVELABEL : INTEGER;EXTERN;
  FUNCTION TYPETREE(VAR T:EXPTREE):INTEGER;EXTERN;

  FUNCTION REMING(TYPH:TYPELIST;INFLAG : BOOLEAN):TYPELIST; 
  FORWARD;

  PROCEDURE TYPD(TYPH:TYPELIST; INFUNCDEF : BOOLEAN;ABSFLG : BOOLEAN);
  FORWARD;

 FUNCTION OUTSTATIC(S : EXPTREE):INTEGER;FORWARD; 



  PROCEDURE EXP;
  FORWARD;
  PROCEDURE EXPRESSION; 
  FORWARD;

  PROCEDURE STMTNT(LOOPSTART,BREAKLAB:INTEGER);FORWARD; 

  PROCEDURE COMPOUNDST(LOOPSTART,BREAKLAB:INTEGER);FORWARD; 

  PROCEDURE DECLTOR(INFLAG : BOOLEAN;VAR SYMTAB : SYMTABPTR);FORWARD; 
  FUNCTION NAMEOFTYPE( VAR TYPENUMBER: INTEGER):BOOLEAN;FORWARD;

  PROCEDURE PARMLIST;FORWARD; 

(* PARSE THE PARAMETER LIST OF A FUNCTION DEFINITION
 *) 
 PROCEDURE PUSHTOKEN;FORWARD; 
   FUNCTION PUTINSYMBOLTABLE(VAR G : SYMTABPTR; 
                              ID       : ALFA;
                              ISTATE   : IDSTATE; 
                              IMODE    : INTEGER):IDTREE; FORWARD;
  PROCEDURE OUTEXPTREE( T : EXPTREE; LEFT : BOOLEAN ;VAR LAB : INTEGER);EXTERN; 
PROCEDURE OUTALFA(STRNG : ALFA ; LENG : INTEGER);EXTERN;
PROCEDURE GETLIST(TYP : INTEGER;MANY : BOOLEAN);FORWARD;
FUNCTION EMPTYTYPELIST(TYPH : TYPELIST) : INTEGER; FORWARD; 


(* END OF FORWARD REFERENCES....................
 *) 

(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *     ERROR HANDLING ROUTINE.
 *     ERROROUT   PRINTS THE ERROR MESSAGES.
 *     ERROR(N)   PUTS THE ERROR CODE N IN THE ERRORLIST. 
 *   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 *) 

 PROCEDURE ERROROUT;

(* PRINT IN OF MARKERS UNDER LAST LINE OF ERRLIST 
 *     SHOWING TOKENS IN ERROR
 *) 
 VAR POS : INTEGER; 
     ERZ : ERRPTR;
     PREVPOS : INTEGER; 
 BEGIN
   PREVPOS:=-1; 
     WHILE ERRLIST<>NIL 
     DO BEGIN 
          IF PREVPOS<>ERRLIST^.POSN 
          THEN BEGIN
         WRITE(ERRFILE,LINENO:6,' ? '); 
          FOR POS:=0 TO ERRLIST^.POSN 
          DO WRITE(ERRFILE,' ');
                 WRITELN(ERRFILE,ERRLIST^.NUM:1); 
                 PREVPOS:=ERRLIST^.POSN;
               END; 
               CASE ERRLIST^.NUM
               OF 
                  3 : WRITELN(ERRFILE,'*** MISSING DECLARATION'); 
29,0,32,41,999   : WRITELN(ERRFILE,'*** LEFT PAREN EXPECTED');
34,38,43,45,47,55,
         50,51,4,11 : WRITELN(ERRFILE,'*** SEMICOLON EXPECTED '); 
             54,5,6 : WRITELN(ERRFILE,'*** RIGHT CURLY BRACKET EXPECTED');
                7,8 : WRITELN(ERRFILE,'*** RIGHT BRACKET EXPECTED');
42,37,24,26,
   27,30,31,35,9,10 : WRITELN(ERRFILE,'*** RIGHT PAREN EXPECTED');
                  12: WRITELN(ERRFILE,'***  EXPRESSION NOT A FUNCTION NAME'); 
               13,14: WRITELN(ERRFILE,'***  EXPRESSION NOT A POINTER OR ARRAYTYPE');
                 15 : WRITELN(ERRFILE,'*** NOT ARITHMETIC TYPE ');
                 16 : WRITELN(ERRFILE,'*** WRONG TYPES FOR * OR / '); 
                 18 : WRITELN(ERRFILE,'*** WRONG TYPE FOR - '); 
                 19 : WRITELN(ERRFILE,'*** TYPES MUST BE INTEGRAL '); 
              49,20 : WRITELN(ERRFILE,'*** INCONSISTENT USE OF IDENTIFIER');
                 36 : WRITELN(ERRFILE,'*** WHILE EXPRESSION EXPECTED'); 
           69,70,48 : WRITELN(ERRFILE,'*** IDENTIFIER EXPECTED'); 
                 53 : WRITELN(ERRFILE,'*** LEFT CURLY BRACKET EXPECTED'); 
                 52 : WRITELN(ERRFILE,'*** INVALID STATEMENT'); 
                 46 : WRITELN(ERRFILE,'*** CONTINUE NOT ALLOWED HERE'); 
                 44 : WRITELN(ERRFILE,'*** BREAK NOT ALLOWED HERE');
                 60 : WRITELN(ERRFILE,'*** DIVIDING BY ZERO');
                 61 : WRITELN(ERRFILE,'*** MISSING LABEL ');
                 62 : WRITELN(ERRFILE,'*** ILLEGAL INITIALISATION');
                 63 : WRITELN(ERRFILE,'*** INVALID EXPRESSION FOR INLISER');
                 65 : WRITELN(ERRFILE,'*** CANNOT TAKE ADDRESS OF FIELD');
                 66 : WRITELN(ERRFILE,'*** NOT A VALID EXPRESSION');
                 67 : WRITELN(ERRFILE,'*** MISSING CLOSING QUOTE..'); 
                 68 : WRITELN(ERRFILE,'*** CODE GEN FAILURE. SEE GENERATED CODE FOR DETAILS');
                 71 : WRITELN(ERRFILE,'*** WON"T FIT.');
                 72 : WRITELN(ERRFILE,'*** WON"T COERSE BETWEEN THOSE TYPES.'); 
                 81 : WRITELN(ERRFILE,'*** STORAGE CLASS DECLARATION NOT ALLOWED.');
                 82 : WRITELN(ERRFILE,'*** TO MANY INITIALISERS ^2 EXPECTED');

                 84 : WRITELN(ERRFILE,'*** CONSTANT EXPRESSION TO COMPLICATED');
                 85 : WRITELN(ERRFILE,'*** STRUCTURES NOT OF SAME SIZE'); 
                 86 : WRITELN(ERRFILE,'*** ONLY SIMPLE ASSIGNMENT ALLOWED FOR STRUCTURES'); 
                 87 : WRITELN(ERRFILE,'*** STRUCTURE HAS NO SIZE'); 

                 OTHERWISE
               END; 
(* CASE 
 *) 
                 ERZ:=ERRLIST;
                 ERRLIST:=ERRLIST^.NEXT;
                 DISPOSE(ERZ);
      END;
 END; 
(* ERRLIST
 *) 

PROCEDURE ERROR(I : ERRORCODES);


   VAR ER,ER1 : ERRPTR; 
   BEGIN
     GLOBALERROR:=TRUE; 
     EXPERROR:=TRUE;
   NEW(ER1);
     ER1^.POSN:=POSITIONOFINPUT;
     ER1^.NUM:=I; 
     ER1^.NEXT:=NIL;
     ER1^.ELINE:=LINENO;
     IF ERRLIST=NIL 
     THEN ERRLIST:=ER1
     ELSE 
       BEGIN
         ER:=ERRLIST; 
         WHILE ER^.NEXT<>NIL
         DO ER:=ER^.NEXT; 
         ER^.NEXT:=ER1; 
       END; 
  END;
(* ERROR
 *) 



PROCEDURE WARN(I : ERRORCODES); 
VAR W,WR1 : ERRPTR; 
BEGIN 
  WARNING:=TRUE;
  NEW(WR1); 
  WITH WR1^ 
  DO BEGIN
       POSN:=POSITIONOFINPUT; 
       NUM:=I;
       NEXT:=NIL; 
       ELINE:=LINENO; 
  END;
  IF WARNLIST=NIL THEN WARNLIST:=WR1
  ELSE BEGIN
         W:=WARNLIST; 
         WHILE W^.NEXT<>NIL 
         DO W:=W^.NEXT; 
         W^.NEXT:=WR1;
       END; 
END;



PROCEDURE WARNOUT;
VAR POS : INTEGER;
    ERZ : ERRPTR; 
    PREVPOS:INTEGER;
BEGIN 
  PREVPOS:=-1;
  WHILE WARNLIST<>NIL 
  DO BEGIN
       IF PREVPOS<>WARNLIST^.POSN 
       THEN BEGIN 
              WRITE(ERRFILE,LINENO:6,' W ');
              FOR POS := 0 TO WARNLIST^.POSN
              DO WRITE(ERRFILE,' ');
              WRITELN(ERRFILE,WARNLIST^.NUM:1); 
              PREVPOS:=WARNLIST^.POSN;
            END;
       CASE WARNLIST^.NUM 
       OF 
          1 : WRITELN(ERRFILE,'WARN  IDENTIFER IS ASSUMED TO BE INTEGER');
          2 : WRITELN(ERRFILE,'WARN  OLD FORM OF ASSIGNMENT OPERATOR'); 
          3 : WRITELN(ERRFILE,'WARN  CHARACTER NOT RECOGNISED. SO IGNORED');
          4 : WRITELN(ERRFILE,'STRING TRUNCATED '); 
          5 : WRITELN(ERRFILE,'WARN  TYPE COERCION MISMATCH. PETE...'); 
          8 : WRITELN(ERRFILE,'WARN  MORE INITIALISERS EXPECTED.PADDED WITH 0');
          9 : WRITELN(ERRFILE,'WARN RE-DECLARATION OVERRIDES EARLIER ONE'); 
         10 : WRITELN(ERRFILE,'WARN RE-DECLARATION IGNORED'); 
         11 : WRITELN(ERRFILE,'WARN STRUCTURE ASSIGMENT ASSUMED');
          OTHERWISE 
       END; 
       ERZ:=WARNLIST; 
       WARNLIST:=WARNLIST^.NEXT;
       DISPOSE(ERZ);
     END; 
END;



(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *     INPUT CHARACTERS,TOKENS  ROUTINES. 
 *    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 *) 
  FUNCTION ISRESERVED(ID:ALFA;VAR SY : SYMMODE;VAR OP:OPSYM;VAR IDTYPE : TYPEMODE): BOOLEAN;

(* PROCEDURE TO DISCOVER IF THE 'ID' IS A 
 *      RESERVED WORD 
 *       RETURNS TRUE IF ID = RESERVED WORD 
 *                    AND 'SY' 'OP' 'IDTYPE'
 *                    ARE ALSO SETUP
 *               FALSE OTHERWISE
 *) 

    LABEL 100; 
    VAR I : 0..NOOFRESERVEDWORDS ;

  BEGIN 
    ISRESERVED := FALSE;
    IF IDCOUNT < 9
    THEN BEGIN
         IF ID = 'SIZEOF    ' 
         THEN BEGIN 
              SY := TERMOP; 
              OP := SIZEOF; 
              IDTYPE := NULLTYPE; 
              ISRESERVED := TRUE
              END 
         ELSE BEGIN 
              FOR I := SUCC(LENGTHTABLE[PRED(IDCOUNT)]) 
                    TO LENGTHTABLE[IDCOUNT] 
              DO IF ID = RESERVED[I]
                 THEN BEGIN 
                      SY := TYPETABLE[I]; 
                      OP := NOOP; 
                      IDTYPE := IDMODETABLE[I]; 
                      ISRESERVED := TRUE; 
                      GOTO 100
                      END;
   100        : 
              END 
         END
  END;





PROCEDURE NEXTCH; 
(*   PROCEDURE TO READ THE NEXT CHARACTER FROM THE
 *               INPUT LINE, IF THE END OF LINE CONDITION IS
 *               MET THE NEXT LINE IS READ IN AND A BLANK IS
 *               RETURNES 
 *) 

  BEGIN 
(* NEXTCH 
 *) 
    IF NOT(EOF(INPUT))
    THEN
    WITH INPUTLINE DO 
    BEGIN 
      IF POSITIONOFINPUT > LENGTH 
      THEN BEGIN
             IF (WARNLIST<>NIL) OR ( ERRLIST<>NIL)
             THEN BEGIN 
                    WRITE(ERRFILE,LINENO:6,'   ');
                    PUTLINE(ERRFILE,INPUTLINE); 
                    WRITELN(ERRFILE); 
                  END;
             IF WARNLIST<>NIL 
             THEN WARNOUT;
            IF ERRLIST<>NIL 
            THEN ERROROUT;
           READLN(INPUT); 
           LENGTH := LINESIZE;
           LINENO:=LINENO+1;
           WRITE(LISTING,LINENO:6,'   '); 
           GETLINE(INPUT,INPUTLINE);
           PUTLINE(LISTING,INPUTLINE);
           WRITE(CODE,'*** ');
           PUTLINE(CODE,INPUTLINE); 
           WRITELN(CODE); 
           WRITELN(LISTING);
           IF LENGTH < LINESIZE 
           THEN STRING[SUCC(LENGTH)] := LINEFEED
           ELSE STRING[LENGTH]  := LINEFEED;
           POSITIONOFINPUT := 1;
           CH := SPACE; 
                  ERRLIST:=NIL; 
           END
      ELSE BEGIN
           CH:= STRING[POSITIONOFINPUT];
           POSITIONOFINPUT := SUCC(POSITIONOFINPUT);
           END
     END
    ELSE CH :=SEMICOL;
  END;
(* NEXTCH 
 *) 






PROCEDURE NEXTINLIST(VAR SHEAD,SNEXT : EXPTREE; 
                        SY1 : SYMMODE; OP1 : OPSYM);
BEGIN 
  IF SHEAD=NIL
  THEN BEGIN
         SHEAD:=NEWEXPNODE(OP1,SY1);
         SNEXT:=SHEAD;
       END
  ELSE BEGIN
         SNEXT^.LFT:=NEWEXPNODE(OP1,SY1); 
         SNEXT:=SNEXT^.LFT; 
       END; 
END; (* NEXTINLIST *) 



    FUNCTION ESCAPECHAR:INTEGER;
    VAR CH1,CH2 : INTEGER;
        CNT : INTEGER ; 
        N   : INTEGER;
    BEGIN 
     CASE CH
     OF 
        110 : BEGIN 
(* \N 
 *) 
                CH1:=10;     (* 27-SEPT-84*)
                NEXTCH; 
              END;
        116 : BEGIN 
(* \T 
 *) 
                CH1:=9; 
                NEXTCH; 
              END;
         98 : BEGIN 
(* \B 
 *) 
                CH1:=8; 
                NEXTCH; 
              END;
        114 : BEGIN 
(* \R 
 *) 
                CH1:=13;
                NEXTCH; 
              END;
        102 : BEGIN 
(* \F 
 *) 
                CH1:=12;
                NEXTCH; 
              END;
        108 : BEGIN (* \L   LINEFEED *) 
                CH1:= 10; 
                NEXTCH; 
              END;

        OTHERWISE BEGIN 
                    N := 1; 
                    IF CH IN [CH0..CH9] 
                    THEN BEGIN
                           CH1:=0;
                           WHILE(CH IN [CH0..CH9] ) AND (N < 4) 
                           DO BEGIN 
                                CH1:=CH1*8+CH-CH0;
                                N:= N+1;
                                NEXTCH; 
                              END;
                         END
                    ELSE BEGIN
                           CH1:=CH; 
                           NEXTCH;
                         END; 
                  END;
        END;
(* CASE 
 *) 
    ESCAPECHAR:=CH1;
  END;
(* ESCAPE CHAR
 *) 

(*
 * INSTRING.
 * PROCEDURE TO READ IN A STRING .
 * BUILDS A LIST POINTED TO BY THE GLOBAL VARIABLE  STRINGLIST. 
 * LENGTH OF THE STRING IS PUT IN THE GLOBAL VAR    STRINGLENG. 
 *) 
 PROCEDURE INSTRING;
 VAR S,S1 : EXPTREE;
     CH1 : CHARACTER; 
     CH2 : INTEGER; 
 BEGIN
   STRINGLENG:=1; 
   S:=NIL;S1:=NIL;
   STRINGLIST:=NIL; 
   WHILE ( CH <> DQUOTE) AND NOT(EOF(INPUT))
   DO BEGIN 
        IF CH=CHBS
        THEN BEGIN
               NEXTCH;
               CH2:=ESCAPECHAR; 
(* 27-SEPT-84  IF(CH2>256)
               THEN BEGIN 
                      NEXTINLIST(S,S1,KONSTANT,NOOP); 
                      S1^.EMODE:=ORD(KAR);
                      S1^.SVALUE:=CH2 DIV 256;
                      STRINGLENG:=SUCC(STRINGLENG); 
                    END;     *) 
               CH1:=CH2 MOD 256;
             END
        ELSE BEGIN
               CH1:=CH; 
               NEXTCH;
             END; 
        IF(STRINGLENG < 257)
        THEN BEGIN
               NEXTINLIST(S,S1,KONSTANT,NOOP);
               S1^.EMODE:=ORD(KAR); 
               S1^.SVALUE:=CH1; 
               STRINGLENG:=SUCC(STRINGLENG);
             END
        ELSE BEGIN
               WARN(4); 
               WHILE(CH <> DQUOTE) AND NOT(EOF(INPUT))
               DO BEGIN 
                    NEXTCH; 
                  END;
             END; 
      END;
   IF EOF(INPUT) THEN ERROR(67);
   NEXTINLIST(S,S1,KONSTANT,NOOP);
   S1^.EMODE:=ORD(KAR); 
   S1^.SVALUE:=0; 
   STRINGLIST:=S; 
 END; 






PROCEDURE FRACFLOAT;
VAR FPART,FTOT: REAL; 
    CH1             : CHAR ;
    ITOT            : INTEGER;
    IDTR            : IDTREE; 
BEGIN 
  FTOT:=0;
  FPART:=1; 
  WHILE CTABLE[CH] IN ['0'..'9']
  DO BEGIN
       FPART:=FPART/10.0; 
       FTOT :=FTOT+FPART*(ORD(CTABLE[CH])-ORD('0'));
       NEXTCH;
     END; 
  FVALU:=VALU+FTOT; 
  IF CTABLE[CH]='E' 
  THEN BEGIN
(* EXPONENT 
 *) 
         NEXTCH;
         FPART := 1;
         IF CH = MINUSCH
         THEN BEGIN 
                NEXTCH; 
                FPART:=-1;
              END 
         ELSE BEGIN 
                IF CH = PLUSCH
                THEN NEXTCH;
              END;
         ITOT:=0; 
         WHILE CTABLE[CH] IN ['0'..'9'] 
         DO BEGIN 
              ITOT:=ITOT*10+ORD(CTABLE[CH])-ORD('0'); 
              NEXTCH; 
            END;
         IF FPART=1 
         THEN BEGIN 
                WHILE ITOT<>0 
                DO BEGIN
                     FVALU:=FVALU*10; 
                     ITOT:=ITOT-1;
                   END
              END 
         ELSE BEGIN 
                WHILE ITOT<>0 
                DO BEGIN
                     FVALU:=FVALU/10.0; 
                     ITOT:=ITOT-1;
                   END; 
              END;
       END; 
  IDTYPE:=DOUBLE; 
  SY:=KONSTANT; 
  OP:=NOOP; 
END;
(* FRACFLOAT
 *) 




PROCEDURE PRINTLITERAL(LREC : EXPRECORD);EXTERN;
PROCEDURE PRINTSTATIC;EXTERN; 
FUNCTION FWIDTH(M,O:INTEGER):INTEGER; 
VAR TMP:INTEGER;
BEGIN 
  IF O<>0 THEN FOR TMP:= 1 TO O DO M:=M DIV 2;
  O := 1; 
  TMP:=1; 
  IF M<>0 
  THEN WHILE O<>M 
  DO BEGIN
       TMP:= TMP+1; 
       O := O*2+1;
     END; 
  FWIDTH:= TMP; 
END;


PROCEDURE PRINTFIELD(LREC : EXPRECORD;FBASE:INTEGER); 
VAR SREC : EXPRECORD; 
    FM,FO : INTEGER;
    I : INTEGER;
BEGIN 
  FM := TYPETAB[LREC.EMODE].FMASK;
  FO := TYPETAB[LREC.EMODE].FOFFSET;
  IF NOT(EOF(DATFILE))
  THEN BEGIN
         READ(DATFILE,STATRECORD);
         WITH STATRECORD
         DO BEGIN 
              IF TYPETAB[EMODE].BASICTYPE=FIELD 
              THEN BEGIN
                     IF TYPETAB[EMODE].FOFFSET>= FBASE
                     THEN PRINTFIELD(STATRECORD,FO+FWIDTH(FM,FO)) 
                     ELSE BEGIN 
                            IF FO+FWIDTH(FM,FO)<60 THEN WRITE(CODE,(60-FO-FWIDTH(FM,FO)):1,'/0,');
                          END;
                   END
              ELSE BEGIN
                     IF FO+FWIDTH(FM,FO) < 60 
                     THEN WRITE(CODE,(60-FO-FWIDTH(FM,FO)):1,'/0,');
                   END; 
            END;
       END; 
  WITH TYPETAB[LREC.EMODE]
  DO BEGIN
       WRITE(CODE,FWIDTH(FM,FO):1,'/'); 
       IF LREC.TOKN=IDENT 
       THEN BEGIN 
              WRITE(CODE,'=X'); 
              FOR I:=1 TO 10 DO IF (LREC.LID[I]<>' ') AND 
                                   (ORD(LREC.LID[I])<>0)
                                THEN WRITE(CODE,LREC.LID[I]); 
            END 
       ELSE CASE BASICTYPE
            OF SHORT,KAR,INT,LONG,UNSIGNED: WRITE(CODE,LREC.SVALUE:1);
               REEL,DOUBLE: WRITE(CODE,LREC.RVALUE);
               ARRAYTYPE: WRITE(CODE,LREC.SVALUE);
               FIELD : WRITE(CODE,LREC.SVALUE:1); 
               OTHERWISE WRITE(CODE,'X'); 
            END;
       IF FO > FBASE
       THEN WRITE(CODE,',',(FO-FBASE):1,'/0');
       IF FBASE=0 
       THEN BEGIN 
              WRITELN(CODE);
              IF NOT(EOF(DATFILE))
              THEN IF TYPETAB[STATRECORD.EMODE].BASICTYPE=FIELD 
                   THEN BEGIN 
                          WRITE(CODE,'          VFD    ');
                          PRINTFIELD(STATRECORD,0); 
                        END 
                   ELSE PRINTLITERAL(STATRECORD); 
(* THERE IS A DUMMY RECORD ALWAYS WRITTEN ON THE FILE. *) 
            END 
       ELSE WRITE(CODE,',');
   (* WATCH THIS *) 
     END; 
END;


PROCEDURE PRINTDATA;
BEGIN 
  STATRECORD.TOKN:=KONSTANT;
  STATRECORD.EMODE:=ORD(INT); 
  STATRECORD.SVALUE:=0; 
  WRITE(DATFILE,STATRECORD);
  RESET(DATFILE); 
  WHILE NOT(EOF(DATFILE)) 
  DO BEGIN
       READ(DATFILE,STATRECORD);
       IF NOT(EOF(DATFILE)) 
       THEN IF TYPETAB[STATRECORD.EMODE].BASICTYPE<>FIELD 
            THEN PRINTLITERAL(STATRECORD) 
            ELSE BEGIN
                   WRITE(CODE,'          VFD    '); 
                   PRINTFIELD(STATRECORD,0);
                 END; 
     END; 
  RESET(DATFILE);REWRITE(DATFILE);
END;
(* PRINTDATA *) 






   PROCEDURE INSYMBOL;



(*
 *            PROCEDURE TO RECOGNISE AND RETURN A TOKEN 
 *) 


       LABEL 10;
     VAR
            CHAR1 : CHAR; 
            AID   : ALPHA;
            I     : INTEGER;
            LCNT  : INTEGER;

FUNCTION ALLOWER(AID : ALPHA): BOOLEAN; 
VAR I : INTEGER;
BEGIN 
  ALLOWER := TRUE;
  LCNT := 0;
  FOR I := 0 TO 13
  DO IF (AID[I] >= 65 ) AND (AID[I] <= 90)
  THEN BEGIN
         ALLOWER := FALSE;
         LCNT := LCNT+1;
       END; 
END;


    BEGIN 
(* INSYMBOL 
 *) 
      10 :  
(* RESTART POINT IF A COMMENT IS FOUND
 *) 
      WHILE CH = SPACE DO NEXTCH; 
      CHAR1 := CTABLE[CH];
      IF CHAR1 IN ['A'..'Z']
      THEN BEGIN
             FOR I := 0 TO 13 
             DO AID[I] := SPACE;
           ID := '          ';
           IDCOUNT := 1;
           ID[1] := CHAR1;
           AID[0] := CH;
           NEXTCH;
           CHAR1 := CTABLE[CH]; 
           WHILE CHAR1 IN [ 'A'..'Z','0'..'9']
           DO BEGIN 
              IDCOUNT := SUCC(IDCOUNT); 
              IF IDCOUNT <= 10
              THEN BEGIN ID[IDCOUNT] := CHAR1;
                     AID[IDCOUNT-1] := CH;
                   END; 
              NEXTCH; 
              CHAR1 := CTABLE[CH] 
              END;
           IF (ALLOWER(AID)) AND (ISRESERVED(ID,SY,OP,IDTYPE))
           THEN BEGIN 
                END 
           ELSE BEGIN 
                SY :=IDENT; 
                OP := NOOP; 
                IDTYPE := NULLTYPE; 
(* CHANGE IDENTIFIER WITH SOME UPPER CASE TO THE FORM ZN... *)
                IF LCNT > 0    (* LCNT = NUMBER OF UPPER CASE CHARS *)
                THEN BEGIN
                       FOR I := 1 TO 8
                       DO BEGIN 
                            ID[11-I] := ID[9-I]; (* SHIFT ALONG *)
                          END;
                       ID[1] := 'Z';
                       ID[2] := CTABLE[64+LCNT];
                     END; 
                END 
           END
       ELSE IF CHAR1 IN ['0'..'9']
       THEN BEGIN 
            IF CHAR1 = '0'
            THEN BEGIN
(* OCTAL CONSTANT 
 *) 
                 VALU := 0; 
                 NEXTCH;
                 CHAR1 := CTABLE[CH]; 
                 IF CHAR1 = 'X' 
                 THEN BEGIN 
                        NEXTCH; 
                        CHAR1:=CTABLE[CH];
                          WHILE CHAR1 IN ['A'..'F','0'..'9']
                          DO BEGIN
                               IF CHAR1 IN ['A'..'F'] 
                               THEN VALU:=VALU*16+ORD(CHAR1)-ORD('A')+10
                               ELSE VALU:=VALU*16+ORD(CHAR1)-ORD('0');
                               NEXTCH;
                               CHAR1:=CTABLE[CH]
                             END ;
                   END ;
                 WHILE CHAR1 IN ['0'..'9']
               DO BEGIN 
                         VALU := VALU * 8 + ORD(CHAR1) -ORD('0'); 
                         NEXTCH;
                         CHAR1 := CTABLE[CH]
                         END
                 END
             ELSE BEGIN 
(* DECIMAL
 *) 
                  VALU := ORD(CHAR1)-ORD('0');
                  NEXTCH; 
                  CHAR1 := CTABLE[CH];
                  WHILE CHAR1 IN ['0'..'9'] 
                  DO BEGIN
                     VALU := VALU *10 + ORD(CHAR1) -ORD('0'); 
                     NEXTCH;
                     CHAR1 := CTABLE[CH]; 
                     END
                  END;
              SY := KONSTANT; 
              IDTYPE := INT;
              OP := NOOP; 
              IF (VALU>262000)OR(VALU< -262000) 
              THEN IDTYPE:=LONG;
              IF (CH = DOTCH) OR (CTABLE[CH] = 'E') 
              THEN BEGIN
                     IF CH = DOTCH THEN NEXTCH; 
                     FRACFLOAT; 
                   END
              ELSE IF CTABLE[CH]='L'
              THEN BEGIN
                     NEXTCH;
                     IDTYPE:=LONG;
                   END; 
             END


          ELSE IF(EOF(INPUT)) 
          THEN BEGIN
                 SY:=EOSSYM;
                 OP:=NOOP;
               END
          ELSE CASE CH OF 
              SEMICOL : 
                        BEGIN 
                         SY := SEMI;
                         OP := NOOP;
                         NEXTCH 
                       END; 

               MINUSCH : BEGIN
                         NEXTCH;
                         IF CH = MINUSCH
                         THEN BEGIN 
                              SY:= TERMOP;
                              OP:= DECR;
                              NEXTCH
                              END 
                         ELSE IF CH = CHGT
(* -> 
 *) 
                         THEN BEGIN 
                              SY := TERMOP; 
                              OP := REFSELECTOR;
                              NEXTCH
                              END 
                         ELSE BEGIN 
                              SY := ADDOP;
                              OP := MINUS 
                              END;
                         IDTYPE := NULLTYPE 
                         END; 
(* MINUA
 *) 

                PLUSCH : BEGIN
                         NEXTCH;
                         IF CH = PLUSCH 
                         THEN BEGIN 
(* ++ 
 *) 
                              SY := TERMOP; 
                              OP := INCR; 
                              NEXTCH
                              END 
                         ELSE BEGIN 
                              SY := ADDOP;
                              OP := PLUS
                              END;
                         IDTYPE := NULLTYPE 
                         END; 
(* PLUSCH 
 *) 

                 SLASH : BEGIN
                         NEXTCH;
                         IF CH = ASTERIX
(* /* COMMENT */
 *) 
                         THEN BEGIN 
                              NEXTCH; 
                   (* LOOK FOR OPTIONS *) 
    IF CH=DOLLAR
    THEN BEGIN
           NEXTCH;
           REPEAT 
           BEGIN
             IF CH = CHO
             THEN BEGIN 
                    NEXTCH; 
                    WHILE CH=SPACE DO NEXTCH; 
                    IF CH <> ASTERIX
                    THEN BEGIN
                           INSYMBOL;
                           ORIGIN:=VALU;
                           ORIGINF:=TRUE; 
                         END; 
                  END 
             ELSE IF CH = CHS 
             THEN BEGIN 
                    SIMPLEFORM:=TRUE; 
                    CODESEG:='CODE      ';
                    DATASEG:='DATA      ';
                    NEXTCH; 
                    WHILE CH=SPACE DO NEXTCH; 
                    IF CH<>ASTERIX
                    THEN BEGIN
                           INSYMBOL;
                           CODESEG:=ID; 
                           IF CH = COLON
                           THEN BEGIN 
                                  NEXTCH; 
                                  INSYMBOL; 
                                  ORIGIN:=VALU; 
                                  ORIGINF:=TRUE;
                                END;
                         END; 
                    IF CH=COMMACH 
                    THEN BEGIN
                           NEXTCH;
                           IF CH <> ASTERIX 
                           THEN BEGIN 
                                  INSYMBOL; 
                                  DATASEG:=ID;
                                END;
                           IF CH = COLON
                           THEN BEGIN 
                                  NEXTCH; 
                                  INSYMBOL; 
                                  DORIGIN:=VALU;
                                  DORIGINF:=TRUE; 
                                END;
                          END;
                   END
          ELSE IF CH = CHD
          THEN BEGIN
                 NEXTCH;
                 DEBUGF:= CH = PLUSCH;
                 NEXTCH;
               END
(*       ELSE BEGIN 
                IF (CH>= CHA) AND (CH <= CHZ) 
                THEN BEGIN
                       LCNT := CH-CHA;
                       NEXTCH;
                       WHILE (CH = SPACE) DO NEXTCH;
                       IF ( CH >= CH0) AND (CH <= CH9)
                       THEN BEGIN 
                              INSYMBOL; 
                              OPTARR[LCNT].ORECTYP := TRUE; 
                              OPTARR[LCNT].OIVAL := VALU; 
                            END 
                       ELSE IF (CH >= CHA) AND ( CH <= CHZ) 
                       THEN BEGIN 
                              INSYMBOL; 
                              OPTARR[LCNT].ORECTYP := FALSE;
                              OPTARR[LCNT].OSVAL := ID; 
                            END;
                     END; 
              END;*)
            END; UNTIL ( CH=SPACE) OR ( CH = ASTERIX);
        END;
                              WHILE TRUE
                              DO BEGIN
                                 IF CH = ASTERIX
                                 THEN BEGIN 
                                      NEXTCH; 
                                      IF CH = SLASH 
                                      THEN BEGIN
                                             NEXTCH;
                                             GOTO 10 ;
                                           END ;
                                      END 
                                 ELSE NEXTCH
                                 END
                              END 
                       ELSE BEGIN 
                            SY := MULTOP; 
                            OP := DIVIDE; 
                            IDTYPE := NULLTYPE
                            END 
                       END; 
(* SLASH
 *) 

               EXCLAM : BEGIN 
                        NEXTCH; 
                        IF CH = CHEQ
(* != 
 *) 
                        THEN BEGIN
                             SY := EQUALITY;
                             OP := NEOP;
                             NEXTCH 
                             END
                        ELSE BEGIN
                             SY := TERMOP;
                             OP := NOTOP; 
                             END
                        END;
(* !
 *) 

               CHEQ : BEGIN 
                      NEXTCH; 
                      SY:=ASSIGN; 
                      OP:=NOOP; 
                      CASE CH 
                      OF
                        CHEQ : BEGIN
                                 SY:=EQUALITY;
                                 OP:=EQOP;
                               END; 

                        PLUSCH:BEGIN
                                 OP:=PLUS;
                               END; 

                        MINUSCH: OP:=MINUS; 

                        CHBAR : OP:=OROP; 

                        CHCAP : OP:=XOROP;
                        ASTERIX : OP:=TIMES;

                        SLASH : OP:=DIVIDE; 

                        AMPERSAND:OP:=ANDOP;

                        CHLT : BEGIN
                                 NEXTCH;
                                 IF CH=CHLT 
                                 THEN OP:=LEFTSHIFT;
                               END; 

                        CHGT : BEGIN
                                 NEXTCH;
                                 IF CH=CHGT 
                                 THEN OP:=RIGHTSHIFT; 
                               END; 

                      OTHERWISE 
                      END;
                      IF NOT(OP IN [NOOP,EQOP]) THEN WARN(2); 
(* CASE 
 *) 
                      IF OP<>NOOP 
                      THEN NEXTCH;
                      IDTYPE := NULLTYPE; 
                      END;
(* =
 *) 

                 CHBAR : BEGIN
                         NEXTCH;
                         IF CH =CHBAR 
(*
 *) 
                         THEN BEGIN 
                              SY := ORFSYM; 
                              OP := ORFOP;
                              NEXTCH
                              END 
                         ELSE BEGIN 
                              SY := ORSYM;
                              OP := OROP
                              END 
                         END; 

                 CHLT  : BEGIN
                         NEXTCH;
                         IF CH = CHEQ 
(* <= 
 *) 
                         THEN BEGIN 
                              SY := COMP; 
                              OP := LEOP; 
                              NEXTCH
                              END 
                         ELSE IF CH = CHLT
(* << 
 *) 
                         THEN BEGIN 
                              SY := SHIFTOP;
                              OP := LEFTSHIFT;
                              NEXTCH
                              END 
                         ELSE BEGIN 
                              SY := COMP; 
                              OP := LTOP
                              END 
                         END; 
(* <
 *) 

                  CHGT : BEGIN
                         NEXTCH;
                         IF CH = CHEQ 
(* >= 
 *) 
                         THEN BEGIN 
                              SY := COMP; 
                              OP := GEOP; 
                              NEXTCH
                              END 
                         ELSE IF CH = CHGT
(* >> 
 *) 
                       THEN BEGIN 
                            SY := SHIFTOP;
                            OP := RIGHTSHIFT; 
                            NEXTCH
                            END 
                       ELSE BEGIN 
                            SY := COMP; 
                            OP := GTOP
                            END 
                       END; 
(* >
 *) 





                 AMPERSAND :  
                       BEGIN
                       NEXTCH;

                       IF CH = AMPERSAND
(* && 
 *) 
                       THEN BEGIN 
                            SY := ANDFSYM;
                            OP := ANDFOP; 
                            NEXTCH
                            END 
                       ELSE BEGIN 
                            SY := ANDSYM; 
                            OP := ANDOP 
                            END 
                       END; 
(* &
 *) 





          LPARENTH    : 

                       BEGIN
                         SY := LP;
                         OP := NOOP;
                         NEXTCH 
                       END; 



           RPARENTH    :  

                       BEGIN
                         SY := RP;
                         OP := NOOP;
                         NEXTCH 
                       END; 



           CHLB    :  

                       BEGIN
                         SY := LB;
                         OP := NOOP;
                         NEXTCH 
                       END; 



            CHRB    : 

                       BEGIN
                         SY := RB;
                         OP := NOOP;
                         NEXTCH 
                       END; 



          COMMACH    :  

                       BEGIN
                         SY := COMMA; 
                         OP := NOOP;
                         NEXTCH 
                       END; 



           QUESTION    :  

                       BEGIN
                         SY := IFOPSYM; 
                         OP := IFOP;
                         NEXTCH 
                       END; 



          COLON    :  

                       BEGIN
                         SY := IFOPSYM; 
                         OP := ELSEOP;
                         NEXTCH 
                       END; 



          CHCAP    : BEGIN
                       SY:=XORSYM;
                       OP:=XOROP; 
                       NEXTCH;
                     END ;


          CHTIL    :  

                       BEGIN
                         SY := COMPLEM; 
                         OP := COMPOP;
                         NEXTCH 
                       END; 



          PERCENT    :  

                       BEGIN
                         SY := MULTOP;
                         OP := MODOP; 
                         NEXTCH 
                       END; 



          ASTERIX    :  

                       BEGIN
                         SY := MULTOP;
                         OP := TIMES; 
                         NEXTCH 
                       END; 



          CHLBR    : BEGIN
                       SY := BEGINSYM;
                       OP := NOOP;
                       NEXTCH 
                     END; 


          CHRBR    : BEGIN
                       SY := ENDSYM;
                       OP := NOOP;
                       NEXTCH 
                     END; 


          DQUOTE : BEGIN
                     NEXTCH ; 
                     INSTRING;
                     NEXTCH;
                     SY:=STRING;
                     OP:=NOOP;
                   END; 


          GRAVE  : BEGIN
                     NEXTCH;
                      IF CH = CHBS
                      THEN BEGIN
                             NEXTCH;
                             VALU:=ESCAPECHAR MOD 256;
                           END
                      ELSE BEGIN
                             VALU:=CH;
                             NEXTCH;
                           END; 
                      IF CH = GRAVE 
                      THEN NEXTCH 
                      ELSE ERROR(0);
(* ERROR 0
 *) 
                     SY:=KONSTANT;
                     OP:=NOOP;
                     IDTYPE:=KAR; 
                   END ;


          DOTCH    :  

                       BEGIN
                         VALU:=0; 
                         SY := TERMOP;
                         OP := SELECTOR;
                         NEXTCH 
                      ;IF CTABLE[CH] IN ['0'..'9']
                       THEN FRACFLOAT ; 
                       END; 

          OTHERWISE BEGIN 
                      WARN(3);
                      NEXTCH; GOTO 10;
                    END;


        END 
   END; 
(* INSYMBOL 
 *) 

(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *     SYMBOL TABLE ROUTINES. 
 *    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 *) 
PROCEDURE DISTREE(T:IDTREE);
VAR T1 : IDTREE;
BEGIN 
  WHILE T<>NIL
  DO BEGIN
         DISTREE(T^.R); 
         T1:=T^.L;
         DISPOSE(T);
         T:=T1; 
       END; 
END;



PROCEDURE DISSYMTAB(T : SYMTABPTR); 
VAR T1 : SYMTABPTR; 
BEGIN 
  WHILE T<>NIL
  DO BEGIN
         DISTREE(T^.TREE);
         T1:=T^.NEXTLEVEL;
         DISPOSE(T);
         T:=T1; 
       END; 
END;

PROCEDURE DISCARD;
VAR T1 : EXPTREE; 
BEGIN 
  WHILE T<>NIL
  DO BEGIN
       IF T^.TOKN=STRING
       THEN DISCARD(T^.STRNG);
         DISCARD(T^.RGHT);
         T1:=T^.LFT;
         DISPOSE(T);
         T:=T1; 
       END; 
END;
(* DISCARD
 *) 





  PROCEDURE NEWSYMTAB(VAR CURRENT : SYMTABPTR); 
   VAR T : SYMTABPTR; 
  BEGIN 
    NEW(T); 
    WITH T^ 
    DO BEGIN
         AUTOSIZE:=0; 
         TREE    :=NIL; 
         NEXTLEVEL:=NIL;
       END; 
    DISSYMTAB(CURRENT^.NEXTLEVEL);
           T^.LASTLEVEL:=CURRENT; 
           CURRENT^.NEXTLEVEL:=T; 
      CURRENT:=T; 
      CURRENT^.SWITCHLEV:=CURRENT^.AUTOSIZE;
      CURRENT^.AUTOSIZE:=CURRENT^.LASTLEVEL^.AUTOSIZE+BASICSIZE(ORD(INT));
  END;
(* NEWSYMTAB(CURRENT) 
 *) 

  FUNCTION SIZEOFTYPE(INN:INTEGER) : INTEGER; 

    BEGIN 
      IF ( INN <= 0) OR (INN > TYPETABSIZE) 
      THEN BEGIN
           SIZEOFTYPE := 0; 
             WRITELN(CODE,'*SIZEOFTYPE: INN=',INN:1); 
           END
      ELSE IF TYPETAB[INN].BASICTYPE = FIELD
      THEN SIZEOFTYPE := BASICSIZE(ORD(INT))
      ELSE IF TYPETAB[INN].BASICTYPE = POINTER
      THEN SIZEOFTYPE := BASICSIZE(ORD(POINTER))
      ELSE SIZEOFTYPE := TYPETAB[INN].SIZE
    END;
(* SIZEOFTYPE 
 *) 





(*
 * ASCII SYMBOLTABLE ROUTINES.


 FUNCTION IDTEST(ID1,ID2 : ALPHA):INTEGER;
 LABEL 1; 
 VAR I : INTEGER; 
 BEGIN
   IDTEST:=0; 
   FOR I:= 0 TO 13
   DO IF ID1[I] < ID2[I]
      THEN BEGIN
             IDTEST:= -1; 
             GOTO 1;
           END
      ELSE IF ID1[I] > ID2[I] 
      THEN BEGIN
             IDTEST:= 1;
             GOTO 1;
           END
      ELSE IF ID1[I] = 0
      THEN GOTO 1;
   1 :  
 END; 



 FUNCTION FIND(T : IDTREE ; ID : ALPHA) : IDTREE; 
 LABEL 1; 
 VAR TEMP : INTEGER;
 BEGIN
   WHILE T<>NIL 
   DO BEGIN 
        TEMP:=IDTEST(T^.IR.NAME1,ID); 
        IF TEMP=0 THEN GOTO 1;
        IF TEMP = -1 THEN T:= T^.L
                     ELSE T:= T^.R; 
      END;
  1 : FIND :=T; 
 END; 



 FUNCTION ISDEF(SYMT : SYMBOLTABLE ; ID : ALPHA ; VAR T : IDTREE):BOOLEAN;
 BEGIN
   T:=FIND(SYMT.TREE,ID); 
   IF (T=NIL) AND (SYMT.LASTLEVEL <> NIL) 
   THEN ISDEF:= ISDEF(SYMT.LASTLEVEL^,ID,T) 
   ELSE ISDEF:=TRUE;
 END; 



 * END OF MAIN ASCII SYMBOL TABLE ROUTINES
 *) 



FUNCTION FINDID(T : IDTREE; ID : ALFA) : IDTREE;


      LABEL 1;
   BEGIN
(* FINDID 
 *) 
     WHILE T <> NIL 
     DO BEGIN 
        IF T^.IR.NAME = ID
        THEN GOTO 1 
        ELSE IF T^.IR.NAME < ID 
        THEN T := T^.L
        ELSE T := T^.R
        END 
;  1: FINDID := T 
   END; 
(* FINDID 
 *) 


FUNCTION ISDEFINED(G : SYMBOLTABLE; ID : ALFA;
                   VAR POSITION : INTEGER;
                   VAR STATE    : IDSTATE;
                   VAR MODE     : INTEGER;
                   VAR T        : IDTREE): BOOLEAN; 

VAR T1 : IDTREE;
       BEGIN
(* ISDEFINED
 *) 
         POSITION := 0; 
         STATE    := UNDEFINED; 
         MODE     := 0; 
         ISDEFINED:= FALSE; 


         T1 := FINDID(G.TREE,ID); 

         IF T1 <> NIL 
(* ID FOUND 
 *) 
         THEN WITH T1^.IR 
              DO BEGIN
                 POSITION  := OFFSET; 
                 MODE      := THISMODE; 
                 STATE     := THISSTATE;
                 ISDEFINED := TRUE
                 END
              ELSE
                IF G.LASTLEVEL<>NIL 
                THEN
                  ISDEFINED:=ISDEFINED(G.LASTLEVEL^,ID,POSITION,STATE,MODE,T1); 
       T:=T1; 
        END;
(* ISDEFINED
 *) 
    PROCEDURE PUTINTREE(VAR G1:IDTREE;VAR T:IDTREE);

(* PUT T IN THE BINARY TREE G.
 *) 
    VAR LOOK : BOOLEAN; 
        G    : IDTREE;
    ID : ALFA;
    BEGIN 
     G:=G1; 
     ID:=T^.IR.NAME;
      LOOK:=TRUE; 
      IF G1=NIL 
      THEN G1:=T
      ELSE
        WHILE LOOK
        DO IF ID>G^.IR.NAME 
           THEN 
            IF G^.L=NIL 
            THEN BEGIN
                   G^.L:=T; 
                   LOOK:=FALSE; 
                 END
            ELSE G:=G^.L
          ELSE
            IF(ID = G^.IR.NAME) AND (G^.IR.THISSTATE = T^.IR.THISSTATE) 
            THEN BEGIN
                   IF (SIZEOFTYPE(T^.IR.THISMODE) <= SIZEOFTYPE(G^.IR.THISMODE))
                      AND NOT(TYPETAB[T^.IR.THISMODE].BASICTYPE = FUNK) 
                   THEN WARN(10)       (* RE-DECLARATION IGNORED *) 
                 ELSE BEGIN 
                        WARN(9);
                        G^.IR.THISSTATE := T^.IR.THISSTATE; 
                        G^.IR.THISMODE  := T^.IR.THISMODE;
                        CASE T^.IR.THISSTATE
                        OF STATICVAR : BEGIN
                                         G^.IR.OFFSET := T^.IR.OFFSET;
                                       END; 
                           PARAMVAR:   BEGIN
                                         G^.IR.OFFSET := T^.IR.OFFSET;
                                       END; 
                           AUTOVAR:    BEGIN
                                         G^.IR.OFFSET := T^.IR.OFFSET;
                                       END; 
                           OTHERWISE     G^.IR.OFFSET := T^.IR.OFFSET;
                        END;
                      END;
                   DISPOSE(T);
                   T := G;
                   LOOK := FALSE; 
                 END
            ELSE
            IF G^.R=NIL 
            THEN BEGIN
                   G^.R:=T; 
                   LOOK:=FALSE; 
                 END
            ELSE G:=G^.R; 
       END; 
(* PUTINTREE
 *) 


   FUNCTION PUTINSYMBOLTABLE; 
  VAR STATE1 : IDSTATE; 
      POS : INTEGER;
      MODE1 : INTEGER;
      T1,T : IDTREE;
      P : EXPTREE;
    FUNCTION FILL : IDTREE; 
        VAR T  : IDTREE;
        BEGIN 
(* FILL 
 *) 
          NEW(T); 
          T^.L:=NIL;
          T^.R:=NIL;
          T^.IR.NAME:=ID; 
          CASE ISTATE 
          OF
            STATICVAR  : BEGIN     (* MODS ON 4-JUNE-84 *)
                           P := NEWEXPNODE(NOOP,KONSTANT);
                           P^.SVALUE := SIZEOFTYPE(IMODE);
                           P^.EMODE  := IMODE;
                           T^.IR.OFFSET := OUTSTATIC(P);
                           DISPOSE(P);
                         END; 

            PARAMVAR   : BEGIN
                          T^.IR.OFFSET:=G^.AUTOSIZE;
                          G^.AUTOSIZE:=G^.AUTOSIZE + SIZEOFTYPE(IMODE); 
                        END;
            AUTOVAR : BEGIN 
                        T^.IR.OFFSET := G^.AUTOSIZE;
                        G^.AUTOSIZE:=G^.AUTOSIZE + SIZEOFTYPE(IMODE); 
                      END;
            OTHERWISE   T^.IR.OFFSET:=0;
          END ; 
(* CASE 
 *) 
          T^.IR.THISSTATE:=ISTATE;
          T^.IR.THISMODE:=IMODE;
           PUTINTREE(G^.TREE,T);
        FILL:=T;
       END ;
(* FILL 
 *) 

  BEGIN 
(* PUTINSYMBOLTABLE 
 *) 
    IF G=NIL
    THEN BEGIN
           NEW(G);
           WITH G^
           DO BEGIN 
                AUTOSIZE:=0;
                TREE:=NIL;
                LASTLEVEL:=NIL; 
                NEXTLEVEL:=NIL; 
              END;
         END; 
    T1:=FILL; 
     PUTINSYMBOLTABLE:=T1;
    END;
(* PUTINSYMBOL
 *) 





PROCEDURE PREVUNKNOWN;
VAR T1 : IDTREE;
    POS : INTEGER;
    STATE : IDSTATE;
    MODE : INTEGER; 
BEGIN 
  IF UNKNOWN <> NIL 
  THEN (* IF ISDEFINED(STRUCTDICT^,UNKNOWN^.IR.NAME,POS,STATE,MODE,T1)
  THEN WITH UNKNOWN^.IR 
  DO   BEGIN
         IF THISSTATE = UNDEFINED 
         THEN BEGIN 
                THISSTATE := EXTERNVAR; 
                OFFSET    := 0; 
                THISMODE  := MODE;
              END;
         UNKNOWN := NIL;
       END
  ELSE *) WITH UNKNOWN^.IR
       DO BEGIN 
            IF THISSTATE=UNDEFINED
            THEN BEGIN
                   THISSTATE:=EXTERNVAR;
                   OFFSET:=0; 
                   THISMODE:=ORD(INT);
                   WARN(1); 
                 END; 
             PUTINTREE(GLOBALSYMBOL^.TREE,UNKNOWN); 
            UNKNOWN:=NIL; 
          END;
END;
(* PREVUNKNOWN
 *) 

 PROCEDURE UNKNOWNID; 
VAR POS : INTEGER;
    STATE : IDSTATE;
    MODE  : INTEGER;
BEGIN 
  PREVUNKNOWN;
  IF NOT ( ISDEFINED(CURRENT^,ID,POS,STATE,MODE,LASTID))
  THEN BEGIN
         NEW(UNKNOWN);
         WITH UNKNOWN^
         DO BEGIN 
              L:=NIL; 
              R:=NIL; 
              WITH IR 
              DO BEGIN
                   NAME:=ID;
                   THISSTATE:=UNDEFINED;
                   THISMODE:=ORD(NULLTYPE); 
                   OFFSET:=0; 
                 END; 
           END; 
         LASTID:=UNKNOWN; 
       END; 
END;
(* UNKNOWNID
 *) 



 PROCEDURE NEXTSYMBOL(S:SYMMODE; E : ERRORCODES); 
   BEGIN
     IF SY = S
     THEN INSYMBOL
     ELSE ERROR(E)
   END ;
(* NEXTSYMBOL 
 *) 


  PROCEDURE SKIP(S : SYMMODESET); 
   BEGIN
(* SKIP 
 *) 
     WHILE NOT(SY IN S ) AND NOT(EOF(INPUT))
     DO INSYMBOL ;
   END; 
(* SKIP 
 *) 

 FUNCTION CHECK;
   BEGIN
(* CHECK
 *) 
     IF SY = SYM
     THEN CHECK := TRUE 
     ELSE BEGIN 
          CHECK := FALSE; 
          ERROR(ERRORNO); 
          SKIP(SYSET) 
          END 
    END;
(* CHECK
 *) 





  FUNCTION ABSNOTYPE(VAR TYPN:INTEGER):BOOLEAN; 
  VAR T : IDTREE; 
      POS:INTEGER;
      STATE:IDSTATE;
      MODE : INTEGER; 
  BEGIN 
    ABSNOTYPE := FALSE; 
    IF SY = TYPESYM 
    THEN BEGIN
           IF IDTYPE IN [STRUCT,UNION]
           THEN TYPN := STRUCTSPEC
           ELSE TYPN := ORD(IDTYPE);
           ABSNOTYPE := TRUE; 
         END
    ELSE IF SY = IDENT
    THEN BEGIN
           IF ISDEFINED(CURRENT^,ID,POS,STATE,MODE,T) 
           THEN BEGIN 
                END 
           ELSE IF ISDEFINED(STRUCTDICT^,ID,POS,STATE,MODE,T) 
           THEN IF (STATE = TYPEVAR) OR (STATE = STRUCTVAR) 
                THEN BEGIN
                       TYPN := MODE;
                       ABSNOTYPE := TRUE; 
                       INSYMBOL;
                     END; 
         END; 
  END;


  FUNCTION ABSDECLAR:BOOLEAN; 
  VAR TYPEHEAD : TYPELIST;
  BEGIN 
    ABSDECLAR:=FALSE; 
    TYPENUMBER:=ORD(NULLTYPE);
    IF ABSNOTYPE(TYPENUMBER)
    THEN BEGIN
           WHILE NAMEOFTYPE(TYPENUMBER)DO ; 
           ABSDECLAR:=TRUE; 
           NEW(TYPEHEAD); 
           TYPEHEAD^.MDE:=NULLTYPE; 
           TYPEHEAD^.NXT:=NIL;
           TYPEHEAD^.SZE:=0;
           TYPD(TYPEHEAD,TRUE,TRUE);
           TYPENUMBER:=EMPTYTYPELIST(TYPEHEAD); 
         END; 
  END; (* ABSDECLAR *)


   FUNCTION EVALEXPRESSIONTREE:INTEGER; 
  VAR TY : INTEGER; 
  BEGIN 
    TY:=TYPETREE(BRACKETS[0]);
     IF BRACKETS[0]=NIL 
     THEN EVALEXPRESSIONTREE:=0 
     ELSE IF BRACKETS[0]^.TOKN=KONSTANT 
     THEN EVALEXPRESSIONTREE:=BRACKETS[0]^.SVALUE 
     ELSE BEGIN 
            EVALEXPRESSIONTREE := 0;
            ERROR(84);
          END;
     IF DEBUGF THEN PRINTTREE(BRACKETS[0],0); 
     DISCARD(BRACKETS[0]);
     BRACKETS[0]:=NIL;
     BRACKETS[1]:=NIL;
     LEVEL:=0;
     LASTNODE[0]:=NIL;
     LASTNODE[1]:=NIL;
    END;
(* EVAL EXPRESSIONTREE
 *) 



 PROCEDURE KONSTEXP;
 BEGIN
          EXP;
          VAL:=EVALEXPRESSIONTREE;
 END ;

  FUNCTION CRTSTRUCT(INN : TYPEMODE) : INTEGER ;
  BEGIN 
    IF TYPEREF<TYPETABSIZE
    THEN BEGIN
           TYPEREF:=SUCC(TYPEREF);
           WITH TYPETAB[TYPEREF]
           DO BEGIN 
                BASICTYPE:=INN; 
                SIZE:=0;
              END;
           CRTSTRUCT:=TYPEREF;
         END
    ELSE WRITELN('TYPE TABLE OVERFLOW');
  END ; 
(* CRTARRAY 
 *) 

  FUNCTION CRTFIELD(MASK,OFFSET:INTEGER):INTEGER; 
  BEGIN 
    IF TYPEREF<TYPETABSIZE
    THEN BEGIN
           TYPEREF:=SUCC(TYPEREF);
           WITH TYPETAB[TYPEREF]
           DO BEGIN 
                FMASK:=MASK;
                BASICTYPE:=FIELD; 
                SIZE:=0;
                FOFFSET:=OFFSET;
              END;
          END 
    ELSE WRITELN('TYPETABLE OVERFLOW ');
    CRTFIELD:=TYPEREF;
  END;
(* CRTFIELD 
 *) 


FUNCTION GREATEROF(A,B : INTEGER) : INTEGER;
BEGIN 
  IF A<B
  THEN GREATEROF:=B 
  ELSE GREATEROF :=A; 
END;
(* GREATEROF
 *) 




FUNCTION SDECLTOR(STYP : IDSTATE; TYP : INTEGER): IDTREE ;
VAR STYPNO : INTEGER; 
    SSC  : IDSTATE; 
BEGIN 
  STYPNO:=TYPENUMBER; 
  SSC:=SC;
  TYPENUMBER:=TYP;
  SC:=STYP; 
  DECLTOR(TRUE,STRUCTDICT); 
  SDECLTOR:=LASTID; 
  TYPENUMBER:=STYPNO; 
  SC:=SSC;
END;



PROCEDURE STRCDECL(STYP : TYPEMODE ; TYP : INTEGER; 
                   VAR FOFFSET : INTEGER; 
                   VAR SIZ : INTEGER ; VAR SHEAD,SNEXT : EXPTREE);
VAR T1 : IDTREE;
    MASK , OFFSET,CNT : INTEGER;
    SIZE : INTEGER; 

  FUNCTION GENMASK(OFFSET,WIDTH : INTEGER) : INTEGER; 
  VAR I,J : INTEGER;
  BEGIN 
    I:=0; 
    IF WIDTH<>0 
    THEN BEGIN
           FOR J:=1 TO WIDTH
           DO I:=I*2+1; 
         END; 
    IF OFFSET<>0
    THEN FOR J:=1 TO OFFSET 
    DO I :=I*2; 
    GENMASK:=I; 
  END;

BEGIN 
  T1:=NIL;
  IF OP<>ELSEOP 
  THEN BEGIN
         T1 :=SDECLTOR(FIELDVAR,TYP); 
       END; 
  IF OP = ELSEOP
  THEN BEGIN
         INSYMBOL ; 
         KONSTEXP;

(* FIELD ALIGNMENT
 *) 
         IF STYP = STRUCT 
         THEN BEGIN 

(*
 *                 * HANDLE FIELDS
 *) 
                IF VAL=0
                THEN BEGIN
                       IF FOFFSET<>0
(* FORCE ALIGNMENT
 *) 
                       THEN BEGIN 
                              SIZ:=SIZ+( FOFFSET -1) DIV SIZEOFWORD+1;
                              FOFFSET := 0; 
(* 2-9-83                              NEXTINLIST(SHEAD,SNEXT,KONSTANT,NOOP); 
                              SNEXT^.EMODE:=ORD(INT); 
                              SNEXT^.SVALUE:=0; 2-9-83*)
                            END;
                     END
                ELSE BEGIN
                       IF (FOFFSET+VAL)>SIZEOFFIELD 
                       THEN BEGIN 
                              SIZ:=SIZ+SIZEOFFIELD DIV SIZEOFWORD;
(* WILL NOT FIT IN CURRENT WORD 
 *) 
                              FOFFSET:=0; 
(* 2-9-83                              NEXTINLIST(SHEAD,SNEXT,KONSTANT,NOOP); 
                              SNEXT^.EMODE:=ORD(INT); 
                              SNEXT^.SVALUE:=0; 2-9-83*)
                            END;
                       OFFSET:=FOFFSET; 
                       SIZE:=SIZ; 
                       MASK:=GENMASK(OFFSET,VAL); 
                       FOFFSET:=FOFFSET+VAL;
                     END; 
              END 
         ELSE BEGIN 
                SIZE:=SIZ;
                SIZ:=GREATEROF(SIZ,SIZEOFTYPE(ORD(INT))); 
                FOFFSET:=0; 
                OFFSET:=FOFFSET;
                MASK:=GENMASK(FOFFSET,VAL); 
              END;
         IF T1<>NIL 
         THEN BEGIN 
                T1^.IR.THISMODE:=CRTFIELD(MASK,OFFSET); 
                T1^.IR.OFFSET:=SIZE;
(* 2-9-83 *)    NEXTINLIST(SHEAD,SNEXT,KONSTANT,NOOP);
                SNEXT^.EMODE := T1^.IR.THISMODE;
                SNEXT^.SVALUE:= 0;
(* 2-9-83 *)
              END;
         IF FOFFSET >= SIZEOFFIELD
         THEN BEGIN 
                SIZ := SIZ + FOFFSET DIV SIZEOFWORD;
                FOFFSET:=0; 
              END;
       END
  ELSE BEGIN
         IF T1=NIL
         THEN ERROR(3)
         ELSE BEGIN 
                IF FOFFSET<>0 
                THEN CASE STYP
                OF  STRUCT : BEGIN
                               SIZ:=SIZ+(FOFFSET-1) DIV SIZEOFWORD+1; 
                               NEXTINLIST(SHEAD,SNEXT,KONSTANT,NOOP); 
                               SNEXT^.EMODE:=ORD(INT);
                               SNEXT^.SVALUE:=0;
                             END; 
(* CANNOT USE THIS WORD 
 *) 
                    UNION  :  ; 
                END;
                TYP:=T1^.IR.THISMODE; 
                CASE STYP 
                OF
                  STRUCT : BEGIN
                             T1^.IR.OFFSET:=SIZ;
                             SIZ:=SIZ+SIZEOFTYPE(TYP);
                             NEXTINLIST(SHEAD,SNEXT,KONSTANT,NOOP); 
                             SNEXT^.EMODE:=TYP; 
                             SNEXT^.SVALUE:= 0; 
                             IF TYPETAB[TYP].BASICTYPE=ARRAYTYPE
                             THEN SNEXT^.SVALUE:=SIZEOFTYPE(TYP); 
                             FOFFSET:=0;
                           END; 
                  UNION : BEGIN 
                            T1^.IR.OFFSET:=0; 
                            SIZ:=GREATEROF(SIZ,SIZEOFTYPE(TYP));
                            FOFFSET:=0; 
                          END;
                END;
(* CASE 
 *) 
             END; 
       END; 
END;
(* STRCDECL 
 *) 






PROCEDURE STRCTDCLL(STYP : TYPEMODE ; VAR FOFFSET : INTEGER;
                    VAR SIZ : INTEGER;VAR SHEAD,SNEXT :EXPTREE);
VAR TYN : INTEGER ; 
BEGIN 
  TYN:=ORD(NULLTYPE); 
  WHILE NAMEOFTYPE(TYN) 
  DO ;

  STRCDECL(STYP,TYN,FOFFSET,SIZ,SHEAD,SNEXT); 
  WHILE SY=COMMA
  DO BEGIN
       INSYMBOL ; 
       STRCDECL(STYP,TYN,FOFFSET,SIZ,SHEAD,SNEXT);
     END; 
END;
(* STRCCTDECL 
 *) 




PROCEDURE STRUCTDECL(STYP : TYPEMODE ; T : INTEGER;VAR SHEAD,SNEXT : EXPTREE) ; 
VAR FOFFSET : INTEGER ; 
    SIZ     : INTEGER ; 
BEGIN 
  FOFFSET:=0; 
  SIZ:=0; 
  WHILE (SY<>ENDSYM) AND NOT(EOF(INPUT))
  DO BEGIN
       STRCTDCLL(STYP,FOFFSET,SIZ,SHEAD,SNEXT); 
       IF FOFFSET<>0
       THEN BEGIN 
(* 6-9-83              NEXTINLIST(SHEAD,SNEXT,KONSTANT,NOOP); 
              SNEXT^.EMODE:=ORD(INT); 
              SNEXT^.SVALUE:=0; 6-9-83 *) 
              SIZ:=SIZ+(FOFFSET-1) DIV SIZEOFWORD+1;
              FOFFSET:=0; 
            END;
       NEXTSYMBOL(SEMI,4);
     END; 
  TYPETAB[T].SIZE:=SIZ; 
  INSYMBOL ;
END;
(* STRUCTDECL 
 *) 




FUNCTION STRUCTSPEC;
VAR T1 : IDTREE;
    T : INTEGER;
    SID : ALFA ;
    POS : INTEGER;
    STATE : IDSTATE;
    MODE : INTEGER; 
    STYP : TYPEMODE;
    SHEAD,SNEXT : EXPTREE;
BEGIN 
  SHEAD:=NIL; 
  SNEXT:=NIL; 
  STYP := IDTYPE; 
  INSYMBOL ;
  IF SY = IDENT 
  THEN BEGIN
         SID:=ID; 
         INSYMBOL ; 
         IF SY = BEGINSYM 
         THEN BEGIN 
                T:=CRTSTRUCT(STYP); 
                T1:=PUTINSYMBOLTABLE(STRUCTDICT,SID,STRUCTVAR,T); 
                INSYMBOL ;
                STRUCTDECL(STYP,T,SHEAD,SNEXT); 
                TYPETAB[T].STEMPLATE:=SHEAD;
              END 
         ELSE BEGIN 
                IF ISDEFINED(STRUCTDICT^,SID,POS,STATE,MODE,T1) 
                THEN T:=MODE
                ELSE T:=0;
              END;
       END
  ELSE BEGIN
         IF SY=BEGINSYM 
         THEN BEGIN 
                T:=CRTSTRUCT(STYP); 
                INSYMBOL ;
                STRUCTDECL(STYP,T,SHEAD,SNEXT); 
                TYPETAB[T].STEMPLATE:=SHEAD;
              END 
         ELSE T:=0; 
      END;
  STRUCTSPEC:=T;
END;
(* STRUCTSPEC 
 *) 






(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *     TYPDLARATION ROUTINES
 *  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
 *) 

FUNCTION NAMEOFTYPE ; 


(*
 *         IF THE TOKEN IS A TYPENAME 
 *         THEN TRUE ELSE FALSE 
 *) 
     VAR T : IDTREE;
         POS: INTEGER ; 
         STATE: IDSTATE;
         MODE : INTEGER;

  BEGIN 
   NAMEOFTYPE:=FALSE; 

   IF SY = TYPESYM
   THEN BEGIN 
         IF IDTYPE IN [UNION,STRUCT]
         THEN 
         BEGIN
            BEGIN 
                  TYPENUMBER:=STRUCTSPEC; 
                  NAMEOFTYPE := TRUE; 
                END;
         END
         ELSE 
         BEGIN
              TYPENUMBER:=ORD(IDTYPE);
           NAMEOFTYPE := TRUE;
           INSYMBOL ; 
         END ;
        END 
    ELSE
    IF SY=IDENT 
    THEN BEGIN
           IF ISDEFINED(STRUCTDICT^,ID,POS,STATE,MODE,T)
           THEN BEGIN 
                  IF(STATE=TYPEVAR) OR (STATE = STRUCTVAR)
                  THEN BEGIN
                         IF TYPENUMBER = ORD(NULLTYPE)
                         THEN 
                              BEGIN 
                                TYPENUMBER:=MODE; 
                                NAMEOFTYPE:=TRUE; 
                                INSYMBOL ;
                              END ; 
                       END; 
                END ; 
        END;

   END; 
(* NAMEOFTYPE 
 *) 

  FUNCTION NAMEOFSC:BOOLEAN;

(*
 *       IF THE TOKEN IS A STORAGE-CLASS NAME 
 *        THEN TRUE ELSE FALSE
 *) 
  BEGIN 
    IF SY IN [AUTO,STATIC,EXTERNSYM,REGISTER,TYPEDEF] 
    THEN
      BEGIN 
        CASE SY OF
          AUTO : SC:=AUTOVAR; 
          STATIC : IF SC IN [EXTERNVAR,UNDEFINED] 
                   THEN SC:=EXTSTATIC 
                   ELSE SC:=STATICVAR;

          REGISTER : SC:= AUTOVAR;
          EXTERNSYM: SC:=EXTERNVAR; 
          TYPEDEF: SC:=TYPEVAR ;
          OTHERWISE 
     END; 
(* CASE 
 *) 
     NAMEOFSC:=TRUE;
     INSYMBOL ; 
     END
    ELSE
      NAMEOFSC:=FALSE ; 
  END ; 
(* NAMEOFSC 
 *) 







FUNCTION OUTLITERAL(S : EXPTREE):INTEGER; 
VAR TEMP : INTEGER; 
BEGIN 
  OUTLITERAL:=LITPOS; 
  WHILE S<>NIL
  DO WITH S^
  DO BEGIN
     IF TOKN=STRING 
     THEN TEMP:=OUTLITERAL(STRNG) 
       ELSE BEGIN 
              IF (TOKN=KONSTANT) AND (TYPETAB[(EMODE)].BASICTYPE=ARRAYTYPE) 
              THEN LITPOS:=LITPOS+SVALUE
              ELSE LITPOS:=LITPOS+SIZEOFTYPE(INDIRECT(EMODE));
              IF OPER=REF 
              THEN BEGIN
                     WHILE RGHT^.OPER = REF DO RGHT := RGHT^.RGHT;
                     IF RGHT^.TOKN IN [IDENT,CALL]
                     THEN IF RGHT^.TOKN = IDENT 
                     THEN WRITE(LITFILE,RGHT^)
                     ELSE WRITE(LITFILE,RGHT^.LFT^) 
                     ELSE BEGIN 
                            SVALUE:=LITPOS; 
                            WRITE(LITFILE,S^);
                            TEMP:=OUTLITERAL(RGHT); 
                          END;
                   END
               ELSE WRITE(LITFILE,S^);
            END;
       S:=S^.LFT; 
     END; 
END;



(*
 * * ROUTINE TO GET AN EXPRESSION 
 * * RETURNS A POINTER TO THE TREE
 *) 


FUNCTION GETEXP:EXPTREE;
VAR TYP : INTEGER;
BEGIN 
  EXP;
  IF INFUNCDEF THEN OUTCODE 
  ELSE BEGIN
         PREVUNKNOWN;      (* DO NOT TYPE THE TREE TWICE *) 
         TYP := TYPETREE(BRACKETS[0]);
       END; 
  GETEXP:=BRACKETS[0];
  IF DEBUGF THEN PRINTTREE(BRACKETS[0],0);
  BRACKETS[0]:=NIL; 
  LEVEL:=0; 
  LASTNODE[0]:=NIL; 
  LASTNODE[1]:=NIL; 
  BRACKETS[1]:=NIL; 
END;
(* GETEXP 
 *) 


FUNCTION SCALAR(TY : INTEGER):BOOLEAN;
BEGIN 
   SCALAR:= TYPETAB[TY].BASICTYPE IN[SHORT,KAR,INT,LONG,UNSIGNED,REEL,DOUBLE,POINTER,FIELD];
END;

FUNCTION GETONE:EXPTREE;
BEGIN 
  IF SY=BEGINSYM
  THEN BEGIN
         INSYMBOL ; 
         GETONE:=GETONE;
         NEXTSYMBOL(ENDSYM,5);
       END
  ELSE GETONE:=GETEXP;
END; (* GETONE *) 



PROCEDURE MAKESTAT(VAR ID : ALFA);
  VAR I : INTEGER;
  BEGIN 
    I:=7; 
     WHILE ID[I]=' ' DO I:=I-1; 
    IF I<>7 THEN I:=I+1;
    ID[I]:='$'; 
  END;



PROCEDURE DATAHED(NAME : ALFA);EXTERN;
PROCEDURE DATAHEADER(TID : IDTREE); 
VAR IDNAME:ALFA;
BEGIN 
  IDNAME := TID^.IR.NAME; 
  IF TID^.IR.THISSTATE=EXTSTATIC
  THEN MAKESTAT(IDNAME);
  DATAHED(IDNAME);
  REACHABLE := TRUE;
END;




(*
 * OUTSTATIC
 * FUNCTION TO OUTPUT A TEMPLATE STRUCTURE TO THE 
 * STATIC AREA FILE AND RETURN THE OFFSET OF THE FIRST ELEMENT
 * FROM THE START OF THE STATIC LIST. 
 *) 
 FUNCTION OUTSTATIC;
  (*
   * OUTST
   * MAIN ROUTINE FOR OUTSTATIC 
   *) 
  PROCEDURE OUTST(S : EXPTREE); 
  BEGIN 
    WHILE S<>NIL
    DO BEGIN
         IF S^.TOKN=STRING
         THEN OUTST(S^.STRNG) 
         ELSE WITH S^ 
         DO BEGIN 
              IF OPER=REF 
              THEN BEGIN
                     WHILE RGHT^.OPER = REF DO RGHT := RGHT^.RGHT;
                     IF RGHT^.TOKN=IDENT
                     THEN BEGIN 
                             RGHT^.EMODE := S^.EMODE; 
                             S:=RGHT
                          END 
                     ELSE IF RGHT^.TOKN = CALL
                     THEN BEGIN 
                            RGHT^.LFT^.EMODE := S^.EMODE; 
                            S:=RGHT^.LFT
                          END 
                     ELSE 
                         SVALUE:=OUTLITERAL(RGHT);
                   END; 
              IF INFUNCDEF
              THEN BEGIN
                     WRITE(STATFIL,S^); 
                             IF TYPETAB[EMODE].BASICTYPE<>ARRAYTYPE 
                            THEN STATICSIZE:=STATICSIZE+SIZEOFTYPE(EMODE) 
                            ELSE STATICSIZE:=STATICSIZE+SVALUE; 
                   END
               ELSE WRITE(DATFILE,S^);
            END;
         S:=S^.LFT; 
       END; 
  END;


 BEGIN
   OUTSTATIC:=STATICSIZE; 
   OUTST(S);
 END; 





PROCEDURE OUTAUTO(IDT : IDTREE ; EXP : EXPTREE);
VAR T,T1 : EXPTREE; 
    LAB : INTEGER;
BEGIN 
  T:=NEWEXPNODE(NOOP,ASSIGN); 
  LAB:=0; 
  T1:=NEWEXPNODE(NOOP,IDENT); 
  WITH T^ 
  DO BEGIN
       RGHT:=EXP; 
       LFT:=T1; 
     END; 
  WITH T1^
  DO BEGIN
       SENTRY:=IDT; 
     END; 
    T^.EMODE := INDIRECT(TYPETREE(T1)); 
    LASTTYPE := T^.EMODE; 
  IF DEBUGF THEN PRINTTREE(T,0);
  OUTEXPTREE(T,FALSE,LAB);
  DISPOSE(T); 
  DISPOSE(T1);
END;
(* OUTAUTO
 *) 



PROCEDURE GETARRAY(TYP : INTEGER);
LABEL 10; 
VAR TEMP : EXPTREE; 
    CNT,ACNT : INTEGER; 
    ATYP : INTEGER; 
    OFSET : INTEGER;
BEGIN 
  ATYP :=INDIRECT(TYP); 
  ACNT:= TYPETAB[TYP].INDEXRANGE; 
  CNT := 0; 
  REPEAT
     BEGIN
       IF (SY = STRING) AND ( ATYP < ORD(REEL)) 
       THEN BEGIN 
              TEMP:=GETONE; 
              CNT:=CNT + TEMP^.STRNGLENG; 
              OFSET:=OUTSTATIC(TEMP); 
              DISCARD(TEMP);
            END 
       ELSE BEGIN 
(* 12-JUNE-84*) GETLIST(ATYP,FALSE);
              CNT := SUCC(CNT); 
            END;
       IF SY = COMMA
       THEN BEGIN 
              INSYMBOL ;
              IF SY=ENDSYM
              THEN BEGIN
                   END; 
            END ; 
       IF ( ACNT <> 0) AND (CNT = ACNT) 
       THEN GOTO 10;
     END; 
  UNTIL (SY = ENDSYM) OR ((ACNT <> 0) AND (ACNT = CNT));
  IF SY = COMMA THEN INSYMBOL;
  10 : IF (ACNT = 0 ) 
  THEN BEGIN
         TYPETAB[TYP].INDEXRANGE:=CNT;
         TYPETAB[TYP].SIZE := CNT * SIZEOFTYPE(ATYP); 
       END; 
(* 29-AUG-84  A^L^W^A^Y^S ^P^A^D ^O^U^T ^A^R^R^A^Y^S ^T^O ^F^U^L^L ^S^I^Z^E *)

                     IF CNT < ACNT
                     THEN BEGIN 
                            TEMP:=NEWEXPNODE(NOOP,KONSTANT);
                            TEMP^.SVALUE:=(ACNT-CNT)*SIZEOFTYPE(ATYP);
                            TEMP^.EMODE:=ORD(ARRAYTYPE);
                            OFSET:=OUTSTATIC(TEMP); 
                            DISCARD(TEMP);
                          END 
(* 29-AUG-84 *) 
END;



PROCEDURE GETLST(TYP : INTEGER; MANY : BOOLEAN);
LABEL 11,12;
VAR TEMP : EXPTREE; 
    T1   : EXPTREE; 
    OFSET: INTEGER; 
    AFLAG : BOOLEAN;
BEGIN 
  CASE TYPETAB[TYP].BASICTYPE 
  OF
    SHORT,KAR,INT,LONG,UNSIGNED:  
      BEGIN 
        TEMP:=GETONE; 
        IF TEMP=NIL 
        THEN BEGIN
             11 : TEMP:=NEWEXPNODE(NOOP,KONSTANT);
               TEMP^.SVALUE:=0; 
               TEMP^.EMODE:=TYP;
             END; 
        IF INDIRECT(TEMP^.EMODE)>=ORD(REEL) 
        THEN BEGIN
               ERROR(63); 
               DISCARD(TEMP); 
               TEMP := NEWEXPNODE(NOOP,KONSTANT); 
               TEMP^.SVALUE := 0; 
               TEMP^.EMODE := ORD(INT); 
             END; 
        OFSET:=OUTSTATIC(TEMP); 
        DISCARD(TEMP);
      END;

    REEL,DOUBLE:  
      BEGIN 
        TEMP:=GETONE; 
        IF TEMP=NIL 
        THEN BEGIN
             12 : TEMP:=NEWEXPNODE(NOOP,KONSTANT);
               TEMP^.RVALUE:=0.0; 
               TEMP^.EMODE:=TYP;
             END; 
        IF NOT(INDIRECT(TEMP^.EMODE) IN [ORD(REEL),ORD(DOUBLE)])
        THEN BEGIN
               ERROR(63); 
               DISCARD(TEMP); 
               TEMP := NEWEXPNODE(NOOP,KONSTANT); 
               TEMP^.RVALUE := 0.0; 
               TEMP^.EMODE  := ORD(REEL); 
             END; 
        OFSET:=OUTSTATIC(TEMP); 
        DISCARD(TEMP);
      END;




    POINTER : BEGIN 
                TEMP:=GETONE; 
                IF TEMP= NIL
                THEN BEGIN    (* 0 == NIL *)
                       ERROR(63); 
                       TEMP := NEWEXPNODE(NOOP,KONSTANT); 
                       TEMP^.EMODE := ORD(INT); 
                       TEMP^.SVALUE:= 0;
                     END
                ELSE
                       IF TEMP^.TOKN = STRING 
                THEN BEGIN
                       T1:= NEWEXPNODE(REF,UNARY);
                       T1^.RGHT:=TEMP;
                       TEMP:=T1 ; 
                     END; 
                OFSET:=OUTSTATIC(TEMP); 
                DISCARD(TEMP);
              END;



    ARRAYTYPE:  
      BEGIN 
        IF SY = SEMI
        THEN BEGIN
               IF SIZEOFTYPE(TYP)<>0
               THEN BEGIN 
                      TEMP:=NEWEXPNODE(NOOP,KONSTANT);
                      TEMP^.SVALUE:=SIZEOFTYPE(TYP);
                      TEMP^.EMODE:=ORD(ARRAYTYPE);
                      OFSET:=OUTSTATIC(TEMP); 
                      DISCARD(TEMP);
                    END 
             END
        ELSE BEGIN
               IF (SY = STRING) AND (INDIRECT(TYP)<ORD(REEL)) 
               THEN BEGIN 
                      TEMP:=GETONE; 
                      TYPETAB[TYP].INDEXRANGE:=GREATEROF(TYPETAB[TYP].INDEXRANGE,TEMP^.STRNGLENG);
                      TYPETAB[TYP].SIZE := TYPETAB[TYP].INDEXRANGE; 
                      OFSET:=OUTSTATIC(TEMP); 
       (* 29-AUG-84*) IF TEMP^.STRNGLENG < TYPETAB[TYP].SIZE
                      THEN BEGIN
                             T1 := NEWEXPNODE(NOOP,KONSTANT); 
                             T1^.SVALUE := TYPETAB[TYP].SIZE - TEMP^.STRNGLENG; 
                             T1^.EMODE := ORD(ARRAYTYPE); 
                             OFSET := OUTSTATIC(T1);
                             DISCARD(T1); 
                           END; 
                      DISCARD(TEMP);
                    END 
               ELSE GETARRAY(TYP);
            END;
      END;


    STRUCT: 
      BEGIN 
        TEMP:= TYPETAB[TYP].STEMPLATE;
        REPEAT
          BEGIN 
            IF NOT(SY IN [ENDSYM,COMMA,SEMI]) 
            THEN BEGIN
                   GETLIST(TEMP^.EMODE,TRUE); 
                 END; 
            TEMP:= TEMP^.LFT; 
            IF TEMP <> NIL
            THEN BEGIN
                   IF SY = COMMA THEN INSYMBOL; 
                 END; 
          END;
        UNTIL (SY IN [ENDSYM,SEMI,COMMA]) OR (TEMP = NIL);
        IF (SY IN [ENDSYM,SEMI,COMMA]) AND (TEMP <> NIL)
(*      THEN ERROR(83); *)     (* NOT ENOUGH INITIALISERS *)
        THEN BEGIN
               WARN(8);        (* NOT ENOUGH INITIALISERS *)
WRITELN(CODE,'*NOT ENOUGH INITIALISERS'); 
               WHILE TEMP <> NIL
               DO BEGIN 
                    T1:= NEWEXPNODE(NOOP,KONSTANT); 
                    T1^.SVALUE := 0;
                    T1^.EMODE  := TEMP^.EMODE;
WRITELN(CODE,'*PADDING WITH ',TEMP^.EMODE); 
                    OFSET:= OUTSTATIC(T1);
                    DISCARD(T1);
                    TEMP:= TEMP^.LFT; 
                  END;
             END; 
      END;



    FIELD : BEGIN       (* 2-9-83 *)
              TEMP:=GETONE; 
              IF TEMP <> NIL
              THEN BEGIN
                     TEMP^.EMODE := TYP;
                     OFSET := OUTSTATIC(TEMP);
                     DISCARD(TEMP); 
                   END; 
            END;
    OTHERWISE ERROR(63);
  END;
END;




 PROCEDURE GETLIST; 
 BEGIN
   IF SY = BEGINSYM 
   THEN BEGIN 
          INSYMBOL; 
          GETLST(TYP,TRUE); 
          NEXTSYMBOL(ENDSYM,82);
        END 
   ELSE GETLST(TYP,MANY); 
 END; 

PROCEDURE ILIST(TYP : INTEGER); 
VAR HEAD : EXPTREE; 
BEGIN 
  HEAD:=NIL;
  TYPID:=LASTID;
  IF INFUNCDEF AND(SC=AUTOVAR)
  THEN BEGIN
         IF NOT(SY IN [SEMI,COMMA]) 
         THEN IF SCALAR(TYP)
         THEN BEGIN 
                HEAD:=GETONE; 
                IF HEAD<>NIL
                THEN OUTAUTO(TYPID,HEAD); 
                DISCARD(HEAD);
              END 
         ELSE ERROR(62);
       END
  ELSE BEGIN
(*       IF INFUNCDEF AND (SC=STATICVAR)
         THEN TYPID^.IR.OFFSET:=STATICSIZE; *)
         IF SY IN [SEMI,COMMA]
         THEN BEGIN 
                IF (SIZEOFTYPE(TYP)<>0) AND NOT(INFUNCDEF) AND (SC<>EXTERNVAR)  AND (TYPETAB[TYP].BASICTYPE <> FUNK)
                THEN BEGIN
                       DATAHEADER(TYPID); 
                       WRITELN(CODE,'          BSSZ     ',SIZEOFTYPE(TYP):1); 
                     END
(*              ELSE IF (SIZEOFTYPE(TYP)<>0) AND (SC=STATICVAR) 
                THEN GETLIST(TYP,FALSE);*)
              END 
         ELSE IF SC <> EXTERNVAR
         THEN BEGIN 
                GETLIST(TYP,FALSE); 
                IF (SIZEOFTYPE(TYP)<>0) AND NOT(INFUNCDEF)
                THEN BEGIN
                  DATAHEADER(TYPID);
                       PRINTDATA; 
                     END; 
              END;
       END; 
END; (* ILIST *)



PROCEDURE PARAMLIST;

(*
 *         CODE TO GET PARAM LIST 
 *) 

  BEGIN 
(* PARAMLIST
 *) 
   WHILE SY IN [COMMA,ADDOP,ANDSYM,IDENT,STRING,KONSTANT,LP,TERMOP,MULTOP]
   DO BEGIN 
        EXP;
        IF SY = COMMA 
        THEN BEGIN
               SY:=PARAM; 
               PUSHTOKEN; 
               INSYMBOL ; 
             END; 
     END; 

  END;
(* PARAMLIST
 *) 




FUNCTION CRTFUNK(INN : INTEGER):INTEGER;

(* FUNCTION CRTS A TYPE FUNCTION RETURNING 'INN'
 *) 
BEGIN 
  IF TYPEREF<TYPETABSIZE
  THEN BEGIN
         TYPEREF:=SUCC(TYPEREF);
         WITH TYPETAB[TYPEREF]
         DO BEGIN 
              BASICTYPE:=FUNK;
              RETRN:=INN; 
              SIZE:=SIZEOFTYPE(INN);
            END;
         CRTFUNK:=TYPEREF;
       END
  ELSE WRITELN('TYPE TABLE OVERFLOW '); 
END;
(* CRTFUNK
 *) 



FUNCTION CRTPOINTER(INN : INTEGER) : INTEGER; 



(*
 *           FUNCTION CRTS A POINTER RECORD 
 *           POINTING TO 'INN'
 *) 

   BEGIN
     IF TYPEREF < TYPETABSIZE 
     THEN BEGIN 
          TYPEREF := SUCC(TYPEREF); 
          WITH TYPETAB[TYPEREF] 
          DO BEGIN
             SIZE := SIZEOFTYPE(INN); 
             BASICTYPE := POINTER;
             TYPEPOINTER := INN 
             END; 
          CRTPOINTER := TYPEREF 
          END 
      ELSE WRITELN(' TYPE TABLE OVERFLOW ') 
   END; 
(* CRTPOINTER 
 *) 



FUNCTION CRTARRAY(INN:INTEGER ;IDR : INTEGER) : INTEGER;



(* FUNCTION TO CRT RECORD 
 *         WHICH IS AN ARRAY OF 
 *              INDEX RANGE IDR 
 *) 

  BEGIN 
    IF TYPEREF < TYPETABSIZE
    THEN BEGIN
         TYPEREF := SUCC(TYPEREF);
         WITH TYPETAB[TYPEREF]
         DO BEGIN 
            INDEXRANGE := IDR ; 
            SIZE := IDR * SIZEOFTYPE(INN) ; 
            TYPENO := INN ; 
            BASICTYPE := ARRAYTYPE
            END;
         CRTARRAY := TYPEREF
         END
     ELSE WRITELN('TYPE TABLE OVERFLOW ') 
  END;
(* CRTARRAY 
 *) 




 FUNCTION ISTYPE(TYP:TYPEMODE;
                 INN:INTEGER; 
             VAR OUT:INTEGER):BOOLEAN;
  VAR FOUND : BOOLEAN ; 
  BEGIN 
(* ISTYPE 
 *) 
    FOUND:=FALSE; 
    OUT  := 1;
    WHILE ((OUT <= TYPEREF ) AND (OUT<>TYPETABSIZE) AND NOT(FOUND)) 
    DO BEGIN
         IF TYPETAB[OUT].BASICTYPE = TYP
         THEN CASE TYP
         OF 
           POINTER : IF TYPETAB[OUT].TYPEPOINTER=INN
                     THEN FOUND := TRUE;
           FUNK    : IF TYPETAB[OUT].RETRN = INN
                     THEN FOUND := TRUE;
           OTHERWISE
         END ;
(* CASE 
 *) 
         IF NOT(FOUND)
         THEN OUT:=SUCC(OUT); 
       END; 
    ISTYPE:=FOUND;
  END;
(* ISTYPE 
 *) 


  FUNCTION POINTERTO(LAST: INTEGER):INTEGER;
  VAR T : INTEGER ; 
  BEGIN 
(* POINTERTO
 *) 
    IF ISTYPE(POINTER,LAST,T) 
    THEN
      POINTERTO:=T
    ELSE
      POINTERTO:=CRTPOINTER(LAST);
  END;
(* POINTERTO
 *) 


  FUNCTION FUNCRETURNING(LAST:INTEGER):INTEGER; 
  VAR T : INTEGER;
  BEGIN 
(* FUNCRETURNING
 *) 
    IF ISTYPE(FUNK,LAST,T)
    THEN
      FUNCRETURNING:=T
    ELSE
      FUNCRETURNING:=CRTFUNK(LAST); 
  END;
(* FUNCRETURNING
 *) 


  FUNCTION EMPTYTYPELIST ;
  VAR TL : TYPELIST;
      LAST: INTEGER;

  BEGIN 
(* EMPTYTYPELIST
 *) 
   IF TYPENUMBER = ORD(NULLTYPE)
   THEN TYPENUMBER:=ORD(INT); 
    LAST:=TYPENUMBER; 
    WHILE TYPH <> NIL 
    DO BEGIN
         CASE TYPH^.MDE 
         OF 
           NULLTYPE: LAST:= TYPENUMBER; 

           POINTER : LAST:=POINTERTO(LAST); 

           ARRAYTYPE: LAST:=CRTARRAY(LAST,TYPH^.SZE); 

           FUNK    : LAST:=FUNCRETURNING(LAST); 
         END ;
(* CASE 
 *) 
         TL:=TYPH;
         TYPH:=TYPH^.NXT; 
         DISPOSE(TL); 
       END; 
(* WHILE
 *) 
   EMPTYTYPELIST:=LAST; 
  END ; 
(* EMPTYTYPELIST
 *) 




 PROCEDURE REMING1(TYPH,TYPL:TYPELIST); 
 VAR TL:TYPELIST; 
 BEGIN
(* REMING1
 *) 
   IF SY=LB 
   THEN BEGIN 
          INSYMBOL ;
          IF SY<>RB 
          THEN KONSTEXP 
          ELSE
            VAL:=0; 
          NEW(TL);
          TL^.NXT:=TYPL;
          TL^.MDE:=ARRAYTYPE; 
          TL^.SZE:=VAL; 
          TYPH^.NXT:=TL;
          IF CHECK(RB,[COMMA,SEMI],7) 
          THEN
            BEGIN 
              INSYMBOL ;
              REMING1(TL,TYPL); 
            END;
        END;
  END;
(* REMING1
 *) 


  FUNCTION REMING ; 

(* ADDS ON TO LIST .
 *     ON EXIT, RETURNS TAIL OF LIST
 *) 
  VAR TL : TYPELIST;
  BEGIN 
(* REMING 
 *) 
    CASE SY 
    OF
      LB : BEGIN
             INSYMBOL ; 
             IF SY<>RB
             THEN 
               KONSTEXP 
             ELSE 
               VAL:=0;
             NEW(TL); 
             TL^.NXT:=NIL;
             TL^.MDE:=ARRAYTYPE;
             TL^.SZE:=VAL;
             TYPH^.NXT:=TL; 
             IF CHECK(RB,[COMMA,SEMI],8)
             THEN BEGIN 
                    INSYMBOL ;
                    REMING1(TYPH,TL); 
                  END;
             REMING:=TL;
           END; 
(* LB 
 *) 

       LP : BEGIN 
              INSYMBOL ;
              IF INFLAG 
              THEN BEGIN
                     IF CHECK(RP,[COMMA,SEMI],9)THEN INSYMBOL ; 
                     NEW(TL); 
                     TL^.NXT:=NIL;
                     TL^.MDE:=FUNK; 
                     TYPH^.NXT:=TL; 
                     REMING:=TL;
                   END
              ELSE
(* EXTERNAL FUNCTION DEF
 *) 
                BEGIN 
                  NEW(TL);
                  TL^.NXT:=NIL; 
                  TL^.MDE:=FUNK;
                  TYPH^.NXT:=TL;
                  REMING:=TL; 
                  PARMLIST; 
                  INFUNCDEF:=TRUE;
                  INSYMBOL ;
                  IF SY IN [COMMA,SEMI] 
                  THEN INFUNCDEF :=FALSE; 
                END;
            END;
(* LP 
 *) 

      OTHERWISE BEGIN 
                  REMING:=TYPH; 
                END;
      END;
(* CASE 
 *) 
   END; 
(* REMING 
 *) 


    PROCEDURE TYPD; 
    VAR TL,TL1 : TYPELIST ; 
    BEGIN 
      IF OP=TIMES 
      THEN BEGIN
             INSYMBOL ; 
             NEW(TL); 
             TL^.NXT:=NIL;
             TL^.MDE:=POINTER;
             TYPH^.NXT:=TL; 
             TYPD(TL,INFUNCDEF,ABSFLG); 
           END
      ELSE
      IF (SY = LP) AND NOT(ABSFLG)
      THEN BEGIN
             INSYMBOL ; 
             NEW(TL); 
             TL^.NXT:=NIL;
             TL^.MDE:=NULLTYPE; 
(* JUST A MARKER
 *) 
             TYPD(TL,INFUNCDEF,ABSFLG); 
             IF CHECK(RP,[SEMI,COMMA],10) 
             THEN BEGIN 
                    INSYMBOL ;
                    TL1:=REMING(TYPH,(TL^.MDE<>NULLTYPE)OR INFUNCDEF);
                    TL1^.NXT:=TL^.NXT;
                    DISPOSE(TL);
                  END;
          END 
     ELSE 
     IF (SY = IDENT ) AND NOT(ABSFLG) 
     THEN BEGIN 
            IDENTNAME:=ID;
            INSYMBOL ;
            TL1:=REMING(TYPH,INFUNCDEF);
          END 
     ELSE IF (SY = LP) AND ABSFLG 
     THEN BEGIN 
            INSYMBOL ;
            IF SY<>RP 
            THEN BEGIN
                     NEW(TL); 
                     TL^.NXT:=NIL;
                     TL^.MDE:=NULLTYPE; 
(* JUST A MARKER
 *) 
                     TYPD(TL,INFUNCDEF,ABSFLG); 
                     IF CHECK(RP,[SEMI,COMMA],10) 
                     THEN BEGIN 
                            INSYMBOL ;
                            TL1:=REMING(TYPH,(TL^.MDE<>NULLTYPE)OR INFUNCDEF);
                            TL1^.NXT:=TL^.NXT;
                            DISPOSE(TL);
                   END; 
                  END 
            ELSE BEGIN
                    NEW(TL);
                    TL^.NXT:=NIL; 
                    TL^.MDE:=FUNK;
                    TYPH^.NXT:=TL;
                 END; 
          END 
     ELSE BEGIN 
            IF NOT(ABSFLG) THEN 
            SKIP([COMMA,SEMI]); 
          END;
   END; 
(* TYPD 
 *) 


  PROCEDURE DECLTOR;
  VAR TYPEHEAD: TYPELIST ;
      T : IDTREE ;
(* MOD TO HANDLE PUTINSYMBOLTABLE MODS
 *) 
   BEGIN
(* DECLTOR
 *) 
     IDENTNAME:='          '; 
     NEW(TYPEHEAD); 
     TYPEHEAD^.MDE:=NULLTYPE; 
     TYPEHEAD^.NXT:=NIL;
     TYPEHEAD^.SZE:=0;
     TYPD(TYPEHEAD,INFLAG,FALSE); 
     TYY:=EMPTYTYPELIST(TYPEHEAD);
     IF IDENTNAME<>'          ' 
     THEN IF SC = UNDEFINED 
    THEN LASTID:=PUTINSYMBOLTABLE(SYMTAB,IDENTNAME,EXTERNVAR,TYY) 
    ELSE  LASTID:=PUTINSYMBOLTABLE(SYMTAB,IDENTNAME,SC,TYY);
(* 15 - AUG - 84 *) 
    TYY := LASTID^.IR.THISMODE;    (* GET REAL TYPE OF OBJECT *)
  END ; 
PROCEDURE INLIZER;
BEGIN 
         IF SY = ASSIGN 
         THEN BEGIN 
                INSYMBOL; 
              END;
         ILIST(TYY);    (* ILIST GENERATES THE HEADERS FOR EXTERNALS. *)
END;
    PROCEDURE INTDLR; 

(*
 *       INTDLR -> DECLATATOR INITALISER-OPT
 *) 

    BEGIN 
      IF SC=TYPEVAR 
      THEN DECLTOR(TRUE,STRUCTDICT) 
      ELSE DECLTOR(TRUE,CURRENT); 
      INLIZER;
    END;
(* INTDLR 
 *) 


    PROCEDURE DECLSPEC; 

(* DECLSPEC -> TYPESPEC DECLSPEC-OPT
 *                   SC-SPEC DECLSPEC-OPT 
 *) 
    BEGIN 
     WHILE NAMEOFTYPE(TYPENUMBER) OR NAMEOFSC 
     DO ; 
    END;
    PROCEDURE INTDLLIST;

(* INTDLLIST -> INIT-DECLTOR
 *                       INIT-DECLTOR , INTDLLIST 
 *) 
 VAR T : INTEGER; 
     BEGIN
(* INTDLIST 
 *) 
       T := TYPENUMBER; 
       INTDLR;
       WHILE SY=COMMA 
       DO BEGIN 
            INSYMBOL ;
            TYPENUMBER := T;
          INTDLR; 
        END;
   END; 
    PROCEDURE DECLTIN;

(*  DECLTION -> DECL-SPEC INIT-DECL-LIST-OPT ;
 *) 

    BEGIN 
      DECLSPEC; 
      INTDLLIST;
      IF CHECK(SEMI,[SEMI],11)THEN INSYMBOL ; 
    END ; 
(* DECLTIN
 *) 

