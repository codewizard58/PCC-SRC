(*$T-,P-,L'THE NEW C COMPILER'*)
PROGRAM CPRE(INPUT,INFILE,OUTPUT);
(* THIS IS A PREPASS FOR THE C PROGRAMMING LANGUAGE.
 *
 * WRITTEN BY P.J.CHURCHYARD. 
 *) 
CONST BIGA  = 65; 
      BIGZ  = 90; 
      LITTLEA=97; 
      LITTLEZ=122;
      ZERO  = 48; 
      NINE  = 57; 
      SPACE = 32; 
      HASHCH  = 35; 
      DQUOTE= 34; 
      SQUOTE= 39; 
      LESSTHAN=60;
      GREATERTHAN=62; 
      UNDERSCORE=95;
      LPAREN=40;
      RPAREN=41;
      BSLASH=92;
      COMMA=44; 
      SLASH=47; 
      ASTERIX=42; 
      PLUS   =43; 
      MINUS  =45; 
      DOT    =46; 



      EOFCH=0;
      EOLNCH=13;

      LINESIZE=300; 
       BODYSIZE=200;
       PARAMSIZE=300; 





TYPE EXPANDTYPE=(MACRO,PARAMETER,FYLE); 
     CHARACTER = 0..127;
     LINE = RECORD
              LENGTH : 0..LINESIZE; 
              STRING : ARRAY [1..LINESIZE] OF CHARACTER 
            END;
     INTERCODE = 0..255;
     (*  TOP BIT=1     0..6 = PARAMETER NUMBER. 
      *          0     0..6 = ASCII CHARACTER . 
      *)

     BODYPTR=  ^BODY; 
     BODY   =PACKED ARRAY[0..BODYSIZE]OF INTERCODE; 

     PARAMPTR=^PARAMREC;
     PARAMREC=RECORD
                NEXT : PARAMPTR;
                PARAMLINE:PACKED ARRAY[0..PARAMSIZE] OF INTERCODE 
              END;

      FILEPTR=^TEXT;

     MACPTR=^MACREC;
     MACREC=RECORD
              PARAMLIST : PARAMPTR; 
              MACBODY   : BODYPTR 
            END;

     LINEPTR=^LINE; 

     ACTPTR=^ACTIVATION;
     ACTIVATION=RECORD
                  FRWD,BACK : ACTPTR; 
                   POSITION : INTEGER;
                  CASE EXPTYP : EXPANDTYPE
                  OF
                     MACRO : (THISMAC : MACPTR);
                     PARAMETER:(THISPARAM : PARAMPTR);
                     FYLE  : (THISFILE : FILEPTR; 
                              THISLINE : LINEPTR )
                  END;
     NAME = PACKED ARRAY[0..13] OF INTERCODE ;

     DICTPTR=^DICTENTRY;
     DICTENTRY=RECORD 
                 FRWD,BACK : DICTPTR; 
                 DICTNAME  : NAME;
                 MACNUMBER : INTEGER ;
                 CASE DICTTYP : EXPANDTYPE
                 OF 
                   MACRO : ( MACDICT : MACPTR );
                   FYLE , 
                   PARAMETER:(PARAMDICT : PARAMPTR) 
               END; 



VAR  MAP : PACKED ARRAY[CHARACTER] OF CHAR; 
     OUT,CURRENT : ACTPTR;

     CH  : INTERCODE ;
    CHN : INTERCODE;
     POSOFINPUT: INTEGER; 
     INPUTLINE : LINE;

     DTOP,CTOP,DTEMP : DICTPTR; 
     QUOTED    : BOOLEAN; 
     FLAG      : BOOLEAN; 

     NAME1     : NAME ; 

     INFILE : TEXT; 

     HASHTAB : PACKED ARRAY[0..511] OF BOOLEAN; 
     INCOMMENT : BOOLEAN; 
     IFLEVEL   : INTEGER; 
     LASTLEVEL:INTEGER; 
     COPY      : BOOLEAN; 
     COPYSTK   : ARRAY [ 0..30] OF BOOLEAN; 
     MEXPAND   : BOOLEAN; 
(*
 * END OF DATA AND TYPES. 
 *
 * FUNCTION AND PROCEDURES
 *) 

  PROCEDURE OPEN(VAR F:TEXT;N:ALFA;W:BOOLEAN);EXTERN; 

  PROCEDURE FILENAME(VAR F:TEXT;VAR N:ALFA);EXTERN; 

  PROCEDURE CLOSE(VAR F:TEXT);EXTERN; 



  PROCEDURE GETLINE(VAR F : TEXT; VAR L : LINE); EXTERN;
  PROCEDURE PUTLINE(VAR F : TEXT; L : LINE); EXTERN;




PROCEDURE PUTCHR(OUT:ACTPTR;CH : INTERCODE);
BEGIN 
  IF COPY 
  THEN BEGIN
         IF (CH = 9) OR (CH = 12)      (* TAB OR FF *)
         THEN CH := SPACE;
         IF OUT<>NIL
         THEN WITH OUT^ 
         DO BEGIN 
              IF EXPTYP=FYLE
              THEN WITH THISLINE^ 
              DO BEGIN
                   IF (POSITION=LINESIZE) OR (CH=EOLNCH)
                   THEN BEGIN 
                          LENGTH:=POSITION; 
                          PUTLINE(THISFILE^,THISLINE^); 
                          WRITELN(THISFILE^); 
                          POSITION:=0;
                        END 
                   ELSE BEGIN 
                          POSITION:=SUCC(POSITION); 
                          STRING[POSITION]:=CH MOD 128; 
                        END 
                  END 
            END 
       END; 
END; (* PUTCHR *) 

PROCEDURE PRTCUR(CURRENT : ACTPTR); 
VAR I : INTEGER;
BEGIN 
  IF CURRENT = NIL THEN WRITELN('------') 
  ELSE WITH CURRENT^
  DO BEGIN
       IF EXPTYP = FYLE 
       THEN BEGIN 
              WRITE('FILE: ');
              PUTLINE(OUTPUT,THISLINE^);
              WRITELN;
              WRITE('      ');
              FOR I := 1 TO POSITION
              DO WRITE(' ');
              WRITELN('^'); 
            END 
       ELSE IF EXPTYP = MACRO 
       THEN WITH THISMAC^ 
       DO BEGIN 
            I := 0; 
            REPEAT
              IF MACBODY^[I] >= 128 
              THEN BEGIN
                     PUTCHR(OUT,61);
                     PUTCHR(OUT,48+MACBODY^[I]-128);
                   END
              ELSE IF MACBODY^[I] = EOFCH 
              THEN I:= BODYSIZE 
              ELSE PUTCHR(OUT,MACBODY^[I]); 
              I:= I+1;
            UNTIL I>= BODYSIZE; 
            PUTCHR(OUT,EOLNCH); 
          END 
     END
END;


PROCEDURE DUMPSTK(CUR : ACTPTR);
BEGIN 
  WHILE(CUR <> NIL) 
  DO BEGIN
       PRTCUR(CUR); 
       CUR := CUR^.BACK;
     END; 
END;

PROCEDURE GETALINE(F : FILEPTR ; VAR LN : LINE);
BEGIN(* GETALINE *) 
  IF F=NIL
  THEN BEGIN (* USE INPUT *)
         IF NOT EOF(INPUT)
         THEN WITH LN 
         DO BEGIN 
              IF EOLN(INPUT)
              THEN READLN(INPUT); 
              LENGTH:=LINESIZE; 
              GETLINE(INPUT,LN);
              IF LENGTH<LINESIZE
               THEN LENGTH:=SUCC(LENGTH); 
               STRING[LENGTH]:=EOLNCH;
              IF EOF(INPUT) 
              THEN STRING[1]:=EOFCH;
            END 
         ELSE 
           LN.STRING[1]:=EOFCH; 
       END
  ELSE
  IF NOT EOF(F^)
  THEN WITH LN
       DO BEGIN 
            IF EOLN(F^) 
            THEN READLN(F^);
            LENGTH:=LINESIZE; 
            GETLINE(F^,LN); 
            IF LENGTH<LINESIZE
            THEN
              LENGTH:=SUCC(LENGTH); 
            STRING[LENGTH]:=EOLNCH; 
            IF EOF(F^)
            THEN STRING[1]:=EOFCH;
          END 
  ELSE
    LN.STRING[1]:=EOFCH;
END; (* GETALINE *) 



FUNCTION GETCHR(CURRENT : ACTPTR ) : INTERCODE ;
BEGIN 
  IF CURRENT=NIL
  THEN
    GETCHR:=EOFCH (* USE INPUT *) 
  ELSE WITH CURRENT^
  DO BEGIN
       CASE EXPTYP
       OF 
         MACRO : BEGIN
                   IF THISMAC=NIL 
                    THEN
                      GETCHR:=EOFCH 
                   ELSE 
                     GETCHR:=THISMAC^.MACBODY^[POSITION]
                 END; 

         PARAMETER:BEGIN
                     IF THISPARAM=NIL 
                     THEN 
                       GETCHR:=EOFCH
                     ELSE 
                       GETCHR:=THISPARAM^.PARAMLINE[POSITION] 
                   END; 

          FYLE  : BEGIN 
                    WITH THISLINE^
                    DO BEGIN
                         IF POSITION= 0 
                         THEN GETCHR:=EOLNCH
                         ELSE BEGIN 
                               IF POSITION>LENGTH 
                               THEN BEGIN 
                                      GETALINE(THISFILE,THISLINE^); 
                                      POSITION:=1;
                                    END;
                               GETCHR:=STRING[POSITION];
                             END; 
                       END; 
                  END;

       END; (* CASE *)
     END; 
END; (* GETCHR *) 






FUNCTION GETCH:CHARACTER; 
VAR CH:INTERCODE; 
    LASTMACRO:ACTPTR; 
    CHT:INTEGER;
    PARM,TEMP:PARAMPTR; 
BEGIN 
  CH:=GETCHR(CURRENT);
  WHILE (( CH DIV 128 ) = 1 ) OR (( CH = EOFCH ) AND(CURRENT<>NIL)) 
  DO BEGIN
       IF (CH DIV 128)=1
       THEN BEGIN 
              CURRENT^.POSITION:=SUCC(CURRENT^.POSITION); 
              LASTMACRO:=CURRENT; 
              WHILE LASTMACRO^.EXPTYP<>MACRO
              DO
                LASTMACRO:=LASTMACRO^.BACK; 
              (* FIND THE NTH PARAMETER *)
              CHT:=1; 
              PARM:=LASTMACRO^.THISMAC^.PARAMLIST;
              WHILE CHT<>(CH MOD 128 )
              DO BEGIN
                   PARM:=PARM^.NEXT;
                   CHT:=SUCC(CHT);
                 END; 
              (* INSTAL THE PARAMETER *)
              NEW(CURRENT^.FRWD); 
              CURRENT^.FRWD^.BACK:=CURRENT; 
              CURRENT:=CURRENT^.FRWD; 
              WITH CURRENT^ 
              DO BEGIN
                   POSITION:=0; 
                   EXPTYP:=PARAMETER; 
                   THISPARAM:=PARM; 
                 END; 
            END 
       ELSE WITH CURRENT^  (* DEACTIVATE TOP RECORD *)
            DO BEGIN
                 IF EXPTYP=FYLE 
                 THEN BEGIN 
                        IF THISFILE<>NIL
                        THEN CLOSE(THISFILE^);
                        DISPOSE(THISLINE) ; 
                      END 
                 ELSE IF EXPTYP=MACRO 
                 THEN BEGIN 
                        PARM:=THISMAC^.PARAMLIST; 
                        WHILE PARM<>NIL 
                        DO BEGIN
                             TEMP:=PARM^.NEXT;
                             DISPOSE(PARM); 
                             PARM:=TEMP;
                           END; 
                      END;
                 IF CURRENT^.BACK<>NIL
                 THEN BEGIN 
                        CURRENT:=CURRENT^.BACK; 
                        DISPOSE(CURRENT^.FRWD); 
                        CURRENT^.FRWD:=NIL; 
                      END 
                 ELSE BEGIN 
                        DISPOSE(CURRENT); 
                        CURRENT:=NIL; 
                      END;
            END;
       CH:=GETCHR(CURRENT); 
  END;
  CHN:=CH MOD 128;
  GETCH:= CHN;
  IF CURRENT<>NIL 
  THEN WITH CURRENT^
  DO BEGIN
       POSITION:=SUCC(POSITION);
     END; 
END;








PROCEDURE SKIPBLANKS; 
VAR CH : CHARACTER; 
BEGIN 
  WHILE (CHN=SPACE) OR (CHN = 12) OR (CHN = 9)
  DO
    CH:=GETCH;
END; (* SKIPBLANKS *) 

PROCEDURE SKIPTOEOLN; 
VAR CH:CHARACTER; 
BEGIN 
  WHILE (CHN <> EOLNCH) AND (CHN <>EOFCH)DO CH:=GETCH;
  IF CHN=EOLNCH THEN CH:=GETCH; 
END;



FUNCTION HASH(NAM : NAME):INTEGER;
LABEL 1;
VAR I,J : INTEGER;
BEGIN 
  J:=0; 
  FOR I := 0 TO 13
  DO BEGIN
       IF (NAM[I] = 0) OR (NAM[I] = SPACE) THEN GOTO 1; 
       J:=J*37 + NAM[I];
     END; 
1: HASH := J MOD 512; 
END;


FUNCTION HASHED(NAM : NAME):BOOLEAN;
BEGIN 
  HASHED := HASHTAB[HASH(NAM)]; 
END;


PROCEDURE INSTALFILE(NAM: ALFA);
(* INSTAL THE FILE NAMED BY NAM 
 *  ON THE ACTIVATION STACK.
 *) 
BEGIN 
  IF CURRENT=NIL
  THEN BEGIN
         NEW(CURRENT);
         CURRENT^.BACK:=NIL;
         CURRENT^.FRWD:=NIL;
       END
  ELSE
  BEGIN 
    CURRENT^.POSITION := PRED(CURRENT^.POSITION); 
    NEW(CURRENT^.FRWD); 
    CURRENT^.FRWD^.BACK:=CURRENT; 
    CURRENT:=CURRENT^.FRWD; 
  END;
  WITH CURRENT^ 
  DO BEGIN
       EXPTYP:=FYLE;
       NEW(THISLINE); 
       NEW(THISFILE); 
       OPEN(THISFILE^,NAM,FALSE); 
       RESET(THISFILE^);
       POSITION:=1; 
       IF EOF(THISFILE^)
       THEN BEGIN 
              WRITELN('EMPTY FILE ',NAM); 
            END;
       GETALINE(THISFILE,THISLINE^);
       CH:=GETCH;     (* READ THE FIRST CHARACTER *)
     END; 
END; (* INSTALFILE *) 




PROCEDURE PRCSSINCLUDE; 
VAR FNAME : ALFA; 
    DELMTR : CHARACTER; 
    POS    : INTEGER; 
    CH : CHARACTER; 
BEGIN 
  SKIPBLANKS; 
  CH:=CHN;
  IF (CH=DQUOTE) OR (CH = LESSTHAN) 
  THEN BEGIN
         IF CH=DQUOTE 
         THEN DELMTR:=DQUOTE
         ELSE DELMTR:=GREATERTHAN;
         FNAME:='          '; 
         CH:=GETCH; 
         POS:=1;
         WHILE (CH<> DELMTR) AND ( POS < 11 ) 
         DO BEGIN 
              FNAME[POS]:=MAP[CH];
              POS:=SUCC(POS); 
              CH:=GETCH;
            END;
         (* SKIP THE REST OF THE LINE *)
         SKIPTOEOLN;
         IF POS<>1
         THEN 
           INSTALFILE(FNAME); 
       END
  ELSE BEGIN
         WRITELN(' ERROR IN FILE NAME '); 
         SKIPTOEOLN;
       END; 
END; (* PRCSS INCLUDE *)




(*
 * DICTIONARY BUILDING ROUTINE
 *) 

PROCEDURE DICTENTER(ITEM : DICTPTR ; VAR DTOP : DICTPTR); 
BEGIN 
  IF ITEM<>NIL
  THEN WITH ITEM^ 
  DO BEGIN
       FRWD:=NIL; 
       BACK:=DTOP;
       IF DTOP <> NIL 
       THEN DTOP^.FRWD:=ITEM; 
       DTOP := ITEM;
     END; 
END; (* DICTENTRY *)




(*
 *  DICTIONARY SCAN ROUTINE 
 *
 *) 


FUNCTION NAMESMATCH(A,B:NAME):BOOLEAN;
VAR I : INTEGER;
    FLAG : BOOLEAN; 
BEGIN 
  FLAG:=TRUE; 
  I:=0; 
  WHILE (I<14) AND FLAG 
  DO
    IF A[I]=B[I]
    THEN I:=SUCC(I) 
    ELSE FLAG:=FALSE; 
  NAMESMATCH:=FLAG; 
END; (* NAMESMATCH *) 





FUNCTION ISDEF(NAM : NAME ; TOP : DICTPTR ) : DICTPTR;
VAR FLAG : BOOLEAN; 
BEGIN 
  IF NOT(HASHED(NAM)) THEN TOP :=NIL; 
  FLAG:=FALSE;
  WHILE (TOP<>NIL) AND NOT(FLAG)
  DO BEGIN
       IF NAMESMATCH(NAM,TOP^.DICTNAME) 
       THEN FLAG:=TRUE
       ELSE TOP:=TOP^.BACK
     END; 
  ISDEF:=TOP; 
  IF TOP <> NIL 
  THEN IF NOT(MEXPAND) AND (TOP^.DICTTYP = MACRO) 
       THEN ISDEF := NIL;   (* DO NOT EXPAND MACROS IN BODIES *)
END; (* ISDEF *)





FUNCTION ISALPHA(CH : CHARACTER):BOOLEAN; 
BEGIN 
  ISALPHA:=((CH>=BIGA) AND ( CH<=BIGZ)) OR
           (( CH>=LITTLEA ) AND ( CH<= LITTLEZ ));
END; (* ISALPHA *)



FUNCTION ALPHANUMERIC(CH:CHARACTER):BOOLEAN;
BEGIN 
  ALPHANUMERIC:=ISALPHA(CH) OR(( CH>=ZERO) AND( CH<=NINE))
                OR (CH = UNDERSCORE );
END; (* ALPHANUMERIC *) 




PROCEDURE GETWORD(VAR NAM : NAME);
VAR CH : CHARACTER; 
    I  : INTEGER; 
BEGIN 
  FOR I:=0 TO 13
  DO NAM[I]:=SPACE; 
  I:=0; 
  WHILE ALPHANUMERIC(CHN) AND (I<14)
  DO BEGIN
       NAM[I]:=CHN; 
       CH:=GETCH; 
       I:=SUCC(I) 
     END; 
  WHILE ALPHANUMERIC(CHN) 
  DO BEGIN
       CH := GETCH;        (* IGNORE CHARACTERS AFTER 14TH.*) 
     END; 
END;




PROCEDURE PRINTNAME(NAM : NAME);
VAR I  : INTEGER; 
BEGIN 
  FOR I:=0 TO 13
  DO
    PUTCHR(OUT,NAM[I]); 
  PUTCHR(OUT,EOLNCH); 
END;











PROCEDURE EXPANDTOKEN(OUT : ACTPTR ; VAR DTOP : DICTPTR); 
(*
 * TOKENISE INPUT 
 * AND EXPAND MACROS
 *) 
VAR CH : CHARACTER; 
    POS : INTEGER;
    TNAME : NAME; 
    QUOTED : BOOLEAN; 
    DELMTR : CHARACTER; 
    TEMP : DICTPTR; 
    XPARAM : PARAMPTR;
    NOOFPARAMS : INTEGER; 




PROCEDURE GETPARAMS;
VAR DELMTR : CHARACTER; 
    ACT1 : ACTPTR;
    PARENLEVEL : INTEGER; 
    CH : CHARACTER; 
    TEMP : PARAMPTR;
    POS : INTEGER;







PROCEDURE PUTINBODY(CH : CHARACTER);
BEGIN 
  IF COPY 
  THEN BEGIN
         TEMP^.PARAMLINE[POS]:=CH;
         IF POS<PARAMSIZE 
         THEN POS:=SUCC(POS); 
       END; 
END;




PROCEDURE SCAN; 
VAR DELMTR : CHARACTER; 
    QUOTED : BOOLEAN; 
BEGIN 
  PUTINBODY(LPAREN);
  CH:=GETCH;
  WHILE (CH <>RPAREN ) AND (CH <>EOFCH) AND (CH<>EOLNCH)
  DO BEGIN
       IF CH = LPAREN 
       THEN BEGIN 
              SCAN; 
            END 
       ELSE IF (CH=DQUOTE) OR (CH = SQUOTE) 
       THEN BEGIN 
              DELMTR:=CH; 
              PUTINBODY(DELMTR);
              REPEAT
                CH:=GETCH;
                IF CH=BSLASH
                THEN BEGIN
                       PUTINBODY(BSLASH); 
                       CH:=GETCH; 
                       PUTINBODY(CH); 
                       CH := BSLASH;
                     END
                ELSE PUTINBODY(CH); 
              UNTIL (CH=DELMTR) OR (CH=EOFCH) OR (CH=EOLNCH); 
              IF CH=DELMTR THEN CH:=GETCH;
            END 
       ELSE BEGIN 
              PUTINBODY(CH);
              CH:=GETCH;
            END;
     END; 
  IF CH=RPAREN
  THEN BEGIN
         PUTINBODY(RPAREN); 
         CH:=GETCH; 
       END; 
END;

BEGIN (* GETPARAMS *) 
  CH:=GETCH; (* READ THE '(' *) 
  CH:=CHN;
  NOOFPARAMS := 1;
  WHILE (CH<>RPAREN) AND (CH<>EOFCH)
  DO BEGIN
       CH:=CHN; 
        TEMP:=XPARAM; 
        IF TEMP<>NIL
        THEN BEGIN
               WHILE TEMP^.NEXT<>NIL
               DO TEMP:=TEMP^.NEXT; 
             END; 
        IF TEMP=NIL 
        THEN BEGIN
               NEW(XPARAM); 
               TEMP:=XPARAM;
             END
        ELSE BEGIN
               NEW(TEMP^.NEXT); 
               TEMP:=TEMP^.NEXT;
             END; 
        (* TEMP ^1OINTS TO A NEW RECORD IN THE LIST *)
        (* NOW COPY THE PARAMETER TEXT INTO THE BODY *) 
        TEMP^.NEXT:=NIL;
        POS:=0; 
        WHILE (CH<>COMMA) AND (CH<>EOFCH) AND (CH<>RPAREN)
        DO BEGIN
             CH:=CHN; 
             IF (CH = DQUOTE ) OR ( CH=SQUOTE)
             THEN BEGIN 
                    DELMTR:=CH; 
                    PUTINBODY(CH);
                    REPEAT
                         CH:=GETCH; 
                         IF CH=BSLASH 
                         THEN BEGIN 
                                PUTINBODY(BSLASH);
                                CH:= GETCH; 
                                PUTINBODY(CH);
                                CH:=BSLASH;     (* HIDE THE ESCAPED CHAR *) 
                              END 
                         ELSE PUTINBODY(CH);
                    UNTIL (CH=DELMTR) OR (CH = EOFCH);
                   IF CH=DELMTR THEN CH:=GETCH; 
                  END 
             ELSE IF CH=LPAREN
                  THEN BEGIN
                         SCAN;
                         CH:=CHN; 
                       END
                  ELSE
                       BEGIN
                         PUTINBODY(CH); 
                         CH:=GETCH; 
                       END; 
           END; 
      IF CH=COMMA 
      THEN CH:=GETCH; 
        PUTINBODY(EOFCH); 
        NOOFPARAMS:=SUCC(NOOFPARAMS); 
    END;
    IF CH=RPAREN
    THEN CH:=GETCH; 
END; (*GETPARAMS *) 




BEGIN 
  CH:=CHN;
  IF (CH=DQUOTE) OR ( CH=SQUOTE)
  THEN BEGIN
         DELMTR:=CH;
         PUTCHR(OUT,DELMTR);
         REPEAT 
              CH:=GETCH;
              IF CH = BSLASH
              THEN BEGIN
                     PUTCHR(OUT,BSLASH);
                     CH:= GETCH;
                     PUTCHR(OUT,CH);
                     CH:= BSLASH; 
                   END
              ELSE PUTCHR(OUT,CH);
         UNTIL (CH=DELMTR) OR (CH=EOFCH) OR (CH=EOLNCH);
         IF CH=DELMTR THEN CH:=GETCH; 
       END
  ELSE IF (CH=SLASH)
  THEN BEGIN
         PUTCHR(OUT,CHN); 
         CH:=GETCH; 
         IF CH=ASTERIX
         THEN BEGIN (* COMMENT *) 
                PUTCHR(OUT,CH); 
                CH:=GETCH;
                QUOTED:=TRUE; 
                WHILE QUOTED AND (CH <> EOFCH)
                DO BEGIN
                      PUTCHR(OUT,CH); 
                     IF CH=ASTERIX
                     THEN BEGIN 
                            CH:=GETCH;
                            CH:=CHN;
                            IF CH=SLASH 
                            THEN BEGIN
                                   QUOTED:=FALSE; 
                                   PUTCHR(OUT,CHN); 
                                 END; 
                          END 
                     ELSE CH:=GETCH;
                   END; 
                IF CH = SLASH THEN CH:= GETCH;   (* GET NEXT CHAR *)
              END;
      END 
  ELSE IF ISALPHA(CH) OR(CH=UNDERSCORE) 
  THEN BEGIN
         GETWORD(TNAME);
         TEMP:=ISDEF(TNAME,DTOP); 
         IF TEMP=NIL
         THEN BEGIN (* NOT A MACRO *) 
                POS:=0; 
                WHILE POS<14
                DO BEGIN
                     PUTCHR(OUT,TNAME[POS]);
                     POS:=SUCC(POS);
                     IF POS<14
                     THEN BEGIN 
                          IF TNAME[POS]=SPACE 
                          THEN POS:=14; 
                          END 
                   END; 
                CH:=CHN;
(*              WHILE ALPHANUMERIC(CH)
                DO BEGIN
                     PUTCHR(OUT,GETCH); 
                     CH:=CHN; 
                   END;   *)
              END 
         ELSE BEGIN 
                (* PRCSS MACRO CALL *)
                XPARAM:=NIL;
                CH:=CHN;
                NOOFPARAMS:=0;
                IF (CH = SPACE) AND (TEMP^.MACNUMBER<>0)
                THEN WHILE CH = SPACE DO CH:=GETCH; 
                IF (CH=LPAREN) AND (TEMP^.MACNUMBER<>0) 
                THEN BEGIN
                       (* GET PARAMETERS *) 
                       GETPARAMS; 
                     END; 
                (* ACTIVATE CALL *) 
                IF CURRENT <> NIL      (* BACKUP THE LAST CHARACTER *)
                THEN WITH CURRENT^
                DO BEGIN
                     POSITION:=PRED(POSITION);
                   END; 
                IF NOOFPARAMS=(TEMP^.MACNUMBER) 
                THEN BEGIN
                     NEW(CURRENT^.FRWD);
                     CURRENT^.FRWD^.BACK:=CURRENT;
                     CURRENT:=CURRENT^.FRWD;
                     WITH CURRENT^
                     DO BEGIN 
                          FRWD:=NIL;
                          EXPTYP:=MACRO;
                          NEW(THISMAC); 
                          THISMAC^.MACBODY:=TEMP^.MACDICT^.MACBODY; 
                          THISMAC^.PARAMLIST:=XPARAM; 
                          POSITION:=0;
                          CH:=GETCH;    (* READ THE FIRST CHARACTER *)
                        END;
                   END
              ELSE BEGIN
                     DUMPSTK(CURRENT);
                     WRITELN(' WRONG NUMBER OF PARAMETERS');
                     WRITE('IN MACRO CALL  ');
                     FOR POS:=0 TO 13 DO WRITE(MAP[TNAME[POS]]);
                     WRITELN; 
                   END; 
              END;
       END
  ELSE IF CH=SPACE
  THEN BEGIN
         WHILE CH=SPACE 
         DO BEGIN 
              PUTCHR(OUT,SPACE);
              CH:=GETCH;
            END;
       END
(* TOKENISE NUMBERS *)
  ELSE IF (CH >= ZERO) AND (CH <= NINE) 
  THEN BEGIN
         WHILE((CH >= ZERO) AND (CH <= NINE)) OR
              ((CH >= BIGA) AND (CH <= BIGA+5))OR 
              ((CH >= LITTLEA)AND(CH <= LITTLEA+5))OR 
              (CH = LITTLEA+25) OR
              (CH = BIGA+25)
         DO BEGIN 
              PUTCHR(OUT,CH); 
              CH := GETCH;
            END;
         IF( CH = BIGA+11) OR (CH = LITTLEA+11) 
         THEN BEGIN   (* LONG NUMBER *) 
                PUTCHR(OUT,CH); 
                CH := GETCH;
              END 
         ELSE IF(CH = DOT)
         THEN BEGIN   (* EXPONENT *)
                PUTCHR(OUT,CH); 
                CH := GETCH;
                WHILE((CH >= ZERO)AND(CH <= NINE))OR
                      (CH = PLUS) OR
                      (CH = MINUS)OR
                      (CH = BIGA+4)OR 
                      (CH = LITTLEA+4)
                DO BEGIN
                     PUTCHR(OUT,CH);
                     CH := GETCH; 
                   END; 
              END;
       END
  ELSE IF CH<>EOLNCH
  THEN WHILE (CH <> DQUOTE) AND (CH <> SQUOTE) AND (CH <> SLASH)
         AND NOT(ISALPHA(CH)) AND (CH <> UNDERSCORE)
         AND (CH <> EOFCH) AND (CH<>EOLNCH) 
       DO BEGIN 
            PUTCHR(OUT,CH); 
            CH:=GETCH;
          END;
END; (* EXPANDTOKEN *)




PROCEDURE PRCSSDEFINE;
VAR CH : CHARACTER; 
    DELMTR : CHARACTER; 
    MDICT :  DICTPTR; 
    I : INTEGER;
    POS : INTEGER;
    QUOTED : BOOLEAN; 
    TEMP : DICTPTR; 
    TNAME : NAME; 

  PROCEDURE GETPARAMS;
  VAR TEMP : DICTPTR; 
    PARAMNUMBER : INTEGER;
  BEGIN 
    PARAMNUMBER :=1;
    CH:=GETCH;
    CH:=CHN;
    WHILE (CH<>RPAREN) AND (CH<>EOFCH)
    DO BEGIN
         IF CH = SPACE
         THEN SKIPBLANKS; 
         CH := CHN; 
         IF ALPHANUMERIC(CH)
         THEN BEGIN 
                NEW(TEMP);
                WITH TEMP^
                DO BEGIN
                     GETWORD(DICTNAME); 
                     HASHTAB[HASH(DICTNAME)]:=TRUE; 
                     DICTTYP:=PARAMETER;
                     MACNUMBER:=PARAMNUMBER;
                     PARAMNUMBER:=SUCC(PARAMNUMBER);
                   END; 
                DICTENTER(TEMP,DTEMP);
              END;
         CH:=CHN; 
         WHILE (CH<>COMMA) AND (CH<>RPAREN) AND (CH<>EOFCH) 
         DO BEGIN 
              CH:=GETCH;
              CH:=CHN;
            END;
         IF CH=COMMA
         THEN CH:=GETCH;
         CH:=CHN; 
       END; 
    CH:=GETCH;
  MDICT^.MACNUMBER:=PARAMNUMBER;
  END; (* GETPARAMS *)






  PROCEDURE PUTINBODY(CH : INTERCODE);
  BEGIN 
    IF COPY 
    THEN BEGIN
           MDICT^.MACDICT^.MACBODY^[POS]:=CH; 
           IF POS<BODYSIZE
           THEN POS:=SUCC(POS); 
         END; 
  END;


 BEGIN
   POS:=0;
   DTEMP:=NIL;
   SKIPBLANKS;
   CH:=CHN; 
   IF ALPHANUMERIC(CH)
   THEN BEGIN 
          NEW(MDICT); 
          WITH MDICT^ 
          DO BEGIN
               MEXPAND := FALSE;
               MACNUMBER:=0;
               GETWORD(DICTNAME); 
               HASHTAB[HASH(DICTNAME)]:=TRUE; 
               DICTTYP:=MACRO;
               NEW(MACDICT);
               WITH MACDICT^
               DO BEGIN 
                    PARAMLIST:=NIL; 
                    NEW(MACBODY); 
                    MACBODY^[0]:=EOFCH; 
                  END;
               CH:=CHN; 
               WHILE ALPHANUMERIC(CH) 
               DO BEGIN 
                    CH:=GETCH;
                    CH:=CHN;
                  END;
               IF CH=LPAREN 
               THEN BEGIN (* PRCSS PARAMETERS *)
                    (*   *) 
                      GETPARAMS;
                    END;
                SKIPBLANKS; 
               CH:=CHN; 

 (*  PRCSS BODY *)

               WHILE (CH <>EOFCH) AND (CH <>EOLNCH) 
               DO BEGIN 
                                     (*   *)
                    CH:=CHN;
(* THIS HAS BEEN TAKEN OUT SINCE PARAMETERS ARE EXPANDED INTO STRINGS *)
(*                  IF (CH=DQUOTE) OR ( CH=SQUOTE)
                    THEN BEGIN
                           DELMTR:=CHN; 
                           PUTINBODY(DELMTR); 
                           REPEAT 
                                CH:=GETCH;
                                IF CH = BSLASH
                                THEN BEGIN
                                       PUTINBODY(CH); 
                                       CH:=GETCH; 
                                       PUTINBODY(CH); 
                                       CH:=BSLASH;
                                     END
                                ELSE PUTINBODY(CH); 
                           UNTIL (CH=DELMTR) OR (CH=EOFCH) OR (CH=EOLNCH);
                           IF CH=DELMTR THEN CH:=GETCH; 
                         END
                    ELSE *)IF CH=SLASH
                    THEN BEGIN
                           CH:=GETCH; 
                           CH:=CHN; 
                           IF CH<>ASTERIX 
                           THEN PUTINBODY(SLASH)
                           ELSE BEGIN 
                                  CH:=GETCH;
                                  QUOTED:=TRUE; 
                                  WHILE QUOTED
                                  DO BEGIN
                                       CH:=CHN; 
                                       IF CH=ASTERIX
                                       THEN BEGIN 
                                              CH:=GETCH;
                                              CH:=CHN;
                                              IF CH=SLASH 
                                              THEN BEGIN
                                                     QUOTED:=FALSE; 
                                                     CH:=GETCH; 
                                                   END
                                            END 
                                       ELSE CH:=GETCH;
                               END; 
                          END;
                     END
                    ELSE IF ISALPHA(CH) OR(CH=UNDERSCORE) 
                    THEN BEGIN
                           GETWORD(TNAME);
                           TEMP:=ISDEF(TNAME,DTEMP);
                           IF TEMP=NIL
                           THEN BEGIN (* NOT A MACRO *) 
                                  I:=0; 
                                  WHILE I<14
                                  DO BEGIN
                                       PUTINBODY(TNAME[I]); 
                                       I:=SUCC(I);
                                       IF I<14
                                       THEN BEGIN 
                                            IF TNAME[I]=SPACE 
                                            THEN I:=14; 
                                            END 
                                     END; 
                                  CH:=CHN;
                                  WHILE ALPHANUMERIC(CH)
                                  DO BEGIN
                                       PUTINBODY(CH); 
                                       CH:=GETCH; 
                                     END; 
                                END 
                           ELSE BEGIN 
                                  (* PRCSS MACRO CALL *)
                                  PUTINBODY(128+TEMP^.MACNUMBER); 
                                END;
                         END
                    ELSE IF CH=BSLASH 
                    THEN BEGIN
                           CH:=GETCH; 
                           CH:=CHN; 
                           IF CH=SPACE
                           THEN BEGIN 
                                   PUTINBODY(CH); 
                                   CH:=GETCH; 
                                   IF CH=EOLNCH 
                                   THEN PUTINBODY(CH);
                                   CH:=GETCH; 
                                END 
                           ELSE IF CH=EOLNCH
                           THEN BEGIN 
                                  PUTINBODY(CH);
                                  CH:=GETCH;
                                END 
                           ELSE BEGIN 
                                  PUTINBODY(BSLASH);
                                  PUTINBODY(CH);
                                  CH:=GETCH;
                                END;
                         END
                    ELSE IF CH<>EOLNCH
                    THEN BEGIN
                           PUTINBODY(CH); 
                           CH:=GETCH; 
                         END; 
                    CH:=CHN;
                  END;
               IF CH=EOLNCH 
               THEN CH:=GETCH;
               MACDICT^.MACBODY^[POS]:=EOFCH; 
             END; 
             DICTENTER(MDICT,DTOP); 
        END;
   TEMP:=DTEMP; 
   WHILE TEMP<>NIL
   DO BEGIN 
        DTEMP:=TEMP^.BACK;
        DISPOSE(TEMP);
        TEMP:=DTEMP;
      END;
  MEXPAND := TRUE;
 END; (* PRCSSDEFINE *) 



PROCEDURE PUSHCOPY; 
BEGIN 
  COPYSTK[IFLEVEL] := COPY; 
END;

FUNCTION POPCOPY:BOOLEAN; 
BEGIN 
  POPCOPY := COPYSTK[IFLEVEL];
END;


PROCEDURE PRCSSCOMMAND; 
VAR ERROR : BOOLEAN;
    CNAME : NAME; 
    TEMP : DICTPTR; 
    CH : CHARACTER; 
BEGIN 
  ERROR:=FALSE; 
  SKIPBLANKS; 
  CH:=CHN;
  IF NOT(ALPHANUMERIC(CH))
  THEN
    ERROR:=TRUE 
  ELSE BEGIN
         GETWORD(CNAME);
         TEMP:=ISDEF(CNAME,CTOP); 
         IF TEMP=NIL
         THEN ERROR:=TRUE 
         ELSE WITH TEMP^
              DO BEGIN
                   CASE MACNUMBER 
                   OF 
                     1 : BEGIN (* INCLUDE *)
                          IF COPY THEN
                           PRCSSINCLUDE;
                         END; 

                     2 : BEGIN (* DEFINE *) 
                          IF COPY THEN
                           PRCSSDEFINE; 
                         END; 
                     3 : BEGIN (* IFDEF *)
                           SKIPBLANKS;
                           GETWORD(CNAME);
                           TEMP:=ISDEF(CNAME,DTOP); 
                           PUSHCOPY;
                           IF COPY
                           THEN BEGIN 
                                  COPY := TEMP<>NIL;
                           IF COPY THEN LASTLEVEL:=IFLEVEL; 
                                END;
                           IFLEVEL:=IFLEVEL+1;
                           SKIPTOEOLN;
                         END; 
                     4 : BEGIN       (* IFNDEF *) 
                           SKIPBLANKS;
                           GETWORD(CNAME);
                           TEMP:=ISDEF(CNAME,DTOP); 
                           PUSHCOPY;
                           IF COPY
                           THEN BEGIN 
                                  COPY := TEMP =NIL;
                                 IF COPY THEN LASTLEVEL:=IFLEVEL; 
                                END;
                           IFLEVEL:=IFLEVEL+1;
                           SKIPTOEOLN;
                         END; 
                     5 : BEGIN (* ELSE *) 
                           IF IFLEVEL > 0 
                           THEN BEGIN 
                                  IF COPYSTK[IFLEVEL-1] 
                                  THEN COPY := NOT(COPY); 
                                END 
                           ELSE WRITELN('#ELSE WITHOUT #IF...');
                           SKIPTOEOLN;
                         END; 
                     6 : BEGIN (* ENDIF *)
                           IF IFLEVEL> 0 THEN IFLEVEL:=IFLEVEL-1; 
                           COPY := POPCOPY; 
                           IF LASTLEVEL>IFLEVEL THEN LASTLEVEL:=IFLEVEL;
                           SKIPTOEOLN;
                         END; 

                   OTHERWISE ERROR:=TRUE; 
                   END; (* CASE *)
                 END; 
       END; 
  IF ERROR
  THEN BEGIN
         IF CH <> EOLNCH
         THEN CH:=GETCH;
         WHILE (CH<>EOLNCH) AND ( CH<>EOFCH)
         DO BEGIN 
              CH:=CHN;
              IF CH=BSLASH
              THEN BEGIN
                     CH:=GETCH; 
                     CH:=CHN; 
                     IF CH = EOLNCH 
                     THEN CH:=GETCH 
                     ELSE IF CH = SPACE 
                     THEN BEGIN 
                            CH:=GETCH;
                            IF CHN=EOLNCH 
                            THEN CH:=GETCH; 
                          END 
                    END;
              CH:=GETCH;
            END;
       END; 
END; (* PRCSSCOMMAND *) 





PROCEDURE EXPAND; 
BEGIN 
  CH:=CHN;
  WHILE (CH<>EOFCH) AND (CH<>EOLNCH)
  DO BEGIN
       EXPANDTOKEN(OUT,DTOP); 
       CH:=CHN; 
     END; 
  IF CH=EOLNCH
  THEN CH:=GETCH; 
  PUTCHR(OUT,EOLNCH); 
END; (* EXPAND *) 





PROCEDURE ADDCOM(MCNUM:INTEGER;CH1,CH2,CH3,CH4,CH5,CH6,CH7,CH8:INTERCODE);
VAR I : INTEGER;
BEGIN 
  NEW(CTOP^.FRWD);
  CTOP^.FRWD^.BACK:=CTOP; 
  CTOP:=CTOP^.FRWD; 
  WITH CTOP^
  DO BEGIN
       FOR I:=0 TO 13 DO DICTNAME[I]:=SPACE;
       DICTTYP:=MACRO;
       MACDICT:=NIL;
       MACNUMBER:=MCNUM;
       FRWD:=NIL; 
       DICTNAME[0]:=CH1;
       DICTNAME[1]:=CH2;
       DICTNAME[2]:=CH3;
       DICTNAME[3]:=CH4;
       DICTNAME[4]:=CH5;
       DICTNAME[5]:=CH6;
       DICTNAME[6]:=CH7;
       DICTNAME[7]:=CH8;
       HASHTAB[HASH(DICTNAME)]:=TRUE; 
     END; 
END;
PROCEDURE INIT; 
VAR I : CHARACTER ; 
    FNAME : ALFA; 
    J : INTEGER;
BEGIN (* INIT *)
  FOR I:=0 TO 127 DO MAP[I]:='Z'; 
  FOR I:=BIGA TO BIGZ DO MAP[I]:=CHR(I-BIGA+ORD('A'));
  FOR I:=LITTLEA TO LITTLEZ DO MAP[I]:=CHR(I-LITTLEA+ORD('A')); 
  FOR I:=ZERO TO NINE DO MAP[I]:=CHR(I-ZERO+ORD('0'));



  (* REST OF INIT 
  *)
  RESET(INPUT); 
  NEW(CURRENT); 
  WITH CURRENT^ 
  DO BEGIN
       FRWD:=NIL; 
       BACK:=NIL; 
       EXPTYP:=FYLE;
       POSITION:=1; 
       THISFILE:=NIL; 
       NEW(THISLINE); 
     END; 
  GETALINE(NIL,CURRENT^.THISLINE^); 
  NEW(OUT); 
  WITH OUT^ 
  DO BEGIN
       EXPTYP:=FYLE;
       FRWD:=NIL; 
       BACK:=NIL; 
       POSITION:=0; 
          NEW(THISFILE);
         FILENAME(INFILE,FNAME);
          OPEN(THISFILE^,FNAME,TRUE); 
       NEW(THISLINE); 
     END; 
REWRITE(OUT^.THISFILE^);

  DTOP:=NIL;
  CTOP:=NIL;
  DTEMP:=NIL; 
  FOR J:= 0 TO 511 DO HASHTAB[J]:=FALSE;

  NEW(CTOP);
  FOR I:=0 TO 13
  DO CTOP^.DICTNAME[I]:=SPACE;
  WITH CTOP^
  DO BEGIN
       FRWD:=NIL; 
       BACK:=NIL; 

       DICTNAME[0]:=LITTLEA+8; (* I *)
       DICTNAME[1]:=LITTLEA+13;(* N *)
       DICTNAME[2]:=LITTLEA+2 ;(* C *)
       DICTNAME[3]:=LITTLEA+11;(* L *)
       DICTNAME[4]:=LITTLEA+20;(* U *)
       DICTNAME[5]:=LITTLEA+3 ;(* D *)
       DICTNAME[6]:=LITTLEA+4 ;(* E *)
       DICTTYP:=MACRO;
       MACDICT:=NIL;
       MACNUMBER:=1;
       HASHTAB[HASH(DICTNAME)]:=TRUE; 
     END; 
       ADDCOM(2,
       LITTLEA+3 ,
       LITTLEA+4 ,
       LITTLEA+5 ,
       LITTLEA+8 ,
       LITTLEA+13,
       LITTLEA+4 ,
       SPACE,SPACE);

  ADDCOM(3,LITTLEA+8,LITTLEA+5,LITTLEA+3,LITTLEA+4,LITTLEA+5,SPACE,SPACE,SPACE);  (* IFDEF *) 
  ADDCOM(4,LITTLEA+8,LITTLEA+5,LITTLEA+13,LITTLEA+3,LITTLEA+4,LITTLEA+5,SPACE,SPACE);    (* IFNDEF*)
  ADDCOM(5,LITTLEA+4,LITTLEA+11,LITTLEA+18,LITTLEA+4,SPACE,SPACE,SPACE,SPACE);  (* ELSE *)
  ADDCOM(6,LITTLEA+4,LITTLEA+13,LITTLEA+3,LITTLEA+8,LITTLEA+5,SPACE,SPACE,SPACE);   (* ENDIF *) 
  ADDCOM(3,LITTLEA+8,LITTLEA+5,SPACE,SPACE,SPACE,SPACE,SPACE,SPACE);  (* ^I^F   ^U^S^E ^I^F^D^E^F ^F^O^R ^N^O^W*) 
  COPY:=TRUE; 
  IFLEVEL:=0; 
  LASTLEVEL:=0; 

  INCOMMENT:=FALSE; 
  MEXPAND  := TRUE; 

END; (* INIT *) 





BEGIN (* MAIN *)
  INIT; 
  CH:=GETCH;
  WHILE CH<>EOFCH 
  DO BEGIN
              IF CH=HASHCH
              THEN BEGIN
                     CH:=GETCH; 
                     PRCSSCOMMAND;
                   END
              ELSE EXPAND;
              CH:=CHN;
            END;
  CLOSE(OUT^.THISFILE^);
END.
