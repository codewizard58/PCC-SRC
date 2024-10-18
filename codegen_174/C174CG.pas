(*$L'THE CYBER CODE GENERATOR'*)
(* CYBER C COMPILER *)
(***************************************************************************
 *                                                                         *
 *   CODE GENERATOR FOR THE CYBER 174. P.J CHURCHYARD 4-JUNE-1982          *
 *                                                                         *
 ***************************************************************************)
 
FUNCTION INDIRECT(N : INTEGER):INTEGER;FORWARD;
FUNCTION NEWEXPNODE(OPR:OPSYM;TOK:SYMMODE):EXPTREE;FORWARD;
FUNCTION SIZEOFTYPE(T : INTEGER):INTEGER;EXTERN;
FUNCTION TYPETREE(VAR T : EXPTREE):INTEGER;FORWARD;
PROCEDURE ERROR(I : ERRORCODES);EXTERN;
PROCEDURE WARN(I : ERRORCODES);EXTERN;
PROCEDURE PREVIOUSLABEL;FORWARD;
PROCEDURE DISCARD(T:EXPTREE);EXTERN;
PROCEDURE OUTEXPRESSION;FORWARD;
PROCEDURE OUTEXPTREE(T : EXPTREE;LEFT : BOOLEAN;VAR LAB : INTEGER);FORWARD;
PROCEDURE PREVUNKNOWN;EXTERN;
FUNCTION POINTERTO(LAST : INTEGER):INTEGER;EXTERN;
FUNCTION GREATEROF(A,B : INTEGER):INTEGER;EXTERN
;FUNCTION OUTLITERAL(S : EXPTREE):INTEGER; EXTERN;
PROCEDURE OUTALFA(STRNG:ALFA;LENG : INTEGER);FORWARD;
FUNCTION GIVELABEL:INTEGER;FORWARD;
FUNCTION OUTSTATIC(T : EXPTREE):INTEGER;EXTERN;
 
PROCEDURE (*$E'CMESSAG'*) CMESS(I : ALFA);EXTERN; 
 
(**************************************************************************
 *                                                                        *
 * END OF EXTERNALS AND FORWARD REFERENCES.                               *
 *                                                                        *
 **************************************************************************)
 
 
 
FUNCTION SIZEOFWORD:INTEGER;
BEGIN
  SIZEOFWORD:=60;
END;
 
 
 
FUNCTION SIZEOFFIELD:INTEGER;
BEGIN
  SIZEOFFIELD:=60;
END;
 
 
PROCEDURE STACKSET;
BEGIN
  WRITELN(CODE,'** STACK SET');
END;
 
 
FUNCTION BASICSIZE(TYP : INTEGER):INTEGER;
BEGIN
  CASE TYP
  OF 0,1,2 : BASICSIZE :=1;
     3,4   : BASICSIZE :=1;
     5,6,8 : BASICSIZE :=1;
     7     : BASICSIZE:=1;
  OTHERWISE BASICSIZE:=1;
  END;
END;
 
 
PROCEDURE OUTNAME(STRNG:ALFA);
VAR I : INTEGER;
BEGIN
  FOR I:=1 TO 7
  DO WRITE(CODE,STRNG[I]);
  WRITE(CODE,'   ');
END;
 
 
 
 
PROCEDURE PRTTOKN(T : EXPTREE); 
BEGIN 
  IF T = NIL THEN WRITE(CODE,'*NIL')
  ELSE WITH T^
  DO BEGIN
       CASE T^.TOKN 
       OF 
          IDENT : WRITE(CODE,'*IDENT(',T^.SENTRY^.IR.NAME,')'); 
          KONSTANT : WRITE(CODE,'*KONST ',T^.SVALUE); 
          INDEXOP : WRITE(CODE,'*INDEXOP'); 
          BINARY: WRITE(CODE,'*BINARY');
          UNARY : WRITE(CODE,'*UNARY'); 
          ASSIGN: WRITE(CODE,'*ASSIGN');
          CALL : WRITE(CODE,'*CALL'); 
          PARAM: WRITE(CODE,'*PARAM');
          PARAMETER:WRITE(CODE,'*PARAMETER'); 
       OTHERWISE WRITE(CODE,'*ORD(TOKN)=',ORD(TOKN)); 
       END; (* CASE *)
        WRITE(CODE,' - ');


       CASE T^.OPER 
       OF 
          INDEX : WRITE(CODE,'INDEX');
          SELECTOR:WRITE(CODE,'SELECTOR');
          REFSELECTOR:WRITE(CODE,'REFSELECTOR');
          REF   : WRITE(CODE,'REF');
          DEREF : WRITE(CODE,'DEREF');
          PLUS  : WRITE(CODE,'PLUS'); 
          MINUS : WRITE(CODE,'MINUS');
          NEG   : WRITE(CODE,'NEG');
          GTOP  : WRITE(CODE,'GTOP'); 
          GEOP  : WRITE(CODE,'GEOP'); 
          LTOP  : WRITE(CODE,'LTOP'); 
          LEOP  : WRITE(CODE,'LEOP'); 
          EQOP  : WRITE(CODE,'EQOP'); 
          NEOP  : WRITE(CODE,'NEOP'); 
          INCR  : WRITE(CODE,'INCR'); 
          DECR  : WRITE(CODE,'DECR'); 
          POSTINC:WRITE(CODE,'POSTINC');
          POSTDEC:WRITE(CODE,'POSTDEC');
          TIMES : WRITE(CODE,'TIMES');
       OTHERWISE WRITE(CODE,'ORD(OPER)=',ORD(OPER));
       END; 
     END;   (* WITH BEGIN *)
END;




PROCEDURE PRINTTYPE(N,DEPTH : INTEGER); 
BEGIN 
  IF DEPTH = 0 THEN WRITE(CODE,'*= TYPE(',N:1,')=='); 
  CASE TYPETAB[N].BASICTYPE 
  OF
    SHORT : WRITE(CODE,' SHORT ');
    KAR   : WRITE(CODE,' KAR ');
    UNSIGNED: WRITE(CODE,' UNSIGNED '); 
    INT   : WRITE(CODE,' INT ');
    LONG  : WRITE(CODE,' LONG '); 
    REEL  : WRITE(CODE,' REEL '); 
    DOUBLE: WRITE(CODE,' DOUBLE '); 
    POINTER: BEGIN
               WRITE(CODE,'*'); 
               PRINTTYPE(INDIRECT(N),DEPTH+1);
             END; 
    FIELD : WRITE(CODE,' FIELD ');
    ARRAYTYPE: BEGIN
                 WRITE(CODE,'[',(TYPETAB[N].INDEXRANGE):1,']'); 
                 PRINTTYPE(TYPETAB[N].TYPENO,DEPTH+1);
               END; 
    FUNK : BEGIN
             WRITE(CODE,' FUNCTION RETURNING ');
             PRINTTYPE(TYPETAB[N].RETRN,DEPTH+1); 
           END; 
    UNION : WRITE(CODE,' UNION ');
    STRUCT: WRITE(CODE,' STRUCT '); 
    OTHERWISE WRITE(CODE,' UNKNOWN TYPE '); 
  END;
  IF DEPTH = 0 THEN WRITELN(CODE);
END;

PROCEDURE PRINTTREE(T : EXPTREE; DEPTH : INTEGER);
VAR CNT : INTEGER;
BEGIN 
  WRITE(CODE,'*-'); 
  FOR CNT:= 1 TO DEPTH DO WRITE(CODE,'-');
         WRITE(CODE,'(',T^.EMODE:1,')  ');
  PRTTOKN(T); WRITELN(CODE);
  IF (T^.LFT <> NIL)OR(T^.RGHT <> NIL)
  THEN BEGIN
         IF T^.LFT <> NIL 
         THEN PRINTTREE(T^.LFT,DEPTH+2);
         IF T^.RGHT <> NIL
         THEN PRINTTREE(T^.RGHT,DEPTH+2); 
       END; 
END;


PROCEDURE PRINTLITERAL(LREC : EXPRECORD);
BEGIN
  WITH LREC
  DO CASE TOKN
  OF
     KONSTANT : CASE TYPETAB[EMODE].BASICTYPE
                OF
                   SHORT,KAR,INT,LONG,UNSIGNED: 
                       WRITELN(CODE,'          DATA     ',SVALUE:1);
 
                   REEL,DOUBLE: 
                       WRITELN(CODE,'          DATA     ',RVALUE);
 
                   ARRAYTYPE: 
                       WRITELN(CODE,'          BSSZ     ',SVALUE:1);
                   OTHERWISE
                       WRITELN(CODE,'          BSSZ     ',SIZEOFTYPE(EMODE):1); 
                END;
 
      IDENT :   BEGIN
                  WRITE(CODE,'          VFD     42/,18/=X');
(* 17-OCT-84 *) (*OUTNAME(LID);*)OUTALFA(LID,7);
                  WRITELN(CODE,'+',IDOFSET:1);
                END;
 
      OTHERWISE BEGIN
                  IF OPER = REF
                  THEN WRITELN(CODE,'          VFD     42/,18/LIT.LIT+',SVALUE:1);
                END;
    END;
END;
 
 
 
PROCEDURE PRINTSTATIC;
BEGIN
  STATRECORD.TOKN:=KONSTANT;
  STATRECORD.EMODE:=ORD(INT);
  STATRECORD.SVALUE:=0;
  IF STATICSIZE<>0
  THEN BEGIN
         WRITE(STATFIL,STATRECORD);
 
       (* THIS MAKES SURE THAT SOMETHING WAS WRITTEN INTO THE FILE
        *)
         RESET(STATFIL);
         WRITELN(CODE,'STATIC    BSS     0');
         WHILE NOT(EOF(STATFIL))
         DO BEGIN
              READ(STATFIL,STATRECORD);
              IF NOT(EOF(STATFIL))
              THEN
              PRINTLITERAL(STATRECORD);
            END;
         RESET(STATFIL);REWRITE(STATFIL);
       END;
  IF LITPOS<>0
  THEN BEGIN
         WRITELN(CODE,'LIT.LIT   BSS     0');
         WRITE(LITFILE,STATRECORD);
         RESET(LITFILE);
         WHILE NOT(EOF(LITFILE))
         DO BEGIN
              READ(LITFILE,STATRECORD);
              IF NOT(EOF(LITFILE))
              THEN
              PRINTLITERAL(STATRECORD);
            END;
         RESET(LITFILE);
         REWRITE(LITFILE);
       END;
  LITPOS:=0;
  STATICSIZE:=0;
END;
(* PRINTSTATIC
 *)
 
 
 
PROCEDURE MAKESTAT(VAR ID : ALFA);
  VAR I : INTEGER;
  BEGIN
    I:=7;
     WHILE ID[I]=' ' DO I:=I-1;
    IF I<>7 THEN I:=I+1;
    ID[I]:='$';
  END;
 
 
 
PROCEDURE DATAHED(NAME : ALFA);
VAR ID : ALFA;
    I  : INTEGER;
BEGIN
  ID:=NAME;
  WRITE(CODE,'          IDENT     ');
  OUTNAME(ID);WRITELN(CODE);
  WRITE(CODE,'          ENTRY     ');
  OUTNAME(ID);WRITELN(CODE);
  WRITE(CODE,'          COMMENT   ');OUTNAME(ID); 
  WRITELN(CODE,' - C COMPILER, ICCC-PJC 1984'); 
  OUTNAME(ID);WRITELN(CODE,'BSS     0');
  CMESS(ID);
END; (* DATAHEADER *)
 
 
 
 
 
FUNCTION ASSOCIATE(T:EXPTREE):ASSOC;
BEGIN
 IF T=NIL
 THEN ASSOCIATE:=NOTASS
 ELSE
  WITH T^ DO
    IF TOKN = ASSIGN THEN ASSOCIATE:=RIGHTASS
                     ELSE ASSOCIATE:=ASSOCIATIVITY[OPER];
END;
(* ASSOCIATE
 *)
 
 
 
 
 
 FUNCTION PRECEDENCE(T:EXPTREE):INTEGER;
 
(* IT T = NIL THEN 0.
 *    THIS STOPS PUSHTOKEN FROM FALLING OFF
 *    THE TREE .
 *)
 BEGIN
   IF T=NIL
   THEN
     PRECEDENCE:=-2
  ELSE IF T=BRACKETS[LEVEL+1]
(* STOPS WALKING INTO SUBEXPRESSIONS
 *)
       THEN PRECEDENCE:=-1
  ELSE IF T^.TOKN IN [IDENT,KONSTANT,STRING]
       THEN PRECEDENCE:=0
  ELSE IF T^.TOKN = ASSIGN
       THEN PRECEDENCE:=28
  ELSE WITH T^
  DO CASE TOKN OF 
      CALL                 : PRECEDENCE := 1; 
      INDEXOP              : PRECEDENCE := 1; 
      CAST                 : PRECEDENCE := 5; 
  OTHERWISE WITH T^ 
   DO CASE OPER OF
      INDEX                : PRECEDENCE := 1; 
      SELECTOR,REFSELECTOR : PRECEDENCE := 1 ;  (* 18-OCT-84 *) 
      POSTINC,POSTDEC      : PRECEDENCE := 1; 
      NOTOP,INCR,DECR,NEG,
      ONESCOMP,SIZEOF,REF  : PRECEDENCE := 5 ;
      DEREF                : PRECEDENCE :=5;
      TIMES,DIVIDE,MODOP   : PRECEDENCE := 6 ;
      PLUS,MINUS           : PRECEDENCE := 8 ;
      LEFTSHIFT,RIGHTSHIFT : PRECEDENCE := 10 ;
      LTOP,LEOP,GEOP,GTOP  : PRECEDENCE := 12 ;
      EQOP,NEOP            : PRECEDENCE := 14 ;
      ANDOP                : PRECEDENCE := 16 ;
      XOROP,COMPOP         : PRECEDENCE := 18 ;
      OROP                 : PRECEDENCE :=20 ;
      ANDFOP               : PRECEDENCE :=22 ;
      ORFOP                : PRECEDENCE :=24 ;
      IFOP,ELSEOP          : PRECEDENCE :=26 ;
      COMMAOP              : PRECEDENCE :=30 ;
      OTHERWISE              PRECEDENCE :=28 ;
(* ALL OTHER CASES AND ASSIGN
 *)
   END ;
(* CASE
 *)
  END;    (* CASE TOKN *) 
 END;
(* PRECEDENCE
 *)
 
 
 
 
FUNCTION INDIRECT(N : INTEGER):INTEGER;
BEGIN
  IF N=0
  THEN BEGIN
         WRITELN(LISTING,'**** TYPE 0 IN INDIRECT ');
         INDIRECT:=ORD(INT);
       END
  ELSE
  WITH TYPETAB[N]
  DO CASE BASICTYPE
     OF
       POINTER : INDIRECT:=TYPEPOINTER;
 
       ARRAYTYPE:INDIRECT:=TYPENO;
       STRUCT : BEGIN 
                  INDIRECT := N;
WRITELN(CODE,'*## INDIRECT OF STRUCT'); 
                END;
       UNION :  BEGIN 
                  INDIRECT := N;
WRITELN(CODE,'*## INDIRECT OF UNION');
                END;
 
       OTHERWISE INDIRECT:=N;
     END;
END;
 
 
 
 
 
 
 
 
 
(* HELPER ROUTINE TO LOCATE AN IDENTIFIER NODE IN THE EXPRESSION TREE *)

FUNCTION FINDIDENT(T : EXPTREE):EXPTREE;
BEGIN 
  FINDIDENT := NIL;    (* INIT THE FUNCTION *)
  IF T <> NIL 
  THEN WITH T^
  DO   BEGIN
         IF TOKN = IDENT
         THEN FINDIDENT := T
         ELSE IF OPER = REF 
         THEN FINDIDENT := FINDIDENT(T^.RGHT);
       END; 
END;
 
FUNCTION NEWEXPNODE(OPR:OPSYM;TOK:SYMMODE):EXPTREE;
VAR T : EXPTREE;
BEGIN
  NEW(T);
  WITH T^ 
  DO BEGIN
       RGHT:=NIL;
       LFT:=NIL;
       OPER:=OPR;
       TOKN:=TOK;
       EMODE:=ORD(INT);
       IF TOK = IDENT 
       THEN BEGIN 
              IDOFSET := 0; 
              SENTRY  := NIL; 
              LID  := '          '; 
            END;
     END;
  NEWEXPNODE:=T;
END;
(* NEW EXP NODE
 *)
 
 
 
FUNCTION SCALAR(TY:INTEGER):BOOLEAN;
BEGIN
  SCALAR:= TYPETAB[TY].BASICTYPE IN [SHORT,KAR,UNSIGNED,INT,LONG,REEL,DOUBLE,
                                     POINTER,FIELD];
END;


PROCEDURE PROCP1(VAR T:EXPTREE;VAR N:INTEGER);
VAR T1 : EXPTREE; 
  MODE : INTEGER; 
BEGIN
  IF T<>NIL
  THEN WITH T^
       DO BEGIN
            IF TOKN=PARAM
            THEN BEGIN
                   PROCP1(LFT,N);
                   OFSET:=N;
                   EMODE:=INDIRECT(TYPETREE(RGHT));
                   IF TYPETAB[EMODE].BASICTYPE IN[ARRAYTYPE,STRUCT,UNION] 
                   THEN BEGIN 
WRITELN(CODE,'*-PARAM ADDED REF. ',EMODE);
                          T1 := NEWEXPNODE(REF,UNARY);
                          T1^.EMODE := POINTERTO(T^.EMODE); 
                          T1^.RGHT := T^.RGHT;
                          T^.RGHT := T1;
                        END;
                   N:=N+1;
(* ONLY ON CYBER VERSION
 *)
                 END
            ELSE BEGIN
                   MODE:=INDIRECT(TYPETREE(T)); 
                   IF TYPETAB[MODE].BASICTYPE IN [ARRAYTYPE,STRUCT,UNION] 
                   THEN BEGIN 
WRITELN(CODE,'*-PARAM ADDED REF. ',EMODE);
                          T1 := NEWEXPNODE(REF,UNARY);
                          T1^.EMODE := POINTERTO(MODE); 
                          T1^.RGHT := T;
                          T := T1;
                        END;
                   N:=1;
                 END;
         END;
END;
 
 
FUNCTION TYPECALL(T : EXPTREE):INTEGER;
VAR T1 : INTEGER ;
    N  : INTEGER;
BEGIN
  N:=0;
  IF T = NIL
  THEN
      TYPECALL := ORD(INT)
  ELSE WITH T^
       DO IF TOKN = CALL
          THEN BEGIN
                 T1:=INDIRECT(TYPETREE(LFT));
                 IF TYPETAB[T1].BASICTYPE<>FUNK
                 THEN BEGIN
                        ERROR(12);
                        TYPECALL:=ORD(INT);
                      END
                 ELSE BEGIN
                        PROCP1(RGHT,N);
                        TYPECALL:=TYPETAB[T1].RETRN;
                      END;
               END
          ELSE TYPECALL:=ORD(INT);
  T^.PARAMSIZE:=N;
END;
(* TYPECALL
 *)
 
 
 
 
PROCEDURE SCLTREE(VAR T : EXPTREE ; N : INTEGER); 
 
(*
 * * "T" IS AN EXPRESSION TREE WHOSE VALUE IS TO BE ADDED TO A POINTER
 * * OF TYPE "N".
 *)
VAR T1,T2 : EXPTREE;
    N1    : INTEGER;
BEGIN
WRITELN(CODE,'*SCLTREE ',N);
   IF SIZEOFTYPE(INDIRECT(N))<>1
  THEN IF T^.TOKN=KONSTANT
  THEN T^.SVALUE:=T^.SVALUE*SIZEOFTYPE(INDIRECT(N)) 
  ELSE BEGIN
         T1:=NEWEXPNODE(NOOP,KONSTANT);
         T1^.EMODE := ORD(INT); 
         T2:=NEWEXPNODE(TIMES,BINARY);
         T2^.EMODE:=ORD(INT); 
         T2^.RGHT:=T1;
         T2^.LFT:=T;
         T:=T2;
  T1^.SVALUE:=SIZEOFTYPE(INDIRECT(N));
       END;
END;
(* SCALETREE
 *)
PROCEDURE SCALETREE(VAR T : EXPTREE; N : INTEGER);
VAR N1 : INTEGER; 
BEGIN 
  N1 := TYPETREE(T);
  SCLTREE(T,N); 
END;
 
 
 
PROCEDURE TYPEINDEX(VAR T:EXPTREE);
VAR N : INTEGER;
    T1 : EXPTREE;
    T2 : EXPTREE; 
BEGIN
  IF T^.TOKN=INDEXOP
  THEN WITH T^
  DO BEGIN
       IF LFT^.TOKN <> INDEXOP
       THEN BEGIN 
              TYPEINDEX(LFT); 
              N:=INDIRECT(LFT^.EMODE);
(* 18-OCT-1984 *) 
            IF (TYPETAB[N].BASICTYPE = ARRAYTYPE) AND (LFT^.OPER <> REF)
              THEN BEGIN
WRITELN(CODE,'*-INDEX ADDED REF. ',N);
                     T1 := NEWEXPNODE(REF,UNARY); 
                     T1^.RGHT := T^.LFT;
                     T1^.EMODE := N;
                     T^.LFT := T1;
                   END; 
            END 
       ELSE BEGIN 
              TYPEINDEX(LFT); 
              N := INDIRECT(LFT^.EMODE);
(* ADDED CODE 30-JULY-84 *) 
              IF TYPETAB[N].BASICTYPE = POINTER 
              THEN BEGIN
WRITELN(CODE,'*ADDED DEREF OPERATOR '); 
                     T1 := NEWEXPNODE(DEREF,UNARY); 
                     T1^.RGHT := T^.LFT;
                     T1^.EMODE := N;
                     T^.LFT := T1;
                   END; 
            END;
       IF TYPETAB[N].BASICTYPE IN [ARRAYTYPE,POINTER]
       THEN BEGIN
              SCALETREE(RGHT,N);
              TOKN:=BINARY;
              OPER:=PLUS;
              EMODE:=POINTERTO(INDIRECT(N));
(* 17-OCT-84 *) 
              IF LFT^.OPER = REF
              THEN BEGIN
                     T2 := LFT^.RGHT; 
                     IF (T2^.TOKN = IDENT)
                     AND(TYPETAB[N].BASICTYPE = ARRAYTYPE)
                     AND(RGHT^.TOKN = KONSTANT) 
                     THEN BEGIN 
                            WRITELN(CODE,'*# APPLYING INDEX OPTIMISATION'); 
                            T2^.IDOFSET := T^.RGHT^.SVALUE + T2^.IDOFSET; 
                            T1 := T^.LFT; 
                            T1^.EMODE := T^.EMODE;
                            T^.LFT := NIL;
                            DISCARD(T); 
                            T := T1;
                          END;
                  END;
           END
       ELSE BEGIN
              ERROR(13);
            END;
     END
  ELSE BEGIN
         N:=TYPETREE(T);
       END;
END;
(* TYPEINDEX
 *)
 
 
FUNCTION ARITHTYPE(VAR N : INTEGER ): BOOLEAN;
 
(* RATIONALISE THE TYPE GIVEN
 *)
 
(* CHAR , SHORT , LONG , UNSIGNED , INT -> INT
 *   FLOAT , DOUBLE                       -> DOUBLE
 *)
BEGIN
  IF ( N=ORD(REEL))OR(N=ORD(DOUBLE))
  THEN BEGIN
         N:=ORD(DOUBLE);
         ARITHTYPE:=TRUE;
       END
  ELSE IF (N<=ORD(LONG))AND(N>ORD(NULLTYPE)) OR (TYPETAB[N].BASICTYPE=FIELD)
  THEN BEGIN
         N:=ORD(INT);
         ARITHTYPE:=TRUE;
       END
  ELSE BEGIN
         ARITHTYPE:=FALSE;
       END;
END;
(* ARITHTYPE
 *)
 
 
 
FUNCTION TWOARITHTYPES(VAR T : EXPTREE): BOOLEAN;
VAR N,N1: INTEGER;
    T1 : EXPTREE ;
BEGIN
  IF T<> NIL
  THEN WITH T^
       DO BEGIN
            N:=TYPETREE(LFT);
            N1:=TYPETREE(RGHT);
            N:=INDIRECT(N);N1:=INDIRECT(N1);
            IF ARITHTYPE(N) AND ARITHTYPE(N1)
            THEN IF(N = ORD(DOUBLE))AND(N1=ORD(INT))
                 THEN BEGIN
(* COERSE THE RIGHT TREE
 *)
                        T1:=NEWEXPNODE(CONVERT,CAST);
                        T1^.EMODE:=ORD(DOUBLE); 
                        T1^.RGHT:=T^.RGHT;
                        RGHT:=T1;
                        EMODE:=N;
                        TWOARITHTYPES:=TRUE;
                      END
                 ELSE IF(N=ORD(INT))AND(N1=ORD(DOUBLE))
                 THEN BEGIN
                        T1:=NEWEXPNODE(CONVERT,CAST);
                        T1^.EMODE:=ORD(DOUBLE); 
                        T1^.RGHT:=T^.LFT; 
                        LFT:=T1;
                        EMODE:=ORD(DOUBLE);
                        TWOARITHTYPES:=TRUE;
                      END
                 ELSE BEGIN
                        TWOARITHTYPES:=TRUE;
                        EMODE:=N;
                      END
            ELSE
              TWOARITHTYPES:=FALSE;
          END
  ELSE
    TWOARITHTYPES:=FALSE;
END;
(* TWOARITHTYPES
 *)
 
 
 
FUNCTION UNARYOP(VAR T : EXPTREE ): BOOLEAN;
VAR N,N1 : INTEGER;
    T1: EXPTREE;
    FLAG:BOOLEAN;
BEGIN
  FLAG:=TRUE;
  IF T<>NIL
  THEN WITH T^
       DO BEGIN
            IF TOKN=CAST
            THEN BEGIN
                   TOKN:=UNARY;
                   OPER:=CONVERT;
                   N1:=INDIRECT(TYPETREE(RGHT));
                   IF EMODE> ORD(DOUBLE)
                   THEN EMODE:=POINTERTO(EMODE);
                   IF (EMODE <> ORD(REEL)) AND (EMODE <> ORD(DOUBLE))
                   THEN IF (N1<>ORD(REEL)) AND (N1<>ORD(DOUBLE))
                   THEN BEGIN
                          T1:=T^.RGHT;
                          DISPOSE(T);
                          T:=T1;
                        END;
                   N:=EMODE;
                 END
            ELSE IF OPER=DEREF
            THEN BEGIN
                   TOKN:=UNARY;
(* 17-OCT-84*)     N:=INDIRECT(TYPETREE(RGHT)); 
                   IF NOT(TYPETAB[N].BASICTYPE IN [INT,LONG,UNSIGNED,SHORT,KAR,ARRAYTYPE,POINTER])
                   THEN BEGIN
                          ERROR(14);
                        END
(* 19-OCT-84       ELSE N:=(INDIRECT(N)) *)  ;
                END
           ELSE IF OPER=REF
           THEN BEGIN
                  N:= POINTERTO(((TYPETREE(T^.RGHT)))); 
                  IF T^.RGHT^.OPER=DEREF
                  THEN BEGIN
                         T1:=T^.RGHT^.RGHT; 
                         DISPOSE(T^.RGHT);
                         DISPOSE(T);
                         T:=T1;
                       END;
                END
           ELSE IF OPER=SIZEOF
           THEN BEGIN
                  TOKN:=UNARY;
                  N:=INDIRECT(TYPETREE(RGHT));
                  T1:=NEWEXPNODE(NOOP,KONSTANT);
                  T1^.SVALUE:=SIZEOFTYPE(N);
                  T1^.EMODE:=ORD(INT);
                  DISPOSE(T);
                  T:=T1;
                  N:=ORD(INT);
                END
            ELSE IF OPER IN[NEG,ONESCOMP,NOTOP]
            THEN BEGIN
                   TOKN:=UNARY;
                   N:=INDIRECT(TYPETREE(RGHT));
                   IF NOT(ARITHTYPE(N)) AND NOT(TYPETAB[N].BASICTYPE = POINTER) 
                   THEN ERROR(15) ;
                   IF RGHT^.TOKN=KONSTANT 
                   THEN IF OPER=NEG
                   THEN BEGIN
                          T1:=RGHT;
                          DISPOSE(T);
                          T:=T1;
                          IF T^.EMODE<ORD(REEL) 
                          THEN T^.SVALUE:= - T^.SVALUE
                          ELSE T^.RVALUE:= -T^.RVALUE;
                        END;
                 END
            ELSE IF OPER IN [INCR,DECR,POSTINC,POSTDEC]
            THEN BEGIN
                   TOKN:=UNARY;
                   IF LFT<>NIL
(* 30-JULY-84*)    THEN N:=(*INDIRECT*)(TYPETREE(LFT))
(* 30-JULY-84*)    ELSE N:=(*INDIRECT*)(TYPETREE(RGHT));
                 END
            ELSE IF OPER=RETURNOP
            THEN BEGIN
                   TOKN:=UNARY;
                   N:=INDIRECT(TYPETREE(RGHT));
                 END
            ELSE FLAG:=FALSE;
            IF FLAG
            THEN T^.EMODE:=N; 
          END
  ELSE FLAG:=FALSE;
  UNARYOP:=FLAG;
END;
(* UNARYOP
 *)
 
 
 
FUNCTION ONEPOINTER(VAR T :EXPTREE):BOOLEAN;
 
(* THIS FUNCTION CHECKS TO SEE IF THERE IS ONE
 *   POINTER TYPE AND ONE INTEGRAL TYPE
 *)
VAR FLAG:BOOLEAN;
    N,N1:INTEGER;
    T1,T2:EXPTREE;
BEGIN
  FLAG:=TRUE;
  IF T<>NIL
  THEN WITH T^
       DO BEGIN
            N:=LFT^.EMODE;
            N1:=RGHT^.EMODE;
            N:=INDIRECT(N);N1:=INDIRECT(N1);
            IF (TYPETAB[N].BASICTYPE IN [POINTER,ARRAYTYPE])AND ARITHTYPE(N1)
            THEN BEGIN
                   SCLTREE(RGHT,N); 
                   T^.EMODE:=LFT^.EMODE;
                   IF (TYPETAB[N].BASICTYPE = ARRAYTYPE)
                   AND(RGHT^.TOKN = KONSTANT) 
                   THEN BEGIN 
                          T1 := FINDIDENT(T^.LFT);
                          IF T1<> NIL 
                          THEN BEGIN
                                 WARN(13);
                                 T1^.IDOFSET := T1^.IDOFSET+T^.RGHT^.SVALUE;
                                 T^.LFT := NIL; 
                                 DISCARD(T);
                                 T := T1; 
                               END; 
                        END;
                 END
            ELSE IF(TYPETAB[N1].BASICTYPE IN [POINTER,ARRAYTYPE])AND ARITHTYPE(N)
            THEN BEGIN
                   SCLTREE(LFT,N1); 
                   T^.EMODE:=RGHT^.EMODE; 
                 END
            ELSE FLAG:=FALSE
         END
  ELSE FLAG:=FALSE;
  ONEPOINTER:=FLAG;
END;
(* ONEPOINTER
 *)
 
 
 
FUNCTION TWOPOINTERS(VAR T:EXPTREE):BOOLEAN;
VAR FLAG:BOOLEAN;
    N,N1:INTEGER;
    T1,T2:EXPTREE;
BEGIN
  FLAG:=TRUE;
  IF T<>NIL
  THEN WITH T^
       DO BEGIN
            N:=LFT^.EMODE;
            N1:=RGHT^.EMODE;
            N:=INDIRECT(N);N1:=INDIRECT(N1);
IF(TYPETAB[N].BASICTYPE IN [POINTER,ARRAYTYPE]) AND
    (TYPETAB[N1].BASICTYPE IN[POINTER,ARRAYTYPE])
            THEN BEGIN
                   T1:=NEWEXPNODE(NOOP,KONSTANT);
                   T1^.SVALUE:=SIZEOFTYPE(INDIRECT(N)); 
                   T1^.EMODE:=ORD(INT); 
                   T2:=NEWEXPNODE(DIVIDE,BINARY);
                   T2^.EMODE:=ORD(INT); 
                   T2^.LFT:=T;
                   T2^.RGHT:=T1;
                   EMODE:=ORD(INT);
                   T:=T2;
                 END
            ELSE FLAG:=FALSE
          END
  ELSE FLAG:=FALSE;
  TWOPOINTERS:=FLAG;
END;
(* TWOPOINTERS
 *)
 
 
 
FUNCTION TWOINTEGRALTYPES(T:EXPTREE):BOOLEAN;
VAR N1,N2 : INTEGER;
BEGIN
  IF T=NIL
  THEN TWOINTEGRALTYPES:=FALSE
  ELSE WITH T^
       DO BEGIN
           N1:=INDIRECT(TYPETREE(LFT));
           N2:=INDIRECT(TYPETREE(RGHT));
           IF ARITHTYPE(N1) THEN ;
           IF ARITHTYPE(N2) THEN ;
            IF (N1=ORD(INT))AND(N2=ORD(INT))
            THEN TWOINTEGRALTYPES:=TRUE
            ELSE TWOINTEGRALTYPES:=FALSE;
            T^.EMODE:=N1; 
          END;
END;
(* TWOINTEGRALTYPES
 *)
 
 
 
(* 
TYPE BITREC = RECORD CASE T1:BOOLEAN
              OF TRUE : ( INT : INTEGER);
                 FALSE: ( BIT : PACKED ARRAY[0..59] OF BOOLEAN);
              END;

 *)

  FUNCTION TWOKONSTS(T : EXPTREE):BOOLEAN;
  VAR FLAG : BOOLEAN;
  BEGIN
    FLAG:=FALSE;
    IF T<>NIL
    THEN WITH T^
      DO IF (RGHT<>NIL) AND (LFT<>NIL)
    THEN IF (RGHT^.TOKN=KONSTANT) AND (LFT^.TOKN=KONSTANT)
    THEN IF (RGHT^.EMODE < ORD(REEL)) AND ( LFT^.EMODE < ORD(REEL)) 
    THEN FLAG:=TRUE;
    TWOKONSTS:=FLAG;
  END;
 
 FUNCTION LOGICOR(A,B : INTEGER):INTEGER;
 VAR ANS : BITREC;
     A1,B1:BITREC;
     CNT : INTEGER;
BEGIN
  A1.INT:=A;
  B1.INT:=B;
  FOR CNT:=0 TO 59
  DO ANS.BIT[CNT]:= A1.BIT[CNT] OR B1.BIT[CNT];
  LOGICOR:=ANS.INT;
END;
 
 FUNCTION LOGICAND(A,B : INTEGER):INTEGER;
 VAR ANS,A1,B1:BITREC;
     CNT : INTEGER;
 BEGIN
   A1.INT:=A;
   B1.INT:=B;
   FOR CNT:= 0 TO 59
   DO ANS.BIT[CNT]:=A1.BIT[CNT] AND B1.BIT[CNT];
   LOGICAND:=ANS.INT;
 END;
 
 FUNCTION LOGICXOR(A,B : INTEGER):INTEGER;
 VAR A1,B1,ANS : BITREC;
     CNT : INTEGER;
 BEGIN
   A1.INT:=A;
   B1.INT:=B;
   FOR CNT:=0 TO 59
   DO ANS.BIT[CNT]:=A1.BIT[CNT] <> B1.BIT[CNT];
   LOGICXOR:=ANS.INT;
 END;


  FUNCTION LFTSHIFT(A,B : INTEGER):INTEGER; 
  BEGIN 
    WHILE( B > 0) 
    DO BEGIN
         A := A*2;
         B := B-1;
       END; 
    LFTSHIFT := A;
  END;

  FUNCTION RGHTSHIFT(A,B : INTEGER):INTEGER;
  BEGIN 
    WHILE B > 0 
    DO BEGIN
         A := A DIV 2;
         B := B-1;
       END; 
    RGHTSHIFT := A; 
  END;
 
FUNCTION BINARYOP(VAR T:EXPTREE):BOOLEAN;
VAR FLAG : BOOLEAN;
    N,N1 : INTEGER;
    T1   : EXPTREE;
    T2   : EXPTREE; 
 
 
BEGIN
  FLAG :=TRUE;
  IF T<>NIL
  THEN WITH T^
       DO CASE OPER
          OF
           TIMES,DIVIDE : BEGIN
                            IF NOT(TWOARITHTYPES(T))
                            THEN ERROR(16)
                            ELSE IF (RGHT^.TOKN=KONSTANT) OR(LFT^.TOKN=KONSTANT)
                            THEN BEGIN
                                   IF RGHT^.TOKN=KONSTANT 
                                   THEN IF RGHT^.EMODE=ORD(INT) 
                                   THEN BEGIN
                                          IF RGHT^.SVALUE=1 
                                          THEN BEGIN
                                                 T1:=LFT;
                                                 LFT:=NIL;
                                                 DISCARD(T);
                                                 T:=T1;
                                               END
                                          ELSE IF RGHT^.SVALUE=0
                                          THEN BEGIN
                                                 IF OPER=DIVIDE
                                                 THEN ERROR(60);
                                                 T1:=RGHT;
                                                 RGHT:=NIL;
                                                 DISCARD(T);
                                                 T:=T1;
                                               END
                                        END;
                                END;
                            IF TWOKONSTS(T) (*AND (T^.OPER=TIMES)  *) 
                            THEN BEGIN
                                   T1:=NEWEXPNODE(NOOP,KONSTANT);
                                   T1^.EMODE:=ORD(INT); 
                                   IF OPER=TIMES
                                   THEN T1^.SVALUE:=RGHT^.SVALUE*LFT^.SVALUE
                                   ELSE IF RGHT^.SVALUE <> 0
                                   THEN T1^.SVALUE := LFT^.SVALUE DIV RGHT^.SVALUE; 
                                   DISCARD(T);
                                   T:=T1;
                                 END;
                          END;
 
           PLUS         : BEGIN
                            IF NOT(TWOARITHTYPES(T))
                            THEN IF NOT(ONEPOINTER(T))
                                 THEN EMODE:=ORD(INT);
                            IF TWOKONSTS(T)
                            THEN BEGIN
                                   T1:=NEWEXPNODE(NOOP,KONSTANT);
                                   T1^.EMODE:=ORD(INT); 
                                   T1^.SVALUE:=RGHT^.SVALUE+LFT^.SVALUE;
                                   DISCARD(T);
                                   T:=T1;
                                 END;
                          END;
 
           MINUS        : BEGIN
                            IF NOT(TWOARITHTYPES(T))
                          THEN IF NOT(ONEPOINTER(T))
                               THEN IF NOT(TWOPOINTERS(T))
                                    THEN BEGIN
                                           ERROR(18);
                                         END;
                           IF TWOKONSTS(T)
                           THEN BEGIN
                                  T1:=NEWEXPNODE(NOOP,KONSTANT);
                                  T1^.EMODE:=ORD(INT);
                                  T1^.SVALUE:=LFT^.SVALUE-RGHT^.SVALUE; 
                                  DISCARD(T);
                                  T:=T1;
                                END;
                          END;
           MODOP,LEFTSHIFT,
           RIGHTSHIFT,ANDOP,
           XOROP,OROP   : BEGIN
                            IF NOT(TWOINTEGRALTYPES(T))
                            THEN BEGIN
                                   ERROR(19);
                                 END;
                          IF TWOKONSTS(T)
                          THEN CASE T^.OPER 
                          OF
                            ANDOP : BEGIN
                                      T1:=NEWEXPNODE(NOOP,KONSTANT);
                                      T1^.EMODE:=T^.EMODE;
                                      T1^.SVALUE:=LOGICAND(T^.RGHT^.SVALUE,T^.LFT^.SVALUE); 
                                      DISCARD(T);
                                      T:=T1;
                                    END;
 
                            OROP  : BEGIN
                                      T1:=NEWEXPNODE(NOOP,KONSTANT);
                                      T1^.EMODE:=T^.EMODE;
                                      T1^.SVALUE:=LOGICOR(RGHT^.SVALUE,LFT^.SVALUE);
                                      DISCARD(T);
                                      T:=T1;
                                    END;
                            XOROP : BEGIN
                                      T1:=NEWEXPNODE(NOOP,KONSTANT);
                                      T1^.EMODE:=T^.EMODE;
                                      T1^.SVALUE:=LOGICXOR(RGHT^.SVALUE,LFT^.SVALUE); 
                                      DISCARD(T);
                                      T:=T1;
                                    END;
 
                            RIGHTSHIFT, 
                            LEFTSHIFT:BEGIN 
                                        T1 := NEWEXPNODE(NOOP,KONSTANT);
                                        T1^.EMODE := T^.EMODE;
                                        IF T^.OPER = RIGHTSHIFT 
                                        THEN T1^.SVALUE := RGHTSHIFT(LFT^.SVALUE,RGHT^.SVALUE)
                                        ELSE T1^.SVALUE := LFTSHIFT(LFT^.SVALUE,RGHT^.SVALUE);
                                        DISCARD(T); 
                                        T := T1;
                                      END;
                            OTHERWISE ;
                          END;
                          END;
 
           EQOP,NEOP,LTOP,LEOP,GTOP,GEOP: 
                          BEGIN 
                            IF NOT(TWOARITHTYPES(T))THEN ;
                          END;

           SELECTOR     : BEGIN
                            N:=INDIRECT(TYPETREE(LFT));
                            N1:=(TYPETREE(RGHT));
                            T1 := FINDIDENT(T^.LFT);
                            IF (T1 <> NIL) AND ( T^.RGHT^.TOKN = IDENT) 
                            THEN BEGIN
WRITELN(CODE,'* APPLYING SELECTOR OPTIMISATION'); 
                                   T1^.IDOFSET := T1^.IDOFSET + T^.RGHT^.SENTRY^.IR.OFFSET; 
                                   T^.LFT := NIL; 
                                   DISCARD(T);
                                   T := T1; 
                                 END; 
                            T^.EMODE := (N1); 
                          END;
 
           REFSELECTOR  : BEGIN
                            N:=INDIRECT(TYPETREE(LFT));
                            N1:=INDIRECT(TYPETREE(RGHT));
                            EMODE:=POINTERTO(N1);
                          END;
 
           OTHERWISE      BEGIN
                           N:=INDIRECT(TYPETREE(LFT));
                           N1:=INDIRECT(TYPETREE(RGHT));
                           EMODE:=N1;
                          END;
         END
(* CASE
 *)
       ELSE FLAG:=FALSE;
  IF FLAG
  THEN WITH T^
  DO BEGIN
       IF (OPER IN COMMUTOP)AND (TOKN <> ASSIGN)
       THEN BEGIN
              IF (LFT^.TOKN IN [IDENT,KONSTANT]) AND
                 NOT(RGHT^.TOKN IN [IDENT,KONSTANT])
              THEN BEGIN
(* SWAP LEFT AND RIGHT TREES
 *)
                     T1:=LFT;
                     LFT:=RGHT;
                     RGHT:=T1;
                   END;
            END;
     END;
  BINARYOP:=FLAG;
END;
(* BINARYOP
 *)
 
 
 
FUNCTION LEFTOF(T : EXPTREE): EXPTREE;
BEGIN
  IF T<>NIL
  THEN LEFTOF:=T^.LFT 
  ELSE LEFTOF:=NIL;
END;
  
 
 
FUNCTION TYPETREE(VAR T : EXPTREE):INTEGER;
VAR T1 : EXPTREE ;
    TYP: INTEGER;
    N,N1 : INTEGER;

BEGIN
  TYP:=ORD(INT);
  N:=TYP;
 
  IF T<> NIL
  THEN WITH T^
       DO CASE TOKN
          OF
            IDENT : BEGIN
                      TYP := POINTERTO(SENTRY^.IR.THISMODE);
                      N:=TYP;
                      IF TYPETAB[SENTRY^.IR.THISMODE].BASICTYPE IN [STRUCT,ARRAYTYPE,UNION] 
                      THEN BEGIN
                             T1 := NEWEXPNODE(REF,UNARY); 
                             T1^.EMODE := N;
                             T1^.RGHT := T; 
                             T^.EMODE := N; 
                             T := T1; 
                             WRITELN(CODE,'*## IDENT - ADDED REF OPERATOR');
                           END; 
                    END;
 
            KONSTANT : BEGIN
                         TYP:=EMODE;
                         N:=TYP;
                       END;
 
            STRING : BEGIN
                       TYP := POINTERTO(ORD(KAR)) ;
                       N:=TYP ;
                     END;
 
            CALL  : BEGIN
                      TYP:=TYPECALL(T);
                      N:=TYP;
                    END;
 
            INDEXOP : BEGIN
                      TYPEINDEX(T);
                             TYP:=  EMODE;
IF DEBUGF THEN PRINTTYPE(TYP,0);
                      IF SCALAR( INDIRECT(TYP) )
                      THEN BEGIN
                             T1:=NEWEXPNODE(DEREF,UNARY); 
                             T1^.EMODE:=TYP;
                             T1^.RGHT:=T; 
                             T:=T1; 
                           END; 
                      N := TYP; 
                    END;
 
            ASSIGN :  BEGIN
                        N:=INDIRECT(TYPETREE(LFT));
                        N1:=INDIRECT(TYPETREE(RGHT));
(* 02-NOV-84 *)         TYP := POINTERTO(N);
                        IF (N = ORD(DOUBLE))OR(N = ORD(REEL)) (* ANOTHER KLUDGE *)
                        THEN TYP:= N; 
                         IF((N IN [ORD(DOUBLE),ORD(REEL)]) AND (N1 < ORD(REEL))) OR((N1 IN [ORD(DOUBLE),ORD(REEL)]) AND (N < ORD(REEL)))
                           THEN BEGIN
                                  T1:=NEWEXPNODE(CONVERT,CAST);
                                  EMODE:=N;
                                  T1^.EMODE:=N; 
                                  T1^.RGHT:=T^.RGHT;
                                  T^.RGHT:=T1;
                                END
(* ADDED 17-OCT-1984 *) ELSE IF (TYPETAB[N].BASICTYPE IN [STRUCT,UNION])
                             AND(TYPETAB[N1].BASICTYPE IN [STRUCT,UNION]) 
                             THEN BEGIN 
                                    WARN(11); 
                                    IF SIZEOFTYPE(N) <> SIZEOFTYPE(N1)
                                    THEN ERROR(85); 
                                    IF OPER <> NOOP 
                                    THEN ERROR(86); (* SIMPLE ASSIGNMENT ONLY *)
                                    IF SIZEOFTYPE(N) < 0
                                    THEN ERROR(87); (* SIZE OF STRUCTURE IS LESS THAN ONE *)
                                  END 
                        ELSE IF (TYPETAB[N].BASICTYPE = POINTER)
(* 31-JULY-84 *)        THEN IF (TYPETAB[N1].BASICTYPE IN[ARRAYTYPE,STRUCT,UNION])
(* ADDED 31-JULY-84             AND (RGHT^.TOKN IN [IDENT]) *)
                             THEN BEGIN
WRITELN(CODE,'*- ASSIGN ADDED REF. ',N,N1); 
                                    T1:=NEWEXPNODE(REF,UNARY);
                                    T1^.EMODE:=POINTERTO(N1); 
                                    T1^.RGHT:= T^.RGHT; 
                                    T^.RGHT := T1;
                                  END 
                             ELSE IF (T^.OPER <> NOOP) AND (TYPETAB[N1].BASICTYPE <> POINTER) 
                             THEN BEGIN (* HAVE TO SCALE THE RIGHT OPERAND*)
                                    IF ONEPOINTER(T) THEN;
                                  END;
                        EMODE:=N;
                      END;
 
 
            OTHERWISE BEGIN
                      IF UNARYOP(T)
                      THEN TYP:=T^.EMODE
                      ELSE IF BINARYOP(T)
                           THEN TYP:=T^.EMODE;
                      N:=TYP;
                      END;
          END;
(* CASE
 *)
  IF T<>NIL
  THEN
       BEGIN
        IF TYP = ORD(REEL)
        THEN TYP := ORD(DOUBLE); (* ALL FLOATING POINT IN "DOUBLE" *) 
        T^.EMODE:=TYP;
      END;
  TYPETREE:=N;
END;
(* TYPETREE
 *)
 
 
 
FUNCTION LENGTHOF(I : INTEGER) : INTEGER;
BEGIN
  IF I<0 THEN LENGTHOF:=LENGTHOF(-I)
  ELSE IF (I DIV 10)=0 THEN LENGTHOF:=1
  ELSE LENGTHOF:=1+LENGTHOF(I DIV 10);
END;
 
 
PROCEDURE OUTLABEL(LAB : INTEGER);
VAR I : INTEGER;
BEGIN
  WRITE(CODE,'L',LAB:1);
  FOR I:=1 TO (9-LENGTHOF(LAB))
  DO WRITE(CODE,' ');
END;
 
 
PROCEDURE PREVIOUSLABEL;
BEGIN
  IF LASTLABEL=0
  THEN WRITE(CODE,'          ')
  ELSE BEGIN
         OUTLABEL(LASTLABEL);
       END;
  LASTLABEL:=0;
END;
 
 
PROCEDURE OUTALFA;
VAR I : INTEGER;
BEGIN
  IF (LENG>=1) AND (LENG<=10)
  THEN BEGIN
         I:=1;
         REPEAT
           WRITE(CODE,STRNG[I]);
           IF I=LENG
           THEN STRNG[I]:=' '
           ELSE I:=SUCC(I);
         UNTIL STRNG[I]=' ' ;
       END;
END;
 
 
 
 
PROCEDURE OUTEA(ID : IDTREE;NUM : INTEGER);
VAR IDNAME : ALFA;
BEGIN
  IF TYPETAB[ID^.IR.THISMODE].BASICTYPE<>FUNK 
  THEN WITH ID^.IR
  DO CASE THISSTATE
  OF
    AUTOVAR : WRITE(CODE,'B5+',(OFFSET+4+FUNCID^.IR.OFFSET+NUM):1); 
 
    EXTERNVAR:BEGIN
                WRITE(CODE,'=X');
                OUTALFA(NAME,7);
                IF NUM<>0 THEN WRITE(CODE,'+',NUM:1);
              END;
 
    EXTSTATIC : BEGIN
                  WRITE(CODE,'=X');
                  IDNAME := NAME; 
                  MAKESTAT(IDNAME); 
                  OUTALFA(IDNAME,7);
                  IF NUM<>0 THEN WRITE(CODE,'+',NUM:1);
                END;
 
    STATICVAR  : WRITE(CODE,'STATIC+',(OFFSET+NUM):1);
 
    PARAMVAR  : WRITE(CODE,'B5+',(OFFSET+4+NUM):1);
 
    UNDEFINED : BEGIN
                  WRITE(CODE,'=X');
                  OUTALFA(NAME,7);
                  IF NUM<>0 THEN WRITE(CODE,'+',NUM:1);
                END;
 
    OTHERWISE
           WRITE(CODE,(OFFSET+NUM):1);
  END
 ELSE BEGIN
        WRITE(CODE,'=X');
        OUTALFA(ID^.IR.NAME,7); 
        IF NUM<>0 THEN WRITE(CODE,'+',NUM:1);
      END;
(* CASE
 *)
END;
(* OUTEA
 *)
 
 
 
 
 
 
 
PROCEDURE STACKREG(RG:INTEGER);
VAR S: RSTKPTR;
BEGIN
  WITH PREGSET[RG]
  DO BEGIN
       NEW(S);
       WITH S^
       DO BEGIN
            SPREG:=RG;
            STKOFF:=TOPOFSTACK;
            NEXT1:=PSTKP;
            LAST1:=NIL;
          END;
       PSTKP:=S;
     END;
  PREVIOUSLABEL;
  WRITELN(CODE,'BX7 X',RG:1);
  WRITELN(CODE,'          SA7 B5+',(TOPOFSTACK):1);
  TOPOFSTACK:=SUCC(TOPOFSTACK);
  REGSET[RG].RFLAGS:=[FREE];
END;
 
 
PROCEDURE INITREGS;
VAR I :INTEGER;
BEGIN
  FOR I:= 0 TO 7
  DO WITH REGSET[I]
  DO BEGIN
       RFLAGS:=[FREE];
       PREG:=I;
     END;
END;
 
 
PROCEDURE INITPSREGS;
VAR I : INTEGER;
BEGIN
  FOR I:= 0 TO NOPREGS
  DO WITH PREGSET[I]
  DO BEGIN
       REGN:=0;
       PSTKP:=NIL;
       COUNT:=0;
       SREGN:=0;
       PTYPE:=0;
     END;
END;
 
 
PROCEDURE UNSTACK(RG:INTEGER);
VAR S : RSTKPTR;
BEGIN
  WITH PREGSET[RG]
  DO BEGIN
       IF PSTKP<>NIL
       THEN IF TOPOFSTACK-1=PSTKP^.STKOFF 
              THEN BEGIN
                     TOPOFSTACK:=PRED(TOPOFSTACK);
                     PREVIOUSLABEL;
                     WRITELN(CODE,'SA',RG:1,' B5+',(PSTKP^.STKOFF):1);
                     S:=PSTKP;
                     PSTKP:=S^.NEXT1; 
                     DISPOSE(S);
                     REGSET[RG].RFLAGS:=[];
                   END
              ELSE BEGIN
                     REGSET[RG].RFLAGS:=[FREE];
                     WRITELN(CODE,'** NOTHING UNSTACKED');
                   END
       ELSE REGSET[RG].RFLAGS:=[FREE];
     END;
END;
 
 
PROCEDURE SAVEREGS( TOPREG : INTEGER);
VAR I : INTEGER;
BEGIN
  FOR I:= 1 TO TOPREG
  DO BEGIN
       IF NOT(FREE IN REGSET[I].RFLAGS)
       THEN BEGIN
              STACKREG(I);
            END;
     END;
END;
(* SAVEREGS
 *)
 
 
PROCEDURE UNSAVEREGS(TOPREG : INTEGER);
VAR I : INTEGER;
BEGIN
  FOR I:=TOPREG DOWNTO 1
  DO BEGIN
       UNSTACK(I);
     END;
END;
 
 
PROCEDURE PUSHREG;
BEGIN
  IF CURRENTREG<5
  THEN CURRENTREG:=SUCC(CURRENTREG)
  ELSE BEGIN
         CURRENTREG:=1;
       END;
  IF NOT(FREE IN REGSET[CURRENTREG].RFLAGS)
  THEN STACKREG(CURRENTREG);
  REGSET[CURRENTREG].RFLAGS:=[];
END;
(* PUSHREG
 *)
 
 
 
PROCEDURE POPREG;
BEGIN
  UNSTACK(CURRENTREG);
  CURRENTREG:=PRED(CURRENTREG);
  IF CURRENTREG=0
  THEN BEGIN
         CURRENTREG:=5;
       END;
END;
(* POPREG
 *)
PROCEDURE POPPREG;
BEGIN
  POPREG;
END;
 
 
 
FUNCTION PREVREG:INTEGER;
BEGIN
  IF CURRENTREG=1
  THEN PREVREG:=5
  ELSE PREVREG:=PRED(CURRENTREG);
END;
 
 
 
PROCEDURE LOADFIELD(REGNO : INTEGER;MASK : INTEGER;OFFSET : INTEGER);
VAR T : EXPTREE;
    LAB : INTEGER;
BEGIN
 
(*
 *   * PROCESS CONTENTS OF X"REGNO"
 *   *
 *   * (REGNO) =((REGNO)&MASK)>>OFFSET
 *)
  IF (MASK>137071) OR (MASK < -137071)
  THEN BEGIN
         T:=NEWEXPNODE(NOOP,KONSTANT);
         T^.EMODE:=ORD(LONG); 
         T^.SVALUE:=MASK; 
         LAB:=0;
         OUTEXPTREE(T,FALSE,LAB);
         DISCARD(T);
         PREVIOUSLABEL;
         WRITELN(CODE,'BX0     X',CURRENTREG:1);
         POPREG;
       END
  ELSE WRITELN(CODE,'          SX0     ',MASK:1);
  WRITELN(CODE,'          BX',REGNO:1,'     X0*X',REGNO:1);
  IF OFFSET<>0 THEN WRITELN(CODE,'          LX',REGNO:1,'     -',OFFSET:1);
END;
(* LOADFIELD
 *)
 
 
PROCEDURE OUTDEREF(EMODE : INTEGER);
BEGIN
  PREVIOUSLABEL;
  WRITELN(CODE,'SA',CURRENTREG:1,'     X',CURRENTREG:1);
  IF TYPETAB[EMODE].BASICTYPE=FIELD
  THEN WITH TYPETAB[EMODE]
  DO
    LOADFIELD(CURRENTREG,FMASK,FOFFSET);
END;
(* OUTDEREF
 *)
 
PROCEDURE STOREFIELD(REGNO,MASK,OFFSET : INTEGER);
VAR T : EXPTREE;
    LAB : INTEGER;
 
(*
 * * STORE FIELD CONTENTS IN X"REGNO" AT ADDRESS GIVEN BY B3
 *)
BEGIN
  WRITELN(CODE,'          BX6     X',REGNO:1);
  IF(MASK>131071) OR (MASK < -131071) 
  THEN BEGIN
         T:=NEWEXPNODE(NOOP,KONSTANT);
         T^.SVALUE:=MASK; 
         T^.EMODE:=ORD(LONG); 
         LAB:=0;
         OUTEXPTREE(T,FALSE,LAB);
          PREVIOUSLABEL;
         WRITELN(CODE,'BX0     X',CURRENTREG:1);
         DISPOSE(T);
         POPREG;
       END
  ELSE WRITELN(CODE,'          SX0     ',MASK:1);
  WRITELN(CODE,'          SA',REGNO:1,'     B3      READ WORD');
  WRITELN(CODE,'          LX6     ',OFFSET:1);
  WRITELN(CODE,'          BX6     X6*X0     MASK FIELD DATA');
  WRITELN(CODE,'          BX',REGNO:1,'     -X0*X',REGNO:1);
  WRITELN(CODE,'          BX6     X6+X',REGNO:1,'     OR IN FIELD DATA');
END;
(* STOREFIELD
 *)
PROCEDURE LOADREG(T : EXPTREE ; LEFT : BOOLEAN; REGNO : INTEGER);
BEGIN
  IF T<>NIL
  THEN WITH T^
  DO   BEGIN
         PREVIOUSLABEL;
         IF LEFT
         THEN WRITE(CODE,'SX',REGNO:1)
         ELSE WRITE(CODE,'SA',REGNO:1);
         WRITE(CODE,'     ');
         OUTEA(T^.SENTRY,T^.IDOFSET); 
         WRITELN(CODE);
         IF TYPETAB[INDIRECT(EMODE)].BASICTYPE=FIELD
         THEN IF NOT(LEFT )
              THEN LOADFIELD(REGNO,TYPETAB[INDIRECT(EMODE)].FMASK,
                                   TYPETAB[INDIRECT(EMODE)].FOFFSET)
               ;
       END;
END;
(* LOAD REG
 *)
 
 
PROCEDURE STOREREG(T : EXPTREE ; REGNO : INTEGER);
BEGIN
  IF T<>NIL
  THEN WITH T^
  DO   BEGIN
         IF TYPETAB[INDIRECT(EMODE)].BASICTYPE<>FIELD
         THEN BEGIN
                PREVIOUSLABEL;
                WRITELN(CODE,'BX6     X',REGNO:1);
              END
         ELSE BEGIN
                PREVIOUSLABEL;
                WRITE(CODE,'SB3     ');
                OUTEA(T^.SENTRY,T^.IDOFSET);WRITELN(CODE);
                STOREFIELD(REGNO,TYPETAB[INDIRECT(EMODE)].FMASK,
                                 TYPETAB[INDIRECT(EMODE)].FOFFSET);
              END;
         WRITE(CODE,'          SA6     ');
         OUTEA(T^.SENTRY,T^.IDOFSET); 
         WRITELN(CODE);
       END;
END;
(* STORE REG
 *)
 
 
 
PROCEDURE CTI;
BEGIN
  PREVIOUSLABEL;
  WRITE(CODE,'UX',CURRENTREG:1,'     B7');WRITELN(CODE);
  WRITELN(CODE,'          LX',CURRENTREG:1,'     B7');
END;
 
 
PROCEDURE CTD;
BEGIN
  PREVIOUSLABEL;
  WRITELN(CODE,'PX',CURRENTREG:1);
  WRITELN(CODE,'          NX',CURRENTREG:1);
END;
 
 
PROCEDURE INOT;
VAR TLAB,TLAB1 : INTEGER;
BEGIN
  WRITELN(CODE,'*  INOT');
                   TLAB:=GIVELABEL;
                   PREVIOUSLABEL;
                   WRITELN(CODE,'ZR     X',CURRENTREG:1,',L',TLAB:1);
                   WRITELN(CODE,'          SX',CURRENTREG:1,'     0');
                   TLAB1:=GIVELABEL;
                   WRITELN(CODE,'          EQ     L',TLAB1:1);
                   OUTLABEL(TLAB);
                   WRITELN(CODE,'SX',CURRENTREG:1,'     1');
                   OUTLABEL(TLAB1);
                   WRITELN(CODE,'BSS     0');
END;
(* INOT
 *)
 
 
 
PROCEDURE ISNEG;
BEGIN
  WRITELN(CODE,'          LX',CURRENTREG:1,'     1');
  PUSHREG;
  WRITELN(CODE,'          MX',CURRENTREG:1,'     59');
  WRITELN(CODE,'          BX',PREVREG:1,'     -X',CURRENTREG:1,'*X',PREVREG:1);
  POPPREG;
END;
(* ISNEG
 *)
 
 
 
PROCEDURE FTST;
BEGIN
(*  PREVIOUSLABEL;
  WRITELN(CODE,'UX',CURRENTREG:1,'     X',CURRENTREG:1); *)
END;
(* FTST
 *)
 
 
 
PROCEDURE APPLY(OP : ALFA;LOP : INTEGER;TY : ALFA;LTY : INTEGER);
BEGIN
  PREVIOUSLABEL;
  OUTALFA(TY,LTY);
  WRITE(CODE,'X',(PREVREG):1,'     X',(PREVREG):1);
  OUTALFA(OP,LOP);
  WRITELN(CODE,'X',CURRENTREG:1);
  POPPREG;
END;
 
 
PROCEDURE EXIT1;
BEGIN
  IF REACHABLE
  THEN BEGIN
       PREVIOUSLABEL;
       WRITELN(CODE,'JP      B5');
       REACHABLE:=FALSE;
     END;
END;
 
 
PROCEDURE RETURNCODE(T : EXPTREE);
VAR LAB : INTEGER;
BEGIN
  LAB:=0;
  IF T<> NIL
  THEN WITH T^
  DO BEGIN
       IF NOT(EXPERROR) AND REACHABLE
       THEN BEGIN
              OUTEXPTREE(T,FALSE,LAB);
            PREVIOUSLABEL;
              WRITE(CODE,'BX6     X',CURRENTREG:1);
              POPPREG;
              WRITELN(CODE);
            END;
     END;
  EXIT1;
  DISCARD(BRACKETS[0]);
  BRACKETS[0]:=NIL;
  LEVEL:=0;
  LASTNODE[0]:=NIL;
  LASTNODE[1]:=NIL;
  EXPERROR:=FALSE;
END;
 
 
PROCEDURE EXITCODE;
BEGIN
  IF REACHABLE
  THEN BEGIN
         PREVIOUSLABEL;
         WRITELN(CODE,'JP     B5');
         REACHABLE:=FALSE;
       END;
END;
 
 
PROCEDURE PROCHEADER(LID : IDTREE);
VAR ID : ALFA;
 
BEGIN
  ID:=LID^.IR.NAME; 
  IF LID^.IR.THISSTATE = EXTSTATIC
  THEN MAKESTAT(ID);
  WRITE(CODE,'          IDENT     ');
  OUTNAME(ID);WRITELN(CODE);
  WRITE(CODE,'          ENTRY     ');
  OUTNAME(ID);WRITELN(CODE);
  WRITE(CODE,'          COMMENT   ');OUTNAME(ID); 
  WRITELN(CODE,' - C COMPILER, ICCC-PJC 1984'); 
  CMESS(ID);
  WRITE(CODE,'         DATA     0L');OUTNAME(ID);WRITELN(CODE);
  WRITELN(CODE,'          LIST M');
  OUTNAME(ID);WRITELN(CODE,'PS');
  WRITE(CODE,'          SA1     ');
  OUTNAME(ID);WRITELN(CODE);
  WRITELN(CODE,'          BX6     X1');
  WRITELN(CODE,'          SA6     B5');
  WRITELN(CODE,'          SA1     A1-B1');
  WRITELN(CODE,'          BX6     X1');
  WRITELN(CODE,'          SX1     B6');
  WRITELN(CODE,'          BX6     X6+X1');
  WRITELN(CODE,'          SA6     B5+3');
  WRITELN(CODE,'          SA1     B5+B1  COPY THROUGH SP+2. ');
  WRITELN(CODE,'          SA1     X1+2');
  WRITELN(CODE,'          BX6     X1');
  WRITELN(CODE,'          SA6     A6-B1  STORE OLD FL IN NEW ENVIRONMENT');
  WRITELN(CODE,'*  END OF PROCEDURE SETUP CODE.');
  REACHABLE:=TRUE;
END;
(* PROCHEADER
 *)
 
 
 FUNCTION GIVELABEL:INTEGER;
 
(* FUNCTION RETURNING A NEW LABEL NUMBER
 *)
  BEGIN
    NEXTLABELPOS:=SUCC(NEXTLABELPOS);
    GIVELABEL:=NEXTLABELPOS;
  END;
(* GIVELABEL
 *)
 
PROCEDURE LABELIT(LAB:INTEGER);
 BEGIN
  IF LASTLABEL<>0
  THEN BEGIN
         PREVIOUSLABEL;
         WRITELN(CODE,'BSS    0');
       END;
  LASTLABEL:=LAB;
  REACHABLE:=TRUE;
 END;
(* LABELIT
 *)
 
PROCEDURE JUMPX(ID : IDTREE);
BEGIN
  IF REACHABLE
  THEN BEGIN
         WRITELN(CODE,'*  JUMPX');
         BRACKETS[0]:=NEWEXPNODE(NOOP,IDENT);
         BRACKETS[0]^.SENTRY:=ID; 
         OUTEXPRESSION;
         PREVIOUSLABEL;
         WRITELN(CODE,'SB3     X2+B1');
         WRITELN(CODE,'          JP     B3');
       END;
END;
 
 
PROCEDURE JUMP(LAB:INTEGER);
BEGIN
  IF REACHABLE
  THEN BEGIN
         WRITELN(CODE,'*  JUMP');
         PREVIOUSLABEL;
         WRITE(CODE,'EQ     ');
         OUTLABEL(LAB);
         WRITELN(CODE);
         REACHABLE:=FALSE;
       END;
END;
(* JUMP
 *)
 
PROCEDURE JUMPNZ(LAB:INTEGER);
BEGIN
  IF REACHABLE
  THEN BEGIN
         WRITELN(CODE,'*  JNZ');
(*       IF LASTTYPE=ORD(DOUBLE)
         THEN CTI; *) 
         PREVIOUSLABEL;
         WRITE(CODE,'NZ     X',CURRENTREG:1,',');
         OUTLABEL(LAB);
         WRITELN(CODE);
         POPREG;
       END;
END;
(* JUMPNZ
 *)
 
PROCEDURE JUMPZ(LAB:INTEGER);
BEGIN
  IF REACHABLE
  THEN BEGIN
         WRITELN(CODE,'*  JZ');
(*       IF LASTTYPE=ORD(DOUBLE)
         THEN CTI; *) 
         PREVIOUSLABEL;
         WRITE(CODE,'ZR     X',CURRENTREG:1,',');
         OUTLABEL(LAB);
         WRITELN(CODE);
         POPREG;
       END;
END;
(* JUMPZ
 *)
 
 
 
FUNCTION NEGCOND(OP:OPSYM):OPSYM;
BEGIN
  CASE OP
  OF EQOP : NEGCOND:=NEOP;
     NEOP : NEGCOND:=EQOP;
     LTOP : NEGCOND:=GEOP;
     LEOP : NEGCOND:=GTOP;
     GEOP : NEGCOND:=LTOP;
     GTOP : NEGCOND:=LEOP;
  END;
END;
 
 
 
PROCEDURE OUTCJUMP(OP : OPSYM;LAB : INTEGER);
VAR TLAB : INTEGER;
BEGIN
  TLAB:=0;
(*  IF LASTTYPE = ORD(DOUBLE) 
  THEN CTI; *)
  PREVIOUSLABEL;
  CASE OP
  OF EQOP : WRITE(CODE,'ZR ');
     NEOP : WRITE(CODE,'NZ ');
     LEOP : BEGIN
              WRITE(CODE,'NG X',CURRENTREG:1,',');
              OUTLABEL(LAB);WRITELN(CODE);
              WRITE(CODE,'          ');
              WRITE(CODE,'ZR ');
            END;
     GEOP : BEGIN
              WRITE(CODE,'PL X',CURRENTREG:1,',');
              OUTLABEL(LAB);WRITELN(CODE);
              WRITE(CODE,'          ZR ');
            END;
     LTOP : BEGIN
              TLAB:=GIVELABEL;
              WRITE(CODE,'ZR X',CURRENTREG:1,',');
              OUTLABEL(TLAB);WRITELN(CODE);
              WRITE(CODE,'          NG ');
            END;
     GTOP : BEGIN
              TLAB:=GIVELABEL;
              WRITE(CODE,'ZR X',CURRENTREG:1,',');
              OUTLABEL(TLAB);WRITELN(CODE);
              WRITE(CODE,'          PL ');
            END;
   END;
   WRITE(CODE,'X',CURRENTREG:1,',');OUTLABEL(LAB);WRITELN(CODE);
   IF TLAB<>0 THEN LABELIT(TLAB);
END;
 
 
 
(*
 * LAB = LABEL TO JUMP TO 
 * T   = EXPRESSION TO TEST 
 * JTF = IF TRUE THE JUMP TO LABEL IF EXPRESSION = 0
 *          FALSE THEN JUMP TO LABEL IF EXPRESSION <> 0 
 * SHORT = DUMMY ARGUMENT USED IN OTHER CODE GENERATORS 
 *) 

PROCEDURE JUMPCS(LAB : INTEGER;T : EXPTREE;JTF : BOOLEAN;SHORT : BOOLEAN);
VAR LAB1,TLAB : INTEGER;
    T1 : EXPTREE;
    OP1 : OPSYM;
    TPOS : INTEGER;
BEGIN
  IF T=NIL
  THEN BEGIN
         IF NOT(JTF)THEN WRITELN(CODE,'* JUMPC(LAB,NIL,FALSE)');
         JUMP(LAB);
       END
  ELSE IF REACHABLE
  THEN BEGIN
         LAB1:=0;
         IF NOT(EXPERROR)
         THEN BEGIN
                T^.COND:=TRUE;
                IF NOT(T^.OPER IN [EQOP,NEOP,LTOP,LEOP,GTOP,GEOP])
                THEN BEGIN
                       IF ( T^.OPER = ANDFOP ) AND JTF
                       THEN BEGIN 
                              JUMPCS(LAB,T^.LFT,JTF = (T^.OPER = ANDFOP),SHORT);
                              POPREG; 
                              JUMPCS(LAB,T^.RGHT,JTF = (T^.OPER = ANDFOP),SHORT); 
                            END 
                       ELSE BEGIN 
                              OUTEXPTREE(T,FALSE,LAB1); 
                              IF JTF THEN JUMPZ(LAB)
                              ELSE JUMPNZ(LAB); 
                            END;
                     END
                ELSE BEGIN
                       OUTEXPTREE(T^.LFT,FALSE,LAB1); 
                       OUTEXPTREE(T^.RGHT,FALSE,LAB1);
(*                     IF T^.EMODE = ORD(DOUBLE)
                       THEN APPLY('-         ',1,'F         ',1)
                       ELSE *) APPLY('-         ',1,'I         ',1);
                       IF JTF THEN T^.OPER:=NEGCOND(T^.OPER); 
                       OUTCJUMP(T^.OPER,LAB); 
                     END;
              END;
       END;
END;
 
PROCEDURE JUMPC(LAB : INTEGER;T:EXPTREE;JTF : BOOLEAN);
BEGIN
  IF DEBUGF THEN PRINTTREE(T,0);
  JUMPCS(LAB,T,JTF,FALSE);
END;
 
 
PROCEDURE OUTOPER(T : EXPTREE);
VAR TLAB,TLAB1 : INTEGER;
BEGIN
  WITH T^ 
  DO BEGIN
       CASE OPER
       OF
         TIMES : BEGIN
                   IF EMODE = ORD(DOUBLE)
                   THEN APPLY('*         ',1,'F         ',1)
                   ELSE APPLY('*         ',1,'I         ',1);
                 END;
 
         DIVIDE: BEGIN
                   IF EMODE = ORD(DOUBLE)
                   THEN APPLY('/         ',1,'F         ',1)
                   ELSE APPLY('/         ',1,'I         ',1);
                 END;
 
         PLUS  : BEGIN
                   IF EMODE = ORD(DOUBLE)
                   THEN APPLY('+         ',1,'F         ',1)
                   ELSE APPLY('+         ',1,'I         ',1);
                 END;
         MINUS : BEGIN
                   IF EMODE = ORD(DOUBLE)
                   THEN APPLY('-         ',1,'F         ',1)
                   ELSE APPLY('-         ',1,'I         ',1);
                 END;
         XOROP : BEGIN
                   PREVIOUSLABEL;
                   WRITELN(CODE,'BX',PREVREG:1,'      X',CURRENTREG:1,'-X',
                                     PREVREG:1);
                   POPPREG;
                 END;
 
         MODOP : BEGIN
                   PREVIOUSLABEL;
                   WRITELN(CODE,'BX7     X',CURRENTREG:1);
                   WRITELN(CODE,'          BX0     X',PREVREG:1);
                   WRITELN(CODE,'          IX6     X',PREVREG:1,'/X',CURRENTREG:1);
                   WRITELN(CODE,'          IX6     X6*X7');
                   POPPREG;
                   WRITELN(CODE,'          IX',CURRENTREG:1,'     X0','-X6');
                 END;
 
        ANDOP : BEGIN
                  APPLY('*         ',1,'B         ',1);
                END;
 
        OROP  : BEGIN
                  APPLY('+         ',1,'B         ',1);
                END;
        CONVERT : BEGIN
                    IF (INDIRECT(EMODE) IN [ORD(DOUBLE),ORD(REEL)]) AND (INDIRECT(RGHT^.EMODE)<ORD(REEL)) 
                    THEN CTD
                    ELSE IF (INDIRECT(EMODE)<ORD(REEL)) AND (INDIRECT(RGHT^.EMODE) IN [ORD(REEL),ORD(DOUBLE)])
                    THEN CTI
                    ELSE ERROR(70);
                  END;
 
ONESCOMP,NEG   : BEGIN
                   PREVIOUSLABEL;
                   IF EMODE = ORD(DOUBLE)
                   THEN BEGIN
                          WRITELN(CODE,'SX7     0');
                          WRITELN(CODE,'          FX',CURRENTREG:1,'     X7-X',
                                       CURRENTREG:1);
                        END
                   ELSE BEGIN
                          WRITELN(CODE,'SX7     0');
                          WRITELN(CODE,'          IX',CURRENTREG:1,'     X7-X',
                                       CURRENTREG:1);
                        END;
                 END;
 
 
         DEREF : BEGIN
                   OUTDEREF(INDIRECT(EMODE));
                 END;
 
         NOTOP : BEGIN
                   INOT;
                 END;
 
         INDEX : BEGIN
                   APPLY('+         ',1,'I         ',1);
                 END;
 
    LEFTSHIFT : BEGIN
                  PREVIOUSLABEL;
                  WRITELN(CODE,'SB7     X',CURRENTREG:1);
                  POPPREG;
                  WRITELN(CODE,'          LX',CURRENTREG:1,'     B7');
                END;
    RIGHTSHIFT:BEGIN
                  PREVIOUSLABEL;
                  WRITELN(CODE,'SB7     X',CURRENTREG:1);
                  WRITELN(CODE,'          SB7     -B7');
                  POPPREG;
                  WRITELN(CODE,'          LX',CURRENTREG:1,'     B7');
                END;
    GEOP,LTOP  : BEGIN
(*                 IF EMODE=ORD(DOUBLE) 
                   THEN BEGIN
                          APPLY('-         ',1,'F         ',1);
                          FTST; 
                        END
                   ELSE *)
                     APPLY('-         ',1,'I         ',1);
                   ISNEG;
                   IF OPER=GEOP
                   THEN
                     INOT;
                 END;
 
        GTOP,LEOP : BEGIN
                      PREVIOUSLABEL;
(*                    IF EMODE=ORD(DOUBLE)
                      THEN BEGIN
                             WRITELN(CODE,'FX',PREVREG:1,'     X',CURRENTREG:1,'-X',PREVREG:1);
                             POPPREG;
                             FTST;
                           END
                      ELSE *) 
                        WRITELN(CODE,'IX',PREVREG:1,'     X',CURRENTREG:1,'-X',PREVREG:1);
                      POPPREG;
                      ISNEG;
                      IF OPER=LEOP
                      THEN
                        INOT;
                    END;
 
        EQOP,NEOP : BEGIN
(*                    IF EMODE=ORD(DOUBLE)
                      THEN BEGIN
                             APPLY('-         ',1,'F         ',1);
                             FTST;
                           END
                      ELSE *) 
                        APPLY('-         ',1,'I         ',1);
                      INOT;
                      IF OPER=NEOP
                      THEN
                        INOT;
                    END;
 
         OTHERWISE WRITE(CODE,'UNIMPLEMENTED OP CODE');
       END;
(* CASE
 *)
       IF (OPER IN [TIMES,DIVIDE,PLUS,MINUS])AND (EMODE=ORD(DOUBLE))
       THEN WRITELN(CODE,'          NX',CURRENTREG:1);
     END;
END;
(* OUTOPER
 *)
 
 
 
 
 
PROCEDURE OUTPARAMS(VAR T : EXPTREE; PARAMBASE : INTEGER);
VAR TEMP : INTEGER;
    LAB : INTEGER;
BEGIN
  IF T<>NIL
  THEN WITH T^
  DO BEGIN
       CURRENTREG:=1;
       TEMP:=TOPOFSTACK;
       IF TOKN=PARAM
       THEN BEGIN
              OUTPARAMS(LFT,PARAMBASE);
              POPREG;
              OUTEXPTREE(RGHT,(NOT(SCALAR((*  INDIRECT  *)(RGHT^.EMODE)))),LAB);
          PREVIOUSLABEL;
              WRITELN(CODE,'BX6     X',CURRENTREG:1);
              WRITELN(CODE,'          SA6     B5+',PARAMBASE+OFSET+4:1);
            END
       ELSE BEGIN
              OUTEXPTREE(T,NOT(SCALAR(  (* INDIRECT  *)(T^.EMODE))),LAB); 
          PREVIOUSLABEL;
              WRITELN(CODE,'BX6     X',CURRENTREG:1);
              WRITELN(CODE,'          SA6     B5+',PARAMBASE+4:1);
            END;
       TOPOFSTACK:=TEMP;
     END;
END;
(* OUTPARAMS
 *)
 
 
 
 
PROCEDURE OUTC1;
BEGIN
  WRITELN(CODE,'*  SET UP RETURN STACK.');
  PREVIOUSLABEL;
  WRITELN(CODE,'SB6     ',T^.PARAMSIZE:1);
  WRITELN(CODE,'          SX6     B5');
  WRITELN(CODE,'          SA6     B5+',(PARAMBASE+1):1);
  WRITELN(CODE,'          SB5     A6-B1');
END;
 
PROCEDURE OUTCALL(VAR T : EXPTREE);
VAR TOPREG : INTEGER;
    PARAMBASE : INTEGER;
    LAB : INTEGER;

BEGIN
  WRITELN(CODE,'*  SET UP A CALL');
  LAB:=0;
  TOPREG:=CURRENTREG;
  SAVEREGS(5);
  PARAMBASE:=TOPOFSTACK+1;
  CURRENTREG:=1;
  WITH T^ 
  DO BEGIN
       TOPOFSTACK:=TOPOFSTACK+PARAMSIZE+4;
       WRITELN(CODE,'*  OUTPUT PARAMETERS');
       OUTPARAMS(RGHT,PARAMBASE);
       IF LFT^.TOKN=IDENT 
       THEN BEGIN
              OUTC1;
              WRITE(CODE,'          RJ      ');
              WRITE(CODE,'=X');
              OUTALFA(LFT^.SENTRY^.IR.NAME,7);
              WRITELN(CODE);
            END
       ELSE BEGIN
              OUTEXPTREE(LFT,TRUE,LAB);
PREVIOUSLABEL;
              WRITELN(CODE,'SB3     X',CURRENTREG:1);
              OUTC1;
              WRITELN(CODE,'          RJ      =XC.RJ');
            END;
  TOPOFSTACK:=PARAMBASE-1;
 
(* RESTORE B5 CODE
 *)
  WRITELN(CODE,'          SA1     B5+B1     RESTORE B5');
  WRITELN(CODE,'          SB5     X1');
  UNSAVEREGS(5);
  CURRENTREG:=TOPREG;
  PUSHREG;
  WRITELN(CODE,'          BX',CURRENTREG:1,'     X6     RETURN RESULT');
  END;
END;
 
 
FUNCTION GETOFFSET(T : EXPTREE):INTEGER;
BEGIN
  IF T<>NIL
  THEN WITH T^
  DO IF TOKN=IDENT
     THEN GETOFFSET:= SENTRY^.IR.OFFSET 
     ELSE GETOFFSET:= SVALUE
  ELSE GETOFFSET:=0;
END;
 
 
 
  PROCEDURE OUTEXPTREE(T : EXPTREE;LEFT : BOOLEAN;VAR LAB : INTEGER);
  VAR F,S : EXPTREE;
      T1,T2 : EXPTREE;
      LAB1,LAB2 : INTEGER;
      TP,TP1  : EXPTREE;
  BEGIN
  IF T<>NIL
    THEN WITH T^
    DO BEGIN
         CASE TOKN
         OF
           IDENT : BEGIN
                     PUSHREG;
                     LOADREG(T,(LEFT OR NOT(SCALAR(INDIRECT(T^.EMODE)))),CURRENTREG); 
                   END;
 
           STRING : BEGIN
                      PUSHREG;
                      PREVIOUSLABEL;
                      WRITELN(CODE,'SX',CURRENTREG:1,'     LIT.LIT+',OUTLITERAL(STRNG):1);
                    END;
 
           KONSTANT:BEGIN
                      PUSHREG;
                      PREVIOUSLABEL;
                      IF LEFT
                      THEN BEGIN
                             WRITELN(CODE,'SX',CURRENTREG:1,'     STATIC+',OUTSTATIC(T):1);
                           END
                      ELSE
                            CASE TYPETAB[EMODE].BASICTYPE
                            OF
 
                               KAR : WRITELN(CODE,'SX',CURRENTREG:1,'    ',SVALUE:1); 

                  DOUBLE,REEL,LONG : BEGIN
                                       WRITELN(CODE,'SA',CURRENTREG:1,'     STATIC+',OUTSTATIC(T):1);
                                     END;
 
                         OTHERWISE
                                    BEGIN 
                                      IF (SVALUE > 131071) OR (SVALUE < -131071)
                                      THEN WRITELN(CODE,'SA',CURRENTREG:1,'    STATIC+',OUTSTATIC(T):1) 
                                       ELSE WRITELN(CODE,'SX',CURRENTREG:1,'     ',SVALUE:1); 
                                    END;
                            END ;
(* CASE
 *)
                    END;
 
           ASSIGN : BEGIN
                       IF (OPER=NOOP) AND (TYPETAB[INDIRECT(EMODE)].BASICTYPE IN [STRUCT,UNION])
(* 17-OCT-84 *)        THEN BEGIN 
                              WRITELN(CODE,'***** STRUCTURE ASSIGNMENT'); 
                              OUTEXPTREE(RGHT,TRUE,LAB);
                              OUTEXPTREE(LFT,TRUE,LAB); 
                              LAB2 := GIVELABEL;
                              PREVIOUSLABEL;WRITELN(CODE,'SA',PREVREG:1,'   X',PREVREG:1);
                              WRITELN(CODE,'          BX6   X',PREVREG:1);
                              WRITELN(CODE,'          SA6   X',CURRENTREG:1); 
         (* IF ONLY 1 WORD LONG DO NOT INCLUDE LOOP *)
                              IF SIZEOFTYPE(INDIRECT(EMODE)) > 1
                              THEN BEGIN
                                     PREVIOUSLABEL;WRITELN(CODE,'SB2   ',(SIZEOFTYPE(INDIRECT(EMODE))-1):1);
                                     LABELIT(LAB2); 
                                     PREVIOUSLABEL;WRITELN(CODE,'SA',PREVREG:1,'   A',PREVREG:1,'+B1'); 
                                     WRITELN(CODE,'          BX6   X',PREVREG:1); 
                                     WRITELN(CODE,'          SA6   A6+B1'); 
                                     WRITELN(CODE,'          SB2   B2-B1'); 
                                     WRITE(CODE,'          NE    B2,');OUTLABEL(LAB2);WRITELN(CODE);
                                   END; 
                              PREVIOUSLABEL;WRITELN(CODE,'BX',PREVREG:1,'   X',CURRENTREG:1); 
                              POPPREG;
                              WRITELN(CODE,'***** END OF STRUCTURE ASSIGNMENT');
                            END 
                       ELSE 
                      IF (OPER=NOOP) AND(LFT^.TOKN=IDENT) 
                      THEN BEGIN
(* SIMPLE ASSIGNMENT
 *)
                             OUTEXPTREE(RGHT,FALSE,LAB);
                             STOREREG(LFT,CURRENTREG);
                           END
                      ELSE BEGIN
                             OUTEXPTREE(LFT,TRUE,LAB);
(* CALCULATE ADDRESS
 *)
                             IF ( OPER<>NOOP)
                             THEN BEGIN
                                    PUSHREG;
                                    PREVIOUSLABEL;
                                    WRITELN(CODE,'BX',CURRENTREG:1,'     X',PREVREG:1);
                                    OUTDEREF(INDIRECT(LFT^.EMODE)); 
                                    OUTEXPTREE(RGHT,FALSE,LAB);
                                    OUTOPER(T);
                                  END
                             ELSE OUTEXPTREE(RGHT,FALSE,LAB);
                             IF TYPETAB[INDIRECT(LFT^.EMODE)].BASICTYPE<>FIELD
                             THEN BEGIN
                                    WRITELN(CODE,'          BX6     X',CURRENTREG:1);
                                    POPPREG;
                                    WRITELN(CODE,'          SA6     X',CURRENTREG:1);
                                    WRITELN(CODE,'          BX',CURRENTREG:1,'     X6');
                                  END
                             ELSE BEGIN
                                    WRITELN(CODE,'          SB3     X',PREVREG:1);
                                    WRITELN(CODE,'          BX',PREVREG:1,'     X',CURRENTREG:1);
                                    STOREFIELD(CURRENTREG,TYPETAB[INDIRECT(LFT^.EMODE)].FMASK,
                                                          TYPETAB[INDIRECT(LFT^.EMODE)].FOFFSET); 
                                    WRITELN(CODE,'          SA6     B3');
                                    POPPREG;
                                  END;
                           END;
                    END;
 
           CALL  : BEGIN
                     OUTCALL(T);
                   END;
 
           ADDOP,BINARY,
           MULTOP: BEGIN
                     OUTEXPTREE(LFT,FALSE,LAB);
                     OUTEXPTREE(RGHT,FALSE,LAB);
                     OUTOPER(T);
                   END;
           OTHERWISE
                   BEGIN
                     CASE OPER
                     OF
                       COMMAOP : BEGIN
                                   OUTEXPTREE(LFT,FALSE,LAB);
                                   POPPREG;
                                   OUTEXPTREE(RGHT,LEFT,LAB);
                                 END;
 
       INCR,DECR,POSTINC,POSTDEC : BEGIN
    IF LFT = NIL
    THEN BEGIN
(* PRE"OP"
 *)
           OUTEXPTREE(RGHT,FALSE,LAB);
                  PUSHREG;
                  WRITELN(CODE,'          SX',CURRENTREG:1,
(* ADDED 31-JULY-84 *)         '     ',SIZEOFTYPE(INDIRECT(INDIRECT(EMODE))):1);
           IF OPER=INCR
           THEN BEGIN
                  WRITELN(CODE,'          IX6',
                               '     X',PREVREG:1,'+X',
                               CURRENTREG:1);
                END
           ELSE BEGIN
                  WRITELN(CODE,'          IX6     X',PREVREG:1,
                               '-X',CURRENTREG:1);
                END;
             IF TYPETAB[INDIRECT(RGHT^.EMODE)].BASICTYPE<>FIELD 
           THEN BEGIN
                  WRITELN(CODE,'          SA6     A',PREVREG:1);
                  POPPREG;
                  WRITELN(CODE,'          BX',CURRENTREG:1,'     X6');
                END
           ELSE BEGIN
                  POPPREG;
                  WRITELN(CODE,'          BX',CURRENTREG:1,'     X6');
                  WRITELN(CODE,'          SB3 A',CURRENTREG:1,'    B3 HAS ADDRESS');
                  STOREFIELD(CURRENTREG,TYPETAB[INDIRECT(RGHT^.EMODE)].FMASK, 
                                        TYPETAB[INDIRECT(RGHT^.EMODE)].FOFFSET);
                  WRITELN(CODE,'          SA6     A',CURRENTREG:1);
                END;
         END
   ELSE BEGIN
 
(* POST"OP"
 *)
          OUTEXPTREE(LFT,FALSE,LAB);
          PUSHREG;
(*31-JULY-84*)WRITELN(CODE,'          SX',CURRENTREG:1,'     ',SIZEOFTYPE(INDIRECT(INDIRECT(EMODE))):1);
          IF OPER IN [INCR,POSTINC]
          THEN BEGIN
                 WRITELN(CODE,'          IX6     X',PREVREG:1,'+X',CURRENTREG:1);
               END
          ELSE WRITELN(CODE,'          IX6     X',PREVREG:1,'-X',CURRENTREG:1);
          POPPREG;
          IF TYPETAB[INDIRECT(LFT^.EMODE)].BASICTYPE<>FIELD 
          THEN BEGIN
                 WRITELN(CODE,'          SA6     A',CURRENTREG:1);
               END
          ELSE BEGIN
                 PUSHREG;
                 WRITELN(CODE,'          BX',CURRENTREG:1,' X6');
                 WRITELN(CODE,'          SB3  A',PREVREG:1);
                 STOREFIELD(CURRENTREG,TYPETAB[INDIRECT(LFT^.EMODE)].FMASK, 
                                       TYPETAB[INDIRECT(LFT^.EMODE)].FOFFSET);
                 WRITELN(CODE,'          SA6     A',PREVREG:1);
                 POPPREG;
               END;
        END;
  END;
 
               ANDFOP,ORFOP : BEGIN
                                LAB1:=0;
                                OUTEXPTREE(LFT,FALSE,LAB1);
                                LAB1:=GIVELABEL;
                                LASTTYPE:=LFT^.EMODE; 
                                IF OPER=ANDFOP
                                THEN JUMPZ(LAB1)
                                ELSE JUMPNZ(LAB1);
                                LAB2:=0;
                                OUTEXPTREE(RGHT,FALSE,LAB2);
                                LABELIT(LAB1);
                              END;
 
                      IFOP : BEGIN
                               OUTEXPTREE(LFT,FALSE,LAB);
                               LAB:=GIVELABEL;
                               LASTTYPE:=LFT^.EMODE;
                               JUMPZ(LAB);
                               LAB1:=0;
                               OUTEXPTREE(RGHT,FALSE,LAB1);
                               WRITELN(CODE,'*  IFOP RETURNING ',LAB:1);
                             END;
 
                   ELSEOP : BEGIN
                              LAB2:=0;
                              OUTEXPTREE(LFT,FALSE,LAB2);
                              LAB1:=GIVELABEL;
                              JUMP(LAB1);
                              WRITELN(CODE,'*   LAB2=',LAB2:1);
                              POPREG;
                              LABELIT(LAB2);
                              LAB2:=0;
                              OUTEXPTREE(RGHT,FALSE,LAB2);
                              LABELIT(LAB1);
                            END;
 
                   REF    : BEGIN
                              OUTEXPTREE(RGHT,TRUE,LAB);
                            END;
 
                   SELECTOR : BEGIN
                                OUTEXPTREE(LFT,TRUE,LAB);
                                OUTEXPTREE(RGHT,TRUE,LAB);
                                TOKN:=BINARY;
                                OPER:=PLUS;
                                T^.EMODE:=T^.RGHT^.EMODE; 
                                OUTOPER(T);
                                IF NOT(LEFT)
                                THEN OUTDEREF(INDIRECT(EMODE));
                              END;
 
                REFSELECTOR : BEGIN
                                OUTEXPTREE(LFT,FALSE,LAB);
                                IF RGHT^.TOKN = IDENT 
                                THEN IF RGHT^.SENTRY <> NIL 
                                     THEN BEGIN 
                                            PREVIOUSLABEL;
                                            IF NOT(LEFT)
                                            THEN BEGIN
                                                   WRITELN(CODE,'SA',CURRENTREG: 1,'  X',CURRENTREG:1,'+',(RGHT^.SENTRY^.IR.OFFSET):1); 
                                                   IF TYPETAB[INDIRECT(EMODE)].BASICTYPE = FIELD
                                                   THEN WITH TYPETAB[INDIRECT(EMODE)] 
                                                   DO LOADFIELD(CURRENTREG,FMASK,FOFFSET);
                                                 END
                                            ELSE BEGIN
                                                   WRITELN(CODE,'SX',CURRENTREG: 1,'  X',CURRENTREG:1,'+',(RGHT^.SENTRY^.IR.OFFSET):1); 
                                                 END; 
                                            LEFT := TRUE;   (* STOP DEREF *)
                                          END 
                                     ELSE
                                ELSE BEGIN
                                       OUTEXPTREE(RGHT,TRUE,LAB);
                                       TOKN := BINARY;
                                       OPER := PLUS;
                                       OUTOPER(T);
                                     END;
                                IF NOT(LEFT)
                                THEN OUTDEREF(INDIRECT(EMODE));
                              END;
 
                   DEREF : BEGIN
                             OUTEXPTREE(RGHT,FALSE,LAB);
                             IF NOT(LEFT)
                             THEN OUTOPER(T);
 
                           END;
 
                   OTHERWISE BEGIN
                               OUTEXPTREE(LFT,FALSE,LAB);
                               OUTEXPTREE(RGHT,FALSE,LAB);
                               OUTOPER(T);
                             END;
                 END;
(* CASE
 *)
                   END;
         END;
(* CASE
 *)
       END;
    IF LASTLABEL <> 0
    THEN BEGIN
           PREVIOUSLABEL;
           WRITELN(CODE,'BSS 0');
     END;
  END;
(* OUTEXPTREE
 *)
 
 
PROCEDURE OUTCODE;
BEGIN
  PREVUNKNOWN;
  INITREGS;
  INITPSREGS;
(* HANDLE POSSIBLE LAST UNKNOWN ID
 *)
 
 IF NOT(EXPERROR) THEN   LASTTYPE:=INDIRECT(TYPETREE(BRACKETS[0]));
 TOPOFSTACK:=CURRENT^.AUTOSIZE+FUNCID^.IR.OFFSET+4; 
  CURRENTREG:=1;
  LEVEL:=0;
  LASTNODE[0]:=NIL;
  BRACKETS[1]:=NIL;
END;
 
 
PROCEDURE OUTEXPRESSION;
VAR T1 : INTEGER ;
    LAB : INTEGER;
    TEMP : INTEGER;
BEGIN
  WRITELN(CODE,'*  OUTPUT AN EXPRESSION');
(* OUTEXPRESSION
 *)
  LAB:=0;
  OUTCODE;
  IF DEBUGF THEN PRINTTREE(BRACKETS[0],0);
  IF EXPERROR THEN
                WRITELN(CODE,'*  ERROR IN EXPRESSION ')
  ELSE IF NOT(REACHABLE)
  THEN WRITELN(CODE,'** CANNOT GET HERE....')
  ELSE
    OUTEXPTREE(BRACKETS[0],FALSE,LAB);
  DISCARD(BRACKETS[0]);
  BRACKETS[0]:=NIL;
  EXPERROR:=FALSE;
END;
(* OUTEXPRESSION
 *)
 
 
PROCEDURE PASS2;EXTERN;
BEGIN
  PASS2;
END.
