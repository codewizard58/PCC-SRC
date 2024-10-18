(*$L'  8086 CODE GENERATOR '*)
(* ******************** C86CG ******************* *)
  
  
(* FORWARD REFERENCES ..................... 
 *) 
  
PROCEDURE OUTEXPRESSION;FORWARD;
PROCEDURE OUTNAME(STRNG : ALFA);FORWARD;
  
FUNCTION NEWEXPNODE(OPR : OPSYM;TOK : SYMMODE): EXPTREE;FORWARD;
FUNCTION INDIRECT(N:INTEGER):INTEGER; FORWARD;
PROCEDURE PREVIOUSLABEL;FORWARD;
PROCEDURE DISCARD(T:EXPTREE); EXTERN; 
  
  PROCEDURE KONSTEXP;EXTERN;
  
  FUNCTION GIVELABEL : INTEGER;FORWARD; 
  FUNCTION TYPETREE(VAR T:EXPTREE):INTEGER;FORWARD; 
  
  PROCEDURE OUTEXPTREE( T : EXPTREE; LEFT : BOOLEAN ;VAR LAB : INTEGER);FORWARD;
PROCEDURE OUTALFA(STRNG : ALFA ; LENG : INTEGER);FORWARD; 
  
(* ***************** EXTERNAL ROUTINES FOUND IN PARSLIB ************* *)
  
  PROCEDURE PREVUNKNOWN; EXTERN ; 
  
  FUNCTION POINTERTO(LAST : INTEGER):INTEGER;EXTERN;
  
  FUNCTION GREATEROF(A,B : INTEGER):INTEGER; EXTERN;
  
  FUNCTION OUTLITERAL(S : EXPTREE) : INTEGER;EXTERN;
  
PROCEDURE ERROR(I : ERRORCODES);EXTERN; 
PROCEDURE WARN(I  : ERRORCODES);EXTERN; 
  
FUNCTION SIZEOFTYPE(INN : INTEGER) : INTEGER; EXTERN; 
  
(* ********************* END OF EXTERNAL DECLARATIONS *************** *)
  
  
  
FUNCTION SIZEOFWORD:INTEGER;
BEGIN 
  SIZEOFWORD:=8;
END;
  
  
  
FUNCTION SIZEOFFIELD:INTEGER; 
BEGIN 
  SIZEOFFIELD:=16;
END;
  
  
  
FUNCTION SAVEDSIZE:INTEGER; 
BEGIN 
  SAVEDSIZE:=16;
END;
  
  
  
  
  
  
PROCEDURE DATAHED(NAME : ALFA); 
VAR ID : ALFA;
    I  : INTEGER; 
BEGIN 
  FOR I:= 0 TO 20 DO REGSET[I].IDPTR:= NIL; 
  ID:=NAME; 
  IF NOT(SIMPLEFORM)
  THEN BEGIN
         WRITE(CODE,'          IDENT     ');
         OUTALFA(ID,6);WRITELN(CODE,'.'); 
         WRITE(CODE,'          RELOC ');
         OUTALFA(ID,6);WRITELN(CODE,',0');
       END; 
  WRITE(CODE,'          ENTRY     '); 
  OUTALFA(ID,7);WRITELN(CODE);
  IF NOT(SIMPLEFORM) AND ORIGINF
  THEN BEGIN
         WRITELN(CODE,'          ORG ',ORIGIN:1); 
         ORIGINF:=FALSE;
       END; 
  OUTNAME(ID);WRITELN(CODE,'BSS     0');
END; (* DATAHEADER *) 
  
PROCEDURE PRINTLITERAL(LREC : EXPRECORD); 
BEGIN 
  WITH LREC 
  DO CASE TOKN
  OF
     KONSTANT : CASE TYPETAB[EMODE].BASICTYPE 
                OF
                   SHORT,KAR : WRITELN(CODE,'          DB ',SVALUE:1);
                   INT,UNSIGNED:  
                       WRITELN(CODE,'          DW     ',SVALUE:1);
                   LONG : BEGIN 
                            WRITELN(CODE,'          DW ',(SVALUE MOD 65536):1); 
                            WRITELN(CODE,'          DW ',(SVALUE DIV 65536):1); 
                          END;
  
                   REEL,DOUBLE: 
                       WRITELN(CODE,'          DATA     ',RVALUE);
  
                   ARRAYTYPE: 
                       WRITELN(CODE,'          BSSZ     ',SVALUE:1);
                END;
  
      IDENT :   BEGIN 
                  WRITE(CODE,'          VFD     42/,18/=X');
                  OUTALFA(LID,7);WRITELN(CODE); 
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
  
FUNCTION BASICSIZE(TYP : INTEGER):INTEGER;
BEGIN 
  CASE TYP
  OF 0,1,2 : BASICSIZE:=1;
     3,4   : BASICSIZE:=2;
     5,6   : BASICSIZE:=4;
     7     : BASICSIZE:=8;
    OTHERWISE BASICSIZE:=2; 
  END;
END;
  
  
(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *  START OF ROUTINES TO HANDLE STMTNTS.
 *  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 *) 
PROCEDURE PRINTTYPE( MODE : INTEGER); 
BEGIN 
  WRITE(CODE,MODE:1,':'); 
  WITH TYPETAB[MODE]
  DO CASE BASICTYPE 
  OF   NULLTYPE : WRITE(CODE,'NULLTYPE ');
       SHORT    : WRITE(CODE,'SHORT '); 
       KAR      : WRITE(CODE,'CHAR ');
       UNSIGNED : WRITE(CODE,'UNSIGNED ');
       INT      : WRITE(CODE,'INT '); 
       LONG     : WRITE(CODE,'LONG ');
       REEL     : WRITE(CODE,'FLOAT '); 
       DOUBLE   : WRITE(CODE,'DOUBLE ');
       POINTER  : BEGIN 
                    PRINTTYPE(TYPEPOINTER); 
                    WRITE(CODE,'*');
                  END;
       ARRAYTYPE: BEGIN 
                    PRINTTYPE(TYPENO);
                    WRITE(CODE,'[',INDEXRANGE:1,']'); 
                  END;
       FUNK     : BEGIN 
                    PRINTTYPE(RETRN); 
                    WRITE(CODE,'()'); 
                  END;
       IOTYP    : BEGIN 
                    WRITE(CODE,'IOTYP '); 
                  END;
       STRUCT   : WRITE(CODE,'STRUCT ');
       UNION    : WRITE(CODE,'UNION '); 
       FIELD    : BEGIN 
                    WRITE(CODE,'FIELD:',FMASK:1,'-',FOFFSET:1); 
                  END;
       OTHERWISE WRITE(CODE,'TYPE= ',MODE:1); 
     END; 
END;
(*$L'TYPE THE EXPRESSION TREE ROUTINES'*) 
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
  
  
  
FUNCTION ISPTRTYPE(TY : INTEGER):BOOLEAN; 
BEGIN 
  IF (TY <= 0) OR (TY > TYPETABSIZE)
  THEN BEGIN
         WRITELN(CODE,'*ISPTRTYPE: TY OUT OF RANGE ',TY:1); 
       END
  ELSE ISPTRTYPE:=TYPETAB[TY].BASICTYPE IN [POINTER,ARRAYTYPE,STRUCT,UNION,FUNK]; 
END;
  
  
FUNCTION ISFIELD(MD : INTEGER):BOOLEAN; 
BEGIN 
  ISFIELD := TYPETAB[MD].BASICTYPE = FIELD; 
END;
  
  
  
FUNCTION ISINTEGRAL(MD : INTEGER):BOOLEAN;
BEGIN 
  IF ISFIELD(MD)
  THEN ISINTEGRAL:=TRUE 
  ELSE ISINTEGRAL:= MD <= ORD(LONG) 
END;
  
  
  
  
  
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
ELSE IF T^.TOKN = CALL
THEN PRECEDENCE:=3
  ELSE IF T^.TOKN = INDEXOP 
  THEN  PRECEDENCE:=3 
ELSE IF T^.OPER = INDEX 
       THEN PRECEDENCE := 3 
  ELSE WITH T^
   DO CASE OPER OF
      SELECTOR,REFSELECTOR : PRECEDENCE := 3 ;
      POSTINC,POSTDEC      : PRECEDENCE := 4; 
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
 END; 
(* PRECEDENCE 
 *) 
  
  
  
  
FUNCTION INDIRECT;
BEGIN 
  IF ( N <= 0) OR (N > TYPETABSIZE) 
  THEN BEGIN
         WRITELN(CODE,'*INDIRECT: TY OUT OF RANGE ',N:1); 
         N:=ORD(INT); 
       END
  ELSE
  WITH TYPETAB[N] 
  DO CASE BASICTYPE 
     OF 
       POINTER : INDIRECT:=TYPEPOINTER; 
  
       ARRAYTYPE:INDIRECT:=TYPENO;
  
       OTHERWISE INDIRECT:=N; 
     END; 
END;
  
  
  
  
  
  
  
  
  
  
FUNCTION NEWEXPNODE;
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
       SETCC := FALSE;
      COND := FALSE;
       WANTRES:=TRUE; 
       STKTMP:= -1; 
     END; 
  NEWEXPNODE:=T;
END;
(* NEW EXP NODE 
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
         ARITHTYPE:=TRUE; 
       END
  ELSE BEGIN
         ARITHTYPE:=FALSE;
       END; 
END;
(* ARITHTYPE
 *) 
  
  
  
PROCEDURE TYPECOERCE(TOO : INTEGER;VAR T:EXPTREE);
VAR T1:EXPTREE; 
    N,N1:INTEGER; 
BEGIN 
  IF ISFIELD(INDIRECT(TOO)) 
  THEN BEGIN
         TOO:=ORD(INT); 
       END; 
  IF T^.TOKN = KONSTANT 
  THEN BEGIN
         IF (INDIRECT(TOO)<>ORD(REEL))AND(INDIRECT(TOO)<>ORD(DOUBLE)) 
         THEN T^.EMODE:=(TOO);
       END; 
  N:=INDIRECT(T^.EMODE);
  N1:=INDIRECT(TOO);
  WHILE N<>N1 
  DO BEGIN
       IF ARITHTYPE(N) AND ARITHTYPE(N1)
       THEN BEGIN 
              T1:=NEWEXPNODE(CONVERT,CAST); 
              IF N < ORD(UNSIGNED)
              THEN IF N1 >= ORD(UNSIGNED) 
              THEN BEGIN
                     T1^.EMODE:=ORD(INT); 
                     T1^.RGHT:=T; 
                     T:=T1; 
                     N:=ORD(INT); 
                   END
              ELSE N:=N1
              ELSE IF (N=ORD(INT))OR(N=ORD(UNSIGNED)) 
              THEN BEGIN
                     IF N1 < ORD(UNSIGNED)
                     THEN BEGIN 
                            T1^.EMODE := N1;
                            T1^.RGHT:=T;
                            T:=T1;
                            N:=N1;
                          END 
                     ELSE IF N1 > ORD(INT)
                     THEN BEGIN 
                            T1^.EMODE:=ORD(LONG); 
                            T1^.RGHT:=T;
                            T:=T1;
                            N:=ORD(LONG); 
                          END 
                     ELSE N:=N1;
                   END
              ELSE IF N=ORD(LONG) 
              THEN BEGIN
                     IF N1 < N
                     THEN BEGIN 
                            T1^.EMODE:=ORD(INT);
                            T1^.RGHT:=T;
                            T:=T1;
                            N:=ORD(INT);
                          END 
                     ELSE BEGIN 
                            T1^.EMODE:=ORD(DOUBLE); 
                            T1^.RGHT:=T;
                            T:=T1;
                            N:=ORD(DOUBLE); 
                          END;
                   END
              ELSE IF N<>N1 
              THEN BEGIN
                     WRITE(CODE,'*TYPECOERCE: N=');PRINTTYPE(N);WRITE(CODE,' N1=');PRINTTYPE(N1); 
                     WRITELN(CODE); 
                     N:=N1; 
                    (* WARN(5); *)
                   END;;
            END 
     ELSE IF ISPTRTYPE(N1)
     THEN BEGIN 
            IF ISPTRTYPE(N) 
            THEN N:=N1
            ELSE BEGIN
                   IF(N=ORD(LONG)) OR (N=ORD(INT))
                   THEN TYPECOERCE(N,T) 
                   ELSE TYPECOERCE(ORD(LONG),T);
                   T1:=NEWEXPNODE(CONVERT,CAST);
                   T1^.EMODE:=TOO;
                   T1^.RGHT:=T; 
                   T:=T1; 
                   N:=N1; 
                 END; 
          END 
     ELSE BEGIN 
            IF ISPTRTYPE(N) 
            THEN BEGIN
                   T1:=NEWEXPNODE(CONVERT,CAST);
                   IF (N1 = ORD(LONG))OR(N1=ORD(INT))OR(N1=ORD(UNSIGNED)) THEN T1^.EMODE := N1
                   ELSE T1^.EMODE :=ORD(LONG);
                   T1^.RGHT := T; 
                   T:=T1; 
                   N:=T1^.EMODE;
                 END
            ELSE BEGIN
                   WRITELN(CODE,'*TYPECOERCE: TOTALLY LOST!');
                   N:=N1; 
                   ERROR(68); 
                 END; 
          END;
     END; 
END;
  
  
  
PROCEDURE PROCP1(VAR T:EXPTREE;VAR N:INTEGER);
VAR T1 : EXPTREE; 
(* TYPEING ROUTINE FOR CALLS *) 
BEGIN 
  IF T<>NIL 
  THEN WITH T^
       DO BEGIN 
            IF TOKN=PARAM 
            THEN BEGIN
                   PROCP1(LFT,N); 
                   OFSET:=N;
                   EMODE:=(TYPETREE(RGHT)); 
                   IF TYPETAB[INDIRECT(EMODE)].BASICTYPE IN [ARRAYTYPE,STRUCT,UNION,FUNK] 
                   THEN BEGIN 
                          T1:=NEWEXPNODE(REF,UNARY);
                          T1^.EMODE:=POINTERTO(T^.EMODE); 
                          T1^.RGHT:=T;
                           T:=T1; 
                        END;
                          IF INDIRECT(EMODE) < ORD(UNSIGNED)
                          THEN BEGIN
                                 TYPECOERCE(ORD(INT),RGHT); 
                               END; 
                          T^.EMODE:=RGHT^.EMODE;
                   N:=N+SIZEOFTYPE(INDIRECT(EMODE));
                 END
            ELSE BEGIN
                   EMODE:=(TYPETREE(T));
                   IF TYPETAB[INDIRECT(EMODE)].BASICTYPE IN [ARRAYTYPE,STRUCT,UNION,FUNK] 
                   THEN BEGIN 
                          T1:=NEWEXPNODE(REF,UNARY);
                          T1^.EMODE:=POINTERTO(T^.EMODE); 
                          T1^.RGHT:=T;
                          T:=T1;
                        END;
                   IF INDIRECT(EMODE) < ORD(UNSIGNED) 
                   THEN BEGIN 
                          TYPECOERCE(ORD(INT),T); 
                        END;
                   N:=SIZEOFTYPE((INDIRECT(T^.EMODE))); 
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
                        TYPECALL:=POINTERTO(TYPETAB[T1].RETRN); 
                      END;
               END
          ELSE TYPECALL:=ORD(INT);
  T^.PARAMSIZE:=N;
END;
(* TYPECALL 
 *) 
  
  
PROCEDURE SCALETREE(VAR T : EXPTREE ; N : INTEGER); 
  
(*
 * * "T" IS AN EXPRESSION TREE WHOSE VALUE IS TO BE ADDED TO A POINTER
 * * OF TYPE "N". 
 *) 
VAR T1,T2 : EXPTREE;
    N1    : INTEGER;
    CNT : INTEGER;
BEGIN 
  N1:=TYPETREE(T);
   IF SIZEOFTYPE(INDIRECT(N))<>1
  THEN IF T^.TOKN=KONSTANT
  THEN T^.SVALUE:=T^.SVALUE*SIZEOFTYPE(INDIRECT(N)) 
  ELSE BEGIN
         T1:=NEWEXPNODE(NOOP,KONSTANT); 
         T1^.EMODE := T^.EMODE; 
         T2:=NEWEXPNODE(TIMES,BINARY);
         CASE INDIRECT(N1)
         OF 0,1,2 : T2^.EMODE:=ORD(INT);
         OTHERWISE  T2^.EMODE:=ORD(LONG); 
         END; 
         T2^.RGHT:=T1;
         T2^.LFT:=T;
         T:=T2; 
  T1^.SVALUE:=SIZEOFTYPE(INDIRECT(N));
         IF INDIRECT(T^.EMODE)<> ORD(INT) 
         THEN TYPECOERCE(ORD(INT),T); 
       END; 
END;
(* SCALETREE
 *) 
  
  
  
PROCEDURE TYPEINDEX(VAR T:EXPTREE); 
VAR N : INTEGER;
    T1 : EXPTREE; 
BEGIN 
  IF T^.TOKN=INDEXOP
  THEN WITH T^
  DO BEGIN
       TYPEINDEX(LFT);
       N:=INDIRECT(LFT^.EMODE); 
       IF ISPTRTYPE(N)
       THEN BEGIN 
              SCALETREE(RGHT,N);
              EMODE:=POINTERTO(INDIRECT(N));
           END
       ELSE BEGIN 
              ERROR(13);
            END;
       IF INDIRECT(RGHT^.EMODE) < ORD(UNSIGNED) 
       THEN TYPECOERCE(ORD(INT),RGHT);
     END
  ELSE BEGIN
         N:=TYPETREE(T);
         T^.EMODE:=(N); 
       END; 
END;
(* TYPEINDEX
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
            IF (RGHT^.TOKN = KONSTANT) AND (N <> N1)
            THEN BEGIN
                   IF N < ORD(UNSIGNED) 
                   THEN IF (RGHT^.SVALUE < 256) OR (RGHT^.SVALUE > -256)
                        THEN RGHT^.EMODE:=N;
                 END
            ELSE IF LFT^.TOKN=KONSTANT
            THEN LFT^.EMODE:=N1;
            N:=INDIRECT(N);N1:=INDIRECT(N1);
            IF ARITHTYPE(N)AND ARITHTYPE(N1)
            THEN IF (N < N1) AND (TOKN <> ASSIGN) 
                 THEN BEGIN 
                        TYPECOERCE(N1,LFT); 
                        TWOARITHTYPES:=TRUE;
                        EMODE:=N1;
                      END 
                 ELSE BEGIN 
                        TYPECOERCE(N,RGHT); 
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
                   N:=EMODE;
                   T1:=T; 
                   T:=T^.RGHT;
                   DISPOSE(T1); 
                   TYPECOERCE(POINTERTO(N),T);
                 END
            ELSE IF OPER=DEREF
            THEN BEGIN
                   TOKN:=UNARY; 
                   N:=(TYPETREE(RGHT)); 
                   IF NOT(TYPETAB[N].BASICTYPE IN [INT,LONG,UNSIGNED,SHORT,KAR,ARRAYTYPE,POINTER])
                   THEN BEGIN 
                          ERROR(14);
                        END 
                   ELSE N:=(INDIRECT(N)); 
                   IF INDIRECT(RGHT^.EMODE)<=ORD(LONG)
                   THEN BEGIN 
                          TYPECOERCE(POINTERTO(RGHT^.EMODE),RGHT);
                        END;
                END 
           ELSE IF OPER=REF 
           THEN BEGIN 
                  N:= POINTERTO(((TYPETREE(T^.RGHT)))); 
                  TOKN:=UNARY;
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
                   IF NOT(ARITHTYPE(N)) 
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
                        END 
                   ELSE IF OPER = ONESCOMP
                   THEN CASE N
                   OF 0,1,2 : BEGIN 
                                T1:=RGHT; 
                                DISPOSE(T); 
                                T:=T1;
                                T^.SVALUE:=255-(T^.SVALUE MOD 256); 
                              END;
                      3,4 :   BEGIN 
                                T1:=RGHT; 
                                DISPOSE(T); 
                                T:=T1;
                                T^.SVALUE := 65535 - (T^.SVALUE MOD 65536); 
                              END;
                      OTHERWISE ; 
                    END;
                 END
            ELSE IF OPER IN [INCR,DECR,POSTINC,POSTDEC] 
            THEN BEGIN
                   TOKN:=UNARY; 
                   IF LFT<>NIL
                   THEN BEGIN 
                          N:=(TYPETREE(LFT)); 
                          RGHT:=NIL;
                        END 
                   ELSE N:=(TYPETREE(RGHT));
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
            IF (ISPTRTYPE(N))AND ARITHTYPE(N1)
            THEN BEGIN
                   SCALETREE(RGHT,N); 
                   T^.EMODE:=LFT^.EMODE;
                 END
            ELSE IF(ISPTRTYPE(N1))AND ARITHTYPE(N)
            THEN BEGIN
                   SCALETREE(LFT,N1); 
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
    CNT : INTEGER;
BEGIN 
  FLAG:=TRUE; 
  IF T<>NIL 
  THEN WITH T^
       DO BEGIN 
            N:=LFT^.EMODE;
            N1:=RGHT^.EMODE;
            N:=INDIRECT(N);N1:=INDIRECT(N1);
            CNT:=SIZEOFTYPE(INDIRECT(N)); 
IF(ISPTRTYPE(N)) AND
    (ISPTRTYPE(N1)) 
            THEN IF CNT > 1 
            THEN BEGIN
                   T1:=NEWEXPNODE(NOOP,KONSTANT); 
                   T1^.SVALUE:=SIZEOFTYPE(INDIRECT(N)); 
                   T1^.EMODE:=ORD(INT); 
                   IF (CNT = 2) OR (CNT = 4)
                   THEN BEGIN 
                          T1^.SVALUE:=CNT DIV 2;
                          T2:=NEWEXPNODE(RIGHTSHIFT,BINARY);
                          T2^.EMODE:=ORD(INT);
                        END 
                   ELSE BEGIN 
                          T2:=NEWEXPNODE(DIVIDE,BINARY);
                          T2^.EMODE:=ORD(INT);
                        END;
                   T2^.LFT:=T;
                   T2^.RGHT:=T1;
                   EMODE:=ORD(INT); 
                   T:=T2; 
                 END
                 ELSE 
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
           IF RGHT^.TOKN = KONSTANT 
           THEN BEGIN 
                  RGHT^.EMODE:=N1;
                  N2:=N1; 
                END;
           IF LFT^.TOKN = KONSTANT
           THEN BEGIN 
                  LFT^.EMODE:=N2; 
                  N1:=N2; 
                END;
           IF ARITHTYPE(N1) THEN ;
           IF ARITHTYPE(N2) THEN ;
           TWOINTEGRALTYPES:=FALSE; 
           IF(N1<>ORD(DOUBLE))AND(N2<>ORD(DOUBLE))
           THEN BEGIN 
                  TWOINTEGRALTYPES:=TRUE; 
                  IF (N1<N2) AND (TOKN <> ASSIGN) 
                   THEN BEGIN 
                         TYPECOERCE(N2,LFT);
                        END 
                   ELSE IF (N2<N1) OR ( (TOKN = ASSIGN) AND ( N1<>N2))
                   THEN TYPECOERCE(N1,RGHT);
                END;
            T^.EMODE:=N1; 
          END;
END;
(* TWOINTEGRALTYPES 
 *) 
  
TYPE BITREC = RECORD CASE T1:BOOLEAN
              OF TRUE : ( INT : INTEGER); 
                 FALSE: ( BIT : PACKED ARRAY[0..59] OF BOOLEAN);
              END;
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
  
  
  
  
 FUNCTION BINARYOP(VAR T:EXPTREE):BOOLEAN;
VAR FLAG : BOOLEAN; 
    N,N1 : INTEGER; 
    T1   : EXPTREE; 
  
  
BEGIN 
  FLAG :=TRUE;
  IF T<>NIL 
  THEN WITH T^
       DO CASE OPER 
          OF
           TIMES,DIVIDE : BEGIN 
                            IF NOT(TWOARITHTYPES(T))
                            THEN (* ERROR(16) *)
                            ELSE BEGIN
                                IF (OPER=TIMES) AND (LFT^.TOKN = KONSTANT)
                                   THEN BEGIN 
                                          T1:=T^.LFT; 
                                          T^.LFT:=T^.RGHT;
                                          T^.RGHT :=T1; 
                                        END;
                                    IF (RGHT^.TOKN=KONSTANT) OR(LFT^.TOKN=KONSTANT) 
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
                                  END;
                            IF TWOKONSTS(T) AND (T^.OPER=TIMES) 
                            THEN BEGIN
                                   T1:=NEWEXPNODE(NOOP,KONSTANT); 
                                   T1^.EMODE:=ORD(INT); 
                                   IF OPER=TIMES
                                   THEN T1^.SVALUE:=RGHT^.SVALUE*LFT^.SVALUE ;
                                   DISCARD(T);
                                   T:=T1; 
                                 END; 
                            IF OPER = TIMES 
                            THEN IF T^.EMODE < ORD(UNSIGNED)
                                 THEN T^.EMODE :=ORD(INT);
                          END;
  
           PLUS         : BEGIN 
                            IF NOT(TWOARITHTYPES(T))
                            THEN IF NOT(ONEPOINTER(T))
                                 THEN (* ERROR(16); *)
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
           LEFTSHIFT,RIGHTSHIFT 
           : BEGIN
               N:= INDIRECT(TYPETREE(LFT)); 
               N1:=INDIRECT(TYPETREE(RGHT));
               IF NOT(ARITHTYPE(N) AND ARITHTYPE(N1)) 
               THEN ERROR(19);
               EMODE:= N; 
               T^.EMODE:=LFT^.EMODE;
             END; 
  
           MODOP,ANDOP, 
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
  
                            OTHERWISE ; 
                          END;
                          END;
  
           EQOP,NEOP,LTOP,LEOP,GTOP,GEOP
           : BEGIN
               IF NOT(TWOARITHTYPES(T)) 
               THEN IF ONEPOINTER(T)
                    THEN ;
               EMODE:= ORD(INT);
             END; 
  
           IFOP : BEGIN 
                    N:=INDIRECT(TYPETREE(LFT)); 
                    N1:=INDIRECT(TYPETREE(RGHT)); 
                    EMODE:=N1;
                  END;
  
           ELSEOP : BEGIN 
                      N:= INDIRECT(TYPETREE(LFT));
                      N1:=INDIRECT(TYPETREE(RGHT)); 
                      IF ARITHTYPE(N) AND ARITHTYPE(N1) 
                      THEN IF N < N1
                           THEN TYPECOERCE(N1,LFT)
                           ELSE TYPECOERCE(N,RGHT)
                      ELSE IF ISPTRTYPE(N)
                           THEN TYPECOERCE(N,RGHT)
                           ELSE TYPECOERCE(N1,LFT); 
                    END;
  
           SELECTOR     : BEGIN 
                            N:=INDIRECT(TYPETREE(LFT)); 
                            N1:=(TYPETREE(RGHT)); 
                            EMODE:=(N1);
                          END;
  
           REFSELECTOR  : BEGIN 
                            N:=INDIRECT(TYPETREE(LFT)); 
                            N1:=INDIRECT(TYPETREE(RGHT)); 
                            EMODE:=POINTERTO(N1); 
                            IF LFT^.TOKN = KONSTANT 
                            THEN LFT^.EMODE := POINTERTO(N1); 
                          END;
  
           ANDFOP,ORFOP : BEGIN 
                            N:=INDIRECT(TYPETREE(LFT)); 
                            N1:=INDIRECT(TYPETREE(RGHT)); 
                            EMODE := POINTERTO(N);
                            TYPECOERCE(N,RGHT); 
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
  BINARYOP:=FLAG; 
END;
(* BINARYOP 
 *) 
  
  
  
  
  
  
FUNCTION TYPETREE;
LABEL 1;
VAR T1 : EXPTREE ;
    TYP: INTEGER; 
    N,N1 : INTEGER; 
FUNCTION LEFTOF(T : EXPTREE): EXPTREE;
BEGIN 
  IF T<>NIL 
  THEN LEFTOF:=T^.LFT 
  ELSE LEFTOF:=NIL; 
END;
  
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
                    END;
  
            KONSTANT : BEGIN
                         TYP:=EMODE;
                         N:=TYP;
                       END; 
  
            STRING : BEGIN
                       TYP := POINTERTO(POINTERTO(ORD(KAR))) ;
                       N:=TYP ; 
                     END; 
  
            CALL  : BEGIN 
                      TYP:=TYPECALL(T); 
                      N:=TYP; 
                    END;
  
            INDEXOP : BEGIN 
                        TYPEINDEX(T); 
                      TYP:=(EMODE); 
                      N:=TYP; 
                    END;
  
            ASSIGN :  BEGIN 
                        IF OPER <> NOOP 
                        THEN GOTO 1;
                        N:=INDIRECT(TYPETREE(LFT)); 
                        N1:=INDIRECT(TYPETREE(RGHT)); 
                        IF RGHT^.TOKN=KONSTANT
                        THEN RGHT^.EMODE:=N;
                        IF (TYPETAB[N].BASICTYPE = POINTER) 
                                AND (T^.OPER = NOOP)
                        THEN IF (TYPETAB[N1].BASICTYPE IN [ARRAYTYPE,STRUCT,UNION,FUNK])
                             THEN BEGIN 
                                    T1:=NEWEXPNODE(REF,UNARY);
                                    T1^.EMODE:=POINTERTO(N1); 
                                    T1^.RGHT:= T^.RGHT; 
                                    T^.RGHT := T1;
                                  END;
                        EMODE:=N; 
                        TYPECOERCE((LFT^.EMODE),RGHT);
                        TYP:=EMODE; 
                      END;
  
  
            OTHERWISE BEGIN 
                   1 : IF UNARYOP(T)
                      THEN TYP:=T^.EMODE
                      ELSE IF BINARYOP(T) 
                           THEN TYP:=T^.EMODE;
                      N:=TYP; 
                      END;
          END;
(* CASE 
 *) 
  IF T<>NIL 
  THEN WITH T^
  DO BEGIN
          T^.EMODE:=TYP;
       END; 
  TYPETREE:=N;
END;
(* TYPETREE 
 *) 
  
  
  
(*$L'CODE GENERATOR ROUTINES'*) 
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
  
  
FUNCTION ALENGOF(I : ALFA):INTEGER; 
LABEL 1;
VAR CNT : INTEGER;
BEGIN 
  FOR CNT := 1 TO 10 DO IF I[CNT]=' ' THEN GOTO 1;
  1 : IF I[CNT]=' ' THEN ALENGOF:=CNT-1 
      ELSE ALENGOF:=CNT;
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
  
  
PROCEDURE OUTNAME;
VAR I : INTEGER;
BEGIN 
  FOR I:=1 TO 7 
  DO WRITE(CODE,STRNG[I]);
  WRITE(CODE,'   ');
END;
(* OUTNAME
 *) 
  
  
PROCEDURE OUTREG(RG:INTEGER); 
BEGIN 
  OUTALFA(RNAME[RG],9); 
END;
PROCEDURE OUTPREG(PR:INTEGER);
BEGIN 
  OUTREG(PREGSET[PR].REGN); 
END;
  
PROCEDURE CODEIT(TXT : ALFA; RG : INTEGER); 
LABEL 1;
VAR CNT : INTEGER;
BEGIN 
  FOR CNT := 1 TO 10
  DO BEGIN
       IF TXT[CNT]=' ' THEN GOTO 1; 
       IF TXT[CNT]<>'.' 
       THEN WRITE(CODE,TXT[CNT])
       ELSE IF RG < 9 THEN WRITE(CODE,'B')
       ELSE WRITE(CODE,'W');
     END; 
 1:   (* COME HERE WHEN FINISHED *) 
END;
  
  
  
PROCEDURE MOVBMR(R : INTEGER);
BEGIN 
  PREVIOUSLABEL;
  WRITE(CODE,'MOVBMR ');OUTREG(R);
  WRITELN(CODE);
END;
  
PROCEDURE MOVWMR(R : INTEGER);
BEGIN 
  PREVIOUSLABEL;
  WRITE(CODE,'MOVWMR ');OUTREG(R);
  WRITELN(CODE);
END;
  
PROCEDURE MOVBRM(R : INTEGER);
BEGIN 
  PREVIOUSLABEL;
  WRITE(CODE,'MOVBRM ');OUTREG(R);
  WRITELN(CODE);
END;
  
PROCEDURE MOVWRM(R : INTEGER);
BEGIN 
  PREVIOUSLABEL;
  WRITE(CODE,'MOVWRM ');OUTREG(R);
  WRITELN(CODE);
END;
  
PROCEDURE MOVBTRI(R,V : INTEGER); 
BEGIN 
  PREVIOUSLABEL;
  WRITE(CODE,'MOVBTRI ');OUTREG(R); 
  WRITELN(CODE,',',V:1);
END;
  
PROCEDURE MOVWTRI(R,V : INTEGER); 
BEGIN 
  PREVIOUSLABEL;
  WRITE(CODE,'MOVWTRI ');OUTREG(R); 
  WRITELN(CODE,',',V:1);
END;
  
PROCEDURE OUTINDR(REGN:INTEGER);
BEGIN 
  CASE REGN 
  OF BX : WRITE(CODE,'BXNI'); 
     SI : WRITE(CODE,'NBSI'); 
     DI : WRITE(CODE,'NBDI'); 
     BP : WRITE(CODE,'BPNI'); 
  OTHERWISE BEGIN 
              WRITE(CODE,'XXXX'); 
              ERROR(68);
            END;
  END;
END;
  
  
PROCEDURE OUTIND(REGN:INTEGER;RELPOS:INTEGER);
BEGIN 
  IF (RELPOS=0)AND(REGN<>BP)
  THEN BEGIN
         WRITE(CODE,'          NODISP '); 
         OUTINDR(REGN); 
         WRITELN(CODE); 
       END
  ELSE IF (RELPOS> -128)AND (RELPOS < 128)
  THEN BEGIN
 WRITE(CODE,'          SHORT ');
         OUTINDR(REGN);(* ;WRITELN(CODE); *)
         WRITELN(CODE,',',RELPOS:1);
       END
  ELSE BEGIN
         WRITE(CODE,'           LONG ');
         OUTINDR(REGN); 
         WRITELN(CODE,',',RELPOS:1);
       END; 
END;
  
  
PROCEDURE OUTINDX(RG1,REL,RG2 : INTEGER); 
BEGIN 
  IF RG2<>0 
  THEN WRITELN(CODE,'*OUTINDX: RG2 NON ZERO');
  OUTIND(RG1,REL);
END;
  
  
  
PROCEDURE LOCAL(N : INTEGER); 
BEGIN 
  OUTIND(BP,-N);
END;
  
PROCEDURE REGISTR(REGN:INTEGER);
BEGIN 
  IF REGN=0 
  THEN BEGIN
         WRITELN(CODE,'*REGN=0 IN REGISTER.AX ASSUMED');
         REGN:=AX;
       END; 
  WRITE(CODE,'          REGISTER ');
  OUTREG(REGN);WRITELN(CODE,'     '); 
END;
  
  
  
FUNCTION GETR16(NOTR : INTEGER):INTEGER;FORWARD;
FUNCTION GETR8(NOTR : INTEGER):INTEGER;FORWARD; 
PROCEDURE FREER16(RG : INTEGER);FORWARD;
  
PROCEDURE SEGCHECK(T:EXPTREE);
VAR RG : INTEGER; 
BEGIN 
  WRITELN(CODE,'* SEGCHECK'); 
END; (* SEGCHECK*)
  
  
  
PROCEDURE SEGOVR(T:EXPTREE);
BEGIN 
  WRITELN(CODE,'* SEGOVR ');
END; (* SEGOVR *) 
  
  
PROCEDURE EXIT1;
VAR MODE:INTEGER; 
BEGIN 
  MODE:=FUNCID^.IR.THISMODE;
  IF TYPETAB[MODE].BASICTYPE = FUNK 
  THEN MODE:=TYPETAB[MODE].RETRN; 
  PREVIOUSLABEL;
  IF MODE < ORD(LONG) 
  THEN WRITELN(CODE,'JMPDS =XCRET,=XCRET.') 
  ELSE IF MODE=ORD(LONG)
  THEN WRITELN(CODE,'JMPDS =XCRETL,=XCRETL.') 
  ELSE IF MODE <=ORD(DOUBLE)
  THEN WRITELN(CODE,'JMPDS =XCRETF,=XCRETF.') 
  ELSE WRITELN(CODE,'JMPDS =XCRETP,=XCRETP.');
  REACHABLE:=FALSE; 
END;
  
  
PROCEDURE OUTADRS(T : EXPTREE;RELPOS : INTEGER);FORWARD;
  
  
PROCEDURE EXITCODE; 
BEGIN 
  IF REACHABLE
  THEN BEGIN
         PREVIOUSLABEL; 
         WRITELN(CODE,'JMPDS =XCRETI,=XCRETI.');
  END;
END;
  
  
 FUNCTION GIVELABEL;
  
(* FUNCTION RETURNING A NEW LABEL NUMBER
 *) 
  BEGIN 
    GIVELABEL:=NEXTLABELPOS;
    NEXTLABELPOS:=SUCC(NEXTLABELPOS); 
  END;
(* GIVELABEL
 *) 
  
PROCEDURE LABELIT(LAB:INTEGER); 
VAR I : INTEGER;
 BEGIN
  FOR I:= 0 TO 20 DO REGSET[I].IDPTR:=NIL;
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
  
PROCEDURE PROCHED(NAME : ALFA); 
VAR I : INTEGER;
BEGIN 
  FOR I:= 0 TO 20 DO REGSET[I].IDPTR:=NIL;
         WRITE(CODE,'          IDENT ');
         OUTALFA(NAME,6); 
         WRITELN(CODE,'.'); 
         WRITE(CODE,'          RELOC ');
         WRITELN(CODE); 
(*  WRITE(CODE,'          DEFM (');OUTALFA(NAME,10);WRITELN(CODE,')');
  WRITELN(CODE,'          DW 0#0A0D');
  WRITELN(CODE,'          DB 0'); 
  WRITELN(CODE,'          DW ',ALENGOF(NAME):1);
*)
  WRITE(CODE,'          ENTRY '); 
  OUTALFA(NAME,7 ); 
  WRITELN(CODE);
  OUTNAME(NAME);
  WRITELN(CODE,'CALLDS =XCSAV,=XCSAV.');
  STACKPTR:=0;
  REACHABLE:=TRUE;
END;
PROCEDURE PROCHEADER(LID : IDTREE); 
BEGIN 
  PROCHED(LID^.IR.NAME);
END;
  
PROCEDURE STACKSET; 
BEGIN 
  PREVIOUSLABEL;
  WRITELN(CODE,'LEA SP'); 
  WRITELN(CODE,'          LONG BPNI,-',(CURRENT^.AUTOSIZE):1);
  TOPOFSTACK:=CURRENT^.AUTOSIZE;
  STACKPTR:=TOPOFSTACK; 
END;
  
  
  
PROCEDURE STACKCHECK; 
VAR DIFF : INTEGER; 
BEGIN 
  IF STACKPTR <> TOPOFSTACK 
  THEN BEGIN
         PREVIOUSLABEL; 
         WRITELN(CODE,'LEA SP');
         LOCAL(TOPOFSTACK); 
         STACKPTR:=TOPOFSTACK;
       END; 
END;
  
  
(*$L'REGISTER ALLOCATION ROUTINES'*)
(* REGISTER ALLOCATION ROUTINES *)
(* IN AN EXPRECORD IS A POINTER TO A PSUEDO REGISTER *) 
(* THE PSUEDO REGISTER IS USED TO KEEP TRACK OF WHERE THE SUBEXPRESSION *)
(* IS CURRENTLY RESIDING AND WHETHER A REGISTER HAS BEEN ASSIGNED IT *) 
  
  
PROCEDURE PSALLOC(T : EXPTREE); 
(* ROUTINE TO WALK THE EXPRESSION TREE AND ALLOCATE PSUEDO REGS 
 * AND CALCULATE THE MAX DEPTH AT A POINT IN THE TREE *)
BEGIN 
  IF T<> NIL
  THEN WITH T^
  DO BEGIN
       PSREG := 0;
       PSALLOC(LFT);
       IF RGHT <> NIL 
       THEN BEGIN 
              PSALLOC(RGHT);
              T^.STKTMP:=RGHT^.STKTMP+1;
            END 
       ELSE T^.STKTMP:=0; 
       IF LFT <> NIL THEN STKTMP:=GREATEROF(LFT^.STKTMP+1,T^.STKTMP); 
     END; 
END;
  
FUNCTION NEXTREG:INTEGER; 
BEGIN 
  CURRENTREG:=CURRENTREG+1; 
  NEXTREG:=CURRENTREG;
END;
  
  
  
PROCEDURE INITPSREGS; 
VAR I : INTEGER;
BEGIN 
  FOR I := 0 TO NOPREGS 
  DO WITH PREGSET[I]
  DO BEGIN
       REGN:=0; 
       PSTKP := NIL;
       COUNT:=0;
       SREGN:=0;
      PTYPE := 0; 
     END; 
END;
  
PROCEDURE INITREGS; 
VAR I : INTEGER;
BEGIN 
  REGSET[0].RFLAGS :=[RES]; 
  REGSET[0].PREG:=0;
  FOR I:= 1 TO 8
  DO WITH REGSET[I] 
  DO BEGIN
       RFLAGS := [FREE,R8]; 
       PREG:= 0;
     END; 
  FOR I:= 9 TO 16 
  DO WITH REGSET[I] 
  DO BEGIN
       RFLAGS:=[FREE,R16];
       PREG:=0; 
     END; 
 FOR I:= 0 TO 20
  DO BEGIN
        REGSET[I].PREG:=0;
      END;
  REGSET[17].RFLAGS:=[FREE];
  REGSET[18].RFLAGS:=[RES]; 
  REGSET[19].RFLAGS:=[RES]; 
  REGSET[20].RFLAGS:=[FREE];
  
  REGSET[13].RFLAGS:=[RES]; 
  REGSET[14].RFLAGS:=[RES]; 
  
  REGSET[12].RFLAGS := [FREE,R16,INDX]; 
  REGSET[15].RFLAGS := [FREE,R16,INDX]; 
  REGSET[16].RFLAGS := [FREE,R16,INDX]; 
END;
  
  
PROCEDURE DMPREGS;
VAR I,J : INTEGER;
BEGIN 
  WRITE(CODE,'*       '); 
  FOR I:= 1 TO 20 
  DO BEGIN
       WRITE(CODE,'  ');OUTREG(I);WRITE(CODE,'  '); 
     END; 
  WRITELN(CODE);
  WRITE(CODE,'* PREG= '); 
  FOR I:=1 TO 20
  DO WITH REGSET[I] 
  DO BEGIN
       FOR J:= 1 TO (4- LENGTHOF(PREG)) DO WRITE(CODE,' '); 
       WRITE(CODE,PREG:1,'  '); 
     END; 
  WRITELN(CODE);
  WRITE(CODE,'* FREE= '); 
  FOR I:=1 TO 20
  DO WITH REGSET[I] 
  DO IF FREE IN RFLAGS THEN WRITE(CODE,' FREE ')
                       ELSE WRITE(CODE,'      '); 
  WRITELN(CODE);
  WRITE(CODE,'* REGN= '); 
  FOR I:=1 TO 20
  DO WITH REGSET[I] 
  DO IF PREG<>0 THEN BEGIN
                        FOR J:= 1 TO (4-LENGTHOF(PREGSET[PREG].REGN))DO WRITE(CODE,' ');
                        WRITE(CODE,(PREGSET[PREG].REGN):1,'  ');
                      END 
                ELSE WRITE(CODE,'      ');
  WRITELN(CODE);
  WRITE(CODE,'* SREGN '); 
  FOR I:=1 TO 20
  DO WITH REGSET[I] 
  DO IF PREG<>0 THEN BEGIN
                       FOR J:= 1 TO (4-LENGTHOF(PREGSET[PREG].SREGN)) DO WRITE(CODE,' '); 
                       WRITE(CODE,(PREGSET[PREG].SREGN):1,'  ');
                     END
                ELSE WRITE(CODE,'      ');
  WRITELN(CODE);
  WRITE(CODE,'*       '); 
  FOR I:= 1 TO 20 DO WRITE(CODE,'-=--=-');
  WRITELN(CODE);
END;
  
PROCEDURE OUTOFSTACK(S : RSTKPTR);
BEGIN 
  WITH S^ 
  DO BEGIN
       IF NEXT1<>NIL
       THEN S^.NEXT1^.LAST1:=S^.LAST1;
       IF LAST1<>NIL
       THEN S^.LAST1^.NEXT1:=S^.NEXT1 
       ELSE BEGIN 
              HEAD:=S^.NEXT1; 
              IF HEAD=NIL 
              THEN TOPOFSTACK:=S^.STKOFF
              ELSE
                TOPOFSTACK:=HEAD^.STKOFF; 
            END;
       DISPOSE(S);
    END;
END;
  
  
  
  
FUNCTION ISLOADED(IDP : IDTREE; DRFED : BOOLEAN) : INTEGER; 
LABEL 1;
VAR I : INTEGER;
BEGIN 
  ISLOADED:= 0; 
  FOR I:= BX TO DI
  DO IF REGSET[I].IDPTR=IDP 
  THEN BEGIN
         IF DRFED = REGSET[I].IDDRFD
         THEN BEGIN 
                ISLOADED := I;
                GOTO 1; 
              END;
       END; 
1:  
END;
  
PROCEDURE FREER(RG : INTEGER);
BEGIN 
  IF RG<>0
  THEN BEGIN
         WITH REGSET[RG]
         DO BEGIN 
              IDPTR:=NIL; 
              RFLAGS:=RFLAGS+[FREE];
              IF PREG<>0
              THEN BEGIN
                     WITH PREGSET[PREG] 
                     DO BEGIN 
                          IF REGN=RG THEN REGN:=0;
                          IF SREGN=RG THEN SREGN:=0;
                        END;
                   END; 
            END;
         IF (RG>8)AND(RG<13)
         THEN BEGIN 
                REGSET[RG-8].RFLAGS:=[FREE,R8]; 
                REGSET[RG-8].PREG:=0; 
                REGSET[RG-4].RFLAGS:=[FREE,R8]; 
                REGSET[RG-4].PREG:=0; 
              END 
         ELSE BEGIN 
                IF (RG >0)AND (RG<9)
                THEN IF FREE IN REGSET[(1-(RG-1)DIV 4 )*4+(RG-1)MOD 4 +1].RFLAGS
                     THEN WITH REGSET[9+(RG-1)MOD 4]
                     DO BEGIN 
                          RFLAGS:=RFLAGS+[FREE]-[PARTIAL];
                          PREG:=0;
                        END;
              END;
       END; 
END;
  
  
  
PROCEDURE PUSHREG(REG : INTEGER); 
LABEL 1,2;
VAR RG : RSTKPTR; 
BEGIN 
  IF REG = 0
  THEN BEGIN
         WRITELN(CODE,'* PR=0 IN PUSHREG'); 
          DMPREGS;
       END
  ELSE WITH PREGSET[REG]
  DO BEGIN
       IF PSTKP<>NIL
       THEN OUTOFSTACK(PSTKP); PSTKP:=NIL;
       IF REGN<>0 
       THEN IF REGN < 9 
            THEN PTYPE:=ORD(KAR)
            ELSE PTYPE:=ORD(INT); 
       IF SREGN<>0
       THEN IF SREGN<>DX
            THEN PTYPE:=POINTERTO(ORD(INT))      (* DEFINETLY A POINTER OF SOME SORT *) 
            ELSE PTYPE:=ORD(LONG);
       IF REGN= 0 
       THEN BEGIN 
              WRITELN(CODE,'*REGN=0 IN PUSHREG'); 
              DMPREGS;
              ERROR(69);
              GOTO 2; 
            END;
       NEW(RG); 
       WITH RG^ 
       DO BEGIN 
            SPREG:=REG; 
            SPTYPE:=PTYPE;
            STKOFF:=TOPOFSTACK+SIZEOFTYPE(PTYPE); 
            NEXT1:=HEAD;
            IF HEAD<>NIL
            THEN HEAD^.LAST1:=RG; 
            RG^.LAST1:=NIL; 
            HEAD:=RG; 
          END;
          PSTKP:=RG;
          IF TOPOFSTACK=STACKPTR
          THEN BEGIN   (* USE PUSH INSTRUCTIONS *)
                 IF SREGN = DX
                 THEN BEGIN 
                        PREVIOUSLABEL;
                        WRITELN(CODE,'PUSHR DX'); 
                        FREER(DX);
                        TOPOFSTACK:=TOPOFSTACK+2; 
                        STACKPTR:=TOPOFSTACK; 
                      END 
                 ELSE IF SREGN<>0 
                 THEN WRITELN(CODE,'* SAVE SEGMENT REGISTER?'); 
                 (* NOW SAVE OTHER REGISTER *)
                 IF (REGN > 8)
                 THEN BEGIN 
                        PREVIOUSLABEL;
                        WRITE(CODE,'PUSHR '); 
                        OUTREG(REGN); 
                        WRITELN(CODE);
                        FREER(REGN);
                        TOPOFSTACK:=TOPOFSTACK+2; 
                        STACKPTR:=TOPOFSTACK; 
                      END 
                 ELSE GOTO 1; 
               END
          ELSE BEGIN
                 IF REGN = 0 THEN GOTO 2; 
         1 :  TOPOFSTACK:=TOPOFSTACK+SIZEOFTYPE(PTYPE); 
              STACKCHECK; 
              IF PREGSET[REG].REGN=0
              THEN BEGIN
                     WRITELN(CODE,'* REGN=0 IN PUSHREG'); 
                     DMPREGS; 
                   END; 
              IF SREGN<>0 
              THEN BEGIN
                     IF SREGN = DX
                     THEN BEGIN 
                            PREVIOUSLABEL;
                            WRITELN(CODE,'MOVWRM DX');
                            WRITELN(CODE,'          LONG BPNI,-',(RG^.STKOFF-2):1); 
                            FREER(DX);
                          END 
                     ELSE WRITELN(CODE,'* SAVE SEGMENT REGISTER?'); 
                  END;
              IF REGN<>0
              THEN BEGIN
                     PREVIOUSLABEL; 
                     WITH REGSET[PREGSET[REG].REGN] 
                     DO BEGIN 
                          IF (PREGSET[REG].REGN>0) AND (PREGSET[REG].REGN < 9)
                          THEN BEGIN
                                 WRITE(CODE,'MOVBRM '); 
                               END
                          ELSE BEGIN
                                 WRITE(CODE,'MOVWRM '); 
                               END; 
                          OUTREG(REGN); 
                          FREER(REGN);
                          WRITELN(CODE);
                          LOCAL(RG^.STKOFF);
                          PREGSET[REG].PSTKP:=RG; 
                        END;
                      END;
            END;
     2 : REGN:=0; 
         SREGN:=0;
       END; 
END;
  
  
FUNCTION SIMPLE(T:EXPTREE):BOOLEAN;FORWARD; 
FUNCTION SIMPLE1(T : EXPTREE):BOOLEAN;
BEGIN 
  SIMPLE1:=SIMPLE(T); 
  IF T^.TOKN=IDENT
  THEN IF T^.SENTRY^.IR.THISSTATE=FIELDVAR
       THEN SIMPLE1:= NOT(ISFIELD(T^.SENTRY^.IR.THISMODE)); 
END;
  
  
FUNCTION SIMPLE;
BEGIN 
  SIMPLE:=FALSE;
  IF T<>NIL 
  THEN WITH T^
  DO BEGIN
       IF TOKN = IDENT
       THEN BEGIN 
              IF (SENTRY^.IR.THISSTATE IN 
                  [AUTOVAR,EXTERNVAR,EXTSTATIC,PARAMVAR,STATICVAR]) 
              AND NOT(ISFIELD(SENTRY^.IR.THISMODE)) 
              THEN SIMPLE := TRUE;
           END
       ELSE IF TOKN = KONSTANT
       THEN SIMPLE:=TRUE
       ELSE IF (OPER=SELECTOR)
       THEN IF SIMPLE(T^.LFT) AND SIMPLE1(T^.RGHT)
       THEN SIMPLE:=TRUE
       ELSE SIMPLE :=FALSE; 
     END; 
END;
  
PROCEDURE OUTADRS;
BEGIN 
  IF T=NIL
  THEN WRITELN(CODE,' NIL ADDRESS IN OUTADRS')
  ELSE WITH T^
  DO BEGIN
       IF (TOKN = IDENT) AND SIMPLE(T)
       THEN WITH SENTRY^.IR 
       DO BEGIN 
            IF THISSTATE IN [AUTOVAR,PARAMVAR]
            THEN BEGIN
                   IF THISSTATE = AUTOVAR 
                   THEN LOCAL(OFFSET-RELPOS+SIZEOFTYPE(INDIRECT(EMODE)))
                   ELSE LOCAL(-(10+OFFSET+RELPOS+SAVEDSIZE)); 
                 END
            ELSE IF THISSTATE IN [EXTERNVAR,EXTSTATIC]
            THEN BEGIN
                   WRITE(CODE,'          ABSOLUTE =X'); 
                   OUTALFA(NAME,7); 
                   IF RELPOS<>0 
                   THEN WRITE(CODE,'+',RELPOS:1); 
                   WRITELN(CODE); 
                 END
            ELSE IF THISSTATE = STATICVAR 
            THEN WRITELN(CODE,'          ABSOLUTE STATPOS+',(OFFSET+RELPOS):1)
            ELSE WRITELN(CODE,'*          IDENTTYPE=',ORD(THISSTATE):1);
          END 
       ELSE IF TOKN=KONSTANT
       THEN BEGIN 
              WITH PREGSET[PSREG] 
              DO BEGIN
                   IF REGN <> 0 THEN REGISTR(REGN)
                   ELSE BEGIN 
                          IF PSTKP=NIL THEN WRITELN(CODE,'*PSTKP=NIL:KONST')
                          ELSE BEGIN
                                 LOCAL(PSTKP^.STKOFF-RELPOS); 
                               END; 
                        END;
                 END; 
            END 
       ELSE IF TOKN=STRING
       THEN WRITELN(CODE,' STRING DUMBO') 
       ELSE IF SIMPLE(T) AND (OPER = SELECTOR)
       THEN BEGIN 
              WHILE T^.OPER=SELECTOR
              DO BEGIN
                   IF RGHT^.TOKN = IDENT
                   THEN RELPOS := RELPOS+ RGHT^.SENTRY^.IR.OFFSET 
                   ELSE IF RGHT^.TOKN = KONSTANT
                   THEN RELPOS := RELPOS+ RGHT^.SVALUE; 
                   T:=T^.LFT; 
                 END; 
              OUTADRS(T,RELPOS);
            END 
       ELSE WITH PREGSET[PSREG] 
       DO BEGIN 
              IF (SREGN <> 0) AND NOT(SIMPLE(T))
              THEN BEGIN
                     IF PREGSET[PSREG].SREGN <> DX
                     THEN OUTIND(PREGSET[PSREG].REGN,RELPOS)
                     ELSE IF RELPOS=0 
                          THEN REGISTR(AX)
                          ELSE REGISTR(DX); 
                   END
              ELSE
              IF REGN<>0
              THEN BEGIN
                     REGISTR(REGN); 
                     IF RELPOS<>0 
                     THEN BEGIN 
                           WRITELN(CODE,'   RELPOS NON ZERO');
                           ERROR(70); 
                          END;
                   END
              ELSE WITH PREGSET[PSREG]
              DO BEGIN
                   IF PSTKP=NIL 
                   THEN WRITELN(CODE,'PSTKP=NIL') 
                   ELSE BEGIN 
                          LOCAL(PSTKP^.STKOFF-RELPOS);
                        END;
                 END; 
            END;
     END; 
  IF DEBUGF THEN WRITELN(CODE,'* OUTADRS'); 
END;
  
PROCEDURE OUTAD1(T : EXPTREE;REL : INTEGER);
BEGIN 
  WITH T^ 
  DO BEGIN
       IF NOT(SIMPLE(T)) AND NOT(TOKN IN [IDENT,KONSTANT])
       THEN IF PREGSET[PSREG].REGN<>0 THEN OUTIND(PREGSET[PSREG].REGN,REL)
                                      ELSE OUTADRS(T,REL) 
       ELSE OUTADRS(T,REL); 
     END; 
END;
  
  
  
PROCEDURE FREEREG(PR : INTEGER);
VAR RG : INTEGER; 
    S1 : IDTREE;
    SD : BOOLEAN; 
BEGIN 
  IF PR = 0 THEN BEGIN
                   WRITELN(CODE,'*PR=0 IN FREEREG');
                 END
  ELSE WITH PREGSET[PR] 
  DO BEGIN
       RG:=REGN;
       WITH REGSET[REGN]
       DO BEGIN 
            S1:= IDPTR; 
            SD:= IDDRFD;
            FREER(RG);
            IDPTR:=S1;
            IDDRFD:=SD; 
          END;
       REGN:=0; 
       IF PSTKP<>NIL
       THEN BEGIN 
              OUTOFSTACK(PSTKP);
              PSTKP:=NIL; 
            END;
       IF SREGN<>0
       THEN WITH REGSET[SREGN]
       DO BEGIN 
            FREER(SREGN); 
            PREG:=0;
          END;
       SREGN:=0;
     END; 
END;
  
PROCEDURE FREER8(RG : INTEGER); 
VAR RG1,PR : INTEGER; 
BEGIN 
WITH REGSET[RG] 
  DO BEGIN
       IF NOT(FREE IN RFLAGS) 
       THEN BEGIN 
              RG1:=9+(RG-1)MOD 4; 
              IF PARTIAL IN REGSET[RG1].RFLAGS
              THEN BEGIN
                     PUSHREG(PREG); 
                     FREER(RG1);
                   END
              ELSE BEGIN
                     PUSHREG(REGSET[RG1].PREG); 
                     PR:=(1-(RG-1)DIV 4)*4+(RG-1)MOD 4 +1;
                     FREER(PR); 
                   END; 
            END;
     END; 
  WITH REGSET[9+(RG-1)MOD 4]
  DO BEGIN
       RFLAGS:=RFLAGS+[PARTIAL]-[FREE]; 
     END; 
END; (* FREER8 *) 
  
FUNCTION GETR8; 
LABEL 1;
VAR PR,COST,I,RG,RG1 : INTEGER; 
BEGIN 
  IF (NOTR=1)OR(NOTR=9) 
  THEN BEGIN
         COST:=PREGSET[REGSET[2].PREG].COUNT; 
         RG:=2; 
       END
  ELSE BEGIN
         COST:=PREGSET[REGSET[1].PREG].COUNT; 
         RG:=1; 
       END; 
  FOR I:= 1 TO 8
  DO WITH REGSET[I] 
  DO BEGIN
       IF (I<>NOTR)AND((9+(I-1)MOD 4)<>NOTR)AND NOT(RES IN RFLAGS)
       THEN BEGIN 
              IF FREE IN RFLAGS 
              THEN BEGIN
                     RG:=I; 
                     GOTO 1;
                   END; 
              IF PARTIAL IN REGSET[9+(I-1)MOD 4].RFLAGS 
              THEN RG1:=PREGSET[REGSET[I].PREG].COUNT 
              ELSE RG1:=PREGSET[REGSET[I].PREG].COUNT+10; 
              IF RG1 < COST 
              THEN BEGIN
                     RG:=I; 
                     COST :=RG1;
                   END; 
            END;
     END; 
1:  
  FREER8(RG); 
  GETR8:=RG;
END; (* GETR8 *)
  
PROCEDURE FREER16;
VAR RG1 : INTEGER;
BEGIN 
   WITH REGSET[RG]
  DO BEGIN
       IF NOT(FREE IN RFLAGS) 
       THEN BEGIN 
              IF NOT(PARTIAL IN RFLAGS) 
              THEN BEGIN
                     PUSHREG(PREG); 
                     FREER(RG); 
                   END
              ELSE IF PARTIAL IN RFLAGS 
              THEN BEGIN
                     RG1:=RG-8; 
                     IF  NOT(FREE IN REGSET[RG1].RFLAGS)
                     THEN BEGIN 
                            PUSHREG(REGSET[RG1].PREG);
                            FREER(RG1); 
                          END;
                     RG1:=RG-4; 
                     IF NOT(FREE IN REGSET[RG1].RFLAGS) 
                     THEN BEGIN 
                            PUSHREG(REGSET[RG1].PREG);
                            FREER(RG1); 
                          END;
                   END; 
            END;
     END; (* WITH *)
END; (* FREER16 *)
  
  
FUNCTION GETR16;
LABEL 1;
VAR PR,COST,I,RG,RG1 : INTEGER; 
BEGIN 
  IF (NOTR>1)AND(NOTR<9)
  THEN NOTR:=9+(NOTR-1)MOD 4; 
  IF NOTR=9 
  THEN BEGIN
         COST:=PREGSET[REGSET[10].PREG].COUNT;
         RG:=10;
       END
  ELSE BEGIN
         COST:=PREGSET[REGSET[9].PREG].COUNT; 
         RG:=9; 
       END; 
  FOR I:=9 TO 16
  DO WITH REGSET[I] 
  DO BEGIN
       IF (I<>NOTR) AND NOT(RES IN RFLAGS)
       THEN BEGIN 
              IF FREE IN RFLAGS 
              THEN BEGIN
                     RG:=I; 
                     GOTO 1;
                   END; 
              IF PARTIAL IN RFLAGS
              THEN RG1:=PREGSET[REGSET[I].PREG].COUNT+10
              ELSE RG1:=PREGSET[REGSET[I].PREG].COUNT;
              IF RG1 < COST 
              THEN BEGIN
                     RG:=I; 
                     COST:=RG1; 
                   END; 
            END;
     END; 
1:  
  FREER16(RG);
  GETR16:=RG; 
END;
  
  
PROCEDURE FREEALL;
VAR RG,PR,I : INTEGER;
BEGIN 
  FOR I:=AX TO BX 
  DO WITH REGSET[I] 
  DO BEGIN
       IF NOT(FREE IN RFLAGS) 
       THEN BEGIN 
              FREER16(I); 
            END;
     END; 
  FOR I:= SI TO DI
  DO IF NOT(FREE IN REGSET[I].RFLAGS)THEN FREER16(I); 
END; (* FREEALL *)
  
PROCEDURE USEIT(RG : INTEGER;PRG : INTEGER);
VAR RG1 : INTEGER;
BEGIN 
  IF PRG=0 THEN BEGIN 
                  WRITE(CODE,'*USEIT: PR = 0, REG = ');OUTREG(RG);WRITELN(CODE);
                END 
  ELSE BEGIN
  WITH REGSET[RG] 
  DO BEGIN
       RFLAGS:=RFLAGS-[FREE,PARTIAL]; 
       PREG := PRG; 
       IF RG<9
       THEN BEGIN 
              RG1:=9+(RG-1)MOD 4; 
              REGSET[RG1].RFLAGS:=REGSET[RG1].RFLAGS+[PARTIAL]-[FREE];
              REGSET[RG1].PREG:=PRG;
            END 
       ELSE IF RG < 13
       THEN BEGIN 
              RG1:=RG-8;
              REGSET[RG1].RFLAGS:=REGSET[RG1].RFLAGS-[FREE,PARTIAL];
              REGSET[RG1].PREG:=PRG;
              RG1:=RG-4;
              REGSET[RG1].RFLAGS:=REGSET[RG1].RFLAGS-[FREE,PARTIAL];
              REGSET[RG1].PREG:=PRG;
            END;
     END; 
  WITH PREGSET[PRG] 
  DO BEGIN
       IF (REGN=0)AND (RG < ES) 
       THEN REGN:=RG
       ELSE IF (SREGN=0) AND ((RG=DX) OR (RG >= ES))
       THEN SREGN:=RG;
       IF RG < 9 THEN PTYPE:=ORD(KAR) 
       ELSE IF SREGN=0 THEN PTYPE:=ORD(INT) 
       ELSE IF SREGN=DX THEN PTYPE:=ORD(LONG) 
       ELSE PTYPE:=POINTERTO(ORD(INT)); 
     END; 
  END; (* ELSE *) 
END; (* USING *)
  
  
PROCEDURE USING2(RG1,RG2,PR : INTEGER); 
BEGIN 
       FREEREG(PR); 
       USEIT(RG1,PR);USEIT(RG2,PR); 
END; (* USING2 *) 
  
  
PROCEDURE USING(RG : INTEGER; PRG : INTEGER); 
BEGIN 
  FREEREG(PRG); 
  USEIT(RG,PRG);
END;
  
  
PROCEDURE GETINDX(PR : INTEGER);
VAR RG : INTEGER; 
BEGIN 
  IF FREE IN REGSET[BX].RFLAGS
  THEN BEGIN
         FREER16(BX); 
         USEIT(BX,PR);
       END
  ELSE IF FREE IN REGSET[SI].RFLAGS 
  THEN BEGIN
         FREER16(SI); 
         USEIT(SI,PR);
       END
  ELSE BEGIN
         FREER16(DI); 
         USEIT(DI,PR);
       END; 
END;
(* ROUTINES TO ENSURE THAT DATA IS IN A SPECIFIED REGISTER
 *
 * WANTIN8 IS USED FOR THE BYTE REGISTERS.
 *
 * WANTIN16 IS USED FOR THE WORD REGISTERS. 
 *) 
  
PROCEDURE WANTIN8(RG,PR : INTEGER); 
BEGIN 
  WITH PREGSET[PR]
  DO BEGIN
       IF REGN<>RG
       THEN BEGIN 
              FREER8(RG); 
              MOVBMR(RG); 
              REGISTR(REGN);
              FREEREG(PR);
              USING(RG,PR); 
            END;
     END; 
END;
  
  
PROCEDURE WANTIN16(RG,PR : INTEGER);
BEGIN 
  WITH PREGSET[PR]
  DO BEGIN
       IF REGN < 9
       THEN BEGIN  (* CONVERT BYTE TO INT *)
              WRITELN(CODE,'*WANTIN16: BYTE FOUND');
              WANTIN8(AL,PR); 
              FREER8(AH); 
              MOVBTRI(AH,0);
              USING(AX,PR); 
            END;
       IF REGN<>RG
       THEN BEGIN 
              FREER16(RG);
              MOVWMR(RG); 
              REGISTR(REGN);
              FREEREG(PR);
              USING(RG,PR); 
            END;
     END; 
END;
  
  
  
PROCEDURE SHIFTIT(PR,OFF: INTEGER; LEFT : BOOLEAN); 
BEGIN 
  IF OFF<>0 
  THEN IF OFF =1
  THEN WITH PREGSET[PR] 
  DO BEGIN
       PREVIOUSLABEL; 
       IF LEFT
       THEN CODEIT('SHL.      ',REGN) 
       ELSE CODEIT('SHR.      ',REGN);
       WRITELN(CODE); 
       REGISTR(REGN); 
     END
  ELSE WITH PREGSET[PR] 
  DO BEGIN
       IF REGN = CX 
       THEN BEGIN 
              REGN:=GETR16(CX);   (* GET A REGISTER THAT IS NOT CX *) 
              MOVWMR(REGN); 
              REGISTR(CX);
              FREER(CX);
            END;
       FREER16(CX);    (* MAKE SURE THAT IT IS EMPTY *) 
       MOVWTRI(CX,OFF); 
       IF LEFT
       THEN BEGIN 
              WRITE(CODE,'          '); 
              CODEIT('SHL.      ',REGN);
              WRITELN(CODE,' CX');
              REGISTR(REGN);
            END 
       ELSE BEGIN 
              WRITE(CODE,'          '); 
              CODEIT('SHR.      ',REGN);
              WRITELN(CODE,' CX');
              REGISTR(REGN);
            END;
       FREER(CX); 
     END; 
END; (* SHIFT IT *) 
  
  
  
PROCEDURE GETFIELD(PR,MASK,OFF : INTEGER);
BEGIN 
  WRITELN(CODE,'         ANDWI ',MASK:1); 
  REGISTR(PREGSET[PR].REGN);
  SHIFTIT(PR,OFF,FALSE);
  PREGSET[PR].PTYPE:=ORD(INT);
END;
  
  
PROCEDURE INREG1(PR : INTEGER); 
BEGIN 
  IF (PR <= 0 ) OR (PR > NOPREGS) 
  THEN BEGIN
         WRITELN(CODE,'*INREG: PR OUT OF RANGE ',PR:1); 
       END
  ELSE WITH PREGSET[PR] 
  DO BEGIN
       IF PSTKP^.STKOFF=STACKPTR
       THEN BEGIN 
              PREVIOUSLABEL;
              WRITE(CODE,'POPR ');OUTREG(REGN); 
              WRITELN(CODE);
              OUTOFSTACK(PSTKP);
              PSTKP:=NIL
;             TOPOFSTACK:=TOPOFSTACK-2; 
              STACKPTR:=STACKPTR-2; 
              IF SREGN<>0 
              THEN BEGIN
                     IF SREGN=DX
                     THEN WRITELN(CODE,'          POPR DX') 
                     ELSE BEGIN 
                            WRITE(CODE,'          POPSR ');OUTREG(SREGN);WRITELN(CODE); 
                          END;
                     STACKPTR:=STACKPTR-2;
                     TOPOFSTACK:=STACKPTR;
                   END; 
            END 
       ELSE BEGIN 
              MOVWMR(REGN); 
              LOCAL(PSTKP^.STKOFF); 
              IF SREGN<>0 
              THEN BEGIN
                     IF SREGN=DX
                     THEN MOVWMR(DX)
                     ELSE BEGIN 
                            WRITE(CODE,'         MOVTSG ');OUTREG(SREGN);WRITELN(CODE); 
                          END;
                     LOCAL(PSTKP^.STKOFF-2);
                   END; 
              OUTOFSTACK(PSTKP);
              PSTKP:=NIL; 
            END;
     END; (* WITH *)
END; (* INREG1 *) 
  
  
  
PROCEDURE INREG(PR : INTEGER;MODE : INTEGER); 
VAR RG : INTEGER; 
BEGIN 
  WITH PREGSET[PR]
  DO BEGIN
       IF (REGN<>0) 
       THEN BEGIN 
              IF ISFIELD(MODE) THEN MODE:=ORD(INT); 
              IF MODE = 0 
              THEN BEGIN
                     WRITE(CODE,'*INREG: MODE ASSIGNED ');PRINTTYPE(PTYPE); 
                     WRITELN(CODE);MODE:=PTYPE; 
                   END; 
              CASE MODE 
              OF 0,1,2 : BEGIN
                       IF REGN > 8
                       THEN BEGIN 
                              WRITELN(CODE,'*INREG: TYPE OVER RIDE'); 
                              WRITE(CODE,'* '); 
                              PRINTTYPE(MODE);WRITELN(CODE);
                              FREEREG(PR);
                            END;
                     END; 
                 3,4 : BEGIN
                       IF (REGN < 9) OR (SREGN<>0)
                       THEN BEGIN 
                              WRITELN(CODE,'*INREG: TYPE OVER RIDE'); 
                            WRITE(CODE,'* ');PRINTTYPE(MODE);WRITELN(CODE); 
                              FREEREG(PR);
                            END;
                     END; 
                 5 : BEGIN
                       IF (SREGN=0) OR (SREGN = DX) 
                       THEN FREEREG(PR);
                     END; 
                 6,7 : BEGIN
                         WRITELN(CODE,'*INREG: CANNOT LOAD FLOAT'); 
                         FREEREG(PR); 
                       END; 
              OTHERWISE BEGIN 
                          IF NOT(REGN IN [BX,SI,DI])
                          THEN BEGIN
                                 WRITE(CODE,'*INREG: NOT AN INDEX REGISTER ');
                                 PRINTTYPE(MODE);WRITELN(CODE); 
                               END; 
                        END;
             END; 
            END;
       IF REGN<>0 
       THEN BEGIN 
              IF SREGN<>0 THEN USING2(REGN,SREGN,PR)
              ELSE USING(REGN,PR);
            END;
       IF REGN=0
       THEN BEGIN 
              IF MODE=0 
              THEN MODE:=PTYPE; 
             IF SIZEOFTYPE((MODE))=1
             THEN BEGIN 
                    REGN:=GETR8(0); 
                    SREGN:=0; 
                    IF PSTKP<>NIL 
                    THEN BEGIN
                           MOVBMR(REGN);
                           LOCAL(PSTKP^.STKOFF);
                           OUTOFSTACK(PSTKP); 
                           PSTKP:=NIL;
                         END; 
                    USING(REGN,PR); 
                  END 
             ELSE 
                  IF ISPTRTYPE(MODE)
                  THEN BEGIN
                         GETINDX(PR); 
                         IF PSTKP<> NIL 
                         THEN BEGIN 
                                INREG1(PR); 
                              END;
                       END
                  ELSE IF MODE = ORD(LONG)
                  THEN BEGIN
                         FREER16(AX);FREER16(DX); 
                         USING2(AX,DX,PR);
                         IF PSTKP<>NIL
                         THEN BEGIN 
                                INREG1(PR); 
                              END;
                       END
             ELSE IF SIZEOFTYPE((MODE))=2 
             THEN BEGIN 
                    REGN:=GETR16(0);
                    SREGN:=0; 
                    USING(REGN,PR); 
                    IF PSTKP<>NIL 
                    THEN BEGIN
                           INREG1(PR);
                         END; 
                  END 
              ELSE BEGIN
                     WRITELN(CODE,'* UNKNOWN TYPE IN INREG'); 
                   END; 
              PTYPE:=MODE;
            END;
     END; 
END; (* INREG *)
  
PROCEDURE INREGISTER(T:EXPTREE);
BEGIN 
  WITH T^ 
  DO BEGIN
       IF (PSREG <= 0)OR (PSREG > NOPREGS)
       THEN BEGIN 
              WRITE(CODE,'*INREGISTER: PSREG OUT OF RANGE ',PSREG:1); 
              WRITE(CODE,'   ');PRINTTYPE(EMODE);WRITELN(CODE); 
            END 
       ELSE INREG(PSREG,INDIRECT(EMODE)); 
     END; 
END; (* INREGISTER *) 
  
PROCEDURE GETREG(T : EXPTREE; LEFT : BOOLEAN);
BEGIN 
  WITH T^ 
  DO WITH PREGSET[PSREG]
  DO BEGIN
       IF LEFT THEN INREG(PSREG,POINTERTO(EMODE)) 
       ELSE INREG(PSREG,INDIRECT(EMODE)); 
     END; 
END; (* GETREG *) 
  
  
(* ROUTINES TO GENERATE THE CODE TO CONVERT FROM A POINTER
 * TO A LONG VARIABLE.
 *
 * PT20   POINTER TO LONG.
 *
 * PF20   LONG TO POINTER.
 *) 
  
PROCEDURE PT20(PR : INTEGER); 
VAR T1,T2 : INTEGER;
BEGIN 
  WITH PREGSET[PR]
  DO BEGIN
       FREER16(AX); 
       FREER16(DX); 
       MOVWMR(AX);
       REGISTR(REGN); 
       MOVWTRI(DX,0); 
       FREEREG(PR); 
       USING2(AX,DX,PR);
     END; 
END;
  
  
  
PROCEDURE PF20(PR : INTEGER); 
VAR T1,T2 : INTEGER;
BEGIN 
  WRITELN(CODE,'* PF20'); 
  INREG(PR,ORD(LONG));
  FREEREG(PR);
  GETINDX(PR);
  MOVWMR(PREGSET[PR].REGN); 
  REGISTR(AX);
  WRITELN(CODE,'* END OF PF20');
END;
  
  
  
(*$L' CODE MODULES '*)
PROCEDURE RETURNCODE(T : EXPTREE);
VAR LAB : INTEGER;
    MODE:INTEGER; 
BEGIN 
  IF REACHABLE
  THEN IF T<>NIL
       THEN WITH T^ 
       DO BEGIN 
            LAB:=0; 
            MODE:=FUNCID^.IR.THISMODE;
            IF TYPETAB[MODE].BASICTYPE=FUNK THEN MODE:=TYPETAB[MODE].RETRN; 
            TYPECOERCE(POINTERTO(MODE),T);
            OUTEXPTREE(T,FALSE,LAB);
            IF ISPTRTYPE(INDIRECT(EMODE)) 
            THEN WITH PREGSET[PSREG]
            DO BEGIN
                 IF REGN=0
                 THEN BEGIN 
                        FREER16(BX);
                        INREG(PSREG,INDIRECT(EMODE)); 
                      END 
                 ELSE IF REGN<>BX 
                 THEN BEGIN 
                        MOVWMR(BX); 
                        REGISTR(REGN);
                      END;
            END 
            ELSE IF INDIRECT(EMODE)= ORD(LONG)
            THEN INREG(PSREG,ORD(LONG)) 
            ELSE IF INDIRECT(EMODE) < ORD(UNSIGNED) 
            THEN
                 WANTIN8(AL,PSREG)
            ELSE IF INDIRECT(EMODE) < ORD(LONG) 
            THEN WANTIN16(AX,PSREG) 
            ELSE BEGIN
                   WRITELN(CODE,'* CANNOT RETURN FLOAT YET'); 
                   WARN(6); 
                 END; 
            FREEREG(PSREG); 
            EXIT1;
          END 
     ELSE BEGIN   (* RETURN WITH EVERYTHING INTACT. *)
            PREVIOUSLABEL;
            WRITELN(CODE,'JMPDS =XCRETI,=XCRETI.'); 
          END;
  DISCARD(BRACKETS[0]); 
  BRACKETS[0]:=NIL; 
  LEVEL:=0; 
  LASTNODE[0]:=NIL; 
  BRACKETS[1]:=NIL; 
  EXPERROR:=FALSE;
END;
  
PROCEDURE OUTPARAM(T:EXPTREE;PARBASE:INTEGER);
VAR LAB1 : INTEGER; 
BEGIN 
  LAB1:=0;
  IF T<> NIL
THEN
  WITH T^ 
  DO BEGIN
       IF TOKN = KONSTANT 
       THEN BEGIN 
              PREVIOUSLABEL;
              IF INDIRECT(EMODE) < ORD(UNSIGNED)
              THEN WRITE(CODE,'MOVBI ') 
              ELSE WRITE(CODE,'MOVWI ');
              WRITELN(CODE,(SVALUE):1); 
              LOCAL(PARBASE); 
            END 
       ELSE IF TOKN<>PARAM
       THEN BEGIN 
              OUTEXPTREE(T,FALSE,LAB1); 
              INREG(T^.PSREG,INDIRECT(EMODE));
              WITH PREGSET[T^.PSREG]
              DO BEGIN
                   IF SREGN= DX 
                   THEN BEGIN 
                          MOVWRM(DX); 
                          LOCAL(PARBASE-2); 
                        END;
                 END; 
              IF INDIRECT(EMODE)< ORD(UNSIGNED) 
              THEN BEGIN
                     MOVBRM(PREGSET[T^.PSREG].REGN);
                   END
              ELSE BEGIN
                     MOVWRM(PREGSET[T^.PSREG].REGN);
                   END; 
              LOCAL(PARBASE); 
              FREEREG(T^.PSREG);
            END 
       ELSE BEGIN 
              OUTPARAM(LFT,PARBASE);
              OUTPARAM(RGHT,PARBASE-OFSET); 
            END;
     END; 
END; (* OUTPARAM *) 
  
PROCEDURE OUTCALL(T:EXPTREE); 
VAR PARBASE:INTEGER;
    MODE : INTEGER; 
    LAB : INTEGER;
BEGIN 
  WITH T^ 
  DO BEGIN
       TOPOFSTACK:=TOPOFSTACK+PARAMSIZE;
       PARBASE:=TOPOFSTACK; 
       IF LFT^.TOKN IN [IDENT,KONSTANT] 
       THEN BEGIN 
              STACKCHECK; 
              FREER16(AX);
              IF ORD(LONG)=INDIRECT(EMODE)
              THEN FREER16(DX); 
              OUTPARAM(RGHT,PARBASE); 
              TOPOFSTACK:=PARBASE;
              STACKCHECK; 
              MOVWTRI(AX,PARAMSIZE);
              WRITELN(CODE,'          PUSHR AX'); 
             WRITELN(CODE,'          PUSHF'); 
              STACKPTR:=STACKPTR+2; 
              TOPOFSTACK:=STACKPTR; 
              WRITE(CODE,'          CALLDS ');
              IF LFT^.TOKN=IDENT
              THEN BEGIN
                     WRITE(CODE,'=X');
                     OUTALFA(LFT^.SENTRY^.IR.NAME,7); 
                     WRITE(CODE,',=X'); 
                     OUTALFA(LFT^.SENTRY^.IR.NAME,6); 
                     WRITELN(CODE,'.'); 
                   END
               ELSE BEGIN 
                     WRITELN(CODE,(LFT^.SVALUE MOD 16):1,',',(LFT^.SVALUE DIV 16):1); 
                    END;
             END
        ELSE BEGIN
              WRITELN(CODE,'* ALLOW SPACE FOR THE ADDRESS OF THE ROUTINE'); 
              PARBASE := PARBASE+4; 
              TOPOFSTACK:=PARBASE;
              STACKCHECK; 
              FREER16(AX);
              IF ORD(LONG) = INDIRECT(EMODE)
              THEN FREER16(DX); 
              OUTPARAM(RGHT,PARBASE); 
               LAB:=0;
               IF LFT^.OPER = DEREF 
               THEN BEGIN 
                      LFT^.RGHT^.PSREG:=NEXTREG;
                      OUTEXPTREE(LFT^.RGHT,FALSE,LAB);
                    END 
               ELSE BEGIN 
                      WRITELN(CODE,'*CALL:WHAT??'); 
                      ERROR(72);
                    END;
               INREG(LFT^.RGHT^.PSREG,POINTERTO(ORD(KAR))); 
               MOVWRM(PREGSET[LFT^.RGHT^.PSREG].REGN);
               LOCAL(PARBASE-PARAMSIZE);
               TOPOFSTACK:=PARBASE; 
               STACKCHECK;
               MOVWTRI(AX,PARAMSIZE); 
               WRITELN(CODE,'          PUSHR AX');
                WRITELN(CODE,'           PUSHF'); 
               WRITELN(CODE,'          MOVFSG CS'); 
               LOCAL(PARBASE-PARAMSIZE-2);
               STACKPTR:=STACKPTR+2;
               TOPOFSTACK:=STACKPTR;
               WRITELN(CODE,'          CALLIS');
               LOCAL(PARBASE-PARAMSIZE);
               TOPOFSTACK:=TOPOFSTACK-4;
             END; 
        INITREGS;         (* MARK ALL REGISTERS AS FREE *)
        MODE := INDIRECT(EMODE);
          IF PSREG=0 THEN PSREG:=NEXTREG; 
        WITH PREGSET[PSREG] 
        DO BEGIN
             IF MODE < ORD(UNSIGNED)
             THEN BEGIN 
                    USING(AL,PSREG);
                  END 
             ELSE IF MODE < ORD(LONG) 
             THEN BEGIN 
                    USING(AX,PSREG);
                  END 
             ELSE IF MODE <= ORD(DOUBLE)
             THEN BEGIN 
                    USING2(AX,DX,PSREG);
                  END 
             ELSE IF ISPTRTYPE(MODE)
             THEN BEGIN 
                    USING(BX,PSREG);
                  END 
             ELSE BEGIN 
                    WRITELN(CODE,'* MODE=',MODE:1,' IN OUT CALL');
                    ERROR(73);
                  END;
           END; (* WITH *)
           TOPOFSTACK:=TOPOFSTACK - PARAMSIZE - 2;
      END;
END; (* OUTCALL *)
  
PROCEDURE DMPPR(PSREG:INTEGER); 
BEGIN 
WRITELN(CODE,'*DUMPPR: PR= ',PSREG:1,' REGN= ',PREGSET[PSREG].REGN,' SREGN= ',PREGSET[PSREG].SREGN:1);
WRITE(CODE,'* PREG= ',REGSET[PREGSET[PSREG].REGN].PREG:1,'  PTYPE= ',PREGSET[PSREG].PTYPE:1); 
IF PREGSET[PSREG].PSTKP<>NIL THEN WRITELN(CODE,' PSTKP NOT NIL')
ELSE WRITELN(CODE,' PSTKP = NIL');
END;
  
PROCEDURE STOREIT(T : EXPTREE ; VAR PR : INTEGER;ADR : INTEGER);FORWARD;
PROCEDURE DEREFCODE(T : EXPTREE; PR : INTEGER;NEEDIT:BOOLEAN;REL,RG : INTEGER);FORWARD; 
  
  
PROCEDURE APPLY(TXT:ALFA;T:EXPTREE);
LABEL 1;
BEGIN 
  IF T=NIL
  THEN WRITELN(CODE,'FLUNKED IT') 
  ELSE WITH T^
  DO IF WANTRES OR (TOKN=ASSIGN)
  THEN BEGIN
       IF (TOKN <> ASSIGN)
       THEN BEGIN 
              INREGISTER(LFT);
              IF (LFT^.PSREG<>T^.PSREG) AND (PREGSET[PSREG].SREGN<>DX)
              THEN BEGIN
                     IF PSREG<>0 THEN FREEREG(PSREG); 
                     PSREG:=LFT^.PSREG; 
                     WRITELN(CODE,'*APPLY:LFT.PSREG<>PSREG'); 
                   END; 
              SEGCHECK(RGHT); 
            END 
       ELSE IF ISFIELD(INDIRECT(EMODE)) 
       THEN BEGIN 
              INREGISTER(T);
            END 
       ELSE BEGIN 
              IF RGHT^.TOKN <> KONSTANT 
              THEN INREGISTER(RGHT);
              IF RGHT^.PSREG<>T^.PSREG
              THEN BEGIN
                     IF PSREG<>0 THEN FREEREG(PSREG); 
                     PSREG:=RGHT^.PSREG;
                     WRITELN(CODE,'*APPLY:RGHT.PSREG<>PSREG');
                   END; 
              SEGCHECK(LFT);
           END; 
       PREVIOUSLABEL; 
       OUTALFA(TXT,9);
       IF(RGHT^.TOKN<>KONSTANT) 
       THEN BEGIN 
              IF (TOKN<>ASSIGN) OR ISFIELD(INDIRECT(EMODE)) 
              THEN CODEIT('.TR       ',PREGSET[PSREG].REGN) 
              ELSE CODEIT('.FR       ',PREGSET[PSREG].REGN);
              WRITE(CODE,' ');OUTREG(PREGSET[PSREG].REGN);
            END 
       ELSE BEGIN 
              IF ((EMODE) < 
                    ORD(UNSIGNED))
                 OR ((PREGSET[PSREG].REGN < 9) AND (PREGSET[PSREG].REGN > 0)) 
              THEN BEGIN
                     WRITE(CODE,'BI ',RGHT^.SVALUE:1);
                     WRITE(CODE,'    ');PRINTTYPE(EMODE); 
                   END
              ELSE BEGIN
                     WRITE(CODE,'WI ',(RGHT^.SVALUE MOD 65536):1);
                     WRITE(CODE,'    ');PRINTTYPE(EMODE); 
                   END; 
              WRITELN(CODE);
              IF TOKN <>ASSIGN THEN OUTADRS(T,0)
              ELSE OUTAD1(LFT,0); 
              GOTO 1; 
            END;
       WRITELN(CODE); 
       IF TOKN<>ASSIGN THEN OUTADRS(RGHT,0) 
       ELSE IF ISFIELD(INDIRECT(EMODE)) 
       THEN BEGIN 
              OUTADRS(RGHT,0);
              IF TOKN = ASSIGN
              THEN STOREIT(LFT,PSREG,0);
            END 
      ELSE OUTAD1(LFT,0); 
      IF RGHT^.PSREG<>T^.PSREG
      THEN IF (RGHT^.PSREG<>0) AND (PREGSET[RGHT^.PSREG].SREGN <> DX) 
           THEN FREEREG(RGHT^.PSREG); 
  1: ;
     END; 
END;
  
(* ROUTINES TO GENERATE CODE FOR JUMP INSTRUCTIONS *) 
(* NEGCOND IS USED TO REVERSE THE SENSE OF THE OPERS LT,LE,GT,GE,EQ,NE *) 
(* THE ROUTINE JUMPC IS PASSED THREE PARAMETERS 
 * 1. THE LABEL VALUE.
 * 2. AN EXPRESSION TREE. 
 * 3. A BOOLEAN VALUE USED TO INDICATE JUMP TRUE/FALSE .
 *) 
  
FUNCTION NEGCOND(OP1:OPSYM):OPSYM;
BEGIN 
  CASE OP1
  OF
    LTOP : NEGCOND := GEOP; 
    LEOP : NEGCOND := GTOP; 
    GTOP : NEGCOND := LEOP; 
    GEOP : NEGCOND := LTOP; 
    EQOP : NEGCOND := NEOP; 
    NEOP : NEGCOND := EQOP; 
  END;
END;
  
  
  
PROCEDURE JUMP(LAB:INTEGER);
BEGIN 
  IF REACHABLE
  THEN BEGIN
         PREVIOUSLABEL; 
         WRITE(CODE,'JMPD '); 
         OUTLABEL(LAB); 
         WRITELN(CODE); 
         REACHABLE:=FALSE;
       END; 
END;
(* JUMP 
 *) 
  
PROCEDURE JUMPS(LAB : INTEGER); 
BEGIN 
  IF REACHABLE
  THEN BEGIN
         PREVIOUSLABEL; 
         WRITE(CODE,'JMPS '); 
         OUTLABEL(LAB); 
         WRITELN(CODE); 
         REACHABLE:=FALSE;
       END; 
END;
  
  
PROCEDURE JUMPNZ(LAB:INTEGER);
VAR TLAB : INTEGER; 
BEGIN 
  IF REACHABLE
  THEN BEGIN
         TLAB:=GIVELABEL; 
         PREVIOUSLABEL; 
         WRITE(CODE,'JZ '); 
         OUTLABEL(TLAB);
         WRITELN(CODE); 
         JUMP(LAB); 
         LABELIT(TLAB); 
       END; 
END;
(* JUMPNZ 
 *) 
  
PROCEDURE JUMPZ(LAB:INTEGER); 
VAR TLAB : INTEGER; 
BEGIN 
  IF REACHABLE
  THEN BEGIN
         TLAB:=GIVELABEL; 
         PREVIOUSLABEL; 
         WRITE(CODE,'JNZ ');
         OUTLABEL(TLAB);
         WRITELN(CODE); 
         JUMP(LAB); 
         LABELIT(TLAB); 
       END; 
END;
(* JUMPZ
 *) 
PROCEDURE TESTIT(T:EXPTREE);
VAR MODE : INTEGER; 
    TR   : INTEGER; 
BEGIN 
  WITH T^ 
  DO WITH PREGSET[PSREG]
  DO IF NOT(SETCC)
  THEN BEGIN
       MODE:=INDIRECT(EMODE); 
       IF MODE < ORD(LONG)
       THEN BEGIN 
              PREVIOUSLABEL;CODEIT('AND.TR    ',REGN);WRITE(CODE,' ');OUTREG(REGN); 
              WRITELN(CODE);REGISTR(REGN);
            END 
       ELSE IF MODE = ORD(LONG) 
       THEN BEGIN 
              PREVIOUSLABEL;
              WRITE(CODE,'ORWTR ');OUTREG(REGN);WRITELN(CODE);
              WRITE(CODE,'          REGISTER ');OUTREG(SREGN);WRITELN(CODE);
            END 
       ELSE IF MODE <= ORD(DOUBLE)
       THEN BEGIN 
              WRITELN(CODE,'* TEST OF FLOAT N.I');
              ERROR(74);
            END 
       ELSE BEGIN 
              PREVIOUSLABEL;
              CODEIT('AND.TR    ',REGN);WRITE(CODE,' ');OUTREG(REGN); 
              WRITELN(CODE);REGISTR(REGN);
            END;
     END; (* WITH *)
END; (* TESTIT *) 
  
PROCEDURE OUTCJUMP(OP:OPSYM;LAB:INTEGER); 
BEGIN 
                     PREVIOUSLABEL; 
                     CASE OP
                     OF 
                       EQOP : BEGIN 
                                WRITE(CODE,'JZ  '); 
                              END;
                       NEOP : WRITE(CODE,'JNZ '); 
                       LTOP : WRITE(CODE,'JL  '); 
                       LEOP : WRITE(CODE,'JLE '); 
                       GEOP : WRITE(CODE,'JGE '); 
                       GTOP : WRITE(CODE,'JG  '); 
                     END; 
  OUTLABEL(LAB);WRITELN(CODE);
END;
  
  
PROCEDURE JUMPCS(LAB : INTEGER;T : EXPTREE;JTF:BOOLEAN;SHORT : BOOLEAN);
VAR LAB1,TLAB: INTEGER; 
    T1 : EXPTREE; 
    OP1 : OPSYM;
    TPOS : INTEGER; 
BEGIN 
  IF REACHABLE
  THEN IF T=NIL 
  THEN BEGIN
         IF NOT(JTF) THEN WRITELN(CODE,'* JUMPC(LAB,NIL,FALSE)'); 
         IF SHORT THEN JUMPS(LAB) 
         ELSE JUMP(LAB);
       END
  ELSE BEGIN
         LAB1:=0; 
         IF NOT(EXPERROR) 
         THEN BEGIN 
                TPOS:=TOPOFSTACK; 
                IF NOT(T^.OPER IN [EQOP,NEOP,LTOP,LEOP,GEOP,GTOP])
                THEN BEGIN
                   T^.COND:=TRUE; 
                   STACKCHECK;
                   OUTEXPTREE(T,FALSE,LAB1);
                   INREGISTER(T); 
                   TOPOFSTACK:=TPOS;
                   STACKCHECK;
                   WRITELN(CODE,'* STACK CHECKED IN JUMPCS'); 
                   TESTIT(T); 
                     IF JTF 
                       THEN JUMPZ(LAB) ELSE JUMPNZ(LAB);
                     END
                ELSE WITH T^
                DO BEGIN
                     IF (JTF) 
                     THEN OPER := NEGCOND(OPER);
                     IF (LFT^.OPER=REF)AND(ISPTRTYPE(INDIRECT(RGHT^.EMODE)))
                     THEN BEGIN 
                            WRITELN(CODE,'*SWAP LEFT AND RIGHT TREES'); 
                            T1:=LFT;
                            LFT:=RGHT;
                            RGHT:=T1; 
                            OPER:=NEGCOND(OPER);
                          END;
                     LFT^.PSREG:=T^.PSREG;
                     OUTEXPTREE(LFT,FALSE,LAB1);
                     PSREG:=LFT^.PSREG; 
                     IF NOT(SIMPLE(RGHT))THEN OUTEXPTREE(RGHT,FALSE,LAB1);
                     INREGISTER(LFT); 
                     TOPOFSTACK:=TPOS;
                     STACKCHECK;
                     APPLY('CMP       ',T); 
                     IF SHORT 
                     THEN OUTCJUMP(T^.OPER,LAB) 
                     ELSE BEGIN 
                            OPER:=NEGCOND(OPER);
                            TLAB:=GIVELABEL;
                            OUTCJUMP(T^.OPER,TLAB); 
                            JUMP(LAB);
                            LABELIT(TLAB);
                          END;
                   END; 
              END 
         ELSE WRITELN(CODE,'* ERROR IN CONDITIONAL EXPRESSION');
       END; 
END;
  
  
PROCEDURE JUMPC(LAB : INTEGER;T : EXPTREE;JTF : BOOLEAN); 
BEGIN 
  JUMPCS(LAB,T,JTF,FALSE);
END;
  
  
PROCEDURE INCDEC(T : EXPTREE;INX : BOOLEAN);
VAR SIZ : INTEGER;
    LAB : INTEGER;
BEGIN 
  LAB:=0; 
  WITH T^ 
  DO BEGIN
       IF ISPTRTYPE(INDIRECT(EMODE))
       THEN BEGIN 
              SIZ:=SIZEOFTYPE(INDIRECT(INDIRECT(EMODE))); 
              PREVIOUSLABEL;
              IF INX
              THEN WRITELN(CODE,'ADDWI ',SIZ:1) 
              ELSE WRITELN(CODE,'SUBWI ',SIZ:1);
              OUTAD1(T,0);
            END 
       ELSE IF ISFIELD(INDIRECT(EMODE)) 
       THEN BEGIN 
              LAB :=NEXTREG;
              DEREFCODE(T,LAB,TRUE,0,0);
              IF INX
              THEN WRITELN(CODE,'          INCW') 
              ELSE WRITELN(CODE,'          DECW');
              REGISTR(PREGSET[LAB].REGN); 
              STOREIT(T,LAB,0); 
              FREEREG(LAB); 
            END 
       ELSE BEGIN 
              PREVIOUSLABEL;
              CASE INDIRECT(EMODE)
              OF 0,1,2 : BEGIN
                           IF INX THEN WRITELN(CODE,'INCB') 
                           ELSE WRITELN(CODE,'DECB'); 
                           OUTAD1(T,0); 
                         END; 
                 3,4 : BEGIN
                         IF INX THEN WRITELN(CODE,'INCW') 
                         ELSE WRITELN(CODE,'DECW'); 
                         OUTAD1(T,0); 
                       END; 
                 5 : BEGIN
                       IF INX THEN WRITELN(CODE,'INCW') 
                       ELSE WRITELN(CODE,'DECW'); 
                       OUTAD1(T,0); 
                       PREVIOUSLABEL; 
                       IF INX THEN WRITELN(CODE,'ADCWI 0')
                       ELSE WRITELN(CODE,'SBBWI 0');
                       OUTAD1(T,2); 
                     END; 
                OTHERWISE BEGIN 
                            WRITE(CODE,'* INC-DEC OF ');PRINTTYPE(INDIRECT(EMODE));WRITELN(CODE); 
                            WARN(8);
                          END;
              END; (* CASE *) 
            END;
        END;
END; (* INCDEC *) 
  
  
  
  
  
  
  
PROCEDURE DEREFCODE;
(* PROCEDURE TO GENERATE THE CODE REQUIRED TO DEREFERENCE A POINTER 
 * PLUS THE OFFSET REL AND/OR PLUS THE INDEX REGISTER RG
 * IF RG=0 OR REL =0 THEN THAT PARAMETER IS NOT USED. 
 *) 
LABEL 1;
VAR LAB1 : INTEGER; (* USED AS A TEMPARY VARIABLE *)
    MODE : INTEGER; 
    R1,S1 : INTEGER;
    TOK : SYMMODE;
BEGIN 
  WRITE(CODE,'* DEREFCODE');
  PRINTTYPE(T^.EMODE);WRITELN(CODE,'  PR=',PR:1); 
  IF T<>NIL 
  THEN WITH T^
  DO BEGIN
   IF PR=0
   THEN BEGIN 
          WRITELN(CODE,'*DEREFCODE:PR=0');
          ERROR(75);
        END;
   IF PSREG=0 
   THEN BEGIN 
          WRITELN(CODE,'*DEREFCODE:PSREG=0'); 
          PSREG:=NEXTREG; 
        END;
(*    EMODE:=INDIRECT(EMODE); *)
(* THIS MOD IS IN DEREF *)
   IF  ISPTRTYPE(INDIRECT(EMODE)) 
   THEN BEGIN 
       1: 
          WITH PREGSET[PSREG] 
          DO BEGIN
               IF R1=0 THEN GETINDX(PSREG); 
               R1:=PREGSET[PSREG].REGN; 
               S1:=PREGSET[PSREG].SREGN;
               IF (NOT(NEEDIT) OR (PR=PSREG)) 
               THEN BEGIN 
                      FREEREG(PSREG); 
                      USING2(R1,S1,PR); 
                    END 
               ELSE GETINDX(PR);
               MOVWMR(PREGSET[PR].REGN);
               OUTINDX(R1,REL,RG);
             END; 
        END 
   ELSE BEGIN 
               BEGIN
                 TOK:=TOKN; 
                 TOKN:=BINARY;
                 IF ISFIELD(INDIRECT(EMODE)) OR ISFIELD(EMODE)
                 THEN MODE:=ORD(FIELD)
                 ELSE MODE:=INDIRECT(EMODE);
                 CASE MODE
                 OF 0,1,2 : BEGIN 
                              LAB1:=GETR8(0); 
                              MOVBMR(LAB1); 
                              OUTAD1(T,REL);
                              IF NOT(NEEDIT)THEN FREEREG(PSREG);
                              USING(LAB1,PR); 
                            END;
                   12,
                   3,4 : BEGIN
                           LAB1:=GETR16(0); 
                           MOVWMR(LAB1);
                           OUTAD1(T,REL); 
                           IF NOT(NEEDIT)THEN FREEREG(PSREG); 
                           USING(LAB1,PR);
                           IF ISFIELD(MODE) 
                           THEN BEGIN 
                                  WRITELN(CODE,'*DEREF:GET FIELD'); 
                                  WITH TYPETAB[INDIRECT(EMODE)] 
                                  DO BEGIN
                                       GETFIELD(PR,FMASK,FOFFSET);
                                     END; 
                                END;
                            END;
                  5 : BEGIN 
                        FREER16(AX);FREER16(DX);
                        MOVWMR(AX);OUTAD1(T,REL); 
                        MOVWMR(DX);OUTAD1(T,REL+2); 
                        IF NOT(NEEDIT) THEN FREEREG(PSREG); 
                        USING2(AX,DX,PR); 
                   END; 
            OTHERWISE BEGIN 
                        WRITE(CODE,'*DEREF OF ');PRINTTYPE(INDIRECT(EMODE));WRITELN(CODE);
                        WARN(9);
                        GOTO 1; 
                      END;
                 END; 
                 TOKN:=TOK; 
               END; 
        END;
     END; (* WITH *)
END;
  
  
  
PROCEDURE STOREIT;
VAR RG : INTEGER; 
    TPOS : INTEGER; 
BEGIN 
  WITH T^ 
  DO BEGIN
       WRITE(CODE,'* STOREIT'); 
       PRINTTYPE(EMODE);WRITELN(CODE,' PR=',PR:1);
        IF ISFIELD(INDIRECT(EMODE)) 
        THEN BEGIN
               WRITELN(CODE,'*STORING INTO A FIELD'); 
               TPOS:= STACKPTR; 
               WITH PREGSET[PR] 
               DO BEGIN 
                    IF REGN < 9 
                    THEN BEGIN
                           WANTIN8(AL,PR);
                           FREER8(AH);
                           PREVIOUSLABEL; 
                           WRITELN(CODE,'CBW'); 
                           RG:=AX;
                           USING(AX,PR);
                           PTYPE:=ORD(INT); 
                         END; 
                  END;
               WITH TYPETAB[INDIRECT(EMODE)]
               DO BEGIN 
                    IF FOFFSET<>0 
                    THEN BEGIN
                           PREVIOUSLABEL; 
                           WRITE(CODE,'PUSHR ');
                           WITH PREGSET[PR] 
                           DO BEGIN 
                                IF REGN < 9 
                                THEN OUTREG(9 + (REGN-1)MOD 4)
                                ELSE OUTREG(REGN);
                              END;
                           WRITELN(CODE); 
                           TOPOFSTACK:=TOPOFSTACK+2;
                           STACKPTR:= STACKPTR+2; 
                           TPOS:=STACKPTR;
                         END; 
                    SHIFTIT(PR,FOFFSET,TRUE); 
                    PREVIOUSLABEL;CODEIT('AND.I     ',PREGSET[PR].REGN);
                    WRITELN(CODE,' ',FMASK:1);
                    REGISTR(PREGSET[PR].REGN);
                    PREVIOUSLABEL;
                    WRITELN(CODE,'ANDWI ',(65535-FMASK):1); 
                    OUTAD1(T,ADR);
                    PREVIOUSLABEL;CODEIT('OR.FR     ',PREGSET[PR].REGN);
                    WRITE(CODE,' ');
                    OUTREG(PREGSET[PR].REGN);WRITELN(CODE); 
                    OUTAD1(T,ADR);
                    IF FOFFSET<>0 
                    THEN BEGIN
                           IF TPOS = STACKPTR 
                           THEN BEGIN 
                                  PREVIOUSLABEL;
                                  WRITE(CODE,'POPR ');
                                  WITH PREGSET[PR]
                                  DO BEGIN
                                       IF REGN < 9
                                       THEN OUTREG(9+(REGN-1)MOD 4) 
                                       ELSE OUTREG(REGN); 
                                     END; 
                                  WRITELN(CODE);
                                  TOPOFSTACK:=TOPOFSTACK-2; 
                                  STACKPTR:=STACKPTR-2; 
                                END 
                           ELSE BEGIN 
                                  WITH PREGSET[PR]
                                  DO BEGIN
                                       IF REGN < 9
                                       THEN MOVWMR(9+(REGN-1)MOD 4) 
                                       ELSE MOVWMR(REGN); 
                                     END; 
                                  LOCAL(TPOS);
                                END;
                         END; 
                  END;
             END
        ELSE BEGIN
               SEGCHECK(T); 
               WITH PREGSET[PR] 
               DO CASE INDIRECT(EMODE)
               OF 
                 0,1,2,3,4,5
                 : BEGIN
                     CASE SIZEOFTYPE(INDIRECT(EMODE)) 
                     OF 1 : BEGIN 
                              IF REGN > 8 
                              THEN BEGIN
                                     IF REGN > BX THEN WANTIN16(AX,PR); 
                                     RG:=REGN-8;
                                     WRITELN(CODE,'*CONVERTED TO BYTE REGISTER'); 
                                     USING(RG,PR);
                                   END; 
                            END;
                        2 : BEGIN 
                              IF REGN < 9 
                              THEN BEGIN
                                     IF REGN < 5
                                     THEN BEGIN 
                                            FREER8(REGN+4); 
                                            MOVBTRI(REGN+4,0);
                                            USING(REGN+8,PR); 
                                          END 
                                     ELSE BEGIN 
                                            FREER8(REGN-4); 
                                            MOVBMR(REGN-4); 
                                            REGISTR(REGN);
                                            MOVBTRI(REGN,0);
                                            USING(REGN+4,PR); 
                                          END;
                                   END; 
                            END;
                        4 : BEGIN 
                            END;
                     END; 
                     PREVIOUSLABEL; 
                     CODEIT('MOV.RM    ',REGN); 
                     WRITE(CODE,' ');OUTREG(REGN);
                     WRITELN(CODE);OUTAD1(T,ADR); 
                     IF INDIRECT(EMODE)=ORD(LONG) 
                     THEN BEGIN 
                            MOVWRM(SREGN);
                            OUTAD1(T,2+ADR);
                          END;
                    END;
                6,7 : BEGIN 
                        WRITELN(CODE,'*ERR CANNOT STORE FLOAT YET');
                        WARN(10); 
                      END;
                OTHERWISE BEGIN 
                             MOVWRM(PREGSET[PR].REGN);
                             OUTAD1(T,ADR); 
                           END; 
               END; 
     END; 
      END;
END;
  
  
PROCEDURE CHANGEIT(VAR T : EXPTREE;LEFT : BOOLEAN;VAR LAB : INTEGER); 
VAR RG : INTEGER; 
BEGIN 
  WITH T^ 
  DO BEGIN
       IF PSREG=0 THEN PSREG:=NEXTREG;  (* 6-9-82 *)
       RGHT^.PSREG:=T^.PSREG; 
       OUTEXPTREE(RGHT,FALSE,LAB);
       PSREG:=RGHT^.PSREG;
       WRITELN(CODE,'*CHANGEIT. PR= ',PSREG:1); 
       IF DEBUGF
       THEN BEGIN 
              WRITE(CODE,'*FROM ');PRINTTYPE(T^.RGHT^.EMODE);WRITELN(CODE); 
              WRITE(CODE,'*TO   ');PRINTTYPE(T^.EMODE);WRITELN(CODE); 
            END;
       CASE EMODE 
       OF 
          0,1,2 : BEGIN 
                    INREG(PSREG,INDIRECT(RGHT^.EMODE)); 
                    WITH PREGSET[PSREG] 
                    DO BEGIN
                         IF INDIRECT(RGHT^.EMODE) > ORD(INT)
                         THEN BEGIN WRITELN(CODE,'*CONVERT:CHAR?',INDIRECT(RGHT^.EMODE):1); 
                                RG:=AL; 
                                WARN(11);END
                         ELSE IF REGN < 9 
                         THEN BEGIN 
                                WRITELN(CODE,'*ALREADY A CHAR');
                                RG:=REGN; 
                              END 
                         ELSE IF REGN < SI
                         THEN BEGIN 
                                RG:=REGN-8; 
                              END 
                         ELSE BEGIN 
                                WANTIN16(AX,PSREG);RG:=AL;
                              END;
                         USING(RG,PSREG); 
                       END; 
                  END;
          3,4 :   BEGIN 
                    INREG(PSREG,INDIRECT(RGHT^.EMODE)); 
                    WITH PREGSET[PSREG] 
                    DO CASE INDIRECT(RGHT^.EMODE) 
                    OF
                      0,1,2 : BEGIN 
                                RG:= REGN;
                                IF RG<>AL 
                                THEN BEGIN
                                       IF RG<>AH
                                       THEN BEGIN 
                                              PREVIOUSLABEL;
                                              WRITELN(CODE,'PUSHR AX'); 
                                            END;
                                       MOVBMR(AL);
                                       REGISTR(RG); 
                                     END; 
                                PREVIOUSLABEL;WRITELN(CODE,'CBW');
                                FREEREG(PSREG); 
                                RG:= (RG-1)MOD 4 + 9; 
                                IF RG<> AX
                                THEN BEGIN
                                       MOVWMR(RG);REGISTR(AX);
                                       WRITELN(CODE,'          POPR AX'); 
                                     END; 
(* 6-9-82 *)
                                USING(RG,PSREG);
                              END;
                       3,4   : BEGIN
                                 USING(REGN,PSREG); 
                               END; 
                       5 : BEGIN
                             USING(AX,PSREG); 
                           END; 
                       OTHERWISE WRITELN(CODE,'*CONVERT:INT?',INDIRECT(RGHT^.EMODE)); 
                     END; 
                  END;
          5   :   BEGIN 
                    INREG(PSREG,INDIRECT(RGHT^.EMODE)); 
                    IF ISPTRTYPE(INDIRECT(RGHT^.EMODE)) 
                    THEN BEGIN
                           PT20(PSREG); 
                         END
                    ELSE IF INDIRECT(RGHT^.EMODE) < ORD(LONG) 
                    THEN BEGIN
                           WANTIN16(AX,PSREG);
                           FREER16(DX); 
                           PREVIOUSLABEL; 
                           WRITELN(CODE,'CWD'); 
                           USING2(AX,DX,PSREG); 
                         END; 
                  END;
          6,7 :   BEGIN 
                    WRITELN(CODE,'* FLOAT NOT IMPLEMENTED YET');
                    WARN(12); 
                  END;
       OTHERWISE  BEGIN 
                    INREG(PSREG,INDIRECT(RGHT^.EMODE)); 
                    IF INDIRECT(RGHT^.EMODE)<> ORD(LONG)
                    THEN WRITELN(CODE,'*CONVERT.OTHERWISE') 
                    ELSE BEGIN
                           PF20(PSREG); 
                         END; 
                  END;
       END; 
     END; 
END;
  
PROCEDURE PRECODE(VAR T : EXPTREE;VAR  LAB : INTEGER);
BEGIN 
  WITH T^ 
  DO BEGIN
       IF PSREG=0 THEN PSREG:=NEXTREG;
       IF ISFIELD(INDIRECT(EMODE))AND (TOKN = ASSIGN) 
       THEN BEGIN 
              WRITELN(CODE,'*PRECODE: IS FIELD'); 
              OUTEXPTREE(LFT,TRUE,LAB); 
              DEREFCODE(LFT,PSREG,TRUE,0,0);
              OUTEXPTREE(RGHT,FALSE,LAB); 
            END 
       ELSE IF (TOKN<>ASSIGN) 
       THEN BEGIN 
              LFT^.PSREG:=T^.PSREG; 
              LFT^.WANTRES:=T^.WANTRES; 
              RGHT^.WANTRES:=T^.WANTRES;
              OUTEXPTREE(LFT,FALSE,LAB);
              IF NOT(SIMPLE(RGHT))
                THEN BEGIN
                    OUTEXPTREE(RGHT,FALSE,LAB)
            END;
              PSREG:=LFT^.PSREG;
            END 
       ELSE BEGIN 
              RGHT^.PSREG:=T^.PSREG;
              IF RGHT^.TOKN <> KONSTANT THEN OUTEXPTREE(RGHT,FALSE,LAB);
              IF NOT(SIMPLE(LFT)) 
               THEN  OUTEXPTREE(LFT,TRUE,LAB);
              T^.PSREG:=RGHT^.PSREG;
            END;
     END; 
END; (* PRECODE *)
  
  
  
PROCEDURE APPLY2(NAM : ALFA;T : EXPTREE); 
BEGIN 
  WITH T^ 
  DO IF RGHT^.TOKN=KONSTANT 
  THEN BEGIN
         PREVIOUSLABEL; 
         OUTALFA(NAM,9);
         WRITELN(CODE,'WI ',(RGHT^.SVALUE DIV 65536):1);
         IF TOKN<>ASSIGN
         THEN REGISTR(DX) 
         ELSE OUTADRS(LFT,2); 
       END
  ELSE BEGIN
       PREVIOUSLABEL; 
       OUTALFA(NAM,9);
       IF TOKN<>ASSIGN
       THEN WRITELN(CODE,'WTR DX')
       ELSE WRITELN(CODE,'WFR DX'); 
       IF TOKN <> ASSIGN
       THEN OUTADRS(RGHT,2) 
       ELSE OUTADRS(LFT,2); 
     END; 
END;
  
  
  
PROCEDURE LOADIT(T : EXPTREE;LEFT : BOOLEAN); 
LABEL 10; 
VAR MODE : INTEGER; 
    RG : INTEGER; 
BEGIN 
  IF T<>NIL 
  THEN WITH T^
  DO BEGIN
       MODE:=INDIRECT(EMODE); 
       IF WANTRES 
       THEN BEGIN 
              IF PSREG=0 THEN PSREG:=NEXTREG; 
               IF NOT(LEFT) 
               THEN WITH PREGSET[PSREG] 
               DO BEGIN 
                    GETREG(T,LEFT); 
                    IF MODE < ORD(UNSIGNED) 
                    THEN BEGIN
                           MOVBMR(REGN);
                           OUTADRS(T,0);
                         END
                    ELSE IF MODE < ORD(LONG)
                    THEN BEGIN
                           MOVWMR(REGN);
                           OUTADRS(T,0);
                         END
                    ELSE IF MODE <= ORD(DOUBLE) 
                    THEN BEGIN
                           MOVWMR(AX);
                           OUTADRS(T,0);
                           MOVWMR(DX);
                           OUTADRS(T,2);
                         END
                    ELSE IF ISPTRTYPE(MODE) 
                    THEN BEGIN
                           IF T^.TOKN = IDENT 
                           THEN BEGIN 
                                  RG:= ISLOADED(SENTRY,TRUE); 
                                  IF RG<>0
                                  THEN BEGIN
                                         WRITE(CODE,'**** FOUND ',SENTRY^.IR.NAME,' IN REG ');OUTREG(RG);WRITELN(CODE); 
                                       END; 
                                END;
                           IF RG<>REGN
                           THEN BEGIN 
                                  MOVWMR(REGN); 
                                  OUTADRS(T,0); 
                                END;
                           IF T^.TOKN = IDENT 
                           THEN WITH REGSET[REGN] 
                           DO BEGIN 
                                WRITE(CODE,'**** REG ');OUTREG(REGN);WRITELN(CODE,' HAS CONTENTS OF ',SENTRY^.IR.NAME); 
                                IDPTR:= SENTRY; 
                                IDDRFD:= TRUE;
                              END;
                         END
                    ELSE BEGIN
                           WRITELN(CODE,'* MODE=',MODE:1,' IN IDENT...'); 
                           GOTO 10; 
                         END; 
                  END (* WITH,THEN *) 
             ELSE WITH SENTRY^.IR 
             DO WITH PREGSET[PSREG] 
             DO BEGIN   (* LOAD ADDRESS OF IDENT *) 
                  INREG(PSREG,POINTERTO(ORD(KAR))); 
             10 : IF THISSTATE IN [ EXTERNVAR,EXTSTATIC]
                  THEN BEGIN
                         PREVIOUSLABEL; 
                         WRITE(CODE,'MOVWTRI ');OUTREG(REGN);WRITE(CODE,',=X');OUTALFA(NAME,7);WRITELN(CODE); 
                       END
                  ELSE IF THISSTATE = STATICVAR 
                  THEN BEGIN
                         PREVIOUSLABEL; 
                         WRITE(CODE,'MOVWTRI ');OUTREG(REGN);WRITELN(CODE,',STATPOS+',(OFFSET):1);
                       END
                  ELSE IF THISSTATE IN [AUTOVAR,PARAMVAR] 
                  THEN BEGIN
                         PREVIOUSLABEL; 
                         WRITE(CODE,'LEA ');OUTREG(REGN);WRITELN(CODE); 
                         OUTADRS(T,0);
                       END
                  ELSE BEGIN
                         MOVWTRI(PREGSET[PSREG].REGN,SENTRY^.IR.OFFSET);
                       END; 
                  WITH REGSET[REGN] 
                  DO BEGIN
                       IDPTR:= SENTRY;
                       IDDRFD:= FALSE;
                       WRITE(CODE,'**** REG ');OUTREG(REGN);WRITELN(CODE,' IS LOADED WITH ADDRESS OF ',SENTRY^.IR.NAME);
                     END; 
                END; (* ELSE,WITH *)
                  END 
                 ELSE WRITELN(CODE,'*WANTRES = FALSE IN LOADIT'); 
     END; 
END;
  
  
  
PROCEDURE OUTEXPTREE; 
LABEL 111,222,333,444,555;
VAR F,S : EXPTREE;
    T1,T2 : EXPTREE;
    LAB1,LAB2 : INTEGER;
    TP,TP1 : EXPTREE; 
    MODE : INTEGER; 
    TPOS : INTEGER; 
BEGIN 
  TPOS:= TOPOFSTACK;   (* NEW MOD! *) 
  IF T<>NIL 
  THEN WITH T^
  DO BEGIN
       IF DEBUGF
       THEN BEGIN 
              WRITE(CODE,'*OUTEXPRESSION: '); 
              IF LEFT THEN WRITELN(CODE,'TRUE ',LAB:1)
              ELSE WRITELN(CODE,'FALSE ',LAB:1);
             WRITE(CODE,'*MODE: '); 
              PRINTTYPE(EMODE); 
             WRITELN(CODE); 
             WRITELN(CODE,'* COND = ',COND,' WANTRES= ',WANTRES); 
             DMPPR(PSREG);
          END;
       MODE :=INDIRECT(EMODE);
       CASE TOKN
       OF 
         IDENT : BEGIN
                 LOADIT(T,LEFT);
                 GOTO 555;
                 END; 
         KONSTANT : BEGIN 
                     IF WANTRES 
                     THEN BEGIN 
                     IF PSREG=0 THEN PSREG:=NEXTREG;
                      GETREG(T,FALSE);
                      IF MODE < ORD(UNSIGNED) 
                      THEN BEGIN
                             MOVBTRI(PREGSET[PSREG].REGN,SVALUE); 
                           END
                      ELSE IF MODE < ORD(LONG)
                      THEN BEGIN
                             MOVWTRI(PREGSET[PSREG].REGN,SVALUE); 
                           END
                      ELSE IF MODE = ORD(LONG)
                      THEN BEGIN
                             MOVWTRI(AX,SVALUE MOD 65536);
                             MOVWTRI(DX,SVALUE DIV 65536);
                           END
                      ELSE IF ISPTRTYPE(MODE) 
                      THEN BEGIN
                             WITH PREGSET[PSREG]
                             DO BEGIN 
                                  MOVWTRI(REGN,SVALUE); 
                                END;
                           END
                      ELSE BEGIN
                             WRITELN(CODE,'* TYPE OF KONSTANT N.I');
                             ERROR(77); 
                           END; 
                         END; (* IF WANTRES *)
                   END; 
  
         STRING : BEGIN 
                    IF PSREG=0 THEN PSREG:=NEXTREG; 
                    GETINDX(PSREG); 
                    IF WANTRES
                    THEN WITH PREGSET[PSREG]
                    DO BEGIN
                           PREVIOUSLABEL; 
                           WRITE(CODE,'MOVWTRI ');
                           OUTALFA(RNAME[REGN],9);
                           WRITELN(CODE,',LIT.LIT+',OUTLITERAL(STRNG):1); 
                          END; (* IF WANTRES *) 
                 END; 
         ADDOP,BINARY,MULTOP,XORSYM,ASSIGN,ANDSYM,ORSYM 
               : BEGIN
                   IF PSREG=0 THEN PSREG:=NEXTREG;
                   IF OPER IN [LEFTSHIFT,RIGHTSHIFT]
                   THEN GOTO 222; 
                    CASE OPER 
                    OF
                      DIVIDE,MODOP, 
                      TIMES : BEGIN 
                                IF TOKN<>ASSIGN 
                                THEN BEGIN
  LFT^.PSREG:= T^.PSREG;
  OUTEXPTREE(LFT,FALSE,LAB);
  PSREG:=LFT^.PSREG;
  IF NOT(SIMPLE(RGHT)) OR (RGHT^.TOKN = KONSTANT) 
  THEN OUTEXPTREE(RGHT,FALSE,LAB);
END 
                                ELSE BEGIN
  IF NOT(SIMPLE(RGHT)) OR (RGHT^.TOKN = KONSTANT) 
  THEN BEGIN
         OUTEXPTREE(RGHT,FALSE,LAB);
       END; 
  IF NOT SIMPLE(LFT)
  THEN BEGIN
         IF PSREG=0 THEN PSREG:=NEXTREG;
         OUTEXPTREE(LFT,TRUE,LAB);
         DEREFCODE(LFT,PSREG,TRUE,0,0); 
       END
  ELSE BEGIN
         LFT^.PSREG:=T^.PSREG;
         OUTEXPTREE(LFT,FALSE,LAB); 
         PSREG:=LFT^.PSREG; 
       END; 
END;
                                MODE:= INDIRECT(LFT^.EMODE);
                                IF INDIRECT(RGHT^.EMODE) < MODE 
                                THEN MODE:=INDIRECT(RGHT^.EMODE); 
                                WITH PREGSET[PSREG] 
                                DO CASE MODE
                                OF 0,1,2:BEGIN
      INREG(PSREG,MODE);
      WANTIN8(AL,PSREG);
      FREER8(AH); 
      IF OPER = TIMES 
      THEN BEGIN
              PREVIOUSLABEL;
              IF INDIRECT(EMODE)=1
              THEN WRITELN(CODE,'MULB') 
              ELSE WRITELN(CODE,'IMULB'); 
            END 
       ELSE BEGIN 
               MOVBTRI(AH,0); 
               PREVIOUSLABEL; 
               IF INDIRECT(EMODE)=1 
               THEN WRITELN(CODE,'DIVB')
               ELSE WRITELN(CODE,'IDIVB');
             END; 
      OUTADRS(RGHT,0);
      FREEREG(PSREG); 
      IF OPER = TIMES 
      THEN USING(AX,PSREG)
      ELSE IF OPER = DIVIDE 
      THEN USING(AL,PSREG)
      ELSE USING(AH,PSREG); 
  
      IF LFT^.PSREG<>PSREG
      THEN IF LFT^.PSREG<>0 
      THEN FREEREG(LFT^.PSREG); 
      LFT^.PSREG:=PSREG;
  
      IF TOKN=ASSIGN
      THEN BEGIN
             STOREIT(LFT,PSREG,0);
             EMODE:=PREGSET[PSREG].PTYPE; 
           END; 
    END;
                               5 : BEGIN
INREG(PSREG,MODE);
GOTO 333; 
                                   END; 
  
                               3,4 : BEGIN
       INREG(PSREG,MODE); 
  WANTIN16(AX,PSREG); 
  FREER16(DX);
333 :                                 IF OPER = TIMES 
  THEN BEGIN
         PREVIOUSLABEL; 
         IF INDIRECT(EMODE)=3 
         THEN WRITELN(CODE,'MULW')
         ELSE WRITELN(CODE,'IMULW');
       END
  ELSE BEGIN
         IF INDIRECT(EMODE)=3 
         THEN BEGIN 
                MOVWTRI(DX,0);
                WRITELN(CODE,'          DIVW'); 
              END 
         ELSE BEGIN 
                PREVIOUSLABEL;WRITELN(CODE,'CWD');
                WRITELN(CODE,'          IDIVW');
              END;
  END;
  OUTADRS(RGHT,0);
  FREEREG(PSREG); 
  IF OPER = TIMES 
  THEN USING(AX,PSREG)
  ELSE IF OPER=DIVIDE 
  THEN USING(AX,PSREG)
  ELSE USING(DX,PSREG); 
  
  IF LFT^.PSREG<>PSREG
  THEN IF LFT^.PSREG<>0 
  THEN FREEREG(LFT^.PSREG); 
  LFT^.PSREG:=PSREG;
  
  IF TOKN=ASSIGN
  THEN BEGIN
         STOREIT(LFT,PSREG,0);
         EMODE:=PREGSET[PSREG].PTYPE; 
       END; 
END;
                          OTHERWISE BEGIN 
 WRITELN(CODE,'*TIMES: ',INDIRECT(EMODE):1);
                                    END;
                         END; (* CASE *)
                       END; (* TIMES *) 
  
  
                      PLUS : BEGIN
                               PRECODE(T,LAB);
                               IF ISINTEGRAL(INDIRECT(RGHT^.EMODE)) 
                               THEN APPLY('ADD       ',T) 
                               ELSE WRITELN(CODE,'* PLUS'); 
                               IF (MODE=ORD(LONG)) AND (RGHT^.TOKN<>KONSTANT) 
                               THEN BEGIN 
 APPLY2('ADC       ',T);
                                    END;
                               SETCC:=TRUE; 
                             END; 
                      MINUS: BEGIN
                               PRECODE(T,LAB);
                               IF ISINTEGRAL(INDIRECT(RGHT^.EMODE)) 
                               THEN APPLY('SUB       ',T) 
                               ELSE WRITELN(CODE,'* MINUS');
                               IF MODE=ORD(LONG)
                               THEN BEGIN 
 APPLY2('SBB       ',T);
END;
                              SETCC:=TRUE;
                             END; 
                      ANDOP: BEGIN
                             PRECODE(T,LAB);
                               IF ISINTEGRAL(INDIRECT(RGHT^.EMODE)) 
                               THEN APPLY('AND       ',T) 
                               ELSE WRITELN(CODE,'* AND');
                               IF MODE=ORD(LONG) THEN APPLY2('AND       ',T); 
                                 SETCC:=TRUE; 
                             END; 
                      OROP : BEGIN
                               PRECODE(T,LAB);
                               IF ISINTEGRAL(INDIRECT(RGHT^.EMODE)) 
                               THEN APPLY('OR        ',T) 
                               ELSE WRITELN(CODE,'* OR'); 
                               IF MODE=ORD(LONG) THEN APPLY2('OR        ',T); 
                               SETCC:=TRUE; 
                             END; 
                      XOROP: BEGIN
                               PRECODE(T,LAB);
                               IF ISINTEGRAL(INDIRECT(RGHT^.EMODE)) 
                               THEN APPLY('XOR       ',T) 
                               ELSE WRITELN(CODE,'* XOR');
                               IF MODE=ORD(LONG) THEN APPLY2('XOR       ',T); 
                               SETCC:=TRUE; 
                             END; 
NOOP : BEGIN
         IF ISFIELD(INDIRECT(EMODE))
         THEN BEGIN 
                IF PSREG=0 THEN PSREG:=NEXTREG; 
                RGHT^.PSREG:=PSREG; 
                OUTEXPTREE(RGHT,FALSE,LAB); 
                PSREG:=RGHT^.PSREG; 
                OUTEXPTREE(LFT,TRUE,LAB); 
                INREG(PSREG,INDIRECT(EMODE)); 
                STOREIT(LFT,T^.PSREG,0);
              END 
         ELSE IF LFT^.OPER = REFSELECTOR
         THEN BEGIN 
                IF LFT^.RGHT^.TOKN IN [IDENT,KONSTANT]
                THEN BEGIN
                       IF PSREG=0 THEN PSREG:=NEXTREG;
                       RGHT^.PSREG := PSREG;
                       IF RGHT^.TOKN <> KONSTANT
                       THEN OUTEXPTREE(RGHT,FALSE,LAB); 
                       IF LFT^.RGHT^.TOKN=IDENT 
                       THEN LAB1:=LFT^.RGHT^.SENTRY^.IR.OFFSET
                       ELSE LAB1:=LFT^.RGHT^.SVALUE;
                       OUTEXPTREE(LFT^.LFT,FALSE,LAB);
                       IF RGHT^.TOKN=KONSTANT 
                       THEN BEGIN 
                              PREVIOUSLABEL;
                              IF INDIRECT(EMODE)< ORD(UNSIGNED) 
                              THEN WRITE(CODE,'MOVBI ') 
                              ELSE WRITE(CODE,'MOVWI ');
                             WRITELN(CODE,(RGHT^.SVALUE):1);
                             OUTIND(PREGSET[LFT^.LFT^.PSREG].REGN,LAB1);
                           END
                      ELSE STOREIT(LFT^.LFT,RGHT^.PSREG,LAB1);
                    END 
               ELSE GOTO 444; 
             END
        ELSE BEGIN
        444 :  PRECODE(T,LAB);
               IF RGHT^.TOKN <> KONSTANT
               THEN BEGIN 
                      INREG(PSREG,(RGHT^.EMODE)); 
              STOREIT(LFT,PSREG,0); 
            END 
       ELSE BEGIN 
              APPLY('MOV       ',T);
            END;
     END; 
   END; 
                      OTHERWISE BEGIN 
                        WRITELN(CODE,'* BINARY OPER TOKN=',ORD(TOKN):1,' OPER=',ORD(OPER)); 
                      END;
                    END;
                   IF(TOKN=ASSIGN) AND NOT(OPER IN [TIMES,DIVIDE,MODOP])
                    THEN BEGIN
                           IF WANTRES 
                           THEN IF NOT(SIMPLE(LFT)) 
                                THEN DEREFCODE(LFT,PSREG,FALSE,0,0) 
                                ELSE BEGIN
  FREEREG(LFT^.PSREG);
  LFT^.PSREG:=T^.PSREG; 
  OUTEXPTREE(LFT,FALSE,LAB);
  PSREG:=LFT^.PSREG;
END;
                         END; 
                 END; 
         CALL : BEGIN 
                  IF NOT(LEFT)
                  THEN BEGIN
                         OUTCALL(T);
                       END
                  ELSE BEGIN
                         LFT^.PSREG:=T^.PSREG;
                         OUTEXPTREE(LFT,TRUE,LAB);
                         PSREG:=LFT^.PSREG; 
                       END; 
                END;
         INDEXOP : BEGIN   (* INDEX OPERATOR *) 
                     IF PSREG=0 THEN PSREG:=NEXTREG;
                   IF POINTER=TYPETAB[INDIRECT(LFT^.EMODE)].BASICTYPE 
                   THEN BEGIN 
                           WRITELN(CODE,'* INDEXOP APPLIED TO POINTER');
                           OUTEXPTREE(LFT,FALSE,LAB); 
                         END
                   ELSE OUTEXPTREE(LFT,TRUE,LAB); 
                   LAB1:=PSREG; 
                   PSREG:=LFT^.PSREG; 
                   IF RGHT^.TOKN = KONSTANT 
                   THEN BEGIN 
                          INREGISTER(LFT);
                          IF NOT(LEFT) THEN DEREFCODE(T,LAB1,FALSE,RGHT^.SVALUE,0)
                          ELSE BEGIN
                                 PREVIOUSLABEL; 
                                 WRITE(CODE,'ADDWI ',(RGHT^.SVALUE):1);WRITELN(CODE); 
                                 REGISTR(PREGSET[PSREG].REGN);
                                 LAB1:=PSREG; 
                               END; 
                        END 
                   ELSE BEGIN 
                          OUTEXPTREE(RGHT,FALSE,LAB); 
                          INREGISTER(LFT);
                          WRITE(CODE,'          ADDWTR ');OUTREG(PREGSET[PSREG].REGN);WRITELN(CODE);
                          REGISTR(PREGSET[RGHT^.PSREG].REGN); 
                          IF NOT(LEFT) THEN DEREFCODE(T,LAB1,FALSE,0,0) 
                          ELSE LAB1:=PSREG; 
                        END;
                   PSREG:=LAB1; 
                 END; 
         OTHERWISE
         BEGIN
           CASE OPER
           OF 
             COMMAOP : BEGIN
                         LFT^.WANTRES:=FALSE; 
                         OUTEXPTREE(LFT,FALSE,LAB); 
                         RGHT^.PSREG:=T^.PSREG; 
                         RGHT^.WANTRES:=T^.WANTRES; 
                         OUTEXPTREE(RGHT,FALSE,LAB);
                         PSREG:=RGHT^.PSREG;
                         FREEREG(LFT^.PSREG); 
                       END; 
  
             NEG, 
             ONESCOMP : BEGIN 
                        IF INDIRECT(EMODE) = ORD(LONG)
                        THEN BEGIN
                               WRITELN(CODE,'* CAN NOT COMP LONGS YET');
                               ERROR(78); 
                             END; 
                        RGHT^.PSREG:=T^.PSREG;
                        OUTEXPTREE(RGHT,FALSE,LAB); 
                        PSREG:=RGHT^.PSREG; 
                        PREVIOUSLABEL;
                        WITH PREGSET[PSREG] 
                        DO BEGIN
                             IF OPER = NEG THEN CODEIT('NEG.      ',REGN) 
                             ELSE CODEIT('NOT.      ',REGN);
                             WRITELN(CODE); 
                             REGISTR(REGN); 
                           END; 
                        WRITELN(CODE);
                      END;
  
             LEFTSHIFT, 
             RIGHTSHIFT : BEGIN 
                      222 : RGHT^.PSREG:=NEXTREG; 
                            IF PSREG=0 THEN PSREG:=NEXTREG; 
                            IF (TOKN <> ASSIGN) OR ISFIELD(MODE)
                            THEN BEGIN
                                   LFT^.PSREG:=T^.PSREG;
                                   OUTEXPTREE(LFT,FALSE,LAB); 
                                   PSREG:=LFT^.PSREG; 
                                 END
                            ELSE BEGIN
                                   IF NOT(SIMPLE(LFT))
                                   THEN OUTEXPTREE(LFT,TRUE,LAB); 
                                 END; 
                            OUTEXPTREE(RGHT,FALSE,LAB); 
                            WITH PREGSET[RGHT^.PSREG] 
                            DO BEGIN
                                 IF REGN=0 THEN INREG(RGHT^.PSREG,ORD(KAR));
                                 IF REGN < 9 THEN WANTIN8(CL,RGHT^.PSREG) 
                                 ELSE WANTIN16(CX,RGHT^.PSREG); 
                               END; 
                            PREVIOUSLABEL;
                            CASE MODE 
                            OF 0,1,2 : BEGIN
    IF OPER=LEFTSHIFT 
    THEN WRITELN(CODE,'SHLB CX')
    ELSE WRITELN(CODE,'SHRB CX'); 
    IF TOKN<>ASSIGN 
    THEN OUTADRS(T,0) 
    ELSE OUTADRS(LFT,0);
  END;
                               3,4 : BEGIN
  IF OPER=LEFTSHIFT 
  THEN WRITELN(CODE,'SHLW CX')
  ELSE WRITELN(CODE,'SHRW CX'); 
  IF TOKN<>ASSIGN 
  THEN OUTADRS(T,0) 
  ELSE OUTADRS(LFT,0);
END;
                             OTHERWISE BEGIN
    WRITELN(CODE,'*SHIFT: TYPE NOT IMPLEMENTED'); 
    ERROR(79);
  END;
                            END;
                            FREEREG(RGHT^.PSREG); 
                            IF (TOKN= ASSIGN) AND WANTRES 
                            THEN BEGIN
                                   IF NOT SIMPLE(LFT) 
                                   THEN DEREFCODE(LFT,PSREG,FALSE,0,0)
                                   ELSE BEGIN 
     LFT^.PSREG:=T^.PSREG;
     OUTEXPTREE(LFT,FALSE,LAB); 
     PSREG:=LFT^.PSREG; 
   END; 
                                 END
                           ELSE IF WANTRES THEN INREG(PSREG,EMODE); 
                          END;
  
  
             NOTOP : BEGIN
                       LAB1:=GIVELABEL; 
                        FREER16(AX);USING(AX,PSREG);
                       JUMPC(LAB1,RGHT,FALSE);
                       GOTO 111;
                     END; 
  
  
             ANDFOP,ORFOP 
             : BEGIN
                 LAB1:= LAB;
                 IF LFT^.OPER = T^.OPER 
                 THEN BEGIN 
                        IF LAB1= 0 THEN LAB1:= GIVELABEL; 
                        OUTEXPTREE(LFT,FALSE,LAB1); 
                      END 
                 ELSE BEGIN 
                        IF LAB1=0 THEN LAB1:= GIVELABEL;
                        IF OPER=ANDFOP
                        THEN JUMPC(LAB1,LFT,TRUE) 
                        ELSE JUMPC(LAB1,RGHT,FALSE);
                      END;
                 IF OPER = ANDFOP 
                 THEN JUMPC(LAB1,RGHT,TRUE) 
                 ELSE JUMPC(LAB1,RGHT,FALSE); 
                 IF LAB = 0 
                 THEN BEGIN 
                        IF PSREG=0 THEN PSREG:=NEXTREG; 
                        WITH PREGSET[PSREG] 
                        DO BEGIN
                             REGN:=GETR16(0); 
                             USING(REGN,PSREG); 
                             PREVIOUSLABEL; 
                             WRITELN(CODE,'ANDWI 0'); 
                             REGISTR(REGN); 
                             IF OPER=ANDFOP 
                             THEN BEGIN 
                                    WRITELN(CODE,'           ORWI 1');
                                    REGISTR(REGN);
                                  END;
                             LAB2:=GIVELABEL; 
                             JUMP(LAB2);
                             LABELIT(LAB1); 
                             PREVIOUSLABEL; 
                             WRITELN(CODE,'ANDWI 0'); 
                             REGISTR(REGN); 
                             IF OPER = ORFOP
                             THEN BEGIN 
                                    WRITELN(CODE,'          ORWI 1'); 
                                    REGISTR(REGN);
                                  END;
                             LABELIT(LAB2); 
                             T^.SETCC:=TRUE;
                             IF T^.COND 
                             THEN BEGIN 
                                    FREEREG(PSREG); 
                                    PSREG:=0; 
                                   END; 
                            END; (* WITH *) 
                         END; (* IF *)
               END; 
  
             IFOP 
             : BEGIN
                 LAB1:=0; 
                 LASTTYPE:=LFT^.EMODE;
                 JUMPC(LAB,LFT,TRUE); 
                 LAB1:=0; 
                 FREEREG(PSREG);
                  RGHT^.PSREG:=T^.PSREG;
                 OUTEXPTREE(RGHT,FALSE,LAB1); 
                 PSREG:=RGHT^.PSREG;
               END; 
  
             ELSEOP 
             : BEGIN
                 LAB1:=GIVELABEL; 
                 LFT^.PSREG:=T^.PSREG;
                 OUTEXPTREE(LFT,FALSE,LAB1);
                 LAB2:=GIVELABEL; 
                 JUMP(LAB2);
                 LABELIT(LAB1); 
                 LAB1:=0; 
              (* MARK CURRENT REGISTER AS BEING UNUSED *) 
                 WITH PREGSET[LFT^.PSREG] 
                 DO BEGIN 
                      REGSET[REGN].RFLAGS := REGSET[REGN].RFLAGS +[FREE]; 
                      IF SREGN<>0 
                      THEN BEGIN
                             WRITELN(CODE,'* POSSIBLE PROBLEMS HERE');
                             WARN(13);
                             REGSET[SREGN].RFLAGS:=REGSET[SREGN].RFLAGS +[FREE];
                           END; 
                    END;
                 T^.PSREG:=LFT^.PSREG;
             (* SAVE THE CURRENT STACK POINTER OFFSET *)
                 MODE := STACKPTR;
                 OUTEXPTREE(RGHT,FALSE,LAB1); 
                 IF PREGSET[RGHT^.PSREG].REGN <> PREGSET[PSREG].REGN
                 THEN BEGIN 
                        IF INDIRECT(EMODE) < ORD(UNSIGNED)
                        THEN BEGIN
                               WANTIN8(PREGSET[PSREG].REGN,RGHT^.PSREG);
                             END
                        ELSE BEGIN
                               WANTIN16(PREGSET[PSREG].REGN,RGHT^.PSREG); 
                             END; 
                      END;
                 STACKPTR := GREATEROF(STACKPTR,MODE);
                 LABELIT(LAB2); 
                 USING(PREGSET[PSREG].REGN,PSREG);
                 IF PREGSET[PSREG].SREGN<>0 
                 THEN USING(PREGSET[PSREG].SREGN,PSREG);
               END; 
  
             DEREF : BEGIN
                       IF PSREG=0 THEN PSREG:=NEXTREG;
                       IF LEFT
                       THEN BEGIN 
                              RGHT^.PSREG:=T^.PSREG;
                              OUTEXPTREE(RGHT,FALSE,LAB); 
                              PSREG:=RGHT^.PSREG; 
                            END 
                       ELSE BEGIN 
                              OUTEXPTREE(RGHT,FALSE,LAB); 
                              RGHT^.EMODE:=INDIRECT(RGHT^.EMODE); 
                              DEREFCODE(RGHT,PSREG,FALSE,0,0);
                            END;
                     END; 
  
           CONVERT : BEGIN
                       CHANGEIT(T,LEFT,LAB);
                     END; 
  
             REF : BEGIN
                     RGHT^.PSREG:=T^.PSREG; 
                     OUTEXPTREE(RGHT,TRUE,LAB); 
                     PSREG:=RGHT^.PSREG;
                   END; 
             REFSELECTOR, 
             SELECTOR : BEGIN 
                          IF NOT(SIMPLE(T)) OR LEFT 
                          THEN BEGIN
                                 WRITELN(CODE,'*SELECT: NOT SIMPLE'); 
                                 IF RGHT^.TOKN <> IDENT 
                                 THEN BEGIN 
   OUTEXPTREE(RGHT,TRUE,LAB); 
 END; 
                                 LFT^.PSREG:=T^.PSREG;
                                 OUTEXPTREE(LFT,(OPER=SELECTOR),LAB); 
                                 PSREG:=LFT^.PSREG; 
                                 IF RGHT^.TOKN=KONSTANT 
                                 THEN LAB1:=RGHT^.SVALUE
                                 ELSE IF (RGHT^.TOKN=IDENT) 
                                 THEN LAB1:=RGHT^.SENTRY^.IR.OFFSET 
                                 ELSE BEGIN 
   PREVIOUSLABEL; 
   WRITE(CODE,'ADDWTR '); 
   OUTREG(PREGSET[PSREG].REGN); 
   WRITELN(CODE); 
   OUTADRS(RGHT,0); 
   LAB1:=0; 
 END; 
                               END
                          ELSE LAB1 := 0; 
                          IF LEFT AND (LAB1 <> 0) 
                          THEN BEGIN
                                 PREVIOUSLABEL; 
                                 WRITELN(CODE,'ADDWI ',LAB1:1); 
                                 REGISTR(PREGSET[PSREG].REGN);
                               END
                          ELSE IF NOT(LEFT) 
                          THEN BEGIN
                                 IF PSREG=0 THEN PSREG:=NEXTREG;
                                 IF SIMPLE(T) 
                                 THEN BEGIN 
   LOADIT(T,FALSE); 
 END
                                 ELSE DEREFCODE(T,PSREG,FALSE,LAB1,0);
                                 IF ISFIELD(INDIRECT(EMODE))
                                 THEN WITH TYPETAB[INDIRECT(EMODE)] 
                                 DO BEGIN 
 GETFIELD(PSREG,FMASK,FOFFSET); 
                                    END;
                               END; 
                         END; 
  
  
             INCR,DECR : BEGIN
                           IF WANTRES AND (PSREG=0) 
                           THEN PSREG:=NEXTREG; 
                           IF NOT(SIMPLE(RGHT)) THEN OUTEXPTREE(RGHT,TRUE,LAB); 
                           INCDEC(RGHT,(OPER=INCR));
                           IF WANTRES 
                           THEN IF SIMPLE(RGHT) 
                           THEN BEGIN 
                                  RGHT^.PSREG:=PSREG; 
                                  LOADIT(RGHT,FALSE); 
                                  PSREG:=RGHT^.PSREG; 
                                END 
                           ELSE  DEREFCODE(RGHT,PSREG,FALSE,0,0); 
                         END; 
  
             POSTINC,POSTDEC : BEGIN
                                 IF WANTRES AND (PSREG = 0) 
                                 THEN PSREG:=NEXTREG; 
                                 IF NOT(SIMPLE(LFT))
                                 THEN OUTEXPTREE(LFT,TRUE,LAB); 
                                 IF WANTRES 
                                 THEN IF SIMPLE(LFT)
                                 THEN BEGIN 
    LFT^.PSREG:=PSREG;
    LOADIT(LFT,FALSE);
    PSREG:=LFT^.PSREG;
   END
                                 ELSE  DEREFCODE(LFT,PSREG,TRUE,0,0); 
                                 INCDEC(LFT,(OPER = POSTINC));
                               END; 
  
             EQOP,NEOP,GTOP,GEOP,LTOP,LEOP
             : BEGIN
                 IF COND
                 THEN BEGIN 
                        LFT^.PSREG:= T^.PSREG;
                        OUTEXPTREE(LFT,FALSE,LAB);
                        PSREG:=LFT^.PSREG;
                        IF NOT(SIMPLE(RGHT))
                        THEN OUTEXPTREE(RGHT,FALSE,LAB);
                        IF NOT(ISPTRTYPE(INDIRECT(LFT^.EMODE))) 
                        THEN BEGIN
                             IF PSREG=0 THEN PSREG:=NEXTREG;
                             APPLY('CMP       ',T); 
                             END
                        ELSE BEGIN
                               WRITELN(CODE,'*COND:TYPE ',INDIRECT(LFT^.EMODE):1);
                               ERROR(80); 
                             END; 
                      END 
                 ELSE BEGIN 
                        LAB1:=GIVELABEL;
                        FREER16(AX);
                        USING(AX,PSREG);
                        JUMPC(LAB1,T,TRUE); 
                        FREEREG(LFT^.PSREG);FREEREG(RGHT^.PSREG); 
                  111 : IF PSREG=0 THEN PSREG:=NEXTREG; 
                        PREGSET[PSREG].REGN := AX;
                        PREGSET[PSREG].PTYPE:= ORD(INT);
                        PREVIOUSLABEL;
                        WRITELN(CODE,'ANDWA 0');
                        WRITELN(CODE,'          ORWA 1'); 
                        LAB2:=GIVELABEL;
                        JUMPS(LAB2);
                        LABELIT(LAB1);
                        PREVIOUSLABEL;
                        WRITELN(CODE,'ANDWA 0');
                        SETCC:=TRUE;
                        LABELIT(LAB2);
                      END 
               END; 
  
             OTHERWISE BEGIN
                     OUTEXPTREE(LFT,FALSE,LAB); 
                     OUTEXPTREE(RGHT,FALSE,LAB);
                     WRITELN(CODE,'* OTHERWISE. TOKN=',ORD(TOKN):1,' OPER=',ORD(OPER):1); 
             END; (* OTHERWISE *) 
           END; (* CASE *)
         END; 
       END;  (* CASE *) 
       IF PREGSET[PSREG].REGN<>0
       THEN BEGIN 
              REGSET[PREGSET[PSREG].REGN].RFLAGS:=REGSET[PREGSET[PSREG].REGN].RFLAGS+[DIRTY]; 
              REGSET[PREGSET[PSREG].REGN].IDPTR:= NIL;
            END;
555 :  IF LFT<>NIL THEN IF PSREG<>LFT^.PSREG THEN 
                             IF LFT^.PSREG<>0 THEN FREEREG(LFT^.PSREG); 
       IF RGHT<>NIL THEN IF PSREG<>RGHT^.PSREG THEN 
                                  IF RGHT^.PSREG<>0 THEN FREEREG(RGHT^.PSREG);
(*       WRITE(CODE,'*OUTEXP:PSREG= ',(T^.PSREG):1);
       WRITE(CODE,' SP= ',STACKPTR:1,' TOS= ',TOPOFSTACK:1);
       WRITE(CODE,' EMODE=');PRINTTYPE(EMODE);WRITELN(CODE);
*)
       IF TOPOFSTACK<>TPOS
       THEN BEGIN 
              INREGISTER(T);
            END;
       STACKCHECK;
     END; 
END;
  
  
  
PROCEDURE OUTCODE;
BEGIN 
  CURRENTREG:=0;
  HEAD:=NIL;
  INITPSREGS; 
  PREVUNKNOWN;
(* HANDLE POSSIBLE LAST UNKNOWN ID
 *) 
  
 IF NOT(EXPERROR) THEN   LASTTYPE:=INDIRECT(TYPETREE(BRACKETS[0])); 
 TOPOFSTACK:=CURRENT^.AUTOSIZE; 
 PSALLOC(BRACKETS[0]);
  STACKCHECK; 
  LEVEL:=0; 
  LASTNODE[0]:=NIL; 
  BRACKETS[1]:=NIL; 
END; (* OUTCODE *)
  
PROCEDURE OUTEXPRESSION;
VAR T1 : INTEGER ;
    LAB : INTEGER;
    TEMP : INTEGER; 
BEGIN 
  LAB:=0; 
  OUTCODE;
  BRACKETS[0]^.WANTRES:=FALSE;
  IF EXPERROR THEN
                WRITELN(CODE,'*  ERROR IN EXPRESSION ') 
  ELSE IF NOT(REACHABLE)
  THEN WRITELN(CODE,'** CANNOT GET HERE......') 
  ELSE
    OUTEXPTREE(BRACKETS[0],FALSE,LAB);
  WITH BRACKETS[0]^ 
  DO BEGIN
       IF PSREG<>0 THEN FREEREG(PSREG); 
     END; 
  DISCARD(BRACKETS[0]); 
  IF (STACKPTR<>TOPOFSTACK) AND REACHABLE 
  THEN BEGIN
         PREVIOUSLABEL; 
         WRITELN(CODE,'ADDWI ',(STACKPTR-TOPOFSTACK):1,'         REPOSITION THE STACK POINTER');
         STACKPTR:=TOPOFSTACK;
         WRITELN(CODE,'          REGISTER SP'); 
       END; 
  BRACKETS[0]:=NIL; 
  EXPERROR:=FALSE;
  INITREGS; 
END;
(* OUTEXPRESSION
 *) 
  
  PROCEDURE PASS2; EXTERN;
BEGIN 
  PASS2;
END.
