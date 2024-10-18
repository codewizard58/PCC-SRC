(*$L'C86P2 - THE PARSER PART 2'*)
 
 
 
(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *    FUNCTION NEWEXPPTR
 *       SETUP A NEW EXPPTR RECORD.
 *    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 *)
  FUNCTION NEWEXPPTR:EXPTREE;
  VAR T:EXPTREE;
  BEGIN
(* NEWEXPPTR
 *)
     BEGIN
           NEW(T);
           T^.LFT:=NIL; 
           T^.RGHT:=NIL;
            WITH T^ 
           DO BEGIN
                SETCC:=FALSE;
                COND :=FALSE;
                WANTRES:= TRUE;
                PSREG:=0;
                STKTMP:= -1;
                OPER:=OP;
                EMODE:=ORD(IDTYPE);
                TOKN:=SY;
                IF SY = IDENT
                THEN BEGIN
                       SENTRY:=LASTID;
                       IF LASTID<>NIL THEN LID:=LASTID^.IR.NAME;
                       IDOFSET := 0;   (* 17-OCT-84 *)
                     END;
                IF SY = KONSTANT
                THEN BEGIN
                       IF IDTYPE IN [REEL,DOUBLE]
                       THEN RVALUE:=FVALU
                       ELSE SVALUE:=VAL;
                     END;
                IF SY = CAST
                THEN
                  EMODE:=TYPENUMBER;
                IF SY=STRING
                THEN BEGIN
                       STRNG:=STRINGLIST;
                       STRNGLENG:=STRINGLENG;
                     END;
              END;
        END;
    NEWEXPPTR:=T;
  END;
(* NEWEXPPTR
 *)
 
 
 
 
 
 
PROCEDURE PUSHTOKEN ;
 
(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *  THIS ROUTINE BUILDS AN EXPRESSION TREE
 *   OUT OF THE TOKEN STREAM PRODUCED BY THE
 *  PARSER.PUSHTOKEN ONLY CONSIDERS THE
 *  PRECEDENCE OF OPERATORS.
 *  THERE ARE TWO AUXILLARY DATA STRUCTURES USED
 *  TO BUILD THE TREE.
 *  I) BRACKETS[  ]
 *       AN ARRAY OF POINTERS INTO THE TREE.
 *       BRACKETS[I] POINTS TO THE START OF THE
 *         CURRENT I TH LEVEL OF BRACKET NESTING.
 *  II) LASTNODE[  ]
 *       THIS IS AN ARRAY OF POINTERS TO THE LAST
 *       NODE INSERTED INTO EACH LEVEL OF BRACKETED
 *       EXPRESSIONS.
 *  LEVEL  IS THE  VARIABLE WHICH RECORDS THE CURRENT
 *       LEVEL OF BRACKETING.
 *  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 *)
 
VAR T,T1 : EXPTREE ;
(* USED TO WALK THE TREE
 *)
    P1   : INTEGER;
(* HOLDS THE PRECEDENCE OF THE INPUT NODE
 *)
BEGIN
  CASE SY
  OF
    LP,LB  : BEGIN
               IF LEVEL < 19
               THEN LEVEL:=SUCC(LEVEL);
               BRACKETS[LEVEL]:=NIL;
               BRACKETS[LEVEL+1]:=NIL;
               LASTNODE[LEVEL]:=NIL;
               LASTNODE[LEVEL+1]:=NIL;
             END;
 
    RP,RB  : BEGIN
               IF LEVEL<>0
               THEN BEGIN
                      LEVEL:=PRED(LEVEL);
                      IF BRACKETS[LEVEL]=NIL
                      THEN BEGIN
                             BRACKETS[LEVEL]:=BRACKETS[LEVEL+1];
                             LASTNODE[LEVEL]:=BRACKETS[LEVEL];
                           END
                      ELSE
                          BEGIN
                           LASTNODE[LEVEL]^.RGHT:=BRACKETS[LEVEL+1];
                           LASTNODE[LEVEL]:=BRACKETS[LEVEL+1];
                          END;
                      LASTNODE[LEVEL+1]:=NIL;
                    END;
             END;
 
    IDENT,KONSTANT,STRING  : BEGIN
               T1:=NEWEXPPTR;
               IF LASTNODE[LEVEL]=NIL
               THEN BEGIN
                      BRACKETS[LEVEL]:=T1;
                    END
               ELSE LASTNODE[LEVEL]^.RGHT:=T1;
               LASTNODE[LEVEL]:=T1;
            END ;
 
 
    OTHERWISE BEGIN
                T1:=NEWEXPPTR;
                P1:=PRECEDENCE(T1);
                T :=BRACKETS[LEVEL];
                IF T = NIL
                THEN BEGIN
                       BRACKETS[LEVEL]:=T1;
                       LASTNODE[LEVEL]:=T1;
                     END
                ELSE BEGIN
                     IF ASSOCIATE(T1)<>RIGHTASS
                     THEN BEGIN
                       IF PRECEDENCE(T)<=P1
                       THEN BEGIN
                              T1^.LFT:=T; 
                              BRACKETS[LEVEL]:=T1;
                            END
                       ELSE BEGIN
                              WHILE PRECEDENCE(T^.RGHT) > P1
                              DO T := T^.RGHT ; 
                              T1^.LFT:=T^.RGHT; 
                              T^.RGHT:=T1;
                            END;
                       LASTNODE[LEVEL]:=T1 ;
                       END
                     ELSE BEGIN
                            IF PRECEDENCE(T)<P1
                            THEN BEGIN
                                   T1^.LFT:=T;
                                   BRACKETS[LEVEL]:=T1;
                                 END
                            ELSE BEGIN
                                   WHILE PRECEDENCE(T^.RGHT)>=P1
                                   DO T:=T^.RGHT; 
                                   T1^.LFT:=T^.RGHT;
                                   T^.RGHT:=T1; 
                                 END;
                            LASTNODE[LEVEL]:=T1;
                          END;
                     END;
           END ;
(* OTHERWISE
 *)
    END;
(* CASE
 *)
END;
(* PUSHTOKEN
 *)
 
 
 
 
 
 
(*NOW FOLLOWS THE PARSER *)
 PROCEDURE PRIM2(PRI:BOOLEAN);
 VAR SY1 : SYMMODE ;
     POS : INTEGER;
     STATE : IDSTATE;
     MODE : INTEGER;
     TYPEHEAD : TYPELIST;
 BEGIN
(* PRIM2  , SECOND PART OF PRIMARY
 *)
    IF POSIVALUE AND(OP IN [SELECTOR,REFSELECTOR])
    THEN PRI:=TRUE;
 
(* NOW CHECK FOR POST PRIM TOKENS
 *)
    WHILE PRI
    DO CASE SY
         OF
           LP     : BEGIN
(* FUNCTION CALL
 *)
                     SY:=CALL;
                      PUSHTOKEN;
                      SY:=LP;
                      PUSHTOKEN;
                      IF LASTID^.IR.THISMODE = ORD(NULLTYPE)
                      THEN BEGIN
(* MARK ID AS A FUNCTION
 *)
                             LASTID^.IR.THISMODE:=(FUNCRETURNING(ORD(INT)));
                              LASTID^.IR.THISSTATE:=EXTERNVAR;
                            END;
                      INSYMBOL ;
                      PARAMLIST;
                      PUSHTOKEN;
                      NEXTSYMBOL(RP,24);
                      POSIVALUE:=FALSE;
                   END;
(* LP
 *)
           LB    : BEGIN
(* ARRAY ACCESS
 *)
                        BEGIN
                          SY:=INDEXOP;
                          OP:=INDEX;
                          PUSHTOKEN;
                          OP:=NOOP;
                          SY:=LB;
                          PUSHTOKEN;
                          INSYMBOL ;
                          EXPRESSION;
                          SKIP([RB]);
                          POSIVALUE:=TRUE;
                          PUSHTOKEN;
                          INSYMBOL ;
                        END;
                  END;
(* LB
 *)
           TERMOP:BEGIN
                    IF (OP=REFSELECTOR) OR (OP=SELECTOR)
                    THEN
                          BEGIN
                              PUSHTOKEN;
                              INSYMBOL ;
                              POSIVALUE:=TRUE;
                              IF SY=IDENT
                              THEN BEGIN
                                     PREVUNKNOWN;
                                     IF NOT(ISDEFINED(STRUCTDICT^,ID,POS,STATE,MODE,LASTID))
                                     THEN BEGIN WRITELN(CODE,'*LOOKING FOR ',ID,' IN CURRENT'); 
                                     IF NOT(ISDEFINED(CURRENT^,ID,POS,STATE,MODE,LASTID)) 
                                      THEN ERROR(3);
                                      END;
                                     PUSHTOKEN;
                                     INSYMBOL ;
                                   END
                              ELSE IF SY=KONSTANT
                              THEN BEGIN
                                     PUSHTOKEN;
                                     INSYMBOL ;
                                   END
                              ELSE ERROR(25);
                           END
                     ELSE 
                     IF OP IN[INCR,DECR]
                     THEN BEGIN 
                            IF OP= INCR THEN OP:= POSTINC 
                            ELSE OP:= POSTDEC;
                            PUSHTOKEN;
                            INSYMBOL; 
                          END 
                     ELSE PRI:= FALSE;
                 END; 
(*         TERMOP 
 *)
         OTHERWISE
              PRI:=FALSE; 
       END;
(* CASE
 *)
  END ;
(* PRIM
 *)
 
  PROCEDURE PRIM;
  VAR PRI : BOOLEAN;
      SY1 : SYMMODE;
      OP1 : OPSYM;
      POS : INTEGER;
      STATE: IDSTATE;
      MODE: INTEGER;
  BEGIN
(* PRIM
 *)
    PRI:=TRUE;
    CASE SY
    OF
      IDENT : BEGIN
               UNKNOWNID;
                POSIVALUE:=TRUE;
                PUSHTOKEN;
                INSYMBOL ;
              END;
(* IDENT
 *)
      TYPESYM : BEGIN
                  VAL := SIZEOFTYPE(ORD(IDTYPE));
                  SY  := KONSTANT;
                  PUSHTOKEN;
                  INSYMBOL;
                END;
 
      KONSTANT:BEGIN
                 VAL:=VALU;
                PUSHTOKEN;
                INSYMBOL ;
              END;
(* KONSTANT
 *)
      STRING: BEGIN
                PUSHTOKEN;
                INSYMBOL ;
              END;
      LP :    BEGIN
                INSYMBOL ;
                IF ABSDECLAR
                THEN BEGIN
                       SY1:=SY;
                       SY:=CAST;
                       PUSHTOKEN;
                       SY:=SY1;
                       PRI:=FALSE;
                       NEXTSYMBOL(RP,26);
                       EXP;
                     END
                ELSE
                     BEGIN
                       SY1:=SY;
                       SY:=LP;
                       PUSHTOKEN;
                       SY:=SY1;
                       EXPRESSION ;
                       PUSHTOKEN;
                      NEXTSYMBOL(RP,27);
                      POSIVALUE:=TRUE;
                      PRI:=TRUE;
                     END;
              END;
(* LP
 *)
      MULTOP: BEGIN
                IF OP=TIMES
                THEN BEGIN
                       POSIVALUE:=TRUE;
                       OP:=DEREF;
                       PUSHTOKEN;
                       INSYMBOL ;
                       EXP;
                       POSIVALUE:=TRUE;
                     END
                   ELSE ERROR(69);
              END;
(* TIMES
 *)
      OTHERWISE
              BEGIN
               PRI:=FALSE;
               ERROR(70);
WRITELN('PRIM: SY=',ORD(SY));
               END;
    END ;
(* CASE
 *)
    PRIM2(PRI);
  END;
(* PRIM
 *)
 
 PROCEDURE EXP2;
 
(* SECOND PART OF EXP
 *)
VAR SY1,SY2 : SYMMODE;
    OP1,OP2 : OPSYM;
 BEGIN
(* EXP2
 *)
   IF POSIVALUE
   THEN IF OP IN[INCR,DECR]
        THEN BEGIN
               IF OP=INCR
               THEN OP:=POSTINC
               ELSE OP:=POSTDEC;
               PUSHTOKEN;
               INSYMBOL ;
             END
        ELSE IF OP IN ASSIGNSET
             THEN BEGIN
                    OP1:=OP;
                    SY1:=SY;
                    INSYMBOL ;
                    IF SY=ASSIGN
                    THEN BEGIN
                           OP:=OP1;
                           PUSHTOKEN;
                           INSYMBOL ;
                           EXP;
                         END
                    ELSE BEGIN
                           SY2:=SY;OP2:=OP;
                           SY:=SY1;OP:=OP1;
                           PUSHTOKEN;
                           SY:=SY2;OP:=OP2;
                           EXP;
                         END;
                  END
        ELSE IF SY=ASSIGN
        THEN BEGIN
               IF NOT(POSIVALUE)
               THEN ERROR(60);
               PUSHTOKEN;
               INSYMBOL ;
               EXP;
             END;
   END;
(* END OF EXP2
 *)
 
 
  PROCEDURE POSTEXPRESSION;
  VAR SY1: SYMMODE; 

  BEGIN
    IF OP IN BINOP
    THEN BEGIN
           PUSHTOKEN;
           INSYMBOL ;
           EXP;
         END
    ELSE IF OP = IFOP
    THEN BEGIN
           PUSHTOKEN;
           SY := LP;     (* 22-OCT-85 *)
           PUSHTOKEN; 
           INSYMBOL ;
           EXPRESSION;
           SY1 := SY; 
           SY := RP;
           PUSHTOKEN; 
           SY := SY1; 
           PUSHTOKEN;
           IF OP=ELSEOP THEN INSYMBOL ;
           SY1 := SY; 
           SY := LP;
           PUSHTOKEN; 
           SY := SY1; 
           EXP;
           SY1 := SY; 
           SY := RP;
           PUSHTOKEN; 
           SY := SY1; 
(* 22-OCT-85  ? : FIX. *) 
         END;
  END;
(* POSTEXPRESSION
 *)
 
 
 
 
 
 
 
  PROCEDURE EXP;
  VAR SY1:SYMMODE;
      OP1:OPSYM;
      PRI : BOOLEAN;
  BEGIN
(* EXP
 *)
   POSIVALUE:=FALSE ;
    IF OP IN [MINUS,NOTOP,COMPOP,ANDOP]
    THEN BEGIN
           IF OP = ANDOP
           THEN OP:=REF;
           IF OP = COMPOP
           THEN OP:=ONESCOMP
           ;IF OP=MINUS
            THEN OP:=NEG;
           PUSHTOKEN;
           INSYMBOL ;
           EXP;
         END
    ELSE IF OP = SIZEOF
    THEN BEGIN
           PUSHTOKEN;
           INSYMBOL;
           IF SY = LP 
           THEN BEGIN 
                  PUSHTOKEN;
                  INSYMBOL; 
                IF ABSDECLAR
                THEN BEGIN
                            SY1 := SY;
                            SY  := KONSTANT;
                            PUSHTOKEN;
(* ADDED 1-AUG-84 *)        LASTNODE[LEVEL]^.EMODE:= POINTERTO(TYPENUMBER); 
                            SY  := SY1; 
                            PUSHTOKEN;
                            NEXTSYMBOL(RP,26);
                     END
                ELSE BEGIN
                    EXPRESSION; 
                    PUSHTOKEN;
                    NEXTSYMBOL(RP,27);
                    POSIVALUE := TRUE;
                    PRI:=TRUE;
                    PRIM2(PRI); 
                  END 
                END 
           ELSE EXP;
         END
    ELSE
      IF OP IN [INCR,DECR]
      THEN BEGIN
             PUSHTOKEN;
             INSYMBOL ;
             PRIM;
             IF NOT(POSIVALUE) THEN ERROR(28);
           END
      ELSE
           BEGIN
             PRIM;
             EXP2;
           END;
      POSTEXPRESSION;
 
  END ;
 
 
  PROCEDURE EXPRESSION;
  BEGIN
    EXP;
    WHILE SY=COMMA
    DO
    BEGIN
      OP:=COMMAOP;
      PUSHTOKEN;
      INSYMBOL ;
      EXP;
    END;
  END ;
(* EXPRESSION
 *)
 
 
 
 
 
PROCEDURE OUTJUMP(LAB : INTEGER;JTF : BOOLEAN);
BEGIN
  OUTCODE;
  JUMPC(LAB,BRACKETS[0],JTF);
  DISCARD(BRACKETS[0]);
  EXPERROR:=FALSE;
  BRACKETS[0]:=NIL;
  INITREGS;
END;
 
 
PROCEDURE IFSTMTNT(LOOPSTART,BREAKLAB:INTEGER);
 
(* PROCEDURE TO PARSE AN IF STMTNT.
 *)
VAR FILAB,ELSELAB:INTEGER;
BEGIN
  IF SY=IFSYM
  THEN BEGIN
         INSYMBOL ;
         NEXTSYMBOL(LP,29);
         ELSELAB:=GIVELABEL;
         EXPRESSION;
         OUTJUMP(ELSELAB,TRUE);
         NEXTSYMBOL(RP,30);
         STMTNT(LOOPSTART,BREAKLAB);
         IF SY=ELSESYM
         THEN BEGIN
                FILAB:=GIVELABEL;
                JUMP(FILAB);
                LABELIT(ELSELAB);
                INSYMBOL ;
                STMTNT(LOOPSTART,BREAKLAB);
                LABELIT(FILAB);
                STACKSET;
              END
         ELSE BEGIN
                LABELIT(ELSELAB);
                STACKSET;
              END;
       END;
 END;
(* IFSTMTNT
 *)
  PROCEDURE WHILESTMTNT;
  VAR STARTWHILE,ENDWHILE : INTEGER ;
  BEGIN
    IF SY=WHILESYM
    THEN BEGIN
           INSYMBOL ;
           STARTWHILE:=GIVELABEL;
           ENDWHILE  :=GIVELABEL;
           NEXTSYMBOL(LP,0);
           LABELIT(STARTWHILE);
           STACKSET;
           EXPRESSION;
           OUTJUMP(ENDWHILE,TRUE);
           NEXTSYMBOL(RP,31);
           STMTNT(STARTWHILE,ENDWHILE);
           JUMP(STARTWHILE);
           LABELIT(ENDWHILE);
           STACKSET;
         END;
  END;
(* WHILESTMTNT
 *)
 
 
  PROCEDURE FORSTMTNT;
  VAR STARTTEST,STARTSTATMT,STARTINC,ENDSTATMT:INTEGER;
  BEGIN
    IF SY=FORSYM
    THEN BEGIN
           INSYMBOL ;
           NEXTSYMBOL(LP,32);
           IF SY<>SEMI
           THEN BEGIN
                  EXPRESSION;
                  OUTEXPRESSION;
                END;
           NEXTSYMBOL(SEMI,33);
           STARTTEST:=GIVELABEL;
           LABELIT(STARTTEST);
           STACKSET;      (* ENSURE THAT THE STACK IS CORRECT *)
           STARTSTATMT:=GIVELABEL;
           ENDSTATMT:=GIVELABEL;
           IF SY<>SEMI
           THEN BEGIN
                  EXPRESSION;
                  OUTJUMP(ENDSTATMT,TRUE);
                END;
           NEXTSYMBOL(SEMI,34);
           IF SY=RP
           THEN
             STARTINC:=STARTTEST
           ELSE BEGIN
                  JUMP(STARTSTATMT);
                  STARTINC:=GIVELABEL;
                  LABELIT(STARTINC);
                  STACKSET;(*ENSURE THAT THE STACK IS CORRECT *)
                  EXPRESSION;
                  OUTEXPRESSION;
                  JUMP(STARTTEST);
                  LABELIT(STARTSTATMT);
                END;
           NEXTSYMBOL(RP,35);
           STMTNT(STARTINC,ENDSTATMT);
           JUMP(STARTINC);
           LABELIT(ENDSTATMT);
           STACKSET;      (* ENSURE THAT THE STACK IS CORRECT *)
         END;
  END;
(* FORSTMTNT
 *)
 
 
  PROCEDURE DOSTMTNT;
  VAR WHILELAB,STARTLAB : INTEGER;
      ENDLAB: INTEGER;
  BEGIN
    IF SY=DOSYM
    THEN BEGIN
           INSYMBOL ;
           STARTLAB:=GIVELABEL;
           LABELIT(STARTLAB);
           WHILELAB:=GIVELABEL;
           ENDLAB:=GIVELABEL;
           STMTNT(WHILELAB,ENDLAB);
           LABELIT(WHILELAB);
           STACKSET;      (* ENSURE THAT THE STACK IS CORRECT *)
           NEXTSYMBOL(WHILESYM,36);
           NEXTSYMBOL(LP,36);
           EXPRESSION;
           OUTJUMP(STARTLAB,FALSE);
           NEXTSYMBOL(RP,37);
           LABELIT(ENDLAB);
           STACKSET;      (* ENSURE THAT THE STACK IS CORRECT *)
         END;
  END;
(* DOSTMTNT
 *)
 
 
PROCEDURE RETURNSTMTNT;
VAR SY1 : SYMMODE;
    OP1 : OPSYM  ;
BEGIN
  IF SY = RETURN
  THEN BEGIN
        INSYMBOL ;
         IF SY <> SEMI
         THEN BEGIN
                EXPRESSION;
                OUTCODE;
              END;
         RETURNCODE(BRACKETS[0]);
         NEXTSYMBOL(SEMI,38);
       END ;
END;
(* RETURNSTMTNT
 *)
 
 
 
 
 
 
PROCEDURE STMTNT;
LABEL 30;
 
     VAR OK : BOOLEAN;
(* DUMMY VARIABLE
 *)
         POS : INTEGER ;
         STATE : IDSTATE;
         MODE : INTEGER ;
         TID : ALFA ;
         TSY : SYMMODE;
         TOP : OPSYM ;
         SWIDPTR : IDTREE;
 
 
PROCEDURE CORDSTMTNT(VAR SWIDLAB : INTEGER;
                            SWIDEND : INTEGER;
                        VAR DEFLAB  : INTEGER);
LABEL 10;
VAR TOP : OPSYM;
    TSY : SYMMODE;
    JLAB: INTEGER;
BEGIN
  IF SY IN [ CASESYM,IDENT,DEFAULT]
  THEN BEGIN
         IF SY=DEFAULT
         THEN BEGIN
                LABELIT(DEFLAB);
                DEFLAB:=0;
                INSYMBOL ;
                IF OP=ELSEOP
                THEN INSYMBOL ;
              END
         ELSE IF SY = IDENT
         THEN BEGIN
            POSIVALUE:=FALSE;
            TID := ID;
            INSYMBOL ;
            PREVUNKNOWN;
            IF OP <> ELSEOP
            THEN BEGIN
(* NOT A LABEL
 *)
                   IF NOT(ISDEFINED(CURRENT^,TID,POS,STATE,MODE,LASTID))
                   THEN BEGIN
                          IF SY=LP
                          THEN BEGIN
                                 LASTID:=PUTINSYMBOLTABLE(CURRENT,TID,EXTERNVAR,FUNCRETURNING(ORD(INT)));
                                 MODE:=FUNCRETURNING(ORD(INT));
                               END
                          ELSE BEGIN
                                 LASTID:=PUTINSYMBOLTABLE(CURRENT,TID,AUTOVAR,ORD(INT));
                                 POSIVALUE:=TRUE;
                                 MODE:=ORD(INT);
                                 WARN(1);
                               END;
                      END;
            TSY:=SY;
            TOP:=OP;
            OP :=NOOP;
            SY:=IDENT;
            TYPENUMBER:=MODE;
            PUSHTOKEN;
            SY:=TSY;
            OP:=TOP;
            POSIVALUE:=TRUE;
            PRIM2(TRUE);
            EXP2;
            POSTEXPRESSION;
            WHILE SY=COMMA
            DO BEGIN
                 OP:=COMMAOP;
                 PUSHTOKEN;
                 INSYMBOL ;
                 EXP;
               END;
            OUTEXPRESSION;
            GOTO 10;
          END
(* NOT A LABEL
 *)
     ELSE BEGIN
(* LABEL
 *)
            IF NOT(ISDEFINED(GLOBALSYMBOL^.NEXTLEVEL^,TID,POS,STATE,MODE,LASTID)) 
            THEN BEGIN
(* NEW LABEL
 *)
                   LASTID:=PUTINSYMBOLTABLE(GLOBALSYMBOL^.NEXTLEVEL,TID,LABELL,ORD(NULLTYPE));
                   LASTID^.IR.OFFSET:=GIVELABEL;
                 END;
            LABELIT(LASTID^.IR.OFFSET); 
            STACKSET;
            INSYMBOL ;
          END;
              END
         ELSE
         BEGIN
              JLAB:=GIVELABEL;
              JUMP(JLAB);
              LABELIT(SWIDLAB);
              SWIDLAB:=GIVELABEL;
              WHILE SY = CASESYM
              DO BEGIN
                   INSYMBOL ;
                   EXPRESSION;
                   TOP:=OP;TSY:=SY;
                   SY:=EQUALITY;
                   OP:=EQOP;
                   PUSHTOKEN;
                   SY:=IDENT;
                   OP:=NOOP;
                   LASTID:=SWIDPTR;
                   PUSHTOKEN;
                   OP:=TOP;SY:=TSY;
                   IF OP=ELSEOP
                   THEN INSYMBOL ;
                   IF SY <> CASESYM
                   THEN BEGIN
                          OUTJUMP(SWIDLAB,TRUE);
                        END
                   ELSE
                        OUTJUMP(JLAB,FALSE);
                 END;
        LABELIT(JLAB);
       END;
        CORDSTMTNT(SWIDLAB,SWIDEND,DEFLAB);
      END
  ELSE
      STMTNT(LOOPSTART,SWIDEND);
  10: 
END;
(* CORDSTMTNT
 *)
 
 
 
PROCEDURE CASESTMTNT(VAR SWIDLAB : INTEGER;
                            SWIDEND : INTEGER;
                        VAR DEFLAB  : INTEGER);
VAR TLAB : INTEGER;
BEGIN
  TLAB:=DEFLAB;
  IF SY=BEGINSYM
  THEN BEGIN
         INSYMBOL ;
         NEWSYMTAB(CURRENT);
         NEWSYMTAB(STRUCTDICT);
         SC:=AUTOVAR;
         TYPENUMBER:=ORD(NULLTYPE);
         WHILE NAMEOFTYPE(TYPENUMBER) OR NAMEOFSC
         DO BEGIN
              WHILE NAMEOFTYPE(TYPENUMBER) OR NAMEOFSC
              DO ;
              IF SC=TYPEVAR
              THEN DECLTOR(TRUE,STRUCTDICT)
              ELSE DECLTOR(TRUE,CURRENT);
             WHILE SY=COMMA
             DO BEGIN
                  INSYMBOL ;
                   IF SC=TYPEVAR
                   THEN DECLTOR(TRUE,STRUCTDICT)
                ELSE DECLTOR(TRUE,CURRENT);
                 END;
              IF SY = SEMI THEN INSYMBOL ;
              SC:=AUTOVAR;
              TYPENUMBER:=ORD(NULLTYPE);
            END;
         WHILE SY<>ENDSYM
         DO
           CORDSTMTNT(SWIDLAB,SWIDEND,TLAB);
         CURRENT:=CURRENT^.LASTLEVEL; 
         STRUCTDICT:=STRUCTDICT^.LASTLEVEL; 
         DISSYMTAB(CURRENT^.NEXTLEVEL); 
         DISSYMTAB(STRUCTDICT^.NEXTLEVEL);
         CURRENT^.NEXTLEVEL:=NIL; 
         STRUCTDICT^.NEXTLEVEL:=NIL;
         INSYMBOL ;
       END
  ELSE
       CORDSTMTNT(SWIDLAB,SWIDEND,TLAB);
  JUMP(SWIDEND);
  LABELIT(SWIDLAB);
  IF TLAB=0
(* THERE WAS A DEFAULT LABEL
 *)
  THEN BEGIN
         JUMP(DEFLAB);
       END;
END;
(* CASESTMTNT
 *)
 
 
 
 
 PROCEDURE SWITCHSTMTNT;
  VAR SWID : ALFA;
      T,T1 : EXPTREE ;
      SWIDLAB : INTEGER;
      SWIDEND : INTEGER;
      DEFLAB  : INTEGER;
      TOP : OPSYM;
      TSY : SYMMODE;
BEGIN
  IF SY = SWITCH
  THEN BEGIN
         INSYMBOL ;
         NEXTSYMBOL(LP,41);
         NEW(SWIDPTR);
         WITH SWIDPTR^
         DO BEGIN
              L:=NIL;
              R:=NIL;
              IR.NAME:='          ';
              IR.THISSTATE:=AUTOVAR;
              IR.OFFSET:=CURRENT^.SWITCHLEV;
              IR.THISMODE:=ORD(INT);
            END;
         LASTID:=SWIDPTR;
         TID:=ID;
         TSY:=SY;
         TOP:=OP;
         SY:=IDENT;
         OP:=NOOP;
         PUSHTOKEN;
         SY:=ASSIGN;
         PUSHTOKEN;
         ID:=TID;
         SY:=TSY;
         OP:=TOP;
         EXPRESSION;
         OUTEXPRESSION;
         NEXTSYMBOL(RP,42);
         DEFLAB:=GIVELABEL;
         SWIDEND:=GIVELABEL;
         SWIDLAB:=GIVELABEL;
         JUMP(SWIDLAB);
         CASESTMTNT(SWIDLAB,SWIDEND,DEFLAB);
         LABELIT(SWIDEND);
         STACKSET;        (* ENSURE THAT THE STACK IS CORRECT *)
         DISPOSE(SWIDPTR);
       END;
END;
(* SWITCH STMTNT
 *)
 
 
 
BEGIN
(* STMTNT1
 *)
  SWIDPTR:=NIL;
  POSIVALUE:=FALSE;
  LASTID:=NIL;
   CASE SY OF
      BEGINSYM : BEGIN
                 COMPOUNDST(LOOPSTART,BREAKLAB);
               END;
 
      IFSYM    : IFSTMTNT(LOOPSTART,BREAKLAB);
 
      WHILESYM : WHILESTMTNT;
 
      DOSYM    : BEGIN
                   DOSTMTNT;
                   NEXTSYMBOL(SEMI,43);
                 END;
 
      FORSYM   : FORSTMTNT ;
 
      BREAK    : BEGIN
                   INSYMBOL ;
                   IF BREAKLAB<>0
                   THEN
                     JUMP(BREAKLAB)
                   ELSE
                      ERROR(44);
                   NEXTSYMBOL(SEMI,45);
                 END ;
 
     CONTINUE  : BEGIN
                   INSYMBOL ;
                   IF LOOPSTART<>0
                   THEN
                     JUMP(LOOPSTART)
                   ELSE
                     ERROR(46);
                   NEXTSYMBOL(SEMI,47);
                 END ;
 
 
     GOTOSYM : BEGIN
                 INSYMBOL ;
                 IF SY<>IDENT
                 THEN
                   ERROR(48)
(* ILLEGAL LABEL
 *)
                 ELSE BEGIN
                        IF ISDEFINED(CURRENT^,ID,POS,STATE,MODE,LASTID) 
                        THEN
                        ELSE BEGIN
                               LASTID:=PUTINSYMBOLTABLE(GLOBALSYMBOL^.NEXTLEVEL,ID,LABELL,ORD(NULLTYPE)); 
                               LASTID^.IR.OFFSET:=GIVELABEL;
                             END;
                         JUMP(LASTID^.IR.OFFSET); 
                        INSYMBOL ;
                      END;
                NEXTSYMBOL(SEMI,50);
              END ;
(* GOTO
 *)
     SWITCH : SWITCHSTMTNT ;
 
     RETURN : RETURNSTMTNT ;
 
  IDENT : BEGIN
            PREVUNKNOWN;
(* POSSIBLE LABEL OR EXPRESSION
 *)
            POSIVALUE:=FALSE;
            TID := ID;
            INSYMBOL ;
            IF OP <> ELSEOP
            THEN BEGIN
(* NOT A LABEL
 *)
                   IF NOT(ISDEFINED(CURRENT^,TID,POS,STATE,MODE,LASTID))
                   THEN BEGIN
                          IF SY=LP
                          THEN BEGIN
                                 LASTID:=PUTINSYMBOLTABLE(CURRENT,TID,EXTERNVAR,FUNCRETURNING(ORD(INT)));
                                 MODE:=FUNCRETURNING(ORD(INT));
                               END
                          ELSE BEGIN
                                 LASTID:=PUTINSYMBOLTABLE(CURRENT,TID,EXTERNVAR,ORD(INT));
                                 POSIVALUE:=TRUE;
                                 MODE:=ORD(INT);
                                 WARN(1);
                               END;
                      END;
            TSY:=SY;
            TOP:=OP;
            OP :=NOOP;
            SY:=IDENT;
            TYPENUMBER:=MODE;
            PUSHTOKEN;
            SY:=TSY;
            OP:=TOP;
            IF SY IN [IDENT,KONSTANT,STRING,RP] THEN ERROR(50);
30:         POSIVALUE:=TRUE;
            PRIM2(TRUE);
            EXP2;
            POSTEXPRESSION;
            WHILE SY=COMMA
            DO BEGIN
                 OP:=COMMAOP;
                 PUSHTOKEN;
                 INSYMBOL ;
                 EXP;
               END;
            OUTEXPRESSION;
          END
(* NOT A LABEL
 *)
     ELSE BEGIN
(* LABEL
 *)
            IF NOT(ISDEFINED(GLOBALSYMBOL^.NEXTLEVEL^,TID,POS,STATE,MODE,LASTID)) 
            THEN BEGIN
(* NEW LABEL
 *)
                   LASTID:=PUTINSYMBOLTABLE(GLOBALSYMBOL^.NEXTLEVEL,TID,LABELL,ORD(NULLTYPE));
                   LASTID^.IR.OFFSET:=GIVELABEL;
                 END;
            LABELIT(LASTID^.IR.OFFSET); 
            STACKSET;
            INSYMBOL ;
            STMTNT(LOOPSTART,BREAKLAB);
          END;
   END;
(* IDENT PART
 *)
 
    KONSTANT : BEGIN
                 POSIVALUE:=TRUE;
                 VAL:=VALU;
                 PUSHTOKEN;
                 INSYMBOL ;
            POSIVALUE:=TRUE;
            PRIM2(TRUE);
            EXP2;
            POSTEXPRESSION;
            WHILE SY=COMMA
            DO BEGIN
                 OP:=COMMAOP;
                 PUSHTOKEN;
                 INSYMBOL ;
                 EXP;
               END;
            OUTEXPRESSION;
               END;
 
 
    TERMOP,
    ADDOP,
    COMPLEM,
    LP,
    ANDSYM,
    MULTOP: BEGIN
                 EXPRESSION;
                 OUTEXPRESSION ;
              IF CHECK(SEMI,[SEMI,ENDSYM],51) THEN INSYMBOL ;
            END ;
  SEMI  : INSYMBOL ;
(* NULL STMTNT
 *)
 
  ENDSYM : BEGIN
           END;
 
  EOSSYM : BEGIN
             ERROR(61);
           END;
 
  OTHERWISE    BEGIN
      ERROR(52);
(* NOT VALID TOKEN
 *)
       SKIP(EXPRSET);
     END;
(* OTHERWISE
 *)
     END;
(* CASE
 *)
       IF SY = SEMI THEN INSYMBOL  ;
(* SKIP SEMI
 *)
   END;
(* STMTNT1
 *)
 
 
 
 
 
 PROCEDURE STMTNTS(LOOPSTART,BREAKLAB:INTEGER);
 
 
 
(*
 *           STMTNTS -> E
 *           STMTNTS -> STATEMENTS  STATEMENT
 *)
 
   BEGIN
(* STMTNTS
 *)
    REPEAT
      STMTNT(LOOPSTART,BREAKLAB);
    UNTIL (SY=ENDSYM)OR EOF(INPUT);
   END;
(* STMTNTS
 *)
 
 
 
 
 
 
 
 
 
PROCEDURE COMPOUNDST;
 
 
 
(*
 *       PROCEDURE TO PARSE A 'C' PROGRAM
 *       PROGRAM ->  DECLTIONS STMTNTS ]
 *)
 
  BEGIN
    NEXTSYMBOL(BEGINSYM,53);
    NEWSYMTAB(CURRENT);
    NEWSYMTAB(STRUCTDICT);
    TYPENUMBER := ORD(NULLTYPE);
    SC:=AUTOVAR;
    WHILE NAMEOFTYPE(TYPENUMBER) OR NAMEOFSC
    DO BEGIN
       DECLTIN;
        TYPENUMBER:=ORD(NULLTYPE);
        SC:=AUTOVAR;
       END;
    STACKSET;
    STMTNTS(LOOPSTART,BREAKLAB);
    IF CHECK(ENDSYM,[ENDSYM],54) THEN INSYMBOL  ;
    CURRENT:=CURRENT^.LASTLEVEL;
    STRUCTDICT:=STRUCTDICT^.LASTLEVEL;
    DISSYMTAB(STRUCTDICT^.NEXTLEVEL); 
    STRUCTDICT^.NEXTLEVEL:=NIL; 
  END;
(* PROGRAMBLOCK
 *)
 
 
 
PROCEDURE DATADEF;
BEGIN
  IF(IDENTNAME<>'          ')AND(SC<>TYPEVAR) THEN
  BEGIN
        INLIZER;
  END;
  WHILE SY=COMMA
  DO
    BEGIN
      IF (SC<>EXTERNVAR) AND NOT(SIMPLEFORM)
      THEN BEGIN
             PRINTSTATIC;
             IF REACHABLE
             THEN WRITELN(CODE,'          END                * DATADEF');
             REACHABLE := FALSE;
           END;
      INSYMBOL ;
            INTDLR;
    END;
  IF SY<>SEMI THEN ERROR(55)
  ELSE INSYMBOL ;
END;
 
 
 
 
 
 PROCEDURE TYPDLLIST;
 BEGIN
   WHILE (SY<>BEGINSYM) AND NOT(EOF(INPUT))
   DO BEGIN
  TYPENUMBER:=ORD(NULLTYPE);
   SC:=AUTOVAR;
        WHILE NAMEOFTYPE(TYPENUMBER) OR NAMEOFSC DO ; 
        IF SC <> AUTOVAR THEN ERROR(81); SC := AUTOVAR; 
        IF SC=TYPEVAR
        THEN DECLTOR(TRUE,STRUCTDICT)
        ELSE DECLTOR(TRUE,CURRENT);
        WHILE SY=COMMA
        DO BEGIN
              INSYMBOL ;
             IF SC=TYPEVAR
             THEN DECLTOR(TRUE,STRUCTDICT)
             ELSE DECLTOR(TRUE,CURRENT);
           END;
        IF SY=SEMI
        THEN INSYMBOL ;
      END;
 END;
(* TYPDLLIST
 *)
 
 
  PROCEDURE FILLINPARAMOFFSETS;
 
(* TAKE IN LIST POINTED TO BY PLIST
 *    AND FILL IN THE OFFSETS
 *)
 VAR T : EXPTREE;
    T1: IDTREE;
    POS : INTEGER;
    STATE : IDSTATE;
    MODE : INTEGER;
    OFF : INTEGER;
 BEGIN
  OFF := 0;
   CURRENT^.AUTOSIZE:=0;
   WHILE PLIST <> NIL
   DO BEGIN
        T1:=FINDID(CURRENT^.TREE,PLIST^.SENTRY^.IR.NAME); 
        IF T1<>NIL
        THEN BEGIN
               MODE:=T1^.IR.THISMODE; 
               T1^.IR.THISSTATE := PARAMVAR;
               IF TYPETAB[MODE].BASICTYPE=ARRAYTYPE
             THEN BEGIN
                    IF TYPETAB[MODE].INDEXRANGE=0
                    THEN BEGIN
 
(* CONVERT ARRAYDEF TO POINTER
 *)
                           T1^.IR.THISMODE:=POINTERTO(INDIRECT(MODE));
                           MODE:=T1^.IR.THISMODE; 
                         END;
                  END;
               T1^.IR.OFFSET:=CURRENT^.AUTOSIZE;
               CURRENT^.AUTOSIZE:=CURRENT^.AUTOSIZE+SIZEOFTYPE(MODE); 
             END
        ELSE BEGIN
             T1:=PUTINSYMBOLTABLE(CURRENT,PLIST^.SENTRY^.IR.NAME,PARAMVAR,ORD(INT));
             MODE:=ORD(INT);
             END;
        T:=PLIST;
        PLIST:=PLIST^.LFT;
        DISPOSE(T);
        T1^.IR.OFFSET:=OFF; 
        OFF := OFF + SIZEOFTYPE(MODE);
      END ;
   DISSYMTAB(TSYMTAB);
   FUNCID^.IR.OFFSET := OFF;    (* RECORD THE NUMBER OF PARAMETERS *) 
   CURRENT^.AUTOSIZE:=0;
 END ;
(* FILLINOFFSETS
 *)
 
 
 PROCEDURE FUNCTIONBODY;
 BEGIN
   INFUNCDEF:=TRUE;
    NEWSYMTAB(CURRENT);
    NEWSYMTAB(STRUCTDICT);
   TYPDLLIST;
   FILLINPARAMOFFSETS;
   COMPOUNDST(0,0);
   CURRENT:=CURRENT^.LASTLEVEL; 
   STRUCTDICT:=STRUCTDICT^.LASTLEVEL; 
   DISSYMTAB(STRUCTDICT^.NEXTLEVEL);
   STRUCTDICT^.NEXTLEVEL:=NIL;
   EXITCODE;
 END;
(* FUNCTIONBODY
 *)
 
 
 
 
PROCEDURE PARMLIST;
VAR T1 : EXPTREE;
BEGIN
PLIST:=NIL;T1:=NIL;
TSYMTAB:=NIL;
  IF SY=IDENT
  THEN BEGIN
         NEXTINLIST(PLIST,T1,IDENT,NOOP);
         T1^.SENTRY:=PUTINSYMBOLTABLE(TSYMTAB,ID,AUTOVAR,ORD(INT)); 
         INSYMBOL ;
         WHILE SY=COMMA
         DO BEGIN
              INSYMBOL ;
              IF SY=IDENT
              THEN BEGIN
                     NEXTINLIST(PLIST,T1,IDENT,NOOP);
                     T1^.SENTRY:=PUTINSYMBOLTABLE(TSYMTAB,ID,PARAMVAR,ORD(INT));
                     INSYMBOL ;
                   END;
              IF NOT(SY IN [COMMA,RP])
              THEN ERROR(48);
            END;
       END;
  SKIP([RP]);
END;
(*PARMLIST
 *)
 
 
 PROCEDURE EXTERNALDEF;
 BEGIN
    REACHABLE := FALSE;
    CURRENT:=GLOBALSYMBOL;
    SC:=UNDEFINED;
    TYPENUMBER:=ORD(NULLTYPE);
    INFUNCDEF:=FALSE;
       BEGIN
          WHILE NAMEOFTYPE(TYPENUMBER) OR NAMEOFSC DO ;
          IF SY IN [MULTOP,LP,IDENT]
          THEN BEGIN
                 REPEAT
                   IF SY = COMMA
                   THEN INSYMBOL;
                    IF SC=TYPEVAR
                    THEN DECLTOR(FALSE,STRUCTDICT)
                    ELSE DECLTOR(FALSE,CURRENT);
                    IF (TYPETAB[TYY].BASICTYPE=FUNK)
                    THEN BEGIN
                           IF (SC IN [UNDEFINED,EXTSTATIC]) AND INFUNCDEF
                           THEN BEGIN
                                  PROCHEADER(LASTID);
                                  FUNCID:=LASTID;
                                  FUNCTIONBODY;
                                  IF NOT(SIMPLEFORM)
                                  THEN BEGIN
                                         PRINTSTATIC;
                                         WRITELN(CODE,'          END        ');
                                    END;
                                END;
                         END
                    ELSE BEGIN
                           INFUNCDEF:=FALSE;
                           IF (IDENTNAME<>'          ')AND(SC<>TYPEVAR) THEN
                           DATADEF;
                           IF (SC<>EXTERNVAR) AND NOT(SIMPLEFORM)
                           THEN BEGIN
                                  PRINTSTATIC;
                                  IF REACHABLE
                                  THEN WRITELN(CODE,'          END        ');
                                  REACHABLE := FALSE;
                                END;
                         END;
             UNTIL SY<>COMMA;
           END
          ELSE SKIP([SEMI,TYPESYM,LP,IDENT,MULTOP]);
         END;
    DISSYMTAB(GLOBALSYMBOL^.NEXTLEVEL); 
    GLOBALSYMBOL:=CURRENT;
    GLOBALSYMBOL^.NEXTLEVEL:=NIL; 
  END;
 (* EXTERNALDEF
  *)
 
 
 
 PROCEDURE CPROGRAM;
 BEGIN
   IF SIMPLEFORM
   THEN BEGIN
          WRITE(CODE,'          IDENT ');OUTALFA(CODESEG,6);
          WRITELN(CODE,'.');
          IF ORIGINF
          THEN BEGIN
                 WRITELN(CODE,'          ORG ',ORIGIN:1);
                 ORIGINF:=FALSE;
               END;
        END;
   WHILE NOT(EOF(INPUT))
   DO
     BEGIN
       IF SY=SEMI THEN INSYMBOL ;
       EXTERNALDEF;
       IF SY=SEMI THEN INSYMBOL ;
     END;
  IF SIMPLEFORM
  THEN BEGIN
         WRITELN(CODE,'          END');
         ORIGIN:=DORIGIN;
         ORIGINF:=DORIGINF;
         WRITE(CODE,'          IDENT ');OUTALFA(DATASEG,6);
         WRITELN(CODE,'.');
         IF ORIGINF
         THEN BEGIN
                WRITELN(CODE,'          ORG ',ORIGIN:1);
                ORIGINF:=FALSE;
              END;
         PRINTSTATIC;
         WRITELN(CODE,'          END');
       END;
 END;
(* CPROGRAM
 *)
 
 
 
PROCEDURE INLISE;
(* PROCEDURE TO SETUP ALL THE DATA NEEDED BY
 *            THE PROGRAM
 *)
          CONST       FIRSTCHA = 0;
                      LASTCHA = 127;
 
          VAR C1 : CHARACTER;
              T1 : TYPEMODE;
              III  : INTEGER; 
      BEGIN
(*INLISE
 *)
         NEXTLABELPOS := 1;
         WITH ENDNODE DO
         BEGIN
           NAME := '          ';
           OFFSET := 0;
           THISMODE := 0;
           THISSTATE := UNDEFINED
         END;
 
        NEW(GLOBALSYMBOL);
        WITH GLOBALSYMBOL^
        DO BEGIN
           AUTOSIZE := 0;
           TREE := NIL;
           LASTLEVEL := NIL;
           NEXTLEVEL := NIL;
           END;
         TYPEREF := ORD(FUNK);
         FOR T1:=NULLTYPE TO FIELD
         DO WITH TYPETAB[ORD(T1)]
         DO BEGIN
              BASICTYPE:=T1;
              SIZE:= BASICSIZE(ORD(T1));
            END;
        WITH TYPETAB[ORD(IOTYP)]
        DO BEGIN
             SIZE:=2;
             BASICTYPE:=IOTYP;
           END;
        FOR C1 := FIRSTCHA TO LASTCHA  DO CTABLE[C1] := ' ';
        INFUNCDEF := FALSE;
(* NOT DEFINING A FUNCTION
 *)
        ASSIGNSET := [PLUS,MINUS,TIMES,DIVIDE,MODOP,LEFTSHIFT,RIGHTSHIFT,
                      ANDOP,OROP,XOROP];
        BINOP := [PLUS,MINUS,TIMES,DIVIDE,MODOP,LEFTSHIFT,RIGHTSHIFT,
                  LTOP,LEOP,GTOP,GEOP,EQOP,NEOP,ANDOP,OROP,ANDFOP,
                  ORFOP,XOROP];
        EXPRSET:=[ SEMI,BEGINSYM,TERMOP,ADDOP,MULTOP,IDENT,KONSTANT,
                   GOTOSYM,WHILESYM,DOSYM,FORSYM,IFSYM,BREAK,LP,
                   ANDSYM,COMPLEM,CONTINUE,RETURN,SWITCH,ENDSYM];
        COMMUTOP:=[PLUS,TIMES,ANDOP,OROP,XOROP,EQOP,NEOP,INDEX];
        CTABLE[CHA] := 'A';
        CTABLE[CHB] := 'B';
        CTABLE[CHC] := 'C';
        CTABLE[CHD] := 'D';
        CTABLE[CHE] := 'E';
        CTABLE[CHF] := 'F';
        CTABLE[CHG] := 'G';
        CTABLE[CHH] := 'H';
        CTABLE[CHI] := 'I';
        CTABLE[CHJ] := 'J';
        CTABLE[CHK] := 'K';
        CTABLE[CHL] := 'L';
        CTABLE[CHM] := 'M';
        CTABLE[CHN] := 'N';
        CTABLE[CHO] := 'O';
        CTABLE[CHP] := 'P';
        CTABLE[CHQ] := 'Q';
        CTABLE[CHR] := 'R';
        CTABLE[CHS] := 'S';
        CTABLE[CHT] := 'T';
        CTABLE[CHU] := 'U';
        CTABLE[CHV] := 'V';
        CTABLE[CHW] := 'W';
        CTABLE[CHX] := 'X';
        CTABLE[CHY] := 'Y';
        CTABLE[CHZ] := 'Z';
 
        CTABLE[CH0] := '0';
        CTABLE[CH1] := '1';
        CTABLE[CH2] := '2';
        CTABLE[CH3] := '3';
        CTABLE[CH4] := '4';
        CTABLE[CH5] := '5';
        CTABLE[CH6] := '6';
        CTABLE[CH7] := '7';
        CTABLE[CH8] := '8';
        CTABLE[CH9] := '9';
        FOR C1:= 141B TO 172B
        DO CTABLE[C1]:=CTABLE[C1-40B];
         CTABLE[CHUS]:='Z';
 
 
(* PETE'S EXTRA CODE....................
 *)
          ERRLIST:=NIL;
          CURRENT:=GLOBALSYMBOL;
          LEVEL:=0;
          LASTNODE[0]:=NIL;
          BRACKETS[1]:=NIL;
          BRACKETS[LEVEL]:=NIL;
          UNKNOWN:=NIL;
          LINENO:=0;
          STRINGLIST := NIL ;
          STATICLIST :=NIL;
          STRINGLENG := 0;
 
          NEW(STRUCTDICT);
          NEW(TEMPDICT);
          WITH STRUCTDICT^
          DO BEGIN
               AUTOSIZE:=0;
               TREE:=NIL;
               LASTLEVEL:=NIL;
               NEXTLEVEL:=NIL;
             END;
          WITH TEMPDICT^
          DO BEGIN
               AUTOSIZE:=0;
               TREE:=NIL;
               LASTLEVEL:=NIL;
               NEXTLEVEL:=NIL;
             END;
          LITPOS:=0;
               STATICSIZE:=0;
 
         GLOBALERROR:=FALSE;
         WARNING    :=FALSE;
          EXPERROR:=FALSE;
         WARNLIST:=NIL;
         INITREGS;
         ORIGINF :=FALSE;
         DEBUGF:= FALSE;
         DORIGINF:=FALSE;
         SIMPLEFORM:=FALSE;
(*      FOR III := 0 TO 51 DO BEGIN OPTARR[III].ORECTYP := FALSE; OPTARR[III].OSVAL := '          ';END;*)
      END;
(* INLISE
 *)
 
PROCEDURE PASS2;
BEGIN
(*MAIN
 *)
  LINENO:=0;
 
  INLISE;
  POSITIONOFINPUT := LINESIZE ;
  INPUTLINE.LENGTH := 1;
  RESET(LISTING); REWRITE(LISTING);
  RESET(ERRFILE);REWRITE(ERRFILE);
  RESET(STATFIL);
  REWRITE(STATFIL);
  RESET(CODE);
   REWRITE(CODE);
   RESET(LITFILE);
   REWRITE(LITFILE);
   RESET(DATFILE);REWRITE(DATFILE);
  NEXTCH;
  INSYMBOL ;
  LASTLABEL:=0;
  CURRENTREG:=1;
CPROGRAM;
 IF ERRLIST<>NIL THEN ERROROUT;
  IF WARNING
  THEN WRITELN('WARNINGS IN C COMPILATION');
  IF GLOBALERROR
  THEN BEGIN
         HALT(' ERRORS IN C PROGRAM ');
       END
  ELSE WRITELN('C COMPILATION COMPLETE');
    RESET(CODE);
END;
 
 
BEGIN
  PASS2;
END.
