(*$P-,E+,T- 
 *) 
  
(*$L' THE C - COMPILER  P.J.CHURCHYARD. 1982 '
 *) 
PROGRAM C(INPUT/,LISTING,OUTPUT,CODE,ERRFILE);
  
  
(*  EXIT IF END OF FILE CONDITION IS
 *                      PREMETURELY MET 
 *) 
  
  
  CONST      ACTSIZE = 0; 
          NOPREGS = 30; (* NUMBER OF PSUEDO REGISTERS *)
           CHA = 101B;
             CHB = 102B;
             CHC = 103B;
             CHD = 104B;
             CHE = 105B;
             CHF = 106B;
             CHG = 107B;
             CHH = 110B;
             CHI = 111B;
             CHJ = 112B;
             CHK = 113B;
             CHL = 114B;
             CHM = 115B;
             CHN = 116B;
             CHO = 117B;
             CHP = 120B;
             CHQ = 121B;
             CHR = 122B;
             CHS = 123B;
             CHT = 124B;
             CHU = 125B;
             CHV = 126B;
             CHW = 127B;
             CHX = 130B;
             CHY = 131B;
             CHZ = 132B;
  
  
  
  
  
  
  
             SPACE = 40B; 
             EXCLAM =41B; 
             DQUOTE =42B; 
             HASH  = 43B; 
             DOLLAR= 44B; 
             PERCENT=45B; 
(* Q^U^I^C^K ^C^L^U^D^G^E *)
             AMPERSAND=46B; 
             GRAVE = 47B; 
             LPARENTH = 50B;
             RPARENTH = 51B;
             ASTERIX  = 52B;
             PLUSCH =53B; 
             COMMACH=54B; 
             MINUSCH=55B; 
             DOTCH  =56B; 
             SLASH = 57B; 
             CH0   = 60B; 
             CH1   = 61B; 
             CH2   = 62B; 
             CH3   = 63B; 
             CH4   = 64B; 
             CH5   = 65B; 
             CH6   = 66B; 
             CH7   = 67B; 
             CH8   = 70B; 
             CH9   = 71B; 
             COLON = 72B; 
             SEMICOL=73B; 
             CHLT  = 74B; 
             CHEQ  = 75B; 
             CHGT  = 76B; 
             QUESTION=77B;
             CHAT  = 100B;
             CHLB  = 133B;
             CHBS  = 134B;
             CHRB  = 135B;
             CHCAP = 136B;
             CHUS  = 137B;
             CHBQ  = 140B;
             CHLBR = 173B;
             CHBAR = 174B;
             CHRBR = 175B;
             CHTIL = 176B;
             CHDEL = 177B;
             LINEFEED  = 12B; 
             LINESIZE  = 300; 
             STACKSIZE   = 20;
             TYPETABSIZE = 200; 
             NOOFRESERVEDWORDS = 29;
  
  
             AL = 1;
             CL = 2;
             DL = 3;
             BL = 4;
             AH = 5;
             DH = 7;
             BH = 8;
  
             AX = 9;
             CX = 10; 
             DX = 11; 
             BX = 12; 
  
             BP = 14; 
             SI = 15; 
             DI = 16; 
  
             ES = 17; 
             CS = 18; 
             SS = 19; 
             DS = 20; 
  
  
  
  
  
  TYPE
       CHARACTER = 0..127;
       LINE = RECORD
                LENGTH : 0..LINESIZE; 
                STRING : ARRAY[1..LINESIZE] OF CHARACTER
              END;
       ALPHA = PACKED ARRAY[0..13] OF CHARACTER;
             IDTREE  =   ^TREERECORD; 
       RPROPS = (RES,FREE,INDX,R8,R16,DIRTY,PARTIAL); 
       RPSET  = SET OF RPROPS;
      RSTKPTR = ^RSTKREC; 
      RSTKREC = RECORD
                  NEXT1,LAST1 : RSTKPTR;
                  STKOFF : INTEGER; 
                  SPREG  : INTEGER; 
                  SPTYPE : INTEGER; 
                END;
      REGREC = RECORD 
                 RFLAGS : RPSET;
                 PREG : INTEGER;
                 IDPTR : IDTREE;           (* USED TI KEED TRACK OF THE ADDRESSES OF VARS *)
                 IDDRFD: BOOLEAN;          (* HAS CONTENTS OF.... *)
               END; 
      PREC = RECORD 
               REGN : INTEGER;
               SREGN : INTEGER; 
               PTYPE : INTEGER; 
               PSTKP: RSTKPTR;
               COUNT: INTEGER;
             END; 
  
             EXPTREE = ^EXPRECORD;
             IDSTATE =   (UNDEFINED,STATICVAR,AUTOVAR,TYPEVAR,EXTERNVAR,LABELL,STRUCTVAR,EXTSTATIC,FIELDVAR,PARAMVAR,STACKVAR) ;
             ADRSRANGE = 0..1000000;
  
             ERRORCODES= 0..200;
  
             SYMMODE =   (LP,RP,BEGINSYM,ENDSYM,SEMI,TYPESYM,LB,RB, 
                          DOSYM,GOTOSYM,AUTO,BREAK, CONTINUE, 
                          STATIC,EXTERNSYM,DEFAULT,REGISTER,RETURN, 
                          IFSYM,ELSESYM,FORSYM,REPEATSYM,WHILESYM,
                          CASESYM,SWITCH,SPAR,COMMA,ASSIGN,IFOPSYM,ORFSYM,
                          ANDFSYM,ORSYM,COMPLEM,ANDSYM,EQUALITY,COMP, 
                          SHIFTOP,ADDOP,MULTOP,INDEXOP,TERMOP,IDENT,
                          BASIC,STRING,KONSTANT,TYPEDEF,XORSYM,EOSSYM,CAST
                            ,CALL,PARAM,PARAMETER,UNARY,BINARY);
             SYMMODESET  = SET OF SYMMODE;
             OPSYM   =   (SELECTOR,REFSELECTOR,NOTOP,INCR,DECR,NEG, 
                          DEREF,REF,TIMES,DIVIDE,MODOP, DIVI, 
                          PLUS,MINUS,LEFTSHIFT,RIGHTSHIFT,LTOP, 
                          LEOP,GTOP,GEOP,EQOP,NEOP,ANDOP,COMPOP,
                          OROP,ANDFOP,ORFOP,IFOP,ELSEOP,SIZEOF, 
                          INDEX,RETURNOP,COMMAOP, 
                          CONVERT,NOOP,XOROP,ONESCOMP,POSTINC,POSTDEC); 
  
             TYPEMODE = (NULLTYPE,SHORT,KAR,UNSIGNED,INT,LONG,REEL,DOUBLE,
                         POINTER,ARRAYTYPE,IOTYP,EXTERN,FIELD,
                         STRUCT,UNION,FUNK);
             IDRECORD = PACKED RECORD 
                           NAME : ALFA; 
                           OFFSET : INTEGER;
                           THISSTATE : IDSTATE; 
                           THISMODE  : INTEGER; 
                        END;
  
  
  
             TREERECORD = RECORD
                          IR  : IDRECORD; 
                          L,R : IDTREE
                        END;
  
  
             SYMTABPTR=^SYMBOLTABLE;
  
             SYMBOLTABLE = RECORD 
                             AUTOSIZE   : INTEGER;
                             SWITCHLEV  : INTEGER;
                             TREE       : IDTREE; 
                             NEXTLEVEL,LASTLEVEL:SYMTABPTR; 
                           END; 
  
  
  
  
  
             TYPERECORD = RECORD
                             SIZE : INTEGER;
                            CASE BASICTYPE : TYPEMODE OF
  
                            FIELD  : ( FMASK : INTEGER ;
                                       FOFFSET: INTEGER); 
  
                            STRUCT  : ( 
                                        STEMPLATE : EXPTREE); 
  
                            ARRAYTYPE : (INDEXRANGE : INTEGER;
                                         TYPENO   : INTEGER); 
  
                            POINTER   : (TYPEPOINTER : INTEGER);
  
                            FUNK      : ( 
                                         RETRN       : INTEGER);
  
                         NULLTYPE,SHORT,KAR,UNSIGNED,INT,LONG,REEL,DOUBLE,IOTYP,EXTERN, 
                            UNION     : ( 
                                       UTEMPLATE : EXPTREE);
  
                          END;
  
  
             TYPELIST = ^TYPELREC ; 
             TYPELREC = RECORD
                          NXT : TYPELIST ;
                          MDE : TYPEMODE ;
                          SZE : INTEGER;
                        END;
  
  
              SYMSET = SET OF SYMMODE;
  
           OPSET     = SET OF OPSYM;
  
           ASSOC = (LEFTASS,RIGHTASS,NOTASS); 
  
  
          ERRPTR=^ERREC;
           ERREC = RECORD 
                     POSN   : INTEGER;
                     NUM    : ERRORCODES; 
                     NEXT   : ERRPTR; 
                      ELINE  : INTEGER; 
                   END; 
  
             EXPRECORD =  RECORD
                           OPER : OPSYM;
                           LFT,RGHT : EXPTREE;
                           EMODE   : INTEGER ;
                           STKTMP  : INTEGER;    (* STACK OFFSET FOR SAVED REGISTER *)
                           PSREG : INTEGER;  (* PSUEDO REGISTER *)
                           SETCC : BOOLEAN;  (* DID LAST OPER SET THE CONDITION CODES *)
                           COND :  BOOLEAN;  (* ONLY THE FLAGS ARE REQUIRED TO BE SET *)
                           WANTRES:BOOLEAN;  (* WANT RESULT OF EXPRESSION *)
                           CASE TOKN:SYMMODE OF 
                             IDENT : (SENTRY : IDTREE ; 
                                       LID : ALFA;
                                         IDOFSET : INTEGER; 
                                       ); 
                             CALL  : (
                                      PARAMSIZE : INTEGER );
                             PARAMETER : (OFSET : INTEGER );
                             KONSTANT:(CASE KTYPE : TYPEMODE
                                       OF 
                                        NULLTYPE,SHORT,KAR,UNSIGNED,POINTER,
                                        ARRAYTYPE,IOTYP,EXTERN,FIELD,STRUCT,UNION,FUNK, 
                                        INT : (SVALUE : INTEGER); 
                                       LONG : (LVALUE : INTEGER); 
                                  DOUBLE,REEL: (RVALUE : REAL   )); 
                          LP,RP,BEGINSYM,ENDSYM,SEMI,TYPESYM,LB,RB, 
                          DOSYM,GOTOSYM,AUTO,BREAK, CONTINUE, 
                          STATIC,EXTERNSYM,DEFAULT,REGISTER,RETURN, 
                          IFSYM,ELSESYM,FORSYM,REPEATSYM,WHILESYM,
                          CASESYM,SWITCH,SPAR,COMMA,ASSIGN,IFOPSYM,ORFSYM,
                          ANDFSYM,ORSYM,COMPLEM,ANDSYM,EQUALITY,COMP, 
                          SHIFTOP,ADDOP,MULTOP,INDEXOP,TERMOP,
                          BASIC,TYPEDEF,XORSYM,EOSSYM,CAST
                            ,PARAM,UNARY,BINARY,
                              STRING : (STRNG : EXPTREE;
                                        STRNGLENG : INTEGER); 
                         END ;
  
  
             NESTING = 0..20 ;
             OPTIONREC = RECORD 
                           CASE ORECTYP:BOOLEAN 
                           OF TRUE : (OIVAL : INTEGER); 
                             FALSE : (OSVAL : ALFA);
                         END; 
  
  
  VAR 
              NEXTLABELPOS  : INTEGER;
              GLOBALSYMBOL: SYMTABPTR;
              TSYMTAB     : SYMTABPTR;
  
  
              TYPEREF   : INTEGER;
(*  POINTER TO NEXT FREE PLACE IN TYPE TABLE
 *) 
              TYPENUMBER : INTEGER; 
              SC      : IDSTATE ; 
              IDENTNAME : ALFA; 
              TYPETAB   : ARRAY[0..TYPETABSIZE] OF TYPERECORD;
  
(*
 *                                         SYMBOL TABLE TO HOLD THE TYPE INFO 
 *) 
              ASSIGNSET :  OPSET; 
(* THE SET OF ALL OPERATORS WHICH ARE 
 *                                             OF THE FORM 'OP' = 
 *) 
              BINOP     :  OPSET; 
(* THE SET OF BINARY OPERATORS
 *) 
              COMMUTOP  :  OPSET; 
              LOOPSTART :  INTEGER; 
(* WHERE THE LOOP STARTED    -
 *                                             USED IN CODE GENERATION
 *) 
              EXPRSET  :   SYMSET ; 
(* SET OF TOKENS THAT CAN START AN EXPRESSION 
 *) 
  
  
              LOOPEND  :   INTEGER; 
(* WHERE THE LOOPENDS 
 *) 
              INFUNCDEF   : BOOLEAN;
              FUNCID : IDTREE;   (* POINTS TO SYMBOLTABLE FOR CURRENT FUNCTION *) 
(* DEFINING A FUNCTION
 *) 
              RESERVED : ARRAY [1..NOOFRESERVEDWORDS] OF ALFA;
              LENGTHTABLE : ARRAY[0..8] OF 0..NOOFRESERVEDWORDS;
              TYPETABLE   : ARRAY[1..NOOFRESERVEDWORDS] OF SYMMODE; 
  
              IDMODETABLE : ARRAY[1..NOOFRESERVEDWORDS] OF TYPEMODE;
  
               STYP : TYPEMODE; 
               TYY ,TN :INTEGER ; 
              INPUTLINE : LINE; 
              CTABLE : PACKED ARRAY [CHARACTER] OF CHAR;
              POSITIONOFINPUT : INTEGER;
              ENDNODE     : IDRECORD; 
  
              LISTING : TEXT; 
              ERRFILE : TEXT; 
              ASSOCIATIVITY: ARRAY[OPSYM] OF ASSOC; 
  
  
(* PETE'S EXTRA CODE......................
 *) 
              CURRENT : SYMTABPTR;
              POSIVALUE : BOOLEAN ; 
              PLIST     : EXPTREE;
              LASTID    : IDTREE ;
              TYPID     : IDTREE ;
(* POINTER TO SYMBOLTABLE ENTRY OF LAST ID
 *) 
              BRACKETS : ARRAY [ NESTING ] OF EXPTREE ; 
              LEVEL    : NESTING ;
              LASTNODE : ARRAY[NESTING] OF EXPTREE ;
          UNKNOWN : IDTREE ;
          ERRLIST : ERRPTR; 
            WARNLIST  : ERRPTR; 
(* LIST OF ERRORCODES FOR THIS LINE 
 *) 
              VAL : INTEGER ; 
(* VALUE OF CONSTANT EXPRESSION 
 *) 
              LINENO:INTEGER; 
              LASTTYPE: INTEGER;
              STRINGLENG: INTEGER ; 
              STRINGLIST : EXPTREE ;
              STATICLIST : EXPTREE; 
              STATRECORD : EXPRECORD ;
              STATFIL : FILE OF EXPRECORD ; 
              LITFILE : FILE OF EXPRECORD;
              DATFILE : FILE OF EXPRECORD;
              CODE    : TEXT ;
              STRUCTDICT,TEMPDICT : SYMTABPTR;
              GLOBALERROR,EXPERROR : BOOLEAN; 
              WARNING              : BOOLEAN; 
              LITPOS : INTEGER; 
              STATICSIZE : INTEGER; 
(*
 *) 
  
  
(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 * *
 * * VARIABLES USED BY TOKENISER ROUTINES.
 * *
 * *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *) 
  
              VALU : INTEGER; 
              FVALU : REAL; 
              CH : CHARACTER; 
              SY : SYMMODE; 
              OP : OPSYM; 
              IDTYPE : TYPEMODE;
              IDCOUNT : 0..100; 
(* LENGTH OF AN IDENTIFIER
 *) 
              ID     : ALFA;
            IDF    : ALPHA; 
(* NAME OF THE LAST IDENTIFIER READ 
 *) 
          RNAME : ARRAY[0..20]OF ALFA;    (* TEXT NAMES OF THE REGISTERS *) 
          REGSET: ARRAY[0..24]OF REGREC;   (* PROPERTIES FOR THE REGISTERS*)
          HEAD : RSTKPTR; 
(* PSEUDO REGISTERS *)
          PREGSET  : ARRAY[0..NOPREGS]OF PREC;    (* USED TO ALLOCATE REAL REGISTERS *) 
  
  
(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 * *
 * * VARIABLES USED BY CODE GENERATION ROUTINES.
 * *
 * *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
 *) 
  
             LASTLABEL : INTEGER; 
             CURRENTREG: INTEGER; 
             TOPOFSTACK: INTEGER; 
         STACKPTR : INTEGER;
         ORIGIN : INTEGER;
         ORIGINF: BOOLEAN;
          DEBUGF : BOOLEAN; 
         CODESEG: ALFA; 
         DATASEG: ALFA; 
         SIMPLEFORM:BOOLEAN;
         DORIGIN: INTEGER;
         REACHABLE: BOOLEAN;
         DORIGINF:BOOLEAN;
  
(*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 * *
 * * END OF VARIABLES USED BY CODE GENERATION ROUTINES. 
 * *
 * *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
 *) 
  
  
(*       OPTARR : ARRAY [ 0..25 ] OF OPTIONREC;*) 

(*
    VALUE      RESERVED = ('IF        ','DO        ', 
                          'INT       ','FOR       ',
                          'CHAR      ','ELSE      ',
                          'CASE      ','GOTO      ','LONG      ','VOID      ',
                          'AUTO      ','BREAK     ','SHORT     ', 
                          'IOVAR     ','FLOAT     ',
                          'UNION     ','WHILE     ','RETURN    ', 
                          'SWITCH    ','DOUBLE    ',
                          'STRUCT    ','STATIC    ',
                          'EXTERN    ','SIZEOF    ','TYPEDEF   ', 
                          'DEFAULT   ','REGISTER  ','UNSIGNED  ', 
                          'CONTINUE  ');
  
              LENGTHTABLE = (28,0,2,4,11,17,24,26,29);
  
              TYPETABLE   = (IFSYM,DOSYM,TYPESYM,FORSYM,
                             TYPESYM,ELSESYM,CASESYM,GOTOSYM, TYPESYM,TYPESYM, 
                     AUTO,BREAK,TYPESYM,TYPESYM,TYPESYM,TYPESYM,WHILESYM,RETURN,
                             SWITCH,TYPESYM,TYPESYM,STATIC, 
                EXTERNSYM,TERMOP,TYPEDEF,DEFAULT,REGISTER, TYPESYM, 
                             CONTINUE 
                            );
  
            ASSOCIATIVITY = (LEFTASS,LEFTASS,RIGHTASS,
                             RIGHTASS,RIGHTASS,RIGHTASS,
                             RIGHTASS,RIGHTASS,LEFTASS, 
                             LEFTASS,LEFTASS,LEFTASS,LEFTASS, 
                             LEFTASS,LEFTASS,LEFTASS, 
                             NOTASS,NOTASS,NOTASS,NOTASS, 
                             NOTASS,NOTASS,LEFTASS,RIGHTASS,
                             LEFTASS,LEFTASS,LEFTASS,NOTASS,
                             NOTASS,RIGHTASS, 
                             LEFTASS, 
                             LEFTASS,LEFTASS,NOTASS,LEFTASS,LEFTASS,RIGHTASS
                              ,LEFTASS,LEFTASS);
             IDMODETABLE =  (NULLTYPE,NULLTYPE, 
                             INT,NULLTYPE,
                             KAR,NULLTYPE,
                             NULLTYPE,NULLTYPE, LONG, INT,     
                             NULLTYPE,NULLTYPE, SHORT,
                             NULLTYPE,REEL, 
                             UNION,NULLTYPE,NULLTYPE, 
                             NULLTYPE,DOUBLE, 
                             STRUCT,NULLTYPE, 
                             NULLTYPE,NULLTYPE, NULLTYPE, 
                             NULLTYPE,NULLTYPE, UNSIGNED, 
                             NULLTYPE); 
  
RNAME = ('MEMORY    ','AL        ','CL        ','DL        ','BL        ',
         'AH        ','CH        ','DH        ','BH        ','AX        ',
         'CX        ','DX        ','BX        ','SP        ','BP        ',
         'SI        ','DI        ','ES        ','CS        ','SS        ',
         'DS        '); 
*)
  
(* DEFS FOR GETLINE AND PUTLINE *)
PROCEDURE GETLINE(VAR F : TEXT; VAR L : LINE); EXTERN;
PROCEDURE PUTLINE(VAR F : TEXT; L : LINE); EXTERN;

