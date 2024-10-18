(*$E+ *)
PROGRAM XXGETL(INPUT/,OUTPUT);
CONST LINESIZE= 300;
TYPE CHARACTER = 0..127;
     LINE = RECORD
              LENGTH : 0..LINESIZE; 
              STRING : ARRAY[1..LINESIZE] OF CHARACTER
            END;

VAR  DIS64TOA : ARRAY [0..63] OF INTEGER; 
     DIS74TOA : ARRAY [0..63] OF INTEGER; 
     DIS76TOA : ARRAY [0..63] OF INTEGER; 
     ATODISTAB: ARRAY [0..127] OF INTEGER;

VALUE 

(* INIT THE ARRAYS *) 

 ATODISTAB = (
(*   *) 4000,
(*   *) 4001,
(*   *) 4002,
(*   *) 4003,
(*   *) 4004,
(*   *) 4005,
(*   *) 4006,
(*   *) 4007,
(*   *) 4008,
(*   *) 4009,
(*   *) 4010,
(*   *) 4011,
(*   *) 4012,
(*   *) 4013,
(*   *) 4014,
(*   *) 4015,
(*   *) 4016,
(*   *) 4017,
(*   *) 4018,
(*   *) 4019,
(*   *) 4020,
(*   *) 4021,
(*   *) 4022,
(*   *) 4023,
(*   *) 4024,
(*   *) 4025,
(*   *) 4026,
(*   *) 4027,
(*   *) 4028,
(*   *) 4029,
(*   *) 4030,
(*   *) 4031,
(*   *) 45,
(* ! *) 54,
(* " *) 52,
(* # *) 48,
(* $ *) 43,
(* % *) 51,
(* & *) 55,
(* ' *) 56,
(* ( *) 41,
(* ) *) 42,
(* * *) 39,
(* + *) 37,
(* , *) 46,
(* - *) 38,
(* . *) 47,
(* / *) 40,
(* 0 *) 27,
(* 1 *) 28,
(* 2 *) 29,
(* 3 *) 30,
(* 4 *) 31,
(* 5 *) 32,
(* 6 *) 33,
(* 7 *) 34,
(* 8 *) 35,
(* 9 *) 36,
(* : *) 3844, 
(* ; *) 63,
(* < *) 58,
(* = *) 44,
(* > *) 59,
(* ? *) 57,
(* @A *) 3841,
(* A *) 1,
(* B *) 2,
(* C *) 3,
(* D *) 4,
(* E *) 5,
(* F *) 6,
(* G *) 7,
(* H *) 8,
(* I *) 9,
(* J *) 10,
(* K *) 11,
(* L *) 12,
(* M *) 13,
(* N *) 14,
(* O *) 15,
(* P *) 16,
(* Q *) 17,
(* R *) 18,
(* S *) 19,
(* T *) 20,
(* U *) 21,
(* V *) 22,
(* W *) 23,
(* X *) 24,
(* Y *) 25,
(* Z *) 26,
(* [ *) 49,
(* \ *) 61,
(* ] *) 50,
(* @B *) 3842,
(* _ *) 53,
(* @G *) 3847,
(* ^A *) 3969,
(* ^B *) 3970,
(* ^C *) 3971,
(* ^D *) 3972,
(* ^E *) 3973,
(* ^F *) 3974,
(* ^G *) 3975,
(* ^H *) 3976,
(* ^I *) 3977,
(* ^J *) 3978,
(* ^K *) 3979,
(* ^L *) 3980,
(* ^M *) 3981,
(* ^N *) 3982,
(* ^O *) 3983,
(* ^P *) 3984,
(* ^Q *) 3985,
(* ^R *) 3986,
(* ^S *) 3987,
(* ^T *) 3988,
(* ^U *) 3989,
(* ^V *) 3990,
(* ^W *) 3991,
(* ^X *) 3992,
(* ^Y *) 3993,
(* ^Z *) 3994,
(* ^0 *) 3995,
(* ^1 *) 3996,
(* ^2 *) 3997,
(* ^3 *) 3998,
(* ^4 *) 3999 
);

(* 64 ^T^A^B^L^E *) 

 DIS64TOA = (
 58,
 65,
 66,
 67,
 68,
 69,
 70,
 71,
 72,
 73,
 74,
 75,
 76,
 77,
 78,
 79,
 80,
 81,
 82,
 83,
 84,
 85,
 86,
 87,
 88,
 89,
 90,
 48,
 49,
 50,
 51,
 52,
 53,
 54,
 55,
 56,
 57,
 43,
 45,
 42,
 47,
 40,
 41,
 36,
 61,
 32,
 44,
 46,
 35,
 91,
 93,
 37,
 34,
 95,
 33,
 38,
 39,
 63,
 60,
 62,
 64,
 92,
 94,
 59 
);

 DIS74TOA = (
 0,
 64,
 94,
 0,
 58,
 0,
 0,
 96,
 13,
 10,
 13,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0,
 0
);

 DIS76TOA = (
 58,
 97,
 98,
 99,
 100,
 101,
 102,
 103,
 104,
 105,
 106,
 107,
 108,
 109,
 110,
 111,
 112,
 113,
 114,
 115,
 116,
 117,
 118,
 119,
 120,
 121,
 122,
 123,
 124,
 125,
 126,
 127,
 0,
 1,
 2,
 3,
 4,
 5,
 6,
 7,
 8,
 9,
 10,
 11,
 12,
 13,
 14,
 15,
 16,
 17,
 18,
 19,
 20,
 21,
 22,
 23,
 24,
 25,
 26,
 27,
 28,
 29,
 30,
 31 
);

PROCEDURE GETLINE( VAR F : TEXT;
                   VAR L : LINE); 
VAR CH:CHAR;
    N,CNT:INTEGER;
    FLAG : BOOLEAN; 
BEGIN 
  WITH L
  DO BEGIN
       LENGTH := 0; 
       FLAG := FALSE; 
       IF EOF(F)
       THEN FLAG := TRUE; 
       WHILE NOT FLAG 
       DO BEGIN 
            IF EOF(F) 
            THEN FLAG := TRUE 
            ELSE IF EOLN(F) 
            THEN FLAG := TRUE 
            ELSE BEGIN
                   READ(F,CH);
                   N := ORD(CH);
                   IF (N = 74B) OR (N = 76B)
                   THEN BEGIN 
                          IF EOF(F) 
                          THEN FLAG := TRUE 
                          ELSE IF EOLN(F) 
                          THEN FLAG := TRUE 
                          ELSE READ(F,CH);
                          IF N = 74B
                          THEN CNT := DIS74TOA[ORD(CH)] 
                          ELSE IF N = 76B 
                          THEN CNT := DIS76TOA[ORD(CH)] 
                        END 
                   ELSE CNT := DIS64TOA[N]; 
                   LENGTH := LENGTH+1;
                   STRING[LENGTH]:=CNT; 

                 END; 
           END; 
     END;    (* WITH *) 

END;      (* GETLINE *) 

PROCEDURE PUTLINE(VAR F : TEXT; 
                      L : LINE);
VAR N : INTEGER;
    CH:CHAR;
    M : INTEGER;
BEGIN 
  WITH L
  DO BEGIN
       N := 1;
       WHILE N <= LENGTH
       DO BEGIN 

            M := ATODISTAB[STRING[N]];
            IF M >= 64
            THEN BEGIN
                   CH := CHR((M DIV 64)MOD 64); 
                   WRITE(F,CH); 
                 END; 
            CH := CHR(M MOD 64);
            WRITE(F,CH);
            N := N+1; 

          END;
     END; 

END;

BEGIN 
END.
