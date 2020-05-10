      >>SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. AssemblerNand2Tetris.
AUTHOR. David G Smith.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT CompTableFile ASSIGN TO "CompTable.txt" 
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT DestTableFile ASSIGN TO "DestTable.txt" 
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT JumpTableFile ASSIGN TO "JumpTable.txt" 
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT PreDefTableFile ASSIGN TO "PreDefTable.txt" 
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT DataFile ASSIGN TO "SlidePuzzle.asm"
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT OutputFile ASSIGN TO "SlidePuzzle.hack"
           ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD CompTableFile.
01 CompTable.
	02 CompBin PIC X(7).
	02 CompAsm PIC XXX.
FD DestTableFile.
01 DestTable.
    02 DestBin PIC XXX.
    02 DestAsm PIC XXXX.
FD JumpTableFile.
01 JumpTable.
    02 JumpBin PIC XXX.
    02 JumpAsm PIC XXXX.
FD PreDefTableFile.
01 PreDefTable.
    02 PreDefInt PIC X(16).
    02 PreDefAsm PIC X(6).
*>FD DataFile.  *>This is where the name of the file goes.  Will do user input name later.
*>FD OutputFile. 
WORKING-STORAGE SECTION.
01  LF PIC X.
01  ROMAddress PIC 9(5) VALUE ZEROS.
01  RAMAddress PIC 9(5) VALUE 16.
01  DisplayLine.
    02 AorC PIC XXX.
    02 CompSect PIC X(7).
    02 DestSect PIC XXX.
    02 JumpSect PIC XXX.
01  CompKey PIC X(7).
*>01  UserDefTable.
*>    02 UserDefSym OCCURS 500 TIMES X(80).
*>    02 UserDefBin OCCURS 500 TIMES X(16).
*>01  InputData.
*>    02 Lines OCCURS 1000 TIMES X(80).
01  CurrentLine PIC X(80).
01  LineIndex PIC 9(4).
*>01  OutputData.
*>    02 BinLines OCCURS 1000 TIMES x(80).

PROCEDURE DIVISION.
Begin.
    OPEN INPUT CompTableFile
    READ CompTableFile
       AT END MOVE HIGH-VALUES TO CompTable
    END-READ
    OPEN INPUT DestTableFile
    READ DestTableFile
       AT END MOVE HIGH-VALUES TO DestTable
    END-READ
    OPEN INPUT JumpTableFile
    READ JumpTableFile
       AT END MOVE HIGH-VALUES TO JumpTable
    END-READ
    OPEN INPUT PreDefTableFile
    READ PreDefTableFile
       AT END MOVE HIGH-VALUES TO PreDefTable
    END-READ
    PERFORM UNTIL CompTable = HIGH-VALUES
       DISPLAY CompAsm SPACE CompBin
       READ CompTableFile
          AT END MOVE HIGH-VALUES TO CompTable
       END-READ
     IF CompBin = "0001100" THEN 
       DISPLAY "This one is the one"
     END-IF
    END-PERFORM
    DISPLAY LF
    PERFORM UNTIL DestTable = HIGH-VALUES
       DISPLAY DestAsm SPACE DestBin
       READ DestTableFile
          AT END MOVE HIGH-VALUES TO DestTable
       END-READ
    END-PERFORM
    DISPLAY LF
    PERFORM UNTIL JumpTable = HIGH-VALUES
       DISPLAY JumpAsm SPACE JumpBin
       READ JumpTableFile
          AT END MOVE HIGH-VALUES TO JumpTable
       END-READ
    END-PERFORM
    DISPLAY LF
    PERFORM UNTIL PreDefTable = HIGH-VALUES
       DISPLAY PreDefAsm SPACE PreDefInt
       READ PreDefTableFile
          AT END MOVE HIGH-VALUES TO PreDefTable
       END-READ
    END-PERFORM
    DISPLAY LF
    CLOSE CompTableFile
    CLOSE DestTableFile
    CLOSE JumpTableFile
    CLOSE PreDefTableFile
*> new stuff below
*>   OPEN INPUT CompTableFile
*>   READ CompTableFile
*>     AT END MOVE HIGH-VALUES TO CompTable
*>   END-READ
*>   PERFORM UNTIL CompTable = HIGH-VALUES
*>     IF CompAsm = 'D' THEN 
*>       DISPLAY CompBin
*>     END-IF
*>     READ CompTableFile
*>        AT END MOVE HIGH-VALUES TO CompTable
*>     END-READ
*>   END-PERFORM
*>   CLOSE CompTableFile
*>    MOVE '0001100' TO CompKey
*>    READ CompTableFile CompBin IS CompKey
*>        INVALID DISPLAY 'No such record'
*>        NOT INVALID DISPLAY CompAsm
*>    END-READ 

*>    MOVE "1111" to AorC
*>    MOVE CompBin (1) to CompSect
*>    MOVE DestBin (1) to DestSect
*>    MOVE JumpBin (1) to JumpSect 
*>    DISPLAY DisplayLine

    STOP RUN.
*>  Build compDestJumpPredef tables
*>  first pass - if array has more lines advance
*>    ignore comments and white toclassify
*>    A or C - romAddress++
*>    L - put in userdef table with romAddress binary
*>  reset to front of array
*>  second pass- if array has more lines advance
*>    ignore cooments and white space to classify
*>      A Cmd - remove @
*>        If numeric convert to binary and write to file
*>        else if predef lookup binary and write to file
*>        else if userdef lookup binary and write to file
*>        else if new, add to userdef with ramAddress then ramAddress++
*>      C Cmd
*>        if contains "=" lookup comp and concat with lookup dest
*>        else if xontains ";" lookup comp and concat with lookup jump
*>        concat 111 to front of string
*>        write to file
*>    close file
