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
    SELECT InputDataFile ASSIGN TO "Mult.asm"
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT OutputFile ASSIGN TO "Mult.hack"
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
FD InputDataFile. 
01  InputDataTable PIC X(80).
FD OutputFile. 
01  HackCode PIC X(16).
WORKING-STORAGE SECTION.
01  LF PIC X.
01  ROMAddress PIC 9(5) VALUE ZEROS.
01  RAMAddress PIC 9(5) VALUE 16.
01  HackLine.
    02 AorC PIC XXX.
    02 CompSect PIC X(7).
    02 DestSect PIC XXX.
    02 JumpSect PIC XXX.
01  UserDefTable.
    02 UserDefSym OCCURS 500 TIMES PIC X(80).
    02 UserDefBin OCCURS 500 TIMES PIC X(16).
01  CurrentLine PIC X(80).
01  LineIndex PIC 9(4).
*>01  OutputData.
*>    02 BinLines OCCURS 1000 TIMES PIC x(80).

PROCEDURE DIVISION.
Begin.
*>>>>>>>>>>>>>>>>>>>> Traverse and read the comp table
    OPEN INPUT CompTableFile
    READ CompTableFile
       AT END MOVE HIGH-VALUES TO CompTable
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
    CLOSE CompTableFile
*>>>>>>>>>>>>>>>>>>>> Traverse and read the dest table
    OPEN INPUT DestTableFile
    READ DestTableFile
       AT END MOVE HIGH-VALUES TO DestTable
    END-READ
    PERFORM UNTIL DestTable = HIGH-VALUES
       DISPLAY DestAsm SPACE DestBin
       READ DestTableFile
          AT END MOVE HIGH-VALUES TO DestTable
       END-READ
    END-PERFORM
    DISPLAY LF
    CLOSE DestTableFile
*>>>>>>>>>>>>>>>>>>>> Traverse and read the jump table
    OPEN INPUT JumpTableFile
    READ JumpTableFile
       AT END MOVE HIGH-VALUES TO JumpTable
    END-READ
    PERFORM UNTIL JumpTable = HIGH-VALUES
       DISPLAY JumpAsm SPACE JumpBin
       READ JumpTableFile
          AT END MOVE HIGH-VALUES TO JumpTable
       END-READ
    END-PERFORM
    DISPLAY LF
    CLOSE JumpTableFile
*>>>>>>>>>>>>>>>>>>>> Traverse and read the predefined table
    OPEN INPUT PreDefTableFile
    READ PreDefTableFile
       AT END MOVE HIGH-VALUES TO PreDefTable
    END-READ
    PERFORM UNTIL PreDefTable = HIGH-VALUES
       DISPLAY PreDefAsm SPACE PreDefInt
       READ PreDefTableFile
          AT END MOVE HIGH-VALUES TO PreDefTable
       END-READ
    END-PERFORM
    DISPLAY LF
    CLOSE PreDefTableFile
*>>>>>>>>>>>>>>>>>>>> Traverse and read the input data file
    OPEN INPUT InputDataFile
    READ InputDataFile
       AT END MOVE HIGH-VALUES TO InputDataTable
    END-READ
    PERFORM UNTIL InputDataTable = HIGH-VALUES
       DISPLAY InputDataTable
       READ InputDataFile
          AT END MOVE HIGH-VALUES TO InputDataTable
       END-READ
    END-PERFORM
    DISPLAY LF
    Close InputDataFile
*>>>>>>>>>>>>>>>>>>>> create and write the output file
    OPEN OUTPUT OutputFile
    MOVE "1111101011100001" TO HackCode
    WRITE HackCode
    MOVE "0000101101011010" TO HackCode
    WRITE HackCode
    CLOSE OutputFile
*>>>>>>>>>>>>>>>>>>>> Traverse and read the output file
    OPEN INPUT OutputFile
    READ OutputFile
       AT END MOVE HIGH-VALUES TO HackCode
    END-READ
    PERFORM UNTIL HackCode = HIGH-VALUES
       DISPLAY HackCode
       READ OutputFile
          AT END MOVE HIGH-VALUES TO HackCode
       END-READ
    END-PERFORM
    DISPLAY LF
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
