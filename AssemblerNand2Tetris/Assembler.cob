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
01  CharHolder PIC X.
01  LabelName PIC X(40).
01  HackLine.
    02 AorC PIC XXX.
    02 CompSect PIC X(7).
    02 DestSect PIC XXX.
    02 JumpSect PIC XXX.
01  UserDefTable.
    02 UserDefSym OCCURS 500 TIMES PIC X(80).
    02 UserDefBin OCCURS 500 TIMES PIC X(16).
01  UserDefSize PIC 999 VALUE ZEROES.
01  UserDefCounter PIC 999 VALUE 001.
01  CurrentLine PIC X(80).
01  LineIndex PIC 9(4).
01  FirstChar PIC X.
01  SecondChar PIC X.
01  ConvertedNum PIC 9(15).
01  ConvertedBin PIC X(15).
01  AnInteger PIC 9(5) VALUE 32768.
01  ABinaryString PIC X(16).
01  DigitCounter PIC 99.
01  ExponCounter PIC 99.
01  Expon PIC 9(5).
01  ConvDivResult PIC 9(5).
01  NumCount PIC 99.
*>01  OutputData.
*>    02 BinLines OCCURS 1000 TIMES PIC x(80).

*>  Build compDestJumpPredef tables

PROCEDURE DIVISION.
Begin.
*>>>>> First Pass <<<<<
DISPLAY LF
DISPLAY "First Pass:"
DISPLAY LF
*>>>>>>>>>>>>>>>>>>>> Traverse and read the input data file
    OPEN INPUT InputDataFile
    READ InputDataFile
       AT END MOVE HIGH-VALUES TO InputDataTable
    END-READ
    PERFORM UNTIL InputDataTable = HIGH-VALUES
       DISPLAY InputDataTable
       MOVE InputDataTable(1:1) TO FirstChar 
       MOVE InputDataTable(2:1) TO SecondChar 
       DISPLAY FirstChar " - " WITH NO ADVANCING
       IF FirstChar = " "
         DISPLAY "White Space - No action required"
        ELSE 
          IF FirstChar = "/"
            DISPLAY "Comment - No action required"
           ELSE 
             IF FirstChar = "("
               DISPLAY "L-Command"
               *> extract label name from string
               UNSTRING InputDataTable DELIMITED BY "("
                 INTO CharHolder, LabelName
               END-UNSTRING
               UNSTRING LabelName DELIMITED BY ")"
                 INTO LabelName, CharHolder
               END-UNSTRING
               DISPLAY LabelName WITH NO ADVANCING
               DISPLAY " at address " RomAddress WITH NO ADVANCING
               *> convert romaddress to binary
*>>>>>>>>>>>>>>>>>>>> convert a numer to a binary String
               MOVE RomAddress TO AnInteger
               MOVE SPACES TO ABinaryString
               DISPLAY " Converting " AnInteger " " WITH NO ADVANCING
               PERFORM VARYING DigitCounter FROM 15 BY -1 
                       UNTIL DigitCounter = 0
                 MOVE 1 to Expon
                 PERFORM VARYING ExponCounter FROM 0 BY 1
                       UNTIL ExponCounter = DigitCounter - 1 
                   MULTIPLY Expon BY 2 GIVING Expon
                 END-PERFORM
                 DIVIDE Expon INTO AnInteger GIVING ConvDivResult
                 IF ConvDivResult >= 1
                   STRING ABinaryString DELIMITED BY SPACES
                          "1" DELIMITED BY SIZE
                     INTO ABinaryString
                   END-STRING
                   SUBTRACT Expon FROM AnInteger GIVING AnInteger
                  ELSE
                    STRING ABinaryString DELIMITED BY SPACES
                           "0" DELIMITED BY SIZE
                      INTO ABinaryString
                    END-STRING
                 END-IF
               END-PERFORM
               DISPLAY " to binary " WITH NO ADVANCING
               DISPLAY ABinaryString
    *> stick both in userDef table and userdeftablecounter++
               MOVE LabelName to UserDefSym(UserDefCounter)
               MOVE ABinaryString to UserDefBin(UserDefCounter)
               ADD 1 TO UserDefCounter
               ADD 1 TO UserDefSize
              ELSE 
                IF FirstChar = "@"
                  ADD 1 TO RomAddress
                  DISPLAY "A-Command - Incrementing ROM Address to " RomAddress
                 ELSE 
                  ADD 1 TO RomAddress
                   DISPLAY "C-Command - Incrementing ROM Address to " RomAddress
                END-IF
             END-IF
          END-IF
       END-IF 
       DISPLAY LF
       READ InputDataFile
          AT END MOVE HIGH-VALUES TO InputDataTable
       END-READ
    END-PERFORM
    CLOSE InputDataFile

*>01  HackLine.
*>    02 AorC PIC XXX.
*>    02 CompSect PIC X(7).
*>    02 DestSect PIC XXX.
*>    02 JumpSect PIC XXX.
*>01  UserDefTable.
*>    02 UserDefSym OCCURS 500 TIMES PIC X(80).
*>    02 UserDefBin OCCURS 500 TIMES PIC X(16).
*>01  UserDefSize PIC 999 VALUE ZEROES.
*>01  UserDefCounter PIC 999 VALUE 001.

DISPLAY LF
DISPLAY "Second Pass:"
DISPLAY LF
MOVE SPACES TO HackString
   OPEN INPUT InputDataFile
    READ InputDataFile
       AT END MOVE HIGH-VALUES TO InputDataTable
    END-READ
    PERFORM UNTIL InputDataTable = HIGH-VALUES
       DISPLAY InputDataTable
       MOVE InputDataTable(1:1) TO FirstChar 
       MOVE InputDataTable(2:1) TO SecondChar 
       *> test userdef readWrite of UserDefTable
       MOVE FirstChar to UserDefSym(UserDefCounter)
       MOVE SecondChar to UserDefBin(UserDefCounter)
       ADD 1 TO UserDefCounter
       ADD 1 TO UserDefSize
       DISPLAY FirstChar " - " WITH NO ADVANCING
       IF FirstChar = " "
         DISPLAY "White Space"
        ELSE 
          IF FirstChar = "/"
            DISPLAY "Comment"
           ELSE 
             IF FirstChar = "("
               DISPLAY "L-Command"
              ELSE 
                IF FirstChar = "@"
                  DISPLAY "A-Command"
                  ADD 1 TO RomAddress
*> check to see if the second char is a number or not
                  DISPLAY "Second char is " WITH NO ADVANCING
                  DISPLAY SecondChar WITH NO ADVANCING
                  MOVE 0 to NumCount
                  INSPECT SecondChar TALLYING 
                    NumCount FOR ALL "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                  IF NumCount > 0 
                    DISPLAY " - a number"
                    ELSE 
                      DISPLAY " - not a number"
                  END-IF
                 ELSE 
                   DISPLAY "C-Command " WITH NO ADVANCING
*> convert alphanumeric to numeric                   
                   MOVE FirstChar TO ConvertedNum
                   ADD 2 TO ConvertedNum
                   MOVE ConvertedNum to ConvertedBin
                   DISPLAY ConvertedNum WITH NO ADVANCING
                   DISPLAY "<- as AlphuNum or as String -> " WITH NO ADVANCING
                   DISPLAY ConvertedBin
                END-IF
             END-IF
          END-IF
       END-IF 
       READ InputDataFile
          AT END MOVE HIGH-VALUES TO InputDataTable
       END-READ
    END-PERFORM
    DISPLAY LF
    CLOSE InputDataFile

*>>>>>>>>>>>>>>>>>>>> Read UserDefTable
DISPLAY "User Defined Table"
MOVE 1 TO UserDefCounter
PERFORM VARYING UserDefCounter FROM 1 BY 1
UNTIL UserDefCounter = UserDefSize + 1
  DISPLAY UserDefSym(UserDefCounter) WITH NO ADVANCING
  DISPLAY " - " WITH NO ADVANCING 
  DISPLAY UserDefBin(UserDefCounter)
END-PERFORM
DISPLAY LF

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
*>>>>>>>>>>>>>>>>>>>> convert a numer to a binary String
DISPLAY AnInteger WITH NO ADVANCING
PERFORM VARYING DigitCounter FROM 15 BY -1 
        UNTIL DigitCounter = 0
  MOVE 1 to Expon
  PERFORM VARYING ExponCounter FROM 0 BY 1
        UNTIL ExponCounter = DigitCounter 
    MULTIPLY Expon BY 2 GIVING Expon
  END-PERFORM
  DIVIDE Expon INTO AnInteger GIVING ConvDivResult
  IF ConvDivResult >= 1
    STRING ABinaryString DELIMITED BY SPACES
           "1" DELIMITED BY SIZE
      INTO ABinaryString
    END-STRING
    SUBTRACT Expon FROM AnInteger GIVING AnInteger
   ELSE
     STRING ABinaryString DELIMITED BY SPACES
            "0" DELIMITED BY SIZE
       INTO ABinaryString
     END-STRING
  END-IF
END-PERFORM
DISPLAY " equals " WITH NO ADVANCING
DISPLAY ABinaryString
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
    CLOSE OutputFile
    STOP RUN.


*>*>>>>>>>>>>>>>> Second Pass
*> *>>>>>>>>>>>>>>>>>>>> Traverse and read the input data file
*>   OPEN INPUT InputDataFile
*>    READ InputDataFile
*>       AT END MOVE HIGH-VALUES TO InputDataTable
*>    END-READ
*>    PERFORM UNTIL InputDataTable = HIGH-VALUES
*>       DISPLAY InputDataTable
*>       MOVE InputDataTable(1:1) TO FirstChar 
*>       MOVE InputDataTable(2:1) TO SecondChar 
*>       *> test userdef readWrite of UserDefTable
*>       MOVE FirstChar to UserDefSym(UserDefCounter)
*>       MOVE SecondChar to UserDefBin(UserDefCounter)
*>       ADD 1 TO UserDefCounter
*>       ADD 1 TO UserDefSize
*>       DISPLAY FirstChar " - " WITH NO ADVANCING
*>       IF FirstChar = " "
*>         DISPLAY "White Space"
*>        ELSE 
*>          IF FirstChar = "/"
*>            DISPLAY "Comment"
*>           ELSE 
*>             IF FirstChar = "("
*>               DISPLAY "L-Command"
*>              ELSE 
*>                IF FirstChar = "@"
*>                  DISPLAY "A-Command"
*>                  ADD 1 TO RomAddress
*>*> check to see if the second char is a number or not
*>                  DISPLAY "Second char is " WITH NO ADVANCING
*>                  DISPLAY SecondChar WITH NO ADVANCING
*>                  MOVE 0 to NumCount
*>                  INSPECT SecondChar TALLYING 
*>                    NumCount FOR ALL "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
*>                  IF NumCount > 0 
*>                    DISPLAY " - a number"
*>                    ELSE 
*>                      DISPLAY " - not a number"
*>                  END-IF
*>                 ELSE 
*>                   DISPLAY "C-Command " WITH NO ADVANCING
*>*> convert alphanumeric to numeric                   
*>                   MOVE FirstChar TO ConvertedNum
*>                   ADD 2 TO ConvertedNum
*>                   MOVE ConvertedNum to ConvertedBin
*>                   DISPLAY ConvertedNum WITH NO ADVANCING
*>                   DISPLAY "<- as AlphuNum or as String -> " WITH NO ADVANCING
*>                   DISPLAY ConvertedBin
*>                END-IF
*>             END-IF
*>          END-IF
*>       END-IF 
*>       READ InputDataFile
*>          AT END MOVE HIGH-VALUES TO InputDataTable
*>       END-READ
*>    END-PERFORM
*>    DISPLAY LF
*>    CLOSE InputDataFile
