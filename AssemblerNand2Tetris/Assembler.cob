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
	02 CompAsm PIC X(4).
FD DestTableFile.
01 DestTable.
    02 DestBin PIC XXX.
    02 DestAsm PIC X(5).
FD JumpTableFile.
01 JumpTable.
    02 JumpBin PIC XXX.
    02 JumpAsm PIC X(5).
FD PreDefTableFile.
01 PreDefTable.
    02 PreDefBin PIC X(16).
    02 PreDefAsm PIC X(40).
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
01  AddressString PIC X(40).
01  PreDefCompareString PIC X(40).
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
01  UserDefCapture PIC 99.
01  CompHash.
    02 CompBinary OCCURS 28 TIMES PIC X(7).
    02 CompAssembly OCCURS 28 TIMES PIC X(4).
01  CompCounter PIC 999 VALUE 001.
01  TempComp PIC X(7) VALUE SPACES.
01  DestHash.
    02 DestBinary OCCURS 8 TIMES PIC X(4).
    02 DestAssembly OCCURS 8 TIMES PIC X(5).
01  DestCounter PIC 999 VALUE 001.
01  TempDest PIC XXXX VALUE SPACES.
01  JumpHash.
    02 JumpBinary OCCURS 8 TIMES PIC XXX.
    02 JumpAssembly OCCURS 8 TIMES PIC X(5).
01  JumpCounter PIC 999 VALUE 001.
01  TempJump PIC XXXX VALUE SPACES.
01  PreDefHash.
    02 PreDefBinary OCCURS 23 TIMES PIC X(16).
    02 PreDefAssembly OCCURS 23 TIMES PIC X(40).
01  PreDefCounter PIC 999 VALUE 001.
01  PreDefCapture PIC 99.
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
01  LetterCount PIC 99.
*>01  OutputData.
*>    02 BinLines OCCURS 1000 TIMES PIC x(80).

*>  Build compDestJumpPredef tables

PROCEDURE DIVISION.
Begin.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>              >>>>>Read data files into RAM<<<<<
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*> Traverse CompFile and build the comp hash table
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
OPEN INPUT CompTableFile
READ CompTableFile
   AT END MOVE HIGH-VALUES TO CompTable
END-READ
MOVE ZEROES to CompCounter
PERFORM UNTIL CompTable = HIGH-VALUES
   ADD 1 to CompCounter
   MOVE CompAsm to CompAssembly(CompCounter)
   UNSTRING CompAssembly(CompCounter) DELIMITED BY ","
     INTO CharHolder, CompAssembly(CompCounter)
   END-UNSTRING
   MOVE CompBin to CompBinary(CompCounter)
   READ CompTableFile
      AT END MOVE HIGH-VALUES TO CompTable
   END-READ
END-PERFORM
CLOSE CompTableFile
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*> Traverse DestFile and build the dest hash table
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
OPEN INPUT DestTableFile
READ DestTableFile
   AT END MOVE HIGH-VALUES TO DestTable
END-READ
MOVE ZEROES to DestCounter
PERFORM UNTIL DestTable = HIGH-VALUES
   ADD 1 to DestCounter
   MOVE DestAsm to DestAssembly(DestCounter)
   UNSTRING DestAssembly(DestCounter) DELIMITED BY ","
     INTO CharHolder, DestAssembly(DestCounter)
   END-UNSTRING
   MOVE DestBin to DestBinary(DestCounter)
  READ DestTableFile
      AT END MOVE HIGH-VALUES TO DestTable
   END-READ
END-PERFORM
CLOSE DestTableFile
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*> Traverse JumpFile and build the jump hash table
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
OPEN INPUT JumpTableFile
READ JumpTableFile
   AT END MOVE HIGH-VALUES TO JumpTable
END-READ
MOVE ZEROES to JumpCounter
PERFORM UNTIL JumpTable = HIGH-VALUES
   ADD 1 to JumpCounter
   MOVE JumpAsm to JumpAssembly(JumpCounter)
   UNSTRING JumpAssembly(JumpCounter) DELIMITED BY ","
     INTO CharHolder, JumpAssembly(JumpCounter)
   END-UNSTRING
   MOVE JumpBin to JumpBinary(JumpCounter)
   READ JumpTableFile
      AT END MOVE HIGH-VALUES TO JumpTable
   END-READ
END-PERFORM
CLOSE JumpTableFile
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*> Traverse PreDefFile and build the pre-def hash table
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
OPEN INPUT PreDefTableFile
READ PreDefTableFile
   AT END MOVE HIGH-VALUES TO PreDefTable
END-READ
MOVE ZEROES to PreDefCounter
PERFORM UNTIL PreDefTable = HIGH-VALUES
   ADD 1 to PreDefCounter
   UNSTRING PreDefAsm DELIMITED BY ","
     INTO CharHolder, PreDefAsm
   END-UNSTRING
   MOVE PreDefAsm to PreDefAssembly(PreDefCounter)
   UNSTRING PreDefAssembly(PreDefCounter) DELIMITED BY ","
     INTO CharHolder, PreDefAssembly(PreDefCounter)
   END-UNSTRING
   MOVE PreDefBin to PreDefBinary(PreDefCounter)
   READ PreDefTableFile
      AT END MOVE HIGH-VALUES TO PreDefTable
   END-READ
END-PERFORM
CLOSE PreDefTableFile


*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>                           >>>> First Pass <<<<<
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>>>>>>>>>>>>>>>>>>>> Traverse and read the input data file
OPEN INPUT InputDataFile
READ InputDataFile
  AT END MOVE HIGH-VALUES TO InputDataTable
END-READ
PERFORM UNTIL InputDataTable = HIGH-VALUES
  MOVE InputDataTable(1:1) TO FirstChar 
  MOVE InputDataTable(2:1) TO SecondChar 
  IF FirstChar = " "
*>>>Whitespace
    ELSE 
      IF FirstChar = "/"
*>>>>>>>Comment
        ELSE 
*>>>>>>>>>Its a Label
          IF FirstChar = "("
*>>>>>>>>>>>extract label name from string
            UNSTRING InputDataTable DELIMITED BY "("
              INTO CharHolder, LabelName
            END-UNSTRING
            UNSTRING LabelName DELIMITED BY ")"
              INTO LabelName, CharHolder
            END-UNSTRING
*>>>>>>>>>>>convert romaddress to binary
            MOVE RomAddress TO AnInteger
            MOVE SPACES TO ABinaryString
            PERFORM VARYING DigitCounter FROM 15 BY -1 
                    UNTIL DigitCounter = 0
              MOVE 1 to Expon
              PERFORM VARYING ExponCounter FROM 0 BY 1
                      UNTIL ExponCounter = DigitCounter - 1 
                MULTIPLY Expon BY 2 GIVING Expon
              END-PERFORM *>ExponCounter
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
            END-PERFORM *>DigitCounter
*>>>>>>>>>>>put label and binary ROM address into userDef table
            MOVE LabelName to UserDefSym(UserDefCounter)
            MOVE ABinaryString to UserDefBin(UserDefCounter)
            ADD 1 TO UserDefCounter
            ADD 1 TO UserDefSize
            ELSE 
              IF FirstChar = "@"
*>>>>>>>>>>>>>>>A-Command, increase ROM Address by 1
                ADD 1 TO RomAddress
                ELSE 
*>>>>>>>>>>>>>>>C-Command, increase ROM Address by 1
                  ADD 1 TO RomAddress
              END-IF *>FirstChar @
          END-IF *>First char (
      END-IF *>First Char /
  END-IF *>First Char " "
  READ InputDataFile
       AT END MOVE HIGH-VALUES TO InputDataTable
  END-READ
END-PERFORM *>InputDataFile
CLOSE InputDataFile
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>                          >>>> Second Pass <<<<<
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
MOVE SPACES TO HackLine
OPEN INPUT InputDataFile
OPEN OUTPUT OutputFile
DISPLAY "Writing Hack Code to output file."
READ InputDataFile
     AT END MOVE HIGH-VALUES TO InputDataTable
END-READ
PERFORM UNTIL InputDataTable = HIGH-VALUES
  MOVE InputDataTable(1:1) TO FirstChar 
  MOVE InputDataTable(2:1) TO SecondChar 
*>Ignore comments and white space
  IF FirstChar = "/" OR FirstChar = " " OR FirstChar = "("
    ELSE 
      IF FirstChar = "@"
*>>>>>>>A-Command
        UNSTRING InputDataTable DELIMITED BY "@"
          INTO CharHolder, AddressString
        END-UNSTRING
*>>>>>>>Check to see if address is numeric
        MOVE ZEROES TO LetterCount
        INSPECT AddressString TALLYING 
                LetterCount FOR ALL "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
        IF LetterCount = 0 
*>>>>>>>>>A-Command with numerical address
*>>>>>>>>>Convert numerical address to a binary String
          MOVE AddressString TO AnInteger
          MOVE SPACES TO ABinaryString
          PERFORM VARYING DigitCounter FROM 15 BY -1 
                  UNTIL DigitCounter = 0
            MOVE 1 to Expon
            PERFORM VARYING ExponCounter FROM 0 BY 1
                    UNTIL ExponCounter = DigitCounter - 1 
              MULTIPLY Expon BY 2 GIVING Expon
            END-PERFORM *>ExponCounter
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
            END-IF *>ConvDivResult
          END-PERFORM *>DigitCounter
          MOVE ABinaryString TO HackLine
          DISPLAY HackLine
          MOVE HackLine to HackCode
          WRITE Hackcode
          ELSE 
*>>>>>>>>>>>A-command with a non-numerical address (LABEL or SYMBOL)
*>>>>>>>>>>>Check the pre-defined table
            UNSTRING AddressString DELIMITED BY " "
              INTO AddressString, CharHolder
            END-UNSTRING
            MOVE ZEROES to PreDefCapture
            PERFORM VARYING PreDefCounter FROM 1 BY 1
                    UNTIL PreDefCounter = 23 OR PreDefCapture <> 0
              IF AddressString = PreDefAssembly(PreDefCounter)
*>>>>>>>>>>>>>>>A-Command in preDefined Table
                MOVE PreDefCounter to PreDefCapture
              END-IF *>Pre-defined table possibility
            END-PERFORM *>PreDefTable
            IF PreDefCapture <> 0
              MOVE PreDefBinary(PreDefCapture) TO HackLine
              DISPLAY HackLine
              MOVE HackLine to HackCode
              WRITE Hackcode
              ELSE
*>>>>>>>>>>>>>>>Not Pre-defined, check User-Defined Table            
                MOVE 1 TO UserDefCounter
                MOVE ZEROES to UserDefCapture
                PERFORM VARYING UserDefCounter FROM 1 BY 1
                        UNTIL UserDefCounter = UserDefSize + 1 OR UserDefCapture <> 0
                  IF AddressString = UserDefSym(UserDefCounter) 
 *>>>>>>>>>>>>>>>>>>A-Command with address in Userdefined table
                    MOVE UserDefCounter TO UserDefCapture
                  END-IF *>User-defined Symbol possibility
                END-PERFORM *> UserDefCounter
                IF UserDefCapture > 0
                  MOVE UserDefBin(UserDefCounter) TO HackLine
                  STRING "0" DELIMITED BY SIZE
                         UserDefBin(UserDefCapture) DELIMITED BY SPACES
                         INTO HackLine
                  END-STRING
                  DISPLAY Hackline
                  MOVE HackLine to HackCode
                  WRITE Hackcode
                  ELSE 
*>>>>>>>>>>>>>>>>>>>Not PreDef or UserDef, A-Command with new user symbol
*>>>>>>>>>>>>>>>>>>>Add to user def with ram address then increase ram address
*>>>>>>>>>>>>>>>>>>>Convert ramaddress to binary
                    MOVE RamAddress TO AnInteger
                    MOVE SPACES TO ABinaryString
                    PERFORM VARYING DigitCounter FROM 15 BY -1 
                            UNTIL DigitCounter = 0
                      MOVE 1 to Expon
                      PERFORM VARYING ExponCounter FROM 0 BY 1
                              UNTIL ExponCounter = DigitCounter - 1 
                        MULTIPLY Expon BY 2 GIVING Expon
                      END-PERFORM *> ExponCounter
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
                      END-IF *>ConvDivResult 
                    END-PERFORM *> DigitCounter
*>>>>>>>>>>>>>>>>>>>Stick label and binary address into user-defined table
                    MOVE AddressString to UserDefSym(UserDefCounter)
                    MOVE ABinaryString to UserDefBin(UserDefCounter)
                    ADD 1 TO UserDefCounter
                    ADD 1 TO UserDefSize
                    ADD 1 TO RamAddress
                END-IF *>UserDefCapture
            END-IF *>PreDefCapture
        END-IF *>Non-numerical - letterCount
        ELSE
*>>>>>>>>>C-Command  
          MOVE ZEROES TO LetterCount
          INSPECT InputDataTable TALLYING LetterCount FOR ALL "="
          IF LetterCount > 0 
*>>>>>>>>>>>If it contains "=" it a comp, dest, no jump
            UNSTRING InputDataTable DELIMITED BY "="
              INTO TempDest, TempComp
            END-UNSTRING
            PERFORM VARYING CompCounter FROM 1 BY 1
                    UNTIL CompCounter = 29
              IF TempComp = CompAssembly(CompCounter)
                MOVE CompBinary(CompCounter) to TempComp
              END-IF *>Comp table possibility
            END-PERFORM *>CompTable
            PERFORM VARYING DestCounter FROM 1 BY 1
                    UNTIL DestCounter = 9
              IF TempDest = DestAssembly(DestCounter)
                MOVE DestBinary(DestCounter) to TempDest
              END-IF *>Dest table possibility
            END-PERFORM *>DestTable
*>>>>>>>>>>>Combine lookups into one binary string
            STRING "111" DELIMITED BY SIZE
                   TempComp DELIMITED BY SPACES
                   TempDest DELIMITED BY SPACES
                   "000" DELIMITED BY SIZE
                   INTO HackLine
            END-STRING
            ELSE *> if it contains ";"
              MOVE ZEROES TO LetterCount
              INSPECT InputDataTable TALLYING LetterCount FOR ALL ";"
              IF LetterCount > 0 
*>>>>>>>>>>>>>>>If it contains ";" its a comp, no dest, jump
                UNSTRING InputDataTable DELIMITED BY ";"
                  INTO TempComp, TempJump
                END-UNSTRING
                PERFORM VARYING CompCounter FROM 1 BY 1
                        UNTIL CompCounter = 29
                  IF TempComp = CompAssembly(CompCounter)
                    MOVE CompBinary(CompCounter) to TempComp
                  END-IF *>Comp table possibility
                END-PERFORM *>CompTable
                PERFORM VARYING JumpCounter FROM 1 BY 1
                        UNTIL JumpCounter = 9
                  IF TempJump = JumpAssembly(JumpCounter)
                    MOVE JumpBinary(JumpCounter) to TempJump
                  END-IF *>Jump table possibility
                END-PERFORM *>JumpTable
              END-IF
*>>>>>>>>>>>>>Combine lookups into one binary string
              STRING "111" DELIMITED BY SIZE
                     TempComp DELIMITED BY SPACES
                     "000" DELIMITED BY SIZE
                     TempJump DELIMITED BY SPACES
                     INTO HackLine
              END-STRING
          END-IF *>LetterCount for NonJump
          DISPLAY Hackline
          MOVE HackLine to HackCode
          WRITE Hackcode
      END-IF *> IF A Command else C Command 
  END-IF *>Not White space or comment
  READ InputDataFile
       AT END MOVE HIGH-VALUES TO InputDataTable
  END-READ
END-PERFORM *>InputDataFile
CLOSE InputDataFile
CLOSE OutputFile
DISPLAY "Completed."
STOP RUN.
