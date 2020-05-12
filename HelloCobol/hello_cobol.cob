      >>SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. MYFirstCobolCode.
AUTHOR. David G. Smith.
*> I don't know what I want this to do yet
DATA DIVISION.
WORKING-STORAGE SECTION.
01  NameIn      PIC X(20).
01  UserPrompt  PIC X(32) VALUE "Type your name then press RETURN".
PROCEDURE DIVISION.
   DISPLAY UserPrompt
   ACCEPT NameIn
   DISPLAY "Great job, ",NameIn
   DISPLAY "You are a rock-star"
   STOP RUN.
