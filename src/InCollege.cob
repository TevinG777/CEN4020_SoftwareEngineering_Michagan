*> This is free-form
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       *> read each file line by line
       SELECT I-FILE ASSIGN TO "InCollege-Input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT O-FILE ASSIGN TO "InCollege-Output.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD I-FILE.
01 I-REC   PIC X(100).  *> Each line is up to 25 chars

FD O-FILE.
01 O-REC   PIC X(100).

WORKING-STORAGE SECTION.
01 W-MSG   PIC X(100).
01 W-TMP   PIC X(100).
01 W-RAW   PIC X(100).
01 W-CLEAN PIC X(100).
01 W-USR-INPT PIC X(100).
*> Loop counters with storage from 00-99 and stored as binary for fast computations
01 i         pic 9(2) comp.
01 j         pic 9(2) comp.


PROCEDURE DIVISION.
MAIN-SECTION.
       OPEN INPUT I-FILE
           OUTPUT O-FILE.

       MOVE "Welcome to InCollege!" TO W-MSG.
       PERFORM DISP-MSG.
       MOVE "Log In" TO W-MSG.
       PERFORM DISP-MSG.
       MOVE "Create New Account" TO W-MSG.
       PERFORM DISP-MSG.
       MOVE "Enter your choice:" TO W-MSG.
       PERFORM DISP-MSG.

       *> Read user input for line 1 of the program
       PERFORM READ-INPUT.

       *> Keep prompting the user until a valid selection is made
       PERFORM UNTIL W-USR-INPT = "createnewaccount" OR W-USR-INPT = "login"
           MOVE "Invalid selection. Please try again:" TO W-MSG
           PERFORM DISP-MSG
           *> Check for the next line in the input file
           PERFORM READ-INPUT
       END-PERFORM

       *> Process good input
       IF W-USR-INPT = "createnewaccount"
           MOVE "You selected Create New Account" TO W-MSG
       ELSE
           MOVE "You selected Log In" TO W-MSG
       END-IF
       PERFORM DISP-MSG.


       CLOSE I-FILE O-FILE.
       STOP RUN.

DISP-MSG.
       DISPLAY W-MSG.
       MOVE W-MSG TO O-REC.
       WRITE O-REC.
       EXIT.
READ-INPUT.
       READ I-FILE INTO W-TMP
           AT END
               MOVE "NO MORE INPUT" TO W-MSG
               PERFORM DISP-MSG
           NOT AT END
               *> Need to sanitize user input by removing all spaces and capitals
               MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(W-TMP)) TO W-RAW
               MOVE SPACES TO W-CLEAN
               MOVE 1 TO J

               *> Loop through each character in W-RAW
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF W-RAW
                   *> I:1 views each character at a time and determines if it is a space
                   IF W-RAW(I:1) NOT = SPACE
                       *> If value is not a space then we can add it to the cleaned string
                       MOVE W-RAW(I:1) TO W-CLEAN(J:1)
                       ADD 1 TO J
                   END-IF
               END-PERFORM
               MOVE W-CLEAN TO W-USR-INPT
       END-READ.
