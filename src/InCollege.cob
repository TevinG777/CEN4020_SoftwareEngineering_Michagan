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
       SELECT U-FILE ASSIGN TO "InCollege-Users.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD I-FILE.
01 I-REC   PIC X(100).  *> Each line is up to 25 chars

FD O-FILE.
01 O-REC   PIC X(100).

FD U-FILE.
01 U-REC   PIC X(100).

WORKING-STORAGE SECTION.
01 W-MSG   PIC X(100).
01 W-TMP   PIC X(100).
01 W-RAW   PIC X(100).
01 W-CLEAN PIC X(100).
01 W-USR-INPT PIC X(100).
01 W-USERNAME PIC X(32).
01 W-PASSWORD PIC X(12).
*> Loop counters with storage from 00-99 and stored as binary for fast computations
01 i         pic 9(2) comp.
01 j         pic 9(2) comp.

*> live storage for 5 user accounts with 12 character passwd
*> define a user table
01 USER-COUNT            PIC 9(2) VALUE 0.
01 USER-TABLE.

       *> user account structure user-table->USER-ENTRY->USER-USERNAME & USER-PASSWORD
       05 USER-ENTRY OCCURS 5 TIMES INDEXED BY UX.
          10 USER-USERNAME  PIC X(32).
          10 USER-PASSWORD  PIC X(12).



PROCEDURE DIVISION.
MAIN-SECTION.
       OPEN INPUT I-FILE
           INPUT U-FILE
           OUTPUT O-FILE.

       *> Load users from InCollege-Users.txt into memory
       PERFORM LOAD-USERS.

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
           PERFORM DISP-MSG
       ELSE
           *> Ask for uname and password
           MOVE "Please enter your username:" TO W-MSG
           PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW
           MOVE W-USR-INPT TO W-USERNAME
           MOVE "Please enter your password:" TO W-MSG
           PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW
           MOVE W-USR-INPT TO W-PASSWORD

           *> Validate user credentials
           PERFORM LOG-IN
       END-IF.

       CLOSE I-FILE U-FILE O-FILE.
       STOP RUN.

DISP-MSG.
       DISPLAY W-MSG.
       MOVE W-MSG TO O-REC.
       WRITE O-REC.
       EXIT.
*> Function to grab user input and sanitize it
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

*> Alternate version that does not sanitize input for username and password
READ-INPUT-RAW.
    READ I-FILE INTO W-TMP
        AT END
            MOVE "NO MORE INPUT" TO W-MSG
            PERFORM DISP-MSG
        NOT AT END
            MOVE FUNCTION TRIM(W-TMP) TO W-USR-INPT
    END-READ.
    EXIT.

LOG-IN.
    *> Loop through all the user profiles and see if the credentials match
    PERFORM VARYING UX FROM 1 BY 1 UNTIL UX > USER-COUNT
        *> Check if username and password match
        IF FUNCTION TRIM(USER-USERNAME(UX)) = FUNCTION TRIM(W-USERNAME) AND
        FUNCTION TRIM(USER-PASSWORD(UX)) = FUNCTION TRIM(W-PASSWORD)
         MOVE "Login successful!" TO W-MSG
         PERFORM DISP-MSG
         EXIT PERFORM
        END-IF
    END-PERFORM

    *> Only display incorrect message if no match was found
    IF UX > USER-COUNT
        MOVE "Incorrect username/password, please try again" TO W-MSG
        PERFORM DISP-MSG
    END-IF
    EXIT.

LOAD-USERS.
       MOVE 0 TO USER-COUNT.
       PERFORM UNTIL 1 = 0
           READ U-FILE INTO W-TMP
               AT END
                   EXIT PERFORM
               NOT AT END
                   *> Trim the line and skip blanks
                   MOVE FUNCTION TRIM(W-TMP) TO W-RAW
                   IF W-RAW NOT = SPACES
                       UNSTRING W-RAW DELIMITED BY ":"
                           INTO W-USERNAME W-PASSWORD
                       END-UNSTRING
                       IF USER-COUNT < 5
                           ADD 1 TO USER-COUNT
                           MOVE FUNCTION TRIM(W-USERNAME) TO USER-USERNAME(USER-COUNT)
                           MOVE FUNCTION TRIM(W-PASSWORD) TO USER-PASSWORD(USER-COUNT)
                       END-IF
                   END-IF
           END-READ
       END-PERFORM.
       EXIT.

