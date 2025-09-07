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
01 I-REC   PIC X(100).  *> Each line is up to 100 chars

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

*> User creation variables each variable has child boolean flags which are related to the parent
01 LOGIN-OK                PIC X VALUE "N".
       88 FOUND            VALUE "Y".
       88 NOT-FOUND        VALUE "N".
01 USER-FOUND              PIC X VALUE "N".
       88 USERNAME-TAKEN   VALUE "Y".
       88 USERNAME-FREE    VALUE "N".
01 PASS-OK                 PIC X VALUE "N".
       88 VALID-PASS       VALUE "Y".
       88 INVALID-PASS     VALUE "N".
01 HAS-UPPER               PIC X VALUE "N".
       88 OK-UPPER         VALUE "Y".
01 HAS-DIGIT               PIC X VALUE "N".
       88 OK-DIGIT         VALUE "Y".
01 HAS-SPECIAL             PIC X VALUE "N".
       88 OK-SPECIAL       VALUE "Y".
01 PW-LEN                  PIC 99 COMP.


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
           *> if create new account is selected
           PERFORM CREATE-ACCOUNT

           *> Only login if user creation was successful
           IF USER-FOUND = "Y"
               *> Login using most recent saved creds
               MOVE USER-USERNAME(USER-COUNT) TO W-USERNAME
               MOVE USER-PASSWORD(USER-COUNT) TO W-PASSWORD
               PERFORM LOG-IN
           END-IF
       ELSE
           SET NOT-FOUND TO TRUE

           *> Keep asking for username and password until the user gets a right
           PERFORM UNTIL FOUND
               MOVE "Please enter your username:" TO W-MSG
               PERFORM DISP-MSG
               PERFORM READ-INPUT-RAW
               MOVE W-USR-INPT TO W-USERNAME

               MOVE "Please enter your password:" TO W-MSG
               PERFORM DISP-MSG
               PERFORM READ-INPUT-RAW
               MOVE W-USR-INPT TO W-PASSWORD

               PERFORM LOG-IN
           END-PERFORM
       END-IF.
       *> TODO: ADD OTHER INFO FOR SPASH PAGE (DEV 2)









       *> LOGIC TO END PROGRAM AND CLOSE FILES
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
               *> When there is no more user input display that and exit the program
               PERFORM DISP-MSG

               *> Close the input and output files
              CLOSE I-FILE U-FILE O-FILE

              STOP RUN
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
              *> When there is no more user input display that and exit the program
              MOVE "NO MORE INPUT" TO W-MSG
              PERFORM DISP-MSG

              *> Close the input and output files
              CLOSE I-FILE U-FILE O-FILE
              STOP RUN
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
         MOVE "You have successfully logged in." TO W-MSG
         SET FOUND TO TRUE
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

       *> close the USER file
       CLOSE U-FILE.
       EXIT.

CREATE-ACCOUNT.
    *> Account limit check
    IF USER-COUNT >= 5
        MOVE "All permitted accounts have been created, please come back later" TO W-MSG
        PERFORM DISP-MSG
        EXIT PARAGRAPH
    END-IF

    *> Prompt for a unique username (case-insensitive uniqueness)
    SET USERNAME-TAKEN TO TRUE
    PERFORM UNTIL USERNAME-FREE
        MOVE "Please enter a username:" TO W-MSG
        PERFORM DISP-MSG
        PERFORM READ-INPUT-RAW
        MOVE FUNCTION TRIM(W-USR-INPT) TO W-USERNAME
        PERFORM CHECK-USERNAME-UNIQUE
        IF USERNAME-TAKEN
            MOVE "Username already exists. Please choose another." TO W-MSG
            PERFORM DISP-MSG
        END-IF
    END-PERFORM

    *> Prompt until password satisfies all rules
    PERFORM UNTIL VALID-PASS
        MOVE "Please enter a password (8-12 chars, 1 uppercase, 1 digit, 1 special):" TO W-MSG
        PERFORM DISP-MSG
        PERFORM READ-INPUT-RAW
        MOVE FUNCTION TRIM(W-USR-INPT) TO W-PASSWORD
        PERFORM VALIDATE-PASSWORD
        IF INVALID-PASS
            MOVE "Password does not meet requirements. Try again." TO W-MSG
            PERFORM DISP-MSG
        END-IF
    END-PERFORM

    *> Create new user profile in memory
    ADD 1 TO USER-COUNT
    MOVE W-USERNAME TO USER-USERNAME(USER-COUNT)
    MOVE W-PASSWORD TO USER-PASSWORD(USER-COUNT)

    *> Persist to file as "username:password"
    PERFORM APPEND-USER-TO-FILE

    MOVE "Account created successfully!" TO W-MSG
    PERFORM DISP-MSG
    EXIT.


CHECK-USERNAME-UNIQUE.
    MOVE "N" TO USER-FOUND

    *> Loop through all of the users and see if the username is present in the current list of users
    PERFORM VARYING UX FROM 1 BY 1 UNTIL UX > USER-COUNT OR USERNAME-TAKEN

       *> If the entered username matches an existing one the update User-found flag to y
       *> Make sure that similar usernames are not permitted
       IF FUNCTION LOWER-CASE(FUNCTION TRIM(USER-USERNAME(UX)))
          = FUNCTION LOWER-CASE(FUNCTION TRIM(W-USERNAME))
           MOVE "Y" TO USER-FOUND
       END-IF
    END-PERFORM

    *> If the USER-FOUND flag is flipped to yes then USERNAME-TAKEN will flip to true and exit
    IF USERNAME-TAKEN
        CONTINUE
    ELSE
       *> if USER-FOUND is still "N" then the username is unique
        MOVE "N" TO USER-FOUND
    END-IF
    EXIT.

VALIDATE-PASSWORD.
       *> Initialize password requirements as not met
       MOVE "N" TO PASS-OK HAS-UPPER HAS-DIGIT HAS-SPECIAL
       MOVE 0 TO PW-LEN

       *> Compute length up to last non-space, allow a max of 12 characters
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
           IF W-PASSWORD(I:1) NOT = SPACE
               MOVE I TO PW-LEN
           END-IF
       END-PERFORM

       *> Scan characters for required classes
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > PW-LEN
           *> Check to see if any of the characters are uppercase letters
           IF W-PASSWORD(I:1) >= "A" AND W-PASSWORD(I:1) <= "Z"
               MOVE "Y" TO HAS-UPPER
           ELSE
               *> Check to see if any of the characters are digits
               IF W-PASSWORD(I:1) >= "0" AND W-PASSWORD(I:1) <= "9"
                   MOVE "Y" TO HAS-DIGIT
               ELSE
                   *> Check to see if the character is not a digit, capital or lowercase letter, if so then special character
                   IF (W-PASSWORD(I:1) < "0" OR W-PASSWORD(I:1) > "9") AND
                      (W-PASSWORD(I:1) < "A" OR W-PASSWORD(I:1) > "Z") AND
                      (W-PASSWORD(I:1) < "a" OR W-PASSWORD(I:1) > "z")
                       MOVE "Y" TO HAS-SPECIAL
                   END-IF
               END-IF
           END-IF
       END-PERFORM

       *> If the password meets all requirements then we have a good password
       IF PW-LEN >= 8 AND PW-LEN <= 12 AND OK-UPPER AND OK-DIGIT AND OK-SPECIAL
           MOVE "Y" TO PASS-OK
       END-IF
       EXIT.

APPEND-USER-TO-FILE.
       *> Open the user file in extend mode to append user
       OPEN EXTEND U-FILE

       *> In order to stop all the NULL characters from appending move all the spaces to U-REC
       MOVE SPACES TO U-REC
       STRING
           FUNCTION TRIM(W-USERNAME) DELIMITED BY SIZE
           ":"                         DELIMITED BY SIZE
           FUNCTION TRIM(W-PASSWORD)   DELIMITED BY SIZE
         INTO U-REC
       END-STRING
       WRITE U-REC
       CLOSE U-FILE
       EXIT.
