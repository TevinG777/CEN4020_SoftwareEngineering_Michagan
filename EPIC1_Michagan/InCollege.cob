*> This is free-form
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       *> read each file line by line
       SELECT I-FILE ASSIGN TO "bin/InCollege-Input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT O-FILE ASSIGN TO "bin/InCollege-Output.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT U-FILE ASSIGN TO "bin/InCollege-Users.txt"
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

*> Validation variables for username
01 USERNAME-LEN          PIC 9(4) COMP.
01 USERNAME-OK           PIC X VALUE "N".
   88 VALID-USERNAME     VALUE "Y".
   88 INVALID-USERNAME   VALUE "N".


01 W-PASS-CANDIDATE PIC X(100). *> Temp storage for password validation
*> Loop counters with storage from 00-99 and stored as binary for fast computations
01 i         pic 9(2) comp.
01 j         pic 9(2) comp.

*> Bool flag to see if user creation was successful
01 CREATED-FLAG        PIC X VALUE "N".
   88 CREATED-OK       VALUE "Y".
   88 NOT-CREATED      VALUE "N".


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

*> Length of the password after trimming but before truncation giving headroom for validation
01 PW-LEN                  PIC 9(4) COMP.


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

       *> Set the NOT-CREATED flag to true to start
       SET NOT-CREATED TO TRUE

       *> Process good input
       IF W-USR-INPT = "createnewaccount"
           *> if create new account is selected
           PERFORM CREATE-ACCOUNT

           *> Auto-login only if create actually succeeded
           IF CREATED-OK
               MOVE USER-USERNAME(USER-COUNT) TO W-USERNAME
               MOVE USER-PASSWORD(USER-COUNT) TO W-PASSWORD
               PERFORM LOG-IN
               SET NOT-CREATED TO TRUE  *> reset flag so it doesn’t affect later logic
           END-IF

       ELSE
           SET NOT-FOUND TO TRUE

           *> Keep asking for username and password until the user gets it right
           PERFORM UNTIL FOUND
               MOVE "Please enter your username:" TO W-MSG
               PERFORM DISP-MSG
               PERFORM READ-INPUT-RAW
               PERFORM VALIDATE-USERNAME

               IF VALID-USERNAME
                   MOVE "Please enter your password:" TO W-MSG
                   PERFORM DISP-MSG
                   PERFORM READ-INPUT-RAW

                   IF FUNCTION LENGTH(FUNCTION TRIM(W-USR-INPT)) > 12
                       MOVE "Password too long (max 12)." TO W-MSG
                       PERFORM DISP-MSG
                   ELSE
                       MOVE FUNCTION TRIM(W-USR-INPT) TO W-PASSWORD
                       PERFORM LOG-IN
                   END-IF
               ELSE
                   MOVE "Invalid username (no spaces, not blank)." TO W-MSG
                   PERFORM DISP-MSG
           END-IF
END-PERFORM

       END-IF.

       *> If the user login/account creation was successful then navigate to the post-login menu
       IF FOUND
           PERFORM POST-LOGIN-NAVIGATION
       END-IF.
POST-LOGIN-NAVIGATION.
       MOVE "You are now logged in. Please select an option:" TO W-MSG
       PERFORM DISP-MSG
       MOVE "1. Search for a job" TO W-MSG
       PERFORM DISP-MSG
       MOVE "2. Find someone you know" TO W-MSG
       PERFORM DISP-MSG
       MOVE "3. Learn a new skill" TO W-MSG
       PERFORM DISP-MSG
       MOVE "4. Return to main menu" TO W-MSG
       PERFORM DISP-MSG
       MOVE "Enter choice (1-4):" TO W-MSG
       PERFORM DISP-MSG
       PERFORM READ-INPUT

       EVALUATE W-USR-INPT
           WHEN "1"
               MOVE "Job search is under construction." TO W-MSG
               PERFORM DISP-MSG
               PERFORM POST-LOGIN-NAVIGATION
           WHEN "2"
               MOVE "Find someone you know is under construction." TO W-MSG
               PERFORM DISP-MSG
               PERFORM POST-LOGIN-NAVIGATION
           WHEN "3"
               PERFORM LEARN-SKILL
           WHEN "4"
               MOVE "Returning to main menu..." TO W-MSG
               PERFORM DISP-MSG
               EXIT
           WHEN OTHER
               MOVE "Invalid selection. Please try again." TO W-MSG
               PERFORM DISP-MSG
               PERFORM POST-LOGIN-NAVIGATION
       END-EVALUATE
       EXIT.

LEARN-SKILL.
       MOVE "Learn a New Skill - choose one from the list:" TO W-MSG
       PERFORM DISP-MSG
       MOVE "1. Public Speaking" TO W-MSG
       PERFORM DISP-MSG
       MOVE "2. Microsoft Excel" TO W-MSG
       PERFORM DISP-MSG
       MOVE "3. Time Management" TO W-MSG
       PERFORM DISP-MSG
       MOVE "4. Leadership" TO W-MSG
       PERFORM DISP-MSG
       MOVE "5. Coding Fundamentals" TO W-MSG
       PERFORM DISP-MSG
       MOVE "6. Return to previous menu" TO W-MSG
       PERFORM DISP-MSG
       MOVE "Enter choice (1-6):" TO W-MSG
       PERFORM DISP-MSG
       PERFORM READ-INPUT

       EVALUATE W-USR-INPT
           WHEN "1"
               MOVE "This skill page is under construction." TO W-MSG
               PERFORM DISP-MSG
               PERFORM LEARN-SKILL
           WHEN "2"
               MOVE "This skill page is under construction." TO W-MSG
               PERFORM DISP-MSG
               PERFORM LEARN-SKILL
           WHEN "3"
               MOVE "This skill page is under construction." TO W-MSG
               PERFORM DISP-MSG
               PERFORM LEARN-SKILL
           WHEN "4"
               MOVE "This skill page is under construction." TO W-MSG
               PERFORM DISP-MSG
               PERFORM LEARN-SKILL
           WHEN "5"
               MOVE "This skill page is under construction." TO W-MSG
               PERFORM DISP-MSG
               PERFORM LEARN-SKILL
           WHEN "6"
               PERFORM POST-LOGIN-NAVIGATION
           WHEN OTHER
               MOVE "Invalid selection. Please try again." TO W-MSG
               PERFORM DISP-MSG
               PERFORM LEARN-SKILL
       END-EVALUATE
       EXIT.

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
              *> Just trim leading and trailing spaces
              MOVE FUNCTION TRIM(FUNCTION TRIM(W-TMP LEADING) TRAILING) TO W-USR-INPT


       END-READ.
       EXIT.

LOG-IN.
    *> Loop through all the user profiles and see if the credentials match
    PERFORM VARYING UX FROM 1 BY 1 UNTIL UX > USER-COUNT
        *> Check if username and password match
        IF FUNCTION TRIM(USER-USERNAME(UX)) = FUNCTION TRIM(W-USERNAME) AND
        FUNCTION TRIM(USER-PASSWORD(UX)) = FUNCTION TRIM(W-PASSWORD)
         MOVE "You have successfully logged in." TO W-MSG

         MOVE SPACES TO W-MSG
         STRING
             "Welcome, "                 DELIMITED BY SIZE
             FUNCTION TRIM(W-USERNAME)   DELIMITED BY SIZE
             "!"                         DELIMITED BY SIZE
         INTO W-MSG
         END-STRING
         PERFORM DISP-MSG

         SET FOUND TO TRUE
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
                   MOVE FUNCTION TRIM(W-TMP) TO W-RAW
                   IF W-RAW NOT = SPACES
                       *> Find position of first colon
                       MOVE 0 TO I
                       INSPECT W-RAW TALLYING I
                           FOR CHARACTERS BEFORE INITIAL ":"

                       *> Username = left of colon
                       MOVE W-RAW(1:I) TO W-USERNAME

                       *> Password = everything after colon (colons allowed inside)
                       MOVE W-RAW(I + 2 :) TO W-PASSWORD

                       IF USER-COUNT < 5
                           ADD 1 TO USER-COUNT
                           MOVE FUNCTION TRIM(W-USERNAME)
                               TO USER-USERNAME(USER-COUNT)
                           MOVE FUNCTION TRIM(W-PASSWORD)
                               TO USER-PASSWORD(USER-COUNT)
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
           MOVE "Please enter a username (No Space/Special Characters):" TO W-MSG
           PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW

           PERFORM VALIDATE-USERNAME

           IF VALID-USERNAME
               PERFORM CHECK-USERNAME-UNIQUE
               IF USERNAME-TAKEN
                   MOVE "Username already exists. Please choose another." TO W-MSG
                   PERFORM DISP-MSG
               END-IF
           ELSE
               MOVE "Invalid username: No spaces allowed." TO W-MSG
               PERFORM DISP-MSG
               *> do NOT run CHECK-USERNAME-UNIQUE here
           END-IF
       END-PERFORM

       *> Prompt until password satisfies all rules
       PERFORM UNTIL VALID-PASS
           MOVE "Please enter a password (8-12 chars, 1 uppercase, 1 digit, 1 special, no spaces):" TO W-MSG
           PERFORM DISP-MSG

          PERFORM READ-INPUT-RAW
          MOVE FUNCTION TRIM(W-USR-INPT) TO W-PASS-CANDIDATE
          PERFORM VALIDATE-PASSWORD

          IF INVALID-PASS
              MOVE "Password does not meet requirements. Try again." TO W-MSG
              PERFORM DISP-MSG
          ELSE
              *> now it's safe to store (truncate to actual length, max 12)
              MOVE SPACES TO W-PASSWORD
              MOVE W-PASS-CANDIDATE(1:PW-LEN) TO W-PASSWORD
          END-IF
       END-PERFORM

       *> Create new user profile in memory
       ADD 1 TO USER-COUNT
       MOVE W-USERNAME TO USER-USERNAME(USER-COUNT)
       MOVE W-PASSWORD TO USER-PASSWORD(USER-COUNT)

       *> Persist to file as "username:password"
       PERFORM APPEND-USER-TO-FILE

       *> Set the CREATED-OK flag to true to indicate success
       SET CREATED-OK TO TRUE
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

VALIDATE-USERNAME.
    MOVE "N" TO USERNAME-OK

    *> trim leading and trailing spaces
    MOVE FUNCTION TRIM(FUNCTION TRIM(W-USR-INPT LEADING) TRAILING) TO W-USERNAME

    *> true content length, not declared size
    MOVE FUNCTION LENGTH(FUNCTION TRIM(W-USERNAME TRAILING)) TO USERNAME-LEN

    IF USERNAME-LEN = 0
        EXIT PARAGRAPH
    END-IF

    *> reject if any space OR special char exists
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > USERNAME-LEN
        IF W-USERNAME(I:1) = SPACE
            EXIT PARAGRAPH
        END-IF

        *> allow only digits and letters (A-Z, a-z, 0-9)
        IF NOT ( (W-USERNAME(I:1) >= "0" AND W-USERNAME(I:1) <= "9")
              OR (W-USERNAME(I:1) >= "A" AND W-USERNAME(I:1) <= "Z")
              OR (W-USERNAME(I:1) >= "a" AND W-USERNAME(I:1) <= "z") )
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    MOVE "Y" TO USERNAME-OK
    EXIT.

VALIDATE-PASSWORD.
       *> Initialize password requirements as not met
       MOVE "N" TO PASS-OK HAS-UPPER HAS-DIGIT HAS-SPECIAL
       MOVE 0 TO PW-LEN

       *> true length after trimming (no truncation to 12 yet)
       MOVE FUNCTION LENGTH(FUNCTION TRIM(W-PASS-CANDIDATE)) TO PW-LEN


       *> hard reject if out of bounds
       IF PW-LEN < 8 OR PW-LEN > 12
           MOVE "N" TO PASS-OK
           EXIT PARAGRAPH
       END-IF

       *> Scan characters for required classes
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > PW-LEN
           *> Check to see if any of the characters are uppercase letters
           IF W-PASS-CANDIDATE(I:1) >= "A" AND W-PASS-CANDIDATE(I:1) <= "Z"
               MOVE "Y" TO HAS-UPPER
           ELSE
               *> Check to see if any of the characters are digits
               IF W-PASS-CANDIDATE(I:1) >= "0" AND W-PASS-CANDIDATE(I:1) <= "9"
                   MOVE "Y" TO HAS-DIGIT
               ELSE
                   *> Check to see if the character is not a digit, capital or lowercase letter, if so then special character
                   IF (W-PASS-CANDIDATE(I:1) < "0" OR W-PASS-CANDIDATE(I:1) > "9") AND
                      (W-PASS-CANDIDATE(I:1) < "A" OR W-PASS-CANDIDATE(I:1) > "Z") AND
                      (W-PASS-CANDIDATE(I:1) < "a" OR W-PASS-CANDIDATE(I:1) > "z")
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
