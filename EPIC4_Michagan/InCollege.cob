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
       SELECT P-FILE ASSIGN TO DYNAMIC W-PROFILE-PATH
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS P-STAT.
       SELECT P-FILE-CUR ASSIGN TO W-PROFILE-PATH-CUR
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT P-TEMP-FILE ASSIGN TO "bin/profiles/te-mp.txt"
           ORGANIZATION IS LINE SEQUENTIAL.


DATA DIVISION.
FILE SECTION.
FD I-FILE.
01 I-REC   PIC X(100).  *> Each line is up to 100 chars

FD O-FILE.
01 O-REC   PIC X(100).

FD U-FILE.
01 U-REC   PIC X(100).

FD P-FILE.
01 P-REC   PIC X(512).

FD  P-FILE-CUR
       DATA RECORD IS P-REC-CUR.
01 P-REC-CUR PIC X(512).

FD P-TEMP-FILE
       RECORD CONTAINS 5000 CHARACTERS
       DATA RECORD IS P-TEMP-REC.
01 P-TEMP-REC PIC X(512).




WORKING-STORAGE SECTION.
01 W-MSG   PIC X(100).
01 W-TMP   PIC X(100).
01 W-RAW   PIC X(100).
01 W-CLEAN PIC X(100).
01 W-USR-INPT PIC X(100).
01 W-USERNAME PIC X(100).
01 W-PASSWORD PIC X(250).

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


*> Profile paths and status
01 W-PROFILE-PATH   PIC X(256).
01 P-STAT           PIC XX.
01 W-USER-LOW       PIC X(32).

*> User profile fields
01 FIRST-NAME       PIC X(30).
01 LAST-NAME        PIC X(30).
01 UNIVERSITY       PIC X(60).
01 MAJOR            PIC X(40).
01 GRAD-YEAR        PIC 9(4).
01 W-YEAR-TEXT      PIC X(4).

01 ABOUT-ME         PIC X(3000).

01 EXP-COUNT        PIC 9     VALUE 0.
01 EXPERIENCE OCCURS 3 TIMES.
   05 EXP-TITLE     PIC X(40).
   05 EXP-COMPANY   PIC X(40).
   05 EXP-DATES     PIC X(40).
   05 EXP-DESC      PIC X(300).

01 EDU-COUNT        PIC 9     VALUE 0.
01 EDUCATION OCCURS 3 TIMES.
   05 EDU-DEGREE    PIC X(40).
   05 EDU-UNIV      PIC X(60).
   05 EDU-YEARS     PIC X(20).

01 CONNECTIONS-LINE PIC X(5000).
01  CONNECTIONS-TABLE.
    05  CONNECTIONS-COUNT       PIC 9(4) VALUE 0.
    05  CONNECTIONS-ENTRY OCCURS 100 TIMES
        INDEXED BY CONN-IDX
        PIC X(50).  *> each username max 50 chars


01 LEN              PIC 9(4) COMP.

*> Additional storage for clean profile viewing
01 VIEW-TEXT         PIC X(3000).
01 VIEW-VAL          PIC X(512).
01 VIEW-LINE         PIC X(512).
01 VIEW-POS          PIC 9(4) COMP VALUE 1.
01 VIEW-LEN          PIC 9(4) COMP VALUE 0.
01 VIEW-CHUNK        PIC 9(4) COMP VALUE 0.
01 VIEW-IDX          PIC 9(2) COMP VALUE 0.
01 CURR-EXP-IDX      PIC 9     VALUE 0.
01 CURR-EDU-IDX      PIC 9     VALUE 0.
01 MODE-FLAG         PIC X VALUE 'N'.
   88 MODE-NONE      VALUE 'N'.
   88 MODE-ABOUT     VALUE 'A'.
   88 MODE-EXP-DESC  VALUE 'D'.
01 IN-BLOCK          PIC X VALUE 'N'.
   88 IN-BEGIN       VALUE 'Y'.
01 W-YEAR-TEXT-VIEW  PIC X(4).
01 W-ACC             PIC X(3000).
01 LINE-IS-TAG       PIC X VALUE 'N'.

*> Generic prompt helpers
01 W-PROMPT          PIC X(250).
01 W-RETRY           PIC X(100).
01 W-OUTPUT          PIC X(300).
01 W-OUTPUT-LONG     PIC X(3000).

*> File list temporary storage
01 FILE-EOF          PIC X VALUE 'N'.
01 FOUND-FILE        PIC X VALUE 'N'.
01 FULL-NAME PIC X(100).
01 SEARCH-NAME PIC X(100).
01 SEARCH-NAME-PATH PIC X(200).

*> Connections Parsing
01 CON-REMAINDER PIC X(1000).
01 CON-USERNAME PIC X(15).
01 CON-P PIC 9(4) VALUE 1.
01 CON-LEN PIC X(50).
01 CON-FOUND PIC X VALUE 'N'.
01 CON-SEARCH-NAME PIC X(50).

*> migration to memory helpers
01 SKIP-CONN-BLOCK     PIC X VALUE 'N'.
01 INSERTED-CONN-BLK   PIC X VALUE 'N'.


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
               PERFORM PRINT-LINE
               MOVE "Please enter your username:" TO W-MSG
               PERFORM DISP-MSG
               PERFORM READ-INPUT-RAW
               PERFORM VALIDATE-USERNAME

               IF VALID-USERNAME
                   MOVE "Please enter your password:" TO W-MSG
                   PERFORM DISP-MSG
                   PERFORM READ-INPUT-RAW
                   PERFORM PRINT-LINE

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

       *> User profile creation
       IF FOUND OR CREATED-OK
           PERFORM INIT-PROFILE-FOR-USER
           PERFORM POST-LOGIN-NAVIGATION
       ELSE
           CLOSE I-FILE U-FILE O-FILE
           PERFORM PROGRAM-END
       END-IF



GO TO PROGRAM-END.

POST-LOGIN-NAVIGATION.
       MOVE "Please select an option:" TO W-MSG PERFORM DISP-MSG
       MOVE "1. Search for a job"      TO W-MSG PERFORM DISP-MSG
       MOVE "2. Find someone you know" TO W-MSG PERFORM DISP-MSG
       MOVE "3. Learn a new skill"     TO W-MSG PERFORM DISP-MSG
       MOVE "4. View my profile"       TO W-MSG PERFORM DISP-MSG
       MOVE "5. Create/Edit Profile"   TO W-MSG PERFORM DISP-MSG
       MOVE "6. Check Connections"   TO W-MSG PERFORM DISP-MSG
       MOVE "7. Return to main menu"   TO W-MSG PERFORM DISP-MSG
       MOVE "Enter choice (1-6):"      TO W-MSG PERFORM DISP-MSG
       PERFORM READ-INPUT

       EVALUATE W-USR-INPT
           WHEN "1"
               MOVE "Job search is under construction." TO W-MSG PERFORM DISP-MSG
               PERFORM POST-LOGIN-NAVIGATION
           WHEN "2"
               PERFORM FIND-NAME
               PERFORM POST-LOGIN-NAVIGATION
           WHEN "3"
               PERFORM LEARN-SKILL
           WHEN "4"
               PERFORM VIEW-PROFILE
               PERFORM POST-LOGIN-NAVIGATION
           WHEN "5"
               PERFORM CREATE-EDIT-PROFILE
               PERFORM POST-LOGIN-NAVIGATION
           WHEN "6"
               PERFORM GET-CONNECTIONS
               PERFORM PARSE-CONNECTIONS
               PERFORM PRINT-CONNECTIONS
               PERFORM POST-LOGIN-NAVIGATION
           WHEN "7"
               MOVE "Returning to main menu..." TO W-MSG PERFORM DISP-MSG
               EXIT
           WHEN OTHER
               MOVE "Invalid selection. Please try again." TO W-MSG PERFORM DISP-MSG
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

           *> Set the CREATED-OK flag to false to indicate failure
           SET NOT-CREATED TO TRUE
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       *> Prompt for a unique username (case-insensitive uniqueness)
       SET USERNAME-TAKEN TO TRUE

       PERFORM UNTIL USERNAME-FREE
           PERFORM PRINT-LINE
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
       PERFORM PRINT-LINE
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

*> USER PROFILE CREATION
INIT-PROFILE-FOR-USER.
       *> Normalize username for filename
       MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(W-USERNAME)) TO W-USER-LOW

       *> Build bin/profiles/<username>.txt
       MOVE SPACES TO W-PROFILE-PATH
       STRING
           "bin/profiles/"                 DELIMITED BY SIZE
           FUNCTION TRIM(W-USER-LOW)       DELIMITED BY SPACE
           ".txt"                          DELIMITED BY SIZE
         INTO W-PROFILE-PATH
       END-STRING

       *> Try to open the file. If it doesn't exist, create a skeleton.
       OPEN INPUT P-FILE
       IF P-STAT = "00"
           CLOSE P-FILE
       ELSE
           PERFORM SAVE-EMPTY-PROFILE
       END-IF
       EXIT.

BUILD-PROFILE-PATH.
       MOVE SPACES TO W-PROFILE-PATH
       STRING
           "bin/profiles/"           DELIMITED BY SIZE
           FUNCTION TRIM(W-USERNAME) DELIMITED BY SPACE   *> avoids trailing spaces
           ".txt"                    DELIMITED BY SIZE
         INTO W-PROFILE-PATH
       END-STRING
       EXIT.


SAVE-EMPTY-PROFILE.
       OPEN OUTPUT P-FILE

       MOVE SPACES TO P-REC
       STRING "USERNAME: " DELIMITED BY SIZE
              W-USER-LOW   DELIMITED BY SPACE
         INTO P-REC
       END-STRING
       WRITE P-REC

       MOVE "[EOF]" TO P-REC
       WRITE P-REC

       CLOSE P-FILE
       EXIT.

CREATE-EDIT-PROFILE.
       PERFORM PRINT-LINE
       MOVE "===== CREATE/EDIT PROFILE =====" TO W-MSG PERFORM DISP-MSG
       PERFORM PRINT-LINE

       *> Required fields (non-blank)
       MOVE "Please enter First Name:"          TO W-PROMPT
       MOVE "First Name is required. Re-enter:" TO W-RETRY
       PERFORM PROMPT-REQUIRED-FIELD
       MOVE W-OUTPUT TO FIRST-NAME

       MOVE "Please enter Last Name:"          TO W-PROMPT
       MOVE "Last Name is required. Re-enter:" TO W-RETRY
       PERFORM PROMPT-REQUIRED-FIELD
       MOVE W-OUTPUT TO LAST-NAME

       MOVE "Please enter University/College Attended:" TO W-PROMPT
       MOVE "University/College is required. Re-enter:" TO W-RETRY
       PERFORM PROMPT-REQUIRED-FIELD
       MOVE W-OUTPUT TO UNIVERSITY

       MOVE "Please enter Major:"          TO W-PROMPT
       MOVE "Major is required. Re-enter:" TO W-RETRY
       PERFORM PROMPT-REQUIRED-FIELD
       MOVE W-OUTPUT TO MAJOR

       *> Grad year: exactly 4 digits between 1900 and 2100
       MOVE "Enter Graduation Year (YYYY):" TO W-MSG PERFORM DISP-MSG
       PERFORM CLEAR-INPUT
       MOVE 0 TO GRAD-YEAR
       PERFORM UNTIL GRAD-YEAR >= 1900 AND GRAD-YEAR <= 2100
           PERFORM READ-INPUT-RAW
           MOVE FUNCTION TRIM(W-USR-INPT) TO W-YEAR-TEXT
           IF FUNCTION LENGTH(W-YEAR-TEXT) = 4 AND W-YEAR-TEXT IS NUMERIC
               MOVE FUNCTION NUMVAL(W-YEAR-TEXT) TO GRAD-YEAR
               IF GRAD-YEAR < 1900 OR GRAD-YEAR > 2100
                   MOVE "Year out of range (1900–2100). Re-enter:" TO W-MSG PERFORM DISP-MSG
               END-IF
           ELSE
               MOVE "Invalid format. Enter 4 digits (e.g., 2025):" TO W-MSG PERFORM DISP-MSG
           END-IF
       END-PERFORM


       *> About Me (optional, finish with END)
       MOVE "About Me (optional). Must be 90 charaters or less:" TO W-PROMPT
       PERFORM CAPTURE-SINGLE-LINE
       MOVE W-OUTPUT-LONG TO ABOUT-ME

       *> Experiences (0..3)
       MOVE 0 TO EXP-COUNT
       MOVE "Add up to 3 experiences. Type YES to add, or DONE to skip/stop."
            TO W-MSG PERFORM DISP-MSG

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           PERFORM UNTIL W-USR-INPT = "yes" OR W-USR-INPT = "done"
               MOVE "Add an experience? Enter YES or DONE:"
                    TO W-MSG PERFORM DISP-MSG
               PERFORM READ-INPUT   *> this already lowercases & trims
               IF W-USR-INPT NOT = "yes" AND W-USR-INPT NOT = "done"
                   MOVE "Invalid input. Please type YES or DONE."
                        TO W-MSG PERFORM DISP-MSG
               END-IF
           END-PERFORM

           IF W-USR-INPT = "done"
               EXIT PERFORM
           END-IF

           ADD 1 TO EXP-COUNT

           MOVE "Title (required):"         TO W-PROMPT
           MOVE "Title required. Re-enter:" TO W-RETRY
           PERFORM PROMPT-REQUIRED-FIELD
           MOVE W-OUTPUT TO EXP-TITLE(EXP-COUNT)

           MOVE "Company/Organization (required):" TO W-PROMPT
           MOVE "Company required. Re-enter:"      TO W-RETRY
           PERFORM PROMPT-REQUIRED-FIELD
           MOVE W-OUTPUT TO EXP-COMPANY(EXP-COUNT)

           MOVE "Dates ('Summer 2024' or 'Jan 2023 - May 2024') (required):" TO W-PROMPT
           MOVE "Dates required. Re-enter:" TO W-RETRY
           PERFORM PROMPT-REQUIRED-FIELD
           MOVE W-OUTPUT TO EXP-DATES(EXP-COUNT)

           MOVE "Short description (optional). Type END to finish description:" TO W-PROMPT
           PERFORM CAPTURE-SINGLE-LINE
           MOVE W-OUTPUT-LONG TO EXP-DESC(EXP-COUNT)
       END-PERFORM

       *> Education
       MOVE 0 TO EDU-COUNT
       MOVE "Add up to 3 education entries. Type YES to add, or DONE to skip/stop."
            TO W-MSG PERFORM DISP-MSG

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           PERFORM UNTIL W-USR-INPT = "yes" OR W-USR-INPT = "done"
               MOVE "Add an education entry? Enter YES or DONE:"
                    TO W-MSG PERFORM DISP-MSG
               PERFORM READ-INPUT   *> lowercased & trimmed
               IF W-USR-INPT NOT = "yes" AND W-USR-INPT NOT = "done"
                   MOVE "Invalid input. Please type YES or DONE."
                        TO W-MSG PERFORM DISP-MSG
               END-IF
           END-PERFORM

           IF W-USR-INPT = "done"
               EXIT PERFORM
           END-IF

           ADD 1 TO EDU-COUNT

           MOVE "Degree (required):"         TO W-PROMPT
           MOVE "Degree required. Re-enter:" TO W-RETRY
           PERFORM PROMPT-REQUIRED-FIELD
           MOVE W-OUTPUT TO EDU-DEGREE(EDU-COUNT)

           MOVE "University/College (required):" TO W-PROMPT
           MOVE "University required. Re-enter:"  TO W-RETRY
           PERFORM PROMPT-REQUIRED-FIELD
           MOVE W-OUTPUT TO EDU-UNIV(EDU-COUNT)

           MOVE "Years Attended (e.g., 2023-2025) (required):" TO W-PROMPT
           MOVE "Years Attended required. Re-enter:"            TO W-RETRY
           PERFORM PROMPT-REQUIRED-FIELD
           MOVE W-OUTPUT TO EDU-YEARS(EDU-COUNT)
       END-PERFORM

       PERFORM PRINT-LINE
       MOVE "===== END CREATE/EDIT PROFILE =====" TO W-MSG PERFORM DISP-MSG
       PERFORM PRINT-LINE

       PERFORM SAVE-PROFILE-TO-FILE

       MOVE "Profile saved successfully." TO W-MSG PERFORM DISP-MSG
       PERFORM PRINT-LINE
       EXIT.

*> Function to prompt for a required filed save profile to a file
SAVE-PROFILE-TO-FILE.
       PERFORM BUILD-PROFILE-PATH
       OPEN OUTPUT P-FILE

       MOVE SPACES TO P-REC
       STRING "USERNAME: " DELIMITED BY SIZE
              W-USER-LOW   DELIMITED BY SPACE
         INTO P-REC
       END-STRING
       WRITE P-REC

       MOVE "[PROFILE]" TO P-REC WRITE P-REC

       MOVE SPACES TO P-REC
       STRING "FIRST-NAME: " DELIMITED BY SIZE
              FUNCTION TRIM(FIRST-NAME) DELIMITED BY SIZE
         INTO P-REC
       END-STRING
       WRITE P-REC

       MOVE SPACES TO P-REC
       STRING "LAST-NAME: " DELIMITED BY SIZE
              FUNCTION TRIM(LAST-NAME) DELIMITED BY SIZE
         INTO P-REC
       END-STRING
       WRITE P-REC

       MOVE SPACES TO P-REC
       STRING "UNIVERSITY: " DELIMITED BY SIZE
              FUNCTION TRIM(UNIVERSITY) DELIMITED BY SIZE
         INTO P-REC
       END-STRING
       WRITE P-REC

       MOVE SPACES TO P-REC
       STRING "MAJOR: " DELIMITED BY SIZE
              FUNCTION TRIM(MAJOR) DELIMITED BY SIZE
         INTO P-REC
       END-STRING
       WRITE P-REC

       MOVE GRAD-YEAR TO W-YEAR-TEXT
       MOVE SPACES TO P-REC
       STRING "GRAD-YEAR: " DELIMITED BY SIZE
              W-YEAR-TEXT   DELIMITED BY SIZE
         INTO P-REC
       END-STRING
       WRITE P-REC

       MOVE "[ABOUT]" TO P-REC WRITE P-REC
       MOVE "BEGIN" TO P-REC WRITE P-REC
       IF FUNCTION LENGTH(FUNCTION TRIM(ABOUT-ME)) > 0
           MOVE ABOUT-ME TO P-REC
       ELSE
           MOVE SPACES TO P-REC
       END-IF
       WRITE P-REC
       MOVE "END" TO P-REC WRITE P-REC
       MOVE "[/ABOUT]" TO P-REC WRITE P-REC

       MOVE "[EXPERIENCES]" TO P-REC WRITE P-REC
       MOVE SPACES TO P-REC
       STRING "COUNT: " DELIMITED BY SIZE
              EXP-COUNT DELIMITED BY SIZE
         INTO P-REC
       END-STRING
       WRITE P-REC

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > EXP-COUNT
           MOVE "[[EXP]]" TO P-REC WRITE P-REC

           MOVE SPACES TO P-REC
           STRING "TITLE: " DELIMITED BY SIZE
                  FUNCTION TRIM(EXP-TITLE(I)) DELIMITED BY SIZE
             INTO P-REC
           END-STRING
           WRITE P-REC

           MOVE SPACES TO P-REC
           STRING "COMPANY: " DELIMITED BY SIZE
                  FUNCTION TRIM(EXP-COMPANY(I)) DELIMITED BY SIZE
             INTO P-REC
           END-STRING
           WRITE P-REC

           MOVE SPACES TO P-REC
           STRING "DATES: " DELIMITED BY SIZE
                  FUNCTION TRIM(EXP-DATES(I)) DELIMITED BY SIZE
             INTO P-REC
           END-STRING
           WRITE P-REC

           MOVE "[DESC]" TO P-REC WRITE P-REC
           MOVE "BEGIN"  TO P-REC WRITE P-REC
           IF FUNCTION LENGTH(FUNCTION TRIM(EXP-DESC(I))) > 0
               MOVE EXP-DESC(I) TO P-REC
               WRITE P-REC
           ELSE
               MOVE SPACES TO P-REC
               WRITE P-REC
           END-IF
           MOVE "END"    TO P-REC WRITE P-REC
           MOVE "[/DESC]" TO P-REC WRITE P-REC

           MOVE "[[/EXP]]" TO P-REC WRITE P-REC
       END-PERFORM

       MOVE "[/EXPERIENCES]" TO P-REC WRITE P-REC

       MOVE "[EDUCATION]" TO P-REC WRITE P-REC
       MOVE SPACES TO P-REC
       STRING "COUNT: " DELIMITED BY SIZE
              EDU-COUNT DELIMITED BY SIZE
         INTO P-REC
       END-STRING
       WRITE P-REC

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > EDU-COUNT
           MOVE "[[EDU]]" TO P-REC WRITE P-REC

           MOVE SPACES TO P-REC
           STRING "DEGREE: " DELIMITED BY SIZE
                  FUNCTION TRIM(EDU-DEGREE(I)) DELIMITED BY SIZE
             INTO P-REC
           END-STRING
           WRITE P-REC

           MOVE SPACES TO P-REC
           STRING "UNIVERSITY: " DELIMITED BY SIZE
                  FUNCTION TRIM(EDU-UNIV(I)) DELIMITED BY SIZE
             INTO P-REC
           END-STRING
           WRITE P-REC

           MOVE SPACES TO P-REC
           STRING "YEARS: " DELIMITED BY SIZE
                  FUNCTION TRIM(EDU-YEARS(I)) DELIMITED BY SIZE
             INTO P-REC
           END-STRING
           WRITE P-REC

           MOVE "[[/EDU]]" TO P-REC WRITE P-REC
       END-PERFORM

       MOVE "[/EDUCATION]" TO P-REC WRITE P-REC
       MOVE "[CONNECTIONS]" TO P-REC WRITE P-REC
       MOVE "CONNECTIONS: " TO P-REC WRITE P-REC
       MOVE "[/CONNECTIONS]" TO P-REC WRITE P-REC
       MOVE "[EOF]"        TO P-REC WRITE P-REC

       CLOSE P-FILE
       EXIT.

*> funtion to view the profile of the user
VIEW-PROFILE.
       PERFORM BUILD-PROFILE-PATH
       OPEN INPUT P-FILE

       IF P-STAT NOT = "00"
           MOVE "No profile found. Create/Edit your profile first." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       PERFORM CLEAR-PROFILE-WS
       PERFORM PARSE-PROFILE-FILE
       CLOSE P-FILE
       PERFORM PRINT-PROFILE-CLEAN
       EXIT.

PROGRAM-END.
    STOP RUN.

CLEAR-INPUT.
       MOVE SPACES TO W-USR-INPT W-TMP W-RAW W-CLEAN
       MOVE 0 TO I J
       EXIT.

*> Prompt for a required single-line field; returns trimmed value in W-OUTPUT
PROMPT-REQUIRED-FIELD.
    *> Display the initial prompt message
    MOVE W-PROMPT TO W-MSG
    PERFORM DISP-MSG

    *> Clear any previous input
    PERFORM CLEAR-INPUT

    *> Loop until the user enters a non-blank value
    PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(W-USR-INPT)) > 0
        *> Read the user's input
        PERFORM READ-INPUT-RAW

        *> If the input is blank, display the retry message
        IF FUNCTION LENGTH(FUNCTION TRIM(W-USR-INPT)) = 0
         MOVE W-RETRY TO W-MSG
         PERFORM DISP-MSG
        END-IF
    END-PERFORM

    *> Store the trimmed input in W-OUTPUT
    MOVE FUNCTION TRIM(W-USR-INPT) TO W-OUTPUT
    EXIT.

*> Capture optional single line
CAPTURE-SINGLE-LINE.
    *> Display the prompt message
    MOVE W-PROMPT TO W-MSG
    PERFORM DISP-MSG

    *> Clear previous input
    MOVE SPACES TO W-USR-INPT
    MOVE SPACES TO W-OUTPUT-LONG

    *> Read just one line of input
    PERFORM READ-INPUT-RAW

    *> Store the input in the output variable
    MOVE W-USR-INPT TO W-OUTPUT-LONG

    EXIT.

    *> Exit the paragraph
    EXIT.

*> Reset profile WS before parsing/printing
CLEAR-PROFILE-WS.
       MOVE SPACES TO FIRST-NAME LAST-NAME UNIVERSITY MAJOR ABOUT-ME
       MOVE 0 TO GRAD-YEAR EXP-COUNT EDU-COUNT CURR-EXP-IDX CURR-EDU-IDX
       MOVE SPACES TO W-YEAR-TEXT-VIEW
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           MOVE SPACES TO EXP-TITLE(I) EXP-COMPANY(I) EXP-DATES(I) EXP-DESC(I)
           MOVE SPACES TO EDU-DEGREE(I) EDU-UNIV(I) EDU-YEARS(I)
       END-PERFORM
       MOVE 'N' TO MODE-FLAG IN-BLOCK
       EXIT.

*> Parse the structured profile text into fields
PARSE-PROFILE-FILE.
       PERFORM UNTIL 1 = 0
           READ P-FILE INTO P-REC
               AT END EXIT PERFORM
               NOT AT END
                   MOVE FUNCTION TRIM(P-REC) TO VIEW-LINE
                   MOVE 'N' TO LINE-IS-TAG

                   *> Section/state handling
                   IF VIEW-LINE = "[ABOUT]"
                       SET MODE-ABOUT TO TRUE
                       MOVE 'N' TO IN-BLOCK
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF
                   IF VIEW-LINE = "[/ABOUT]"
                       SET MODE-NONE TO TRUE
                       MOVE 'N' TO IN-BLOCK
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF

                   IF VIEW-LINE = "[DESC]"
                       SET MODE-EXP-DESC TO TRUE
                       MOVE 'N' TO IN-BLOCK
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF
                   IF VIEW-LINE = "[/DESC]"
                       SET MODE-NONE TO TRUE
                       MOVE 'N' TO IN-BLOCK
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF

                   IF VIEW-LINE = "BEGIN"
                       MOVE 'Y' TO IN-BLOCK
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF
                   IF VIEW-LINE = "END"
                       MOVE 'N' TO IN-BLOCK
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF

                   *> Experience/Education entry starts
                   IF VIEW-LINE = "[[EXP]]"
                       IF EXP-COUNT < 3
                           ADD 1 TO EXP-COUNT
                           MOVE EXP-COUNT TO CURR-EXP-IDX
                       END-IF
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF
                   IF VIEW-LINE = "[[/EXP]]"
                       MOVE 0 TO CURR-EXP-IDX
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF

                   IF VIEW-LINE = "[[EDU]]"
                       IF EDU-COUNT < 3
                           ADD 1 TO EDU-COUNT
                           MOVE EDU-COUNT TO CURR-EDU-IDX
                       END-IF
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF
                   IF VIEW-LINE = "[[/EDU]]"
                       MOVE 0 TO CURR-EDU-IDX
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF

                   *> Handle blocks (About or Experience Description) and key:value only when not a tag line
                   IF LINE-IS-TAG NOT = 'Y'
                       IF MODE-ABOUT AND IN-BEGIN
                           MOVE ABOUT-ME TO W-ACC
                           PERFORM APPEND-FROM-VIEW-LINE
                           MOVE W-ACC TO ABOUT-ME
                       END-IF

                       IF MODE-EXP-DESC AND IN-BEGIN AND CURR-EXP-IDX > 0
                           MOVE EXP-DESC(CURR-EXP-IDX) TO W-ACC
                           PERFORM APPEND-FROM-VIEW-LINE
                           MOVE W-ACC TO EXP-DESC(CURR-EXP-IDX)
                       END-IF

                       *> Key:Value lines (FIRST-NAME, LAST-NAME, etc.)
                       MOVE 0 TO I
                       INSPECT VIEW-LINE TALLYING I FOR CHARACTERS BEFORE INITIAL ":"
                       IF I > 0 AND I < 100
                           *> Extract key and value after colon+space
                           MOVE FUNCTION TRIM(VIEW-LINE(1:I)) TO VIEW-TEXT
                           MOVE FUNCTION TRIM(VIEW-LINE(I + 2:)) TO VIEW-VAL

                           EVALUATE VIEW-TEXT
                               WHEN "FIRST-NAME"
                                   MOVE VIEW-VAL TO FIRST-NAME
                               WHEN "LAST-NAME"
                                   MOVE VIEW-VAL TO LAST-NAME
                               WHEN "UNIVERSITY"
                                   IF CURR-EDU-IDX > 0
                                       MOVE VIEW-VAL TO EDU-UNIV(CURR-EDU-IDX)
                                   ELSE
                                       MOVE VIEW-VAL TO UNIVERSITY
                                   END-IF
                               WHEN "MAJOR"
                                   MOVE VIEW-VAL TO MAJOR
                               WHEN "GRAD-YEAR"
                                   MOVE VIEW-VAL(1:4) TO W-YEAR-TEXT-VIEW
                               WHEN "TITLE"
                                   IF CURR-EXP-IDX > 0
                                       MOVE VIEW-VAL TO EXP-TITLE(CURR-EXP-IDX)
                                   END-IF
                               WHEN "COMPANY"
                                   IF CURR-EXP-IDX > 0
                                       MOVE VIEW-VAL TO EXP-COMPANY(CURR-EXP-IDX)
                                   END-IF
                               WHEN "DATES"
                                   IF CURR-EXP-IDX > 0
                                       MOVE VIEW-VAL TO EXP-DATES(CURR-EXP-IDX)
                                   END-IF
                               WHEN "DEGREE"
                                   IF CURR-EDU-IDX > 0
                                       MOVE VIEW-VAL TO EDU-DEGREE(CURR-EDU-IDX)
                                   END-IF
                               WHEN "YEARS"
                                   IF CURR-EDU-IDX > 0
                                       MOVE VIEW-VAL TO EDU-YEARS(CURR-EDU-IDX)
                                   END-IF
                               WHEN OTHER
                                   CONTINUE
                           END-EVALUATE
                       END-IF
                   END-IF
            END-READ
       END-PERFORM
       EXIT.

*> Print a clean, formatted profile
PRINT-PROFILE-CLEAN.
       *> Print header
       PERFORM PRINT-LINE
       STRING "===== USER PROFILE =====" DELIMITED BY SIZE
          INTO W-MSG
       END-STRING
       PERFORM DISP-MSG
       PERFORM PRINT-LINE
       EXIT.

       *> Print name
       MOVE SPACES TO W-MSG
       STRING "Name: "                DELIMITED BY SIZE
              FUNCTION TRIM(FIRST-NAME) DELIMITED BY SIZE
              " "                    DELIMITED BY SIZE
              FUNCTION TRIM(LAST-NAME)  DELIMITED BY SIZE
         INTO W-MSG
       END-STRING
       PERFORM DISP-MSG

       *> Print university
       MOVE SPACES TO W-MSG
       STRING "University: "           DELIMITED BY SIZE
              FUNCTION TRIM(UNIVERSITY) DELIMITED BY SIZE
         INTO W-MSG
       END-STRING
       PERFORM DISP-MSG

       *> Print major
       MOVE SPACES TO W-MSG
       STRING "Major: "               DELIMITED BY SIZE
              FUNCTION TRIM(MAJOR)     DELIMITED BY SIZE
         INTO W-MSG
       END-STRING
        PERFORM DISP-MSG

       *> Print graduation year
       MOVE SPACES TO W-MSG
       STRING "Graduation Year: "     DELIMITED BY SIZE
              FUNCTION TRIM(W-YEAR-TEXT-VIEW) DELIMITED BY SIZE
         INTO W-MSG
       END-STRING
       PERFORM DISP-MSG

       *> Print "about me"
       PERFORM PRINT-LINE
       IF FUNCTION LENGTH(FUNCTION TRIM(ABOUT-ME)) = 0
           MOVE "About Me: (none)" TO W-MSG
           PERFORM DISP-MSG
       ELSE
           STRING "About Me: " DELIMITED BY SIZE
           FUNCTION TRIM(ABOUT-ME) DELIMITED BY SIZE
           INTO W-MSG
           END-STRING
           PERFORM DISP-MSG
       END-IF

       *> Print "experiences"
       PERFORM PRINT-LINE
       MOVE "Experiences:" TO W-MSG PERFORM DISP-MSG
       IF EXP-COUNT = 0
           PERFORM PRINT-LINE
           MOVE "    (none)" TO W-MSG PERFORM DISP-MSG
       ELSE
           PERFORM VARYING VIEW-IDX FROM 1 BY 1 UNTIL VIEW-IDX > EXP-COUNT
               PERFORM PRINT-LINE

               *> Print title
               MOVE SPACES TO W-MSG
               STRING "    Title: "           DELIMITED BY SIZE
                      FUNCTION TRIM(EXP-TITLE(VIEW-IDX))   DELIMITED BY SIZE
                 INTO W-MSG
               END-STRING
               PERFORM DISP-MSG

               *> Print company
               MOVE SPACES TO W-MSG
               STRING "    Company: "         DELIMITED BY SIZE
                      FUNCTION TRIM(EXP-COMPANY(VIEW-IDX)) DELIMITED BY SIZE
                 INTO W-MSG
               END-STRING
               PERFORM DISP-MSG

               *> Print dates
               MOVE SPACES TO W-MSG
               STRING "    Dates: "           DELIMITED BY SIZE
                      FUNCTION TRIM(EXP-DATES(VIEW-IDX))   DELIMITED BY SIZE
                 INTO W-MSG
               END-STRING
               PERFORM DISP-MSG

               *> Print description
               IF FUNCTION LENGTH(FUNCTION TRIM(EXP-DESC(VIEW-IDX))) = 0
                   MOVE "    Description: (none)" TO W-MSG
                   PERFORM DISP-MSG
               ELSE
                   STRING "    Description: " DELIMITED BY SIZE
                   FUNCTION TRIM(EXP-DESC(VIEW-IDX)) DELIMITED BY SIZE
                       INTO W-MSG
                   END-STRING
                   PERFORM DISP-MSG
               END-IF
           END-PERFORM
       END-IF

       *> Print education
       PERFORM PRINT-LINE
       MOVE "Education:" TO W-MSG PERFORM DISP-MSG
       IF EDU-COUNT = 0
           PERFORM PRINT-LINE
           MOVE "    (none)" TO W-MSG PERFORM DISP-MSG
       ELSE
           PERFORM VARYING VIEW-IDX FROM 1 BY 1 UNTIL VIEW-IDX > EDU-COUNT
               PERFORM PRINT-LINE

               *> Print degree
               MOVE SPACES TO W-MSG
               STRING "    Degree: "          DELIMITED BY SIZE
                      FUNCTION TRIM(EDU-DEGREE(VIEW-IDX))  DELIMITED BY SIZE
                 INTO W-MSG
               END-STRING
               PERFORM DISP-MSG

               *> Print university
               MOVE SPACES TO W-MSG
               STRING "    University: "      DELIMITED BY SIZE
                      FUNCTION TRIM(EDU-UNIV(VIEW-IDX))    DELIMITED BY SIZE
                 INTO W-MSG
               END-STRING
               PERFORM DISP-MSG

               *> Print years
               MOVE SPACES TO W-MSG
               STRING "    Years: "           DELIMITED BY SIZE
                      FUNCTION TRIM(EDU-YEARS(VIEW-IDX))   DELIMITED BY SIZE
                 INTO W-MSG
               END-STRING
               PERFORM DISP-MSG
           END-PERFORM
       END-IF

       *> Print footer
       MOVE SPACES TO W-MSG
       PERFORM DISP-MSG
       STRING "===== END USER PROFILE =====" DELIMITED BY SIZE
          INTO W-MSG
       END-STRING
       PERFORM DISP-MSG
       MOVE SPACES TO W-MSG
       PERFORM DISP-MSG   *> blank line for spacing
       EXIT.

*> Prints an empty line
PRINT-LINE.
       MOVE SPACES TO W-MSG
       PERFORM DISP-MSG
       EXIT.

*> Helper: append trimmed VIEW-LINE to accumulator W-ACC with a space
APPEND-FROM-VIEW-LINE.
       IF FUNCTION LENGTH(FUNCTION TRIM(VIEW-LINE)) > 0
           MOVE FUNCTION TRIM(VIEW-LINE) TO VIEW-VAL
           IF FUNCTION LENGTH(FUNCTION TRIM(W-ACC)) = 0
               MOVE VIEW-VAL TO W-ACC
           ELSE
               STRING FUNCTION TRIM(W-ACC) DELIMITED BY SIZE
                      ' '                DELIMITED BY SIZE
                      VIEW-VAL           DELIMITED BY SIZE
                 INTO W-ACC
               END-STRING
           END-IF
       END-IF
       EXIT.

FIND-NAME.
       MOVE 'N' TO FOUND-FILE
       MOVE "Enter full name to search:" TO W-MSG
       PERFORM DISP-MSG
       PERFORM READ-INPUT
       MOVE FUNCTION TRIM(W-USR-INPT) TO SEARCH-NAME

       IF SEARCH-NAME = SPACES
           MOVE "Invalid input" TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       *> Generate temporary file listing all .txt profiles
       CALL "SYSTEM" USING "ls bin/profiles/*.txt > bin/profiles/file-list.txt"

       MOVE "bin/profiles/file-list.txt" TO W-PROFILE-PATH-CUR

       OPEN INPUT P-FILE-CUR
       MOVE 'N' TO FILE-EOF

       PERFORM UNTIL FILE-EOF = 'Y' OR FOUND-FILE = 'Y'
           READ P-FILE-CUR
               AT END
                   MOVE 'Y' TO FILE-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(P-REC-CUR) TO W-PROFILE-PATH
                   IF W-PROFILE-PATH = "bin/profiles/file-list.txt"
                       CONTINUE
                   ELSE
                       OPEN INPUT P-FILE
                       PERFORM CLEAR-PROFILE-WS
                       PERFORM PARSE-PROFILE-FILE
                       CLOSE P-FILE

                       MOVE SPACES TO FULL-NAME
                       STRING
                           FUNCTION LOWER-CASE(FUNCTION TRIM(FIRST-NAME)) DELIMITED BY SIZE
                           FUNCTION LOWER-CASE(FUNCTION TRIM(LAST-NAME)) DELIMITED BY SIZE
                           INTO FULL-NAME
                       END-STRING

                       IF FULL-NAME = W-USR-INPT
                           MOVE 'Y' TO FOUND-FILE
                           PERFORM PRINT-PROFILE-CLEAN
                           MOVE W-PROFILE-PATH TO SEARCH-NAME-PATH

                           MOVE SEARCH-NAME-PATH(14:) TO W-TMP
                           UNSTRING W-TMP
                               DELIMITED BY ".txt"
                               INTO SEARCH-NAME
                           END-UNSTRING

                       END-IF
                   END-IF

           END-READ
       END-PERFORM

       CLOSE P-FILE
       CLOSE P-FILE-CUR
       CALL "SYSTEM" USING "rm /workspace/bin/profiles/file-list.txt"

       IF SEARCH-NAME = W-USERNAME
           MOVE "Cannot create connection with self, returning to menu." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       IF FOUND-FILE = 'Y'

           MOVE "Send connection request?" TO W-MSG
               PERFORM DISP-MSG
           MOVE "1. Yes" TO W-MSG
               PERFORM DISP-MSG
           MOVE "2. No" TO W-MSG
               PERFORM DISP-MSG

           PERFORM UNTIL W-USR-INPT = '1' OR W-USR-INPT = 'yes'
           PERFORM READ-INPUT

               IF W-USR-INPT = '2' OR W-USR-INPT = 'no'
                   MOVE "Returning to main menu." TO W-MSG
                   PERFORM DISP-MSG
                   EXIT PERFORM
               END-IF

               IF W-USR-INPT = '1' OR W-USR-INPT = 'yes'
                   MOVE "Sending request" TO W-MSG
                   PERFORM DISP-MSG
                   PERFORM ADD-CONNECTION
                   EXIT PERFORM
               END-IF

               MOVE "Invalid response please try again" TO W-MSG
               PERFORM DISP-MSG
           END-PERFORM

       END-IF

       IF FOUND-FILE = 'N'
           MOVE "Nobody by that name could be found." TO W-MSG
               PERFORM DISP-MSG
       ELSE
           MOVE 'N' TO FOUND-FILE
       END-IF

       PERFORM BUILD-PROFILE-PATH

       EXIT.

*> Helper for search. Takes SEARCH-NAME-PATH as input and appends a connection
ADD-CONNECTION.
       MOVE 'N' TO LINE-IS-TAG
       MOVE 'N' TO FILE-EOF

       PERFORM BUILD-PROFILE-PATH

       *> Verify the other user has not sent YOU a request
       MOVE SEARCH-NAME TO CON-SEARCH-NAME

       PERFORM GET-CONNECTIONS
       PERFORM PARSE-CONNECTIONS
       PERFORM COMPARE-CONNECTIONS
       IF CON-FOUND = 'Y'
           MOVE "This user has already sent you a connection request." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       MOVE SEARCH-NAME-PATH TO W-PROFILE-PATH

       *> Verify YOU have not already requested them
       MOVE W-USERNAME TO CON-SEARCH-NAME
       PERFORM GET-CONNECTIONS
       PERFORM PARSE-CONNECTIONS
       PERFORM COMPARE-CONNECTIONS
       IF CON-FOUND = 'Y'
           MOVE "You have already requested a connection." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       *> Now append to the recipient’s file
       MOVE SEARCH-NAME-PATH TO W-PROFILE-PATH

       OPEN INPUT  P-FILE
       OPEN OUTPUT P-TEMP-FILE

       PERFORM UNTIL FILE-EOF = 'Y'
           READ P-FILE INTO P-REC
               AT END
                   MOVE 'Y' TO FILE-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(P-REC) TO VIEW-LINE

                   IF VIEW-LINE(1:12) = "CONNECTIONS:"
                       MOVE 'Y' TO LINE-IS-TAG
                   END-IF

                   IF LINE-IS-TAG = 'Y'

                       *> Append new username to the line
                       STRING FUNCTION TRIM(VIEW-LINE) DELIMITED BY SIZE
                              FUNCTION TRIM(W-USERNAME) DELIMITED BY SIZE
                              ","                        DELIMITED BY SIZE
                              INTO VIEW-LINE
                       END-STRING

                       WRITE P-TEMP-REC FROM VIEW-LINE
                       MOVE 'N' TO LINE-IS-TAG
                   ELSE
                       WRITE P-TEMP-REC FROM P-REC
                   END-IF
           END-READ
       END-PERFORM

       CLOSE P-FILE
       CLOSE P-TEMP-FILE

       *> Replace original with temp
       STRING "mv bin/profiles/te-mp.txt " DELIMITED BY SIZE
           W-PROFILE-PATH                 DELIMITED BY SIZE
           INTO W-TMP
       END-STRING

       CALL "SYSTEM" USING W-TMP

       MOVE "Connection added successfully." TO W-MSG
       PERFORM DISP-MSG
       EXIT.

*> Takes W-PROFILE-PATH as input and outputs a string of names separated by commas CONNECTIONS
GET-CONNECTIONS.
       *> Initialize
       MOVE 'N' TO FILE-EOF
       MOVE 'N' TO LINE-IS-TAG
       MOVE SPACES TO CONNECTIONS-LINE

       OPEN INPUT P-FILE
       PERFORM UNTIL FILE-EOF = 'Y'
           READ P-FILE INTO P-REC
               AT END
                   MOVE 'Y' TO FILE-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(P-REC) TO VIEW-LINE

                   *> Check if the line starts with "CONNECTIONS:"
                   IF FUNCTION TRIM(VIEW-LINE(1:12)) = "CONNECTIONS:"
                       MOVE VIEW-LINE TO CONNECTIONS-LINE
                      MOVE 'Y' TO FILE-EOF
                   END-IF
           END-READ
       END-PERFORM
       CLOSE P-FILE
       MOVE 'N' TO FILE-EOF
       EXIT.

*> Meant to be used after GET-CONNECTIONS, populates table CONNECTIONS. Uses current W-PROFILE-PATH
PARSE-CONNECTIONS.
       *> Initialize
       MOVE 0 TO CONNECTIONS-COUNT

       *> Skip the "Connections:" prefix
       MOVE FUNCTION TRIM(CONNECTIONS-LINE(13:)) TO CON-REMAINDER

       *> Get numeric length of remainder
       MOVE FUNCTION LENGTH(CON-REMAINDER) TO CON-LEN

       MOVE 1 TO CON-P  *> pointer for UNSTRING

       *> Loop through remainder using pointer
       PERFORM UNTIL CON-P > CON-LEN
           UNSTRING CON-REMAINDER
               DELIMITED BY ","
               INTO CON-USERNAME
               WITH POINTER CON-P
           END-UNSTRING

           *> Only store non-blank usernames
           IF FUNCTION TRIM(CON-USERNAME) NOT = SPACES
               ADD 1 TO CONNECTIONS-COUNT
               MOVE FUNCTION TRIM(CON-USERNAME) TO CONNECTIONS-ENTRY(CONNECTIONS-COUNT)
           END-IF
       END-PERFORM

       EXIT.

       PRINT-CONNECTIONS.
       *> Check if there are any connections
       IF CONNECTIONS-COUNT = 0
           MOVE "No connections found" TO W-MSG
           PERFORM DISP-MSG
       ELSE
           MOVE "Connections:" TO W-MSG
           PERFORM DISP-MSG
           PERFORM VARYING CONN-IDX FROM 1 BY 1 UNTIL CONN-IDX > CONNECTIONS-COUNT
               STRING
                   " - " DELIMITED BY SIZE
                   CONNECTIONS-ENTRY(CONN-IDX) DELIMITED BY SIZE
                   INTO W-MSG
               END-STRING
               PERFORM DISP-MSG
               *>DISPLAY " - " CONNECTIONS-ENTRY(CONN-IDX)
           END-PERFORM
       END-IF
       EXIT.

*> Takes CON-SEARCH-NAME and outputs CON-FOUND
COMPARE-CONNECTIONS.
       *> Initialize
       MOVE 'N' TO CON-FOUND

       *> Loop through the connections table
       PERFORM VARYING CON-P FROM 1 BY 1 UNTIL CON-P > CONNECTIONS-COUNT OR CON-FOUND = 'Y'
           IF FUNCTION TRIM(CONNECTIONS-ENTRY(CON-P)) = FUNCTION TRIM(CON-SEARCH-NAME)
               MOVE 'Y' TO CON-FOUND
           END-IF
       END-PERFORM

       *> CON-FOUND = 'Y' if username exists, 'N' otherwise
       EXIT.
