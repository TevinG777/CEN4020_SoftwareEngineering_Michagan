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

       SELECT EC-FILE ASSIGN TO "bin/established-connections.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT JOB-FILE ASSIGN TO "bin/InCollege_jobListings.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS JOB-STAT.
       SELECT APP-FILE ASSIGN TO "bin/InCollege_jobApplications.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS APP-STAT.


DATA DIVISION.
FILE SECTION.
FD I-FILE.
01 I-REC   PIC X(256).  *> Raised to support 200-char inputs

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

FD EC-FILE.
01 EC-REC PIC X(120).

FD JOB-FILE.
01 JOB-REC PIC X(512).





FD APP-FILE.
01 APP-REC PIC X(512).

WORKING-STORAGE SECTION.
01 W-MSG   PIC X(100).
01 W-TMP   PIC X(256).
01 W-RAW   PIC X(256).
01 W-CLEAN PIC X(256).
01 W-USR-INPT PIC X(256).
01 W-USERNAME PIC X(100).
01 W-PASSWORD PIC X(250).

*> Validation variables for username
01 USERNAME-LEN          PIC 9(4) COMP.
01 USERNAME-OK           PIC X VALUE "N".
   88 VALID-USERNAME     VALUE "Y".
   88 INVALID-USERNAME   VALUE "N".


01 W-PASS-CANDIDATE PIC X(100). *> Temp storage for password validation
*> Loop counters with storage from 00-99 and stored as binary for fast computations
01 i         PIC 9(4) COMP.
01 j         PIC 9(4) COMP.

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
01 JOB-STAT         PIC XX.
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

*> ---- Established connections helpers ----
01 EC-LINE        PIC X(120).
01 EC-U1          PIC X(50).
01 EC-U2          PIC X(50).
01 EC-OTHER       PIC X(50).
01 EC-PAIR        PIC X(120).
01 EC-EXISTS      PIC X VALUE 'N'.
01 EC-COUNT       PIC 9(4) VALUE 0.

*> ---- Removal / rewrite helpers ----
01 RQ-NAME        PIC X(50).      *> pending requester being processed
01 NEW-CONN-LINE  PIC X(5000).
01 ANY-WRITTEN    PIC X VALUE 'N'.

*> ---- Fetch other user's summary (for network print) ----
01 OTHER-PATH     PIC X(256).
01 SAVE-FIRST     PIC X(30).
01 SAVE-LAST      PIC X(30).
01 SAVE-UNIV      PIC X(60).
01 SAVE-MAJOR     PIC X(40).
01 SAVE-YEAR      PIC X(4).

*> Job posting storage
01 JOB-COUNT        PIC 9(4) COMP VALUE 0.
01 JOB-NEXT-ID      PIC 9(4) COMP VALUE 0.
01 JOB-ID-TEXT      PIC Z(5).
01 JOB-TITLE        PIC X(100).
01 JOB-DESCRIPTION  PIC X(200).
01 JOB-EMPLOYER     PIC X(100).
01 JOB-LOCATION     PIC X(100).
01 JOB-SALARY       PIC X(60).
01 JOB-LINE         PIC X(512).
01 JOB-EOF          PIC X VALUE 'N'.
01 JOB-PIPE-COUNT   PIC 9(4) COMP.

01 MAX-JOB-ENTRIES  PIC 9(4) COMP VALUE 100.
01 JOB-LIST.
       05 JOB-INFO OCCURS 100 TIMES.
          10 JOB-INFO-ID        PIC X(5).
          10 JOB-INFO-TITLE     PIC X(100).
          10 JOB-INFO-DESC      PIC X(200).
          10 JOB-INFO-EMPLOYER  PIC X(100).
          10 JOB-INFO-LOCATION  PIC X(100).
          10 JOB-INFO-SALARY    PIC X(60).
          10 JOB-INFO-POSTER    PIC X(100).
01 JOB-SUB         PIC 9(4) COMP.
01 JOB-SELECTION   PIC 9(4) COMP.
01 CURRENT-JOB-SUB PIC 9(4) COMP.
01 JOB-NUM-TEXT    PIC Z(3).
01 JOB-LIST-FLAG   PIC X VALUE 'N'.
   88 JOB-LIST-DONE   VALUE 'Y'.
   88 JOB-LIST-ACTIVE VALUE 'N'.
01 JOB-LOAD-FLAG   PIC X VALUE 'Y'.
   88 JOB-LOAD-SUCCESS VALUE 'Y'.
   88 JOB-LOAD-FAILED  VALUE 'N'.
01 JOB-POINTER     PIC 9(4) COMP.

01 APP-STAT         PIC XX.
01 APP-LINE         PIC X(512).
01 APP-EOF          PIC X VALUE 'N'.
01 APP-POINTER      PIC 9(4) COMP.
01 APP-USER-FIELD   PIC X(100).
01 APP-JOB-ID-FIELD PIC X(5).
01 APP-TITLE-FIELD  PIC X(100).
01 APP-EMP-FIELD    PIC X(100).
01 APP-LOC-FIELD    PIC X(100).
01 APP-REPORT-COUNT PIC 9(4) COMP VALUE 0.
01 APP-ALREADY      PIC X VALUE 'N'.
   88 APP-ALREADY-YES VALUE 'Y'.
   88 APP-ALREADY-NO  VALUE 'N'.


PROCEDURE DIVISION.
MAIN-SECTION.
       OPEN INPUT I-FILE
            INPUT U-FILE
            OUTPUT O-FILE.

       PERFORM LOAD-USERS.

       MOVE "Welcome to InCollege!" TO W-MSG PERFORM DISP-MSG
       MOVE "1. Log In"             TO W-MSG PERFORM DISP-MSG
       MOVE "2. Create New Account" TO W-MSG PERFORM DISP-MSG
       MOVE "Enter your choice:"    TO W-MSG PERFORM DISP-MSG

       PERFORM READ-INPUT

       PERFORM UNTIL W-USR-INPT = "1" OR W-USR-INPT = "2" OR
                        W-USR-INPT = "login" OR W-USR-INPT = "createnewaccount"
           MOVE "Invalid selection. Please try again:" TO W-MSG
           PERFORM DISP-MSG
           PERFORM READ-INPUT
       END-PERFORM

       SET NOT-CREATED TO TRUE

       IF W-USR-INPT = "2" OR W-USR-INPT = "createnewaccount"
           PERFORM CREATE-ACCOUNT
           IF CREATED-OK
               MOVE USER-USERNAME(USER-COUNT) TO W-USERNAME
               MOVE USER-PASSWORD(USER-COUNT) TO W-PASSWORD
               PERFORM LOG-IN
               SET NOT-CREATED TO TRUE
           END-IF
       ELSE
           SET NOT-FOUND TO TRUE
           PERFORM UNTIL FOUND
               PERFORM PRINT-LINE
               MOVE "Please enter your username:" TO W-MSG PERFORM DISP-MSG
               PERFORM READ-INPUT-RAW
               PERFORM VALIDATE-USERNAME
               IF VALID-USERNAME
                   MOVE "Please enter your password:" TO W-MSG PERFORM DISP-MSG
                   PERFORM READ-INPUT-RAW
                   PERFORM PRINT-LINE
                   IF FUNCTION LENGTH(FUNCTION TRIM(W-USR-INPT)) > 12
                       MOVE "Password too long (max 12)." TO W-MSG PERFORM DISP-MSG
                   ELSE
                       MOVE FUNCTION TRIM(W-USR-INPT) TO W-PASSWORD
                       PERFORM LOG-IN
                   END-IF
               ELSE
                   MOVE "Invalid username (no spaces, not blank)." TO W-MSG PERFORM DISP-MSG
               END-IF
           END-PERFORM
       END-IF

       IF FOUND OR CREATED-OK
           PERFORM INIT-PROFILE-FOR-USER
           PERFORM POST-LOGIN-NAVIGATION-W5
       ELSE
           CLOSE I-FILE U-FILE O-FILE
           PERFORM PROGRAM-END
       END-IF




GO TO PROGRAM-END.

POST-LOGIN-NAVIGATION-W5.
       MOVE "1. View My Profile"                     TO W-MSG PERFORM DISP-MSG
       MOVE "2. Search for User"                     TO W-MSG PERFORM DISP-MSG
       MOVE "3. Learn a New Skill"                   TO W-MSG PERFORM DISP-MSG
       MOVE "4. View My Pending Connection Requests" TO W-MSG PERFORM DISP-MSG
       MOVE "5. View My Network"                     TO W-MSG PERFORM DISP-MSG
       MOVE "6. Job search/internship"               TO W-MSG PERFORM DISP-MSG
       MOVE "Enter your choice:"                     TO W-MSG PERFORM DISP-MSG
       PERFORM READ-INPUT

       EVALUATE W-USR-INPT
           WHEN "1"
               PERFORM VIEW-PROFILE
               PERFORM POST-LOGIN-NAVIGATION-W5
           WHEN "2"
               PERFORM FIND-NAME
               PERFORM POST-LOGIN-NAVIGATION-W5
           WHEN "3"
               PERFORM LEARN-SKILL
               PERFORM POST-LOGIN-NAVIGATION-W5
           WHEN "4"
               PERFORM VIEW-PENDING-REQUESTS
               PERFORM POST-LOGIN-NAVIGATION-W5
           WHEN "5"
               PERFORM VIEW-NETWORK
               PERFORM POST-LOGIN-NAVIGATION-W5
           WHEN "6"
               PERFORM JOB-SEARCH-MENU
               PERFORM POST-LOGIN-NAVIGATION-W5
           WHEN OTHER
               MOVE "Invalid selection. Please try again." TO W-MSG PERFORM DISP-MSG
               PERFORM POST-LOGIN-NAVIGATION-W5
       END-EVALUATE
       EXIT.

POST-LOGIN-NAVIGATION.
       PERFORM POST-LOGIN-NAVIGATION-W5
       EXIT.

JOB-SEARCH-MENU.
       MOVE "--- Job Search/Internship Menu ---" TO W-MSG PERFORM DISP-MSG
       MOVE "1. Post a Job/Internship"           TO W-MSG PERFORM DISP-MSG
       MOVE "2. Browse Jobs/Internships"         TO W-MSG PERFORM DISP-MSG
       MOVE "3. View My Applications"            TO W-MSG PERFORM DISP-MSG
       MOVE "4. Back to Main Menu"               TO W-MSG PERFORM DISP-MSG
       MOVE "Enter your choice:"                 TO W-MSG PERFORM DISP-MSG
       PERFORM READ-INPUT

       EVALUATE W-USR-INPT
           WHEN "1"
               PERFORM POST-JOB
               PERFORM JOB-SEARCH-MENU
           WHEN "2"
               PERFORM BROWSE-JOBS
               PERFORM JOB-SEARCH-MENU
           WHEN "3"
               PERFORM VIEW-MY-APPLICATIONS
               PERFORM JOB-SEARCH-MENU
           WHEN "4"
               EXIT PARAGRAPH
           WHEN OTHER
               MOVE "Invalid selection. Please try again." TO W-MSG PERFORM DISP-MSG
               PERFORM JOB-SEARCH-MENU
       END-EVALUATE
       EXIT.

POST-JOB.
       PERFORM ENSURE-JOB-FILE
       IF JOB-STAT NOT = "00"
           MOVE "Unable to access job postings. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       MOVE SPACES TO JOB-TITLE JOB-DESCRIPTION JOB-EMPLOYER JOB-LOCATION JOB-SALARY

       MOVE "--- Post a New Job/Internship ---" TO W-MSG PERFORM DISP-MSG

       MOVE SPACES TO W-USR-INPT
       PERFORM UNTIL FUNCTION TRIM(W-USR-INPT) NOT = SPACES
           MOVE "Enter Job Title:" TO W-MSG PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW
           MOVE FUNCTION TRIM(W-USR-INPT) TO W-TMP
           MOVE 0 TO JOB-PIPE-COUNT
           INSPECT W-TMP TALLYING JOB-PIPE-COUNT FOR ALL "|"
           IF JOB-PIPE-COUNT > 0
               MOVE "The '|' character is not allowed. Please try again." TO W-MSG
               PERFORM DISP-MSG
               MOVE SPACES TO W-USR-INPT
           ELSE
               IF FUNCTION TRIM(W-USR-INPT) = SPACES
                   MOVE "Job title is required. Please try again." TO W-MSG PERFORM DISP-MSG
               END-IF
           END-IF
       END-PERFORM
       MOVE FUNCTION TRIM(W-USR-INPT) TO JOB-TITLE

       MOVE SPACES TO W-USR-INPT
       PERFORM UNTIL FUNCTION TRIM(W-USR-INPT) NOT = SPACES
           MOVE "Enter Description (max 200 chars):" TO W-MSG PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW
           MOVE FUNCTION TRIM(W-USR-INPT) TO W-TMP
           MOVE 0 TO JOB-PIPE-COUNT
           INSPECT W-TMP TALLYING JOB-PIPE-COUNT FOR ALL "|"
           IF JOB-PIPE-COUNT > 0
               MOVE "The '|' character is not allowed. Please try again." TO W-MSG
               PERFORM DISP-MSG
               MOVE SPACES TO W-USR-INPT
           ELSE
               IF FUNCTION TRIM(W-USR-INPT) = SPACES
                   MOVE "Job description is required. Please try again." TO W-MSG PERFORM DISP-MSG
               END-IF
           END-IF
       END-PERFORM
       MOVE FUNCTION TRIM(W-USR-INPT) TO JOB-DESCRIPTION

       MOVE SPACES TO W-USR-INPT
       PERFORM UNTIL FUNCTION TRIM(W-USR-INPT) NOT = SPACES
           MOVE "Enter Employer Name:" TO W-MSG PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW
           MOVE FUNCTION TRIM(W-USR-INPT) TO W-TMP
           MOVE 0 TO JOB-PIPE-COUNT
           INSPECT W-TMP TALLYING JOB-PIPE-COUNT FOR ALL "|"
           IF JOB-PIPE-COUNT > 0
               MOVE "The '|' character is not allowed. Please try again." TO W-MSG
               PERFORM DISP-MSG
               MOVE SPACES TO W-USR-INPT
           ELSE
               IF FUNCTION TRIM(W-USR-INPT) = SPACES
                   MOVE "Employer name is required. Please try again." TO W-MSG PERFORM DISP-MSG
               END-IF
           END-IF
       END-PERFORM
       MOVE FUNCTION TRIM(W-USR-INPT) TO JOB-EMPLOYER

       MOVE SPACES TO W-USR-INPT
       PERFORM UNTIL FUNCTION TRIM(W-USR-INPT) NOT = SPACES
           MOVE "Enter Location:" TO W-MSG PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW
           MOVE FUNCTION TRIM(W-USR-INPT) TO W-TMP
           MOVE 0 TO JOB-PIPE-COUNT
           INSPECT W-TMP TALLYING JOB-PIPE-COUNT FOR ALL "|"
           IF JOB-PIPE-COUNT > 0
               MOVE "The '|' character is not allowed. Please try again." TO W-MSG
               PERFORM DISP-MSG
               MOVE SPACES TO W-USR-INPT
           ELSE
               IF FUNCTION TRIM(W-USR-INPT) = SPACES
                   MOVE "Job location is required. Please try again." TO W-MSG PERFORM DISP-MSG
               END-IF
           END-IF
       END-PERFORM
       MOVE FUNCTION TRIM(W-USR-INPT) TO JOB-LOCATION

       MOVE 1 TO JOB-PIPE-COUNT
       PERFORM UNTIL JOB-PIPE-COUNT = 0
           MOVE "Enter Salary (optional, enter 'NONE' to skip):" TO W-MSG PERFORM DISP-MSG
           PERFORM READ-INPUT-RAW
           MOVE FUNCTION TRIM(W-USR-INPT) TO W-TMP
           MOVE 0 TO JOB-PIPE-COUNT
           INSPECT W-TMP TALLYING JOB-PIPE-COUNT FOR ALL "|"
           IF JOB-PIPE-COUNT > 0
               MOVE "The '|' character is not allowed. Please try again." TO W-MSG
               PERFORM DISP-MSG
           END-IF
       END-PERFORM
       IF FUNCTION TRIM(W-USR-INPT) = SPACES
           MOVE "Not provided" TO JOB-SALARY
       ELSE
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(W-USR-INPT)) = "NONE"
               MOVE "Not provided" TO JOB-SALARY
           ELSE
               MOVE FUNCTION TRIM(W-USR-INPT) TO JOB-SALARY
           END-IF
       END-IF

       MOVE JOB-NEXT-ID TO JOB-ID-TEXT
       MOVE SPACES TO JOB-LINE
       STRING FUNCTION TRIM(JOB-ID-TEXT) DELIMITED BY SIZE
              "|"                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-TITLE)     DELIMITED BY SIZE
              "|"                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-DESCRIPTION) DELIMITED BY SIZE
              "|"                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-EMPLOYER)  DELIMITED BY SIZE
              "|"                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-LOCATION)  DELIMITED BY SIZE
              "|"                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-SALARY)    DELIMITED BY SIZE
              "|"                          DELIMITED BY SIZE
              FUNCTION TRIM(W-USERNAME)    DELIMITED BY SIZE
          INTO JOB-LINE
       END-STRING

       OPEN EXTEND JOB-FILE
       IF JOB-STAT NOT = "00"
           MOVE "Unable to save job posting. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       MOVE JOB-LINE TO JOB-REC
       WRITE JOB-REC
       CLOSE JOB-FILE

       MOVE "Job posted successfully!" TO W-MSG PERFORM DISP-MSG
       MOVE "----------------------------------" TO W-MSG PERFORM DISP-MSG
       EXIT.

ENSURE-JOB-FILE.
       MOVE 0 TO JOB-COUNT JOB-NEXT-ID
       MOVE 'N' TO JOB-EOF

       OPEN INPUT JOB-FILE
       IF JOB-STAT = "00"
           PERFORM UNTIL JOB-EOF = 'Y'
               READ JOB-FILE INTO JOB-REC
                   AT END
                       MOVE 'Y' TO JOB-EOF
                   NOT AT END
                       ADD 1 TO JOB-COUNT
               END-READ
           END-PERFORM
           CLOSE JOB-FILE
       ELSE
           IF JOB-STAT = "35"
               OPEN OUTPUT JOB-FILE
               IF JOB-STAT = "00"
                   CLOSE JOB-FILE
               END-IF
           END-IF
       END-IF

       MOVE JOB-COUNT TO JOB-NEXT-ID
       ADD 1 TO JOB-NEXT-ID
       EXIT.



ENSURE-APP-FILE.
       MOVE 'N' TO APP-EOF
       OPEN INPUT APP-FILE
       IF APP-STAT = "00"
           CLOSE APP-FILE
       ELSE
           IF APP-STAT = "35"
               OPEN OUTPUT APP-FILE
               IF APP-STAT = "00"
                   CLOSE APP-FILE
               END-IF
           END-IF
       END-IF
       EXIT.

LOAD-JOB-LIST.
       PERFORM ENSURE-JOB-FILE
       MOVE SPACES TO JOB-LIST
       MOVE 0 TO JOB-COUNT
       MOVE 'Y' TO JOB-LOAD-FLAG
       MOVE 'N' TO JOB-EOF

       OPEN INPUT JOB-FILE
       IF JOB-STAT NOT = "00"
           MOVE "Unable to load job listings right now. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           MOVE 'N' TO JOB-LOAD-FLAG
           EXIT PARAGRAPH
       END-IF

       PERFORM UNTIL JOB-EOF = 'Y'
           READ JOB-FILE INTO JOB-REC
               AT END
                   MOVE 'Y' TO JOB-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(JOB-REC) TO JOB-LINE
                   IF JOB-LINE NOT = SPACES
                       IF JOB-COUNT < MAX-JOB-ENTRIES
                           ADD 1 TO JOB-COUNT
                           MOVE JOB-COUNT TO JOB-SUB
                           PERFORM PARSE-JOB-LINE
                       END-IF
                   END-IF
           END-READ
       END-PERFORM
       CLOSE JOB-FILE
       EXIT.

PARSE-JOB-LINE.
       MOVE 1 TO JOB-POINTER
       MOVE SPACES TO JOB-INFO-ID(JOB-SUB)
       MOVE SPACES TO JOB-INFO-TITLE(JOB-SUB)
       MOVE SPACES TO JOB-INFO-DESC(JOB-SUB)
       MOVE SPACES TO JOB-INFO-EMPLOYER(JOB-SUB)
       MOVE SPACES TO JOB-INFO-LOCATION(JOB-SUB)
       MOVE SPACES TO JOB-INFO-SALARY(JOB-SUB)
       MOVE SPACES TO JOB-INFO-POSTER(JOB-SUB)

       UNSTRING JOB-LINE DELIMITED BY "|"
           INTO JOB-INFO-ID(JOB-SUB)
                JOB-INFO-TITLE(JOB-SUB)
                JOB-INFO-DESC(JOB-SUB)
                JOB-INFO-EMPLOYER(JOB-SUB)
                JOB-INFO-LOCATION(JOB-SUB)
                JOB-INFO-SALARY(JOB-SUB)
                JOB-INFO-POSTER(JOB-SUB)
           WITH POINTER JOB-POINTER
       END-UNSTRING

       MOVE FUNCTION TRIM(JOB-INFO-ID(JOB-SUB))       TO JOB-INFO-ID(JOB-SUB)
       MOVE FUNCTION TRIM(JOB-INFO-TITLE(JOB-SUB))    TO JOB-INFO-TITLE(JOB-SUB)
       MOVE FUNCTION TRIM(JOB-INFO-DESC(JOB-SUB))     TO JOB-INFO-DESC(JOB-SUB)
       MOVE FUNCTION TRIM(JOB-INFO-EMPLOYER(JOB-SUB)) TO JOB-INFO-EMPLOYER(JOB-SUB)
       MOVE FUNCTION TRIM(JOB-INFO-LOCATION(JOB-SUB)) TO JOB-INFO-LOCATION(JOB-SUB)
       MOVE FUNCTION TRIM(JOB-INFO-SALARY(JOB-SUB))   TO JOB-INFO-SALARY(JOB-SUB)
       MOVE FUNCTION TRIM(JOB-INFO-POSTER(JOB-SUB))   TO JOB-INFO-POSTER(JOB-SUB)
       EXIT.

DISPLAY-JOB-SUMMARY.
       MOVE "--- Available Job Listings ---" TO W-MSG PERFORM DISP-MSG
       PERFORM VARYING JOB-SUB FROM 1 BY 1 UNTIL JOB-SUB > JOB-COUNT
           MOVE JOB-SUB TO JOB-NUM-TEXT
           MOVE SPACES TO W-MSG
           STRING FUNCTION TRIM(JOB-NUM-TEXT) ". "
                  FUNCTION TRIM(JOB-INFO-TITLE(JOB-SUB))     DELIMITED BY SIZE
                  " at "                                    DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-INFO-EMPLOYER(JOB-SUB)) DELIMITED BY SIZE
                  " ("                                      DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-INFO-LOCATION(JOB-SUB)) DELIMITED BY SIZE
                  ")"                                       DELIMITED BY SIZE
              INTO W-MSG
           END-STRING
           PERFORM DISP-MSG
       END-PERFORM
       MOVE "-----------------------------" TO W-MSG PERFORM DISP-MSG
       EXIT.

BROWSE-JOBS.
       PERFORM LOAD-JOB-LIST
       IF JOB-LOAD-FAILED
           EXIT PARAGRAPH
       END-IF

       IF JOB-COUNT = 0
           MOVE "--- Available Job Listings ---" TO W-MSG PERFORM DISP-MSG
           MOVE "No job listings are currently available." TO W-MSG PERFORM DISP-MSG
           MOVE "-----------------------------" TO W-MSG PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       MOVE 'N' TO JOB-LIST-FLAG
       PERFORM UNTIL JOB-LIST-DONE
           PERFORM DISPLAY-JOB-SUMMARY
           MOVE "Enter job number to view details, or 0 to go back:" TO W-MSG PERFORM DISP-MSG
           PERFORM READ-INPUT

           IF W-USR-INPT = "0"
               MOVE 'Y' TO JOB-LIST-FLAG
           ELSE
               IF W-USR-INPT NUMERIC
                   MOVE FUNCTION NUMVAL(W-USR-INPT) TO JOB-SELECTION
                   IF JOB-SELECTION >= 1 AND JOB-SELECTION <= JOB-COUNT
                       MOVE JOB-SELECTION TO CURRENT-JOB-SUB
                       PERFORM SHOW-JOB-DETAILS
                   ELSE
                       MOVE "Invalid selection. Please try again." TO W-MSG PERFORM DISP-MSG
                   END-IF
               ELSE
                   MOVE "Invalid selection. Please try again." TO W-MSG PERFORM DISP-MSG
               END-IF
           END-IF
       END-PERFORM
       EXIT.

SHOW-JOB-DETAILS.
       IF CURRENT-JOB-SUB < 1 OR CURRENT-JOB-SUB > JOB-COUNT
           EXIT PARAGRAPH
       END-IF

       PERFORM UNTIL 1 = 0
           MOVE "--- Job Details ---" TO W-MSG PERFORM DISP-MSG

           MOVE SPACES TO W-MSG
           STRING "Title: " DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-INFO-TITLE(CURRENT-JOB-SUB)) DELIMITED BY SIZE
              INTO W-MSG
           END-STRING
           PERFORM DISP-MSG

           MOVE SPACES TO W-MSG
           STRING "Description: " DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-INFO-DESC(CURRENT-JOB-SUB)) DELIMITED BY SIZE
              INTO W-MSG
           END-STRING
           PERFORM DISP-MSG

           MOVE SPACES TO W-MSG
           STRING "Employer: " DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-INFO-EMPLOYER(CURRENT-JOB-SUB)) DELIMITED BY SIZE
              INTO W-MSG
           END-STRING
           PERFORM DISP-MSG

           MOVE SPACES TO W-MSG
           STRING "Location: " DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-INFO-LOCATION(CURRENT-JOB-SUB)) DELIMITED BY SIZE
              INTO W-MSG
           END-STRING
           PERFORM DISP-MSG

           MOVE SPACES TO W-MSG
           STRING "Salary: " DELIMITED BY SIZE
                  FUNCTION TRIM(JOB-INFO-SALARY(CURRENT-JOB-SUB)) DELIMITED BY SIZE
              INTO W-MSG
           END-STRING
           PERFORM DISP-MSG

           MOVE "-------------------" TO W-MSG PERFORM DISP-MSG
           MOVE "1. Apply for this Job" TO W-MSG PERFORM DISP-MSG
           MOVE "2. Back to Job List"   TO W-MSG PERFORM DISP-MSG
           MOVE "Enter your choice:"    TO W-MSG PERFORM DISP-MSG
           PERFORM READ-INPUT

           EVALUATE W-USR-INPT
               WHEN "1"
                   PERFORM APPLY-TO-JOB
                   EXIT PARAGRAPH
               WHEN "2"
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE "Invalid selection. Please try again." TO W-MSG PERFORM DISP-MSG
           END-EVALUATE
       END-PERFORM
       EXIT.

APPLY-TO-JOB.
       PERFORM ENSURE-APP-FILE
       IF APP-STAT NOT = "00"
           MOVE "Unable to record job applications at this time. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       MOVE 'N' TO APP-ALREADY
       MOVE 'N' TO APP-EOF

       OPEN INPUT APP-FILE
       IF APP-STAT = "00"
           PERFORM UNTIL APP-EOF = 'Y'
               READ APP-FILE INTO APP-REC
                   AT END
                       MOVE 'Y' TO APP-EOF
                   NOT AT END
                       MOVE FUNCTION TRIM(APP-REC) TO APP-LINE
                       IF APP-LINE NOT = SPACES
                           MOVE 1 TO APP-POINTER
                           MOVE SPACES TO APP-USER-FIELD APP-JOB-ID-FIELD APP-TITLE-FIELD
                           MOVE SPACES TO APP-EMP-FIELD APP-LOC-FIELD
                           UNSTRING APP-LINE DELIMITED BY "|"
                               INTO APP-USER-FIELD
                                    APP-JOB-ID-FIELD
                                    APP-TITLE-FIELD
                                    APP-EMP-FIELD
                                    APP-LOC-FIELD
                               WITH POINTER APP-POINTER
                           END-UNSTRING

                           IF FUNCTION LOWER-CASE(FUNCTION TRIM(APP-USER-FIELD))
                              = FUNCTION LOWER-CASE(FUNCTION TRIM(W-USERNAME))
                              AND FUNCTION TRIM(APP-JOB-ID-FIELD)
                              = FUNCTION TRIM(JOB-INFO-ID(CURRENT-JOB-SUB))
                               SET APP-ALREADY-YES TO TRUE
                               EXIT PERFORM
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE APP-FILE
       ELSE
           MOVE "Unable to read existing applications right now. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       IF APP-ALREADY-YES
           MOVE SPACES TO W-MSG
           STRING "You have already applied for "
                  FUNCTION TRIM(JOB-INFO-TITLE(CURRENT-JOB-SUB))
                  " at "
                  FUNCTION TRIM(JOB-INFO-EMPLOYER(CURRENT-JOB-SUB))
                  "." INTO W-MSG
           END-STRING
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       OPEN EXTEND APP-FILE
       IF APP-STAT NOT = "00"
           MOVE "Unable to save your application. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       MOVE SPACES TO APP-LINE
       STRING FUNCTION TRIM(W-USERNAME)                    DELIMITED BY SIZE
              "|"                                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-INFO-ID(CURRENT-JOB-SUB))  DELIMITED BY SIZE
              "|"                                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-INFO-TITLE(CURRENT-JOB-SUB)) DELIMITED BY SIZE
              "|"                                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-INFO-EMPLOYER(CURRENT-JOB-SUB)) DELIMITED BY SIZE
              "|"                                          DELIMITED BY SIZE
              FUNCTION TRIM(JOB-INFO-LOCATION(CURRENT-JOB-SUB)) DELIMITED BY SIZE
           INTO APP-LINE
       END-STRING

       MOVE APP-LINE TO APP-REC
       WRITE APP-REC
       CLOSE APP-FILE

       MOVE SPACES TO W-MSG
       STRING "Your application for "
              FUNCTION TRIM(JOB-INFO-TITLE(CURRENT-JOB-SUB))
              " at "
              FUNCTION TRIM(JOB-INFO-EMPLOYER(CURRENT-JOB-SUB))
              " has been submitted."
           INTO W-MSG
       END-STRING
       PERFORM DISP-MSG
       EXIT.

VIEW-MY-APPLICATIONS.
       PERFORM ENSURE-APP-FILE
       IF APP-STAT NOT = "00"
           MOVE "Unable to access job applications right now. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       MOVE "--- Your Job Applications ---" TO W-MSG PERFORM DISP-MSG
       MOVE SPACES TO W-MSG
       STRING "Application Summary for "
              FUNCTION TRIM(W-USERNAME)
           INTO W-MSG
       END-STRING
       PERFORM DISP-MSG
       MOVE "------------------------------" TO W-MSG PERFORM DISP-MSG

       MOVE 0 TO APP-REPORT-COUNT
       MOVE 'N' TO APP-EOF

       OPEN INPUT APP-FILE
       IF APP-STAT NOT = "00"
           MOVE "Unable to read job applications right now. Please try again later." TO W-MSG
           PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       PERFORM UNTIL APP-EOF = 'Y'
           READ APP-FILE INTO APP-REC
               AT END
                   MOVE 'Y' TO APP-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(APP-REC) TO APP-LINE
                   IF APP-LINE NOT = SPACES
                       MOVE 1 TO APP-POINTER
                       MOVE SPACES TO APP-USER-FIELD APP-JOB-ID-FIELD APP-TITLE-FIELD
                       MOVE SPACES TO APP-EMP-FIELD APP-LOC-FIELD
                       UNSTRING APP-LINE DELIMITED BY "|"
                           INTO APP-USER-FIELD
                                APP-JOB-ID-FIELD
                                APP-TITLE-FIELD
                                APP-EMP-FIELD
                                APP-LOC-FIELD
                           WITH POINTER APP-POINTER
                       END-UNSTRING

                       IF FUNCTION LOWER-CASE(FUNCTION TRIM(APP-USER-FIELD))
                          = FUNCTION LOWER-CASE(FUNCTION TRIM(W-USERNAME))
                           ADD 1 TO APP-REPORT-COUNT

                           MOVE SPACES TO W-MSG
                           STRING "Job Title: " DELIMITED BY SIZE
                                  FUNCTION TRIM(APP-TITLE-FIELD) DELIMITED BY SIZE
                               INTO W-MSG
                           END-STRING
                           PERFORM DISP-MSG

                           MOVE SPACES TO W-MSG
                           STRING "Employer: " DELIMITED BY SIZE
                                  FUNCTION TRIM(APP-EMP-FIELD) DELIMITED BY SIZE
                               INTO W-MSG
                           END-STRING
                           PERFORM DISP-MSG

                           MOVE SPACES TO W-MSG
                           STRING "Location: " DELIMITED BY SIZE
                                  FUNCTION TRIM(APP-LOC-FIELD) DELIMITED BY SIZE
                               INTO W-MSG
                           END-STRING
                           PERFORM DISP-MSG

                           MOVE "---" TO W-MSG PERFORM DISP-MSG
                       END-IF
                   END-IF
           END-READ
       END-PERFORM
       CLOSE APP-FILE

       IF APP-REPORT-COUNT = 0
           MOVE "You have not applied to any jobs yet." TO W-MSG PERFORM DISP-MSG
       END-IF

       MOVE "------------------------------" TO W-MSG PERFORM DISP-MSG
       MOVE APP-REPORT-COUNT TO JOB-ID-TEXT
       MOVE SPACES TO W-MSG
       STRING "Total Applications: "
              FUNCTION TRIM(JOB-ID-TEXT)
           INTO W-MSG
       END-STRING
       PERFORM DISP-MSG
       MOVE "------------------------------" TO W-MSG PERFORM DISP-MSG
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


VIEW-PENDING-REQUESTS.
       PERFORM BUILD-PROFILE-PATH
       MOVE "-----------------------------------"           TO W-MSG PERFORM DISP-MSG
       MOVE "--- Pending Connection Requests ---"           TO W-MSG PERFORM DISP-MSG

       PERFORM GET-CONNECTIONS
       PERFORM PARSE-CONNECTIONS

       IF CONNECTIONS-COUNT = 0
           MOVE "(none)" TO W-MSG PERFORM DISP-MSG
           MOVE "-----------------------------------"        TO W-MSG PERFORM DISP-MSG
           EXIT PARAGRAPH
       END-IF

       PERFORM VARYING CONN-IDX FROM 1 BY 1 UNTIL CONN-IDX > CONNECTIONS-COUNT
           MOVE FUNCTION TRIM(CONNECTIONS-ENTRY(CONN-IDX)) TO RQ-NAME
           IF RQ-NAME NOT = SPACES
               MOVE SPACES TO W-MSG
               STRING "Request from: " RQ-NAME INTO W-MSG
               END-STRING
               PERFORM DISP-MSG

               MOVE "1. Accept" TO W-MSG PERFORM DISP-MSG
               MOVE "2. Reject" TO W-MSG PERFORM DISP-MSG
               MOVE SPACES TO W-MSG
               STRING "Enter your choice for " RQ-NAME ":" INTO W-MSG
               END-STRING
               PERFORM DISP-MSG

               PERFORM READ-INPUT

               IF W-USR-INPT = "1" OR W-USR-INPT = "accept"
                   PERFORM ACCEPT-REQUEST
                   MOVE SPACES TO W-MSG
                   STRING "Connection request from " RQ-NAME " accepted!"
                      INTO W-MSG
                   END-STRING
                   PERFORM DISP-MSG
               ELSE
                   PERFORM REJECT-REQUEST
                   MOVE SPACES TO W-MSG
                   STRING "Connection request from " RQ-NAME " rejected."
                      INTO W-MSG
                   END-STRING
                   PERFORM DISP-MSG
               END-IF
           END-IF
       END-PERFORM

       MOVE "-----------------------------------" TO W-MSG PERFORM DISP-MSG
       EXIT.

ACCEPT-REQUEST.
       *> 1) Add to established connections (if not already there)
       MOVE FUNCTION TRIM(W-USERNAME) TO EC-U1
       MOVE FUNCTION TRIM(RQ-NAME)    TO EC-U2
       PERFORM NORMALIZE-PAIR
       PERFORM ENSURE-PAIR-IN-ECFILE

       *> 2) Remove from pending list in current user's profile
       PERFORM REMOVE-PENDING-REQUEST
       EXIT.

REJECT-REQUEST.
       PERFORM REMOVE-PENDING-REQUEST
       EXIT.

NORMALIZE-PAIR.
       *> Sort EC-U1 and EC-U2 lexicographically so (a,b) == (b,a)
       IF FUNCTION LOWER-CASE(EC-U1) > FUNCTION LOWER-CASE(EC-U2)
           MOVE EC-U1 TO W-TMP
           MOVE EC-U2 TO EC-U1
           MOVE W-TMP TO EC-U2
       END-IF
       EXIT.

ENSURE-PAIR-IN-ECFILE.
       MOVE 'N' TO EC-EXISTS
       OPEN INPUT EC-FILE
       PERFORM UNTIL 1 = 0
           READ EC-FILE INTO EC-LINE
               AT END EXIT PERFORM
               NOT AT END
                   MOVE FUNCTION TRIM(EC-LINE) TO EC-LINE
                   IF EC-LINE NOT = SPACES
                       UNSTRING EC-LINE DELIMITED BY ","
                           INTO EC-U1, EC-U2
                       END-UNSTRING
                       PERFORM NORMALIZE-PAIR
                       MOVE SPACES TO EC-PAIR
                       STRING EC-U1 "," EC-U2 INTO EC-PAIR
                       END-STRING
                       IF FUNCTION TRIM(EC-PAIR) =
                          FUNCTION TRIM(EC-LINE)
                           MOVE 'Y' TO EC-EXISTS
                       END-IF
                   END-IF
           END-READ
       END-PERFORM
       CLOSE EC-FILE

       IF EC-EXISTS = 'N'
           OPEN EXTEND EC-FILE
           MOVE SPACES TO EC-REC
           STRING EC-U1 "," EC-U2 INTO EC-REC
           END-STRING
           WRITE EC-REC
           CLOSE EC-FILE
       END-IF
       EXIT.

REMOVE-PENDING-REQUEST.
       *> We will rebuild the CONNECTIONS: line without RQ-NAME
       PERFORM GET-CONNECTIONS
       PERFORM PARSE-CONNECTIONS

       OPEN INPUT  P-FILE
       OPEN OUTPUT P-TEMP-FILE

       MOVE 'N' TO FILE-EOF
       MOVE 'N' TO LINE-IS-TAG

       PERFORM UNTIL FILE-EOF = 'Y'
           READ P-FILE INTO P-REC
               AT END
                   MOVE 'Y' TO FILE-EOF
               NOT AT END
                   MOVE FUNCTION TRIM(P-REC) TO VIEW-LINE

                   IF FUNCTION TRIM(VIEW-LINE(1:12)) = "CONNECTIONS:"
                       MOVE "CONNECTIONS: " TO NEW-CONN-LINE
                       MOVE 'N' TO ANY-WRITTEN
                       PERFORM VARYING CONN-IDX FROM 1 BY 1
                               UNTIL CONN-IDX > CONNECTIONS-COUNT
                           IF FUNCTION TRIM(CONNECTIONS-ENTRY(CONN-IDX))
                              NOT = FUNCTION TRIM(RQ-NAME)
                               IF ANY-WRITTEN = 'Y'
                                   STRING FUNCTION TRIM(NEW-CONN-LINE) ","
                                      INTO NEW-CONN-LINE
                                   END-STRING
                               END-IF
                               STRING FUNCTION TRIM(NEW-CONN-LINE)
                                      FUNCTION TRIM(CONNECTIONS-ENTRY(CONN-IDX))
                                      INTO NEW-CONN-LINE
                               END-STRING
                               MOVE 'Y' TO ANY-WRITTEN
                           END-IF
                       END-PERFORM
                       WRITE P-TEMP-REC FROM NEW-CONN-LINE
                   ELSE
                       WRITE P-TEMP-REC FROM P-REC
                   END-IF
           END-READ
       END-PERFORM

       CLOSE P-FILE
       CLOSE P-TEMP-FILE

       STRING "mv bin/profiles/te-mp.txt " W-PROFILE-PATH INTO W-TMP
       END-STRING
       CALL "SYSTEM" USING W-TMP
       EXIT.

VIEW-NETWORK.
       MOVE "--------------------"  TO W-MSG PERFORM DISP-MSG
       MOVE "--- Your Network ---"  TO W-MSG PERFORM DISP-MSG

       OPEN INPUT EC-FILE
       MOVE 0 TO EC-COUNT

       PERFORM UNTIL 1 = 0
           READ EC-FILE INTO EC-LINE
               AT END EXIT PERFORM
               NOT AT END
                   MOVE FUNCTION TRIM(EC-LINE) TO EC-LINE
                   IF EC-LINE NOT = SPACES
                       UNSTRING EC-LINE DELIMITED BY ","
                           INTO EC-U1, EC-U2
                       END-UNSTRING

                       IF FUNCTION TRIM(EC-U1) = FUNCTION TRIM(W-USERNAME)
                           MOVE EC-U2 TO EC-OTHER
                           PERFORM PRINT-OTHER-SUMMARY
                           ADD 1 TO EC-COUNT
                       ELSE
                           IF FUNCTION TRIM(EC-U2) = FUNCTION TRIM(W-USERNAME)
                               MOVE EC-U1 TO EC-OTHER
                               PERFORM PRINT-OTHER-SUMMARY
                               ADD 1 TO EC-COUNT
                           END-IF
                       END-IF
                   END-IF
           END-READ
       END-PERFORM
       CLOSE EC-FILE

       IF EC-COUNT = 0
           MOVE "(none)" TO W-MSG PERFORM DISP-MSG
       END-IF

       MOVE "--------------------" TO W-MSG PERFORM DISP-MSG
       EXIT.

PRINT-OTHER-SUMMARY.
       *> Save current parsed profile fields (we’ll reuse the same WS)
       MOVE FIRST-NAME  TO SAVE-FIRST
       MOVE LAST-NAME   TO SAVE-LAST
       MOVE UNIVERSITY  TO SAVE-UNIV
       MOVE MAJOR       TO SAVE-MAJOR
       MOVE W-YEAR-TEXT-VIEW TO SAVE-YEAR

       *> Open other user’s profile and parse to get University/Major
       MOVE SPACES TO OTHER-PATH
       STRING "bin/profiles/" FUNCTION LOWER-CASE(EC-OTHER) ".txt"
          INTO OTHER-PATH
       END-STRING

       MOVE OTHER-PATH TO W-PROFILE-PATH
       OPEN INPUT P-FILE
       IF P-STAT = "00"
           PERFORM CLEAR-PROFILE-WS
           PERFORM PARSE-PROFILE-FILE
           CLOSE P-FILE

           MOVE SPACES TO W-MSG
           STRING "Connected with: "
                  EC-OTHER
                  " (University: "
                  FUNCTION TRIM(UNIVERSITY)
                  ", Major: "
                  FUNCTION TRIM(MAJOR)
                  ")"
              INTO W-MSG
           END-STRING
           PERFORM DISP-MSG
       ELSE
           CLOSE P-FILE
           MOVE SPACES TO W-MSG
           STRING "Connected with: " EC-OTHER INTO W-MSG
           END-STRING
           PERFORM DISP-MSG
       END-IF

       *> Restore saved fields (keeps your current user’s profile intact)
       MOVE SAVE-FIRST TO FIRST-NAME
       MOVE SAVE-LAST  TO LAST-NAME
       MOVE SAVE-UNIV  TO UNIVERSITY
       MOVE SAVE-MAJOR TO MAJOR
       MOVE SAVE-YEAR  TO W-YEAR-TEXT-VIEW
       EXIT.
