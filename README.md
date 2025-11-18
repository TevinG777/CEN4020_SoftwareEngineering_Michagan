# InCollege - LinkedIn-Style Social Network Application

## Overview

InCollege is a console-based LinkedIn-style social networking prototype written in COBOL. The application provides a comprehensive platform for college students and professionals to create profiles, connect with others, search for jobs, and communicate through messages. All program input is read from `bin/InCollege-Input.txt`, and every message displayed on screen is also written to `bin/InCollege-Output.txt` for logging purposes.

## Table of Contents

- [System Requirements](#system-requirements)
- [Compilation Instructions](#compilation-instructions)
- [Running the Program](#running-the-program)
- [Project Structure](#project-structure)
- [Core Functionalities](#core-functionalities)
- [User Account Management](#user-account-management)
- [Profile Management](#profile-management)
- [Networking Features](#networking-features)
- [Job Board](#job-board)
- [Messaging System](#messaging-system)
- [Data Files](#data-files)
- [Input/Output Format](#inputoutput-format)
- [Limitations](#limitations)
- [Troubleshooting](#troubleshooting)

## System Requirements

- **COBOL Compiler**: GnuCOBOL (cobc) version 3.0 or later
- **Operating System**: Linux, macOS, or Windows (with WSL2)
- **File System**: Read/write access to the `bin/` directory
- **Development Environment**: VS Code with devcontainer (recommended)

## Compilation Instructions

### Basic Compilation

To compile the InCollege program, use the following command:

```bash
cobc -x -o bin/InCollege src/InCollege.cob
```

This command:
- `-x`: Creates an executable program
- `-o bin/InCollege`: Specifies the output binary location
- `src/InCollege.cob`: The source COBOL file

### Compilation Options

For debugging or additional features, you can use:

```bash
# With debugging symbols
cobc -x -g -o bin/InCollege src/InCollege.cob

# With verbose output
cobc -x -v -o bin/InCollege src/InCollege.cob
```

### Verify Compilation

After compilation, verify the executable was created:

```bash
ls -lh bin/InCollege
```

## Running the Program

### Prerequisites

Before running the program, ensure:

1. The `bin/` directory exists
2. `bin/InCollege-Input.txt` contains your input commands (see [Input Format](#input-format))
3. The executable has been compiled successfully

### Execution

#### On Linux/macOS/WSL:

```bash
cd bin
./InCollege
```

#### On Windows PowerShell:

```powershell
cd bin
.\InCollege
```

### Output

- **Console Output**: All messages are displayed on the console
- **Log File**: All output is simultaneously written to `bin/InCollege-Output.txt`

## Project Structure

```
workspace/
├── src/
│   └── InCollege.cob          # Main COBOL source file
├── bin/                        # Compiled binary and data files
│   ├── InCollege              # Compiled executable
│   ├── InCollege-Input.txt    # User input file
│   ├── InCollege-Output.txt   # Program output log
│   ├── inCollege-Users.txt   # User accounts database
│   ├── InCollege_jobListings.txt      # Job postings
│   ├── InCollege_jobApplications.txt # Job applications
│   ├── established-connections.txt   # User connections
│   └── profiles/              # User profile files
│       └── [username].txt     # Individual user profiles
└── README.md                  # This file
```

## Core Functionalities

### Main Menu

Upon starting the program, users are presented with:

1. **Log In** - Access existing account
2. **Create New Account** - Register a new user

### Post-Login Navigation Menu

After successful login, users can access:

1. **View My Profile** - Display current user's profile
2. **Search for User** - Find users by full name
3. **Learn a New Skill** - Access skill learning modules
4. **View My Pending Connection Requests** - Manage connection requests
5. **View My Network** - See all established connections
6. **Job search/internship** - Access job board features
7. **Messages** - Send and receive messages
8. **Create/Edit Profile** - Manage profile information

## User Account Management

### Account Creation

#### Username Requirements:
- Must be non-blank
- Cannot contain spaces
- Can only contain alphanumeric characters (A-Z, a-z, 0-9)
- Maximum 5 users can be registered in the system

#### Password Requirements:
- **Length**: 8-12 characters
- **Must contain**:
  - At least one uppercase letter (A-Z)
  - At least one digit (0-9)
  - At least one special character (any non-alphanumeric character)

#### Example Valid Passwords:
- `Pass123!` ✓
- `MyP@ssw0rd` ✓
- `Secure#1` ✓

#### Example Invalid Passwords:
- `password` ✗ (no uppercase, no digit, no special)
- `PASSWORD1` ✗ (no special character)
- `Pass!` ✗ (too short, less than 8 characters)
- `VeryLongPassword123!` ✗ (too long, more than 12 characters)

### Login Process

1. Enter username (case-sensitive)
2. Enter password (case-sensitive)
3. System validates credentials against stored user accounts
4. Upon successful login, user is automatically logged in and redirected to the main navigation menu

### User Storage

- User accounts are stored in `bin/inCollege-Users.txt`
- Format: `username|password` (one per line)
- Maximum capacity: 5 users in memory (loaded at startup)

## Profile Management

### Profile Fields

#### Required Fields:
- **First Name** (max 30 characters)
- **Last Name** (max 30 characters)
- **University/College** (max 60 characters)
- **Major** (max 40 characters)
- **Graduation Year** (4 digits, between 1900-2100)

#### Optional Fields:
- **About Me** (max 90 characters) - Brief personal description

#### Experience Entries (up to 3):
- **Title** (required, max 40 characters)
- **Company/Organization** (required, max 40 characters)
- **Dates** (required, max 40 characters) - e.g., "Summer 2024" or "Jan 2023 - May 2024"
- **Description** (optional, max 300 characters)

#### Education Entries (up to 3):
- **Degree** (required, max 40 characters)
- **University/College** (required, max 60 characters)
- **Years Attended** (required, max 20 characters) - e.g., "2023-2025"

### Profile Storage

- Each user's profile is stored in `bin/profiles/[username].txt`
- Profile files use a structured format with tags like `[FIRST_NAME]`, `[LAST_NAME]`, `[EXPERIENCE]`, etc.
- Profiles are automatically created when a user first logs in

### Viewing Profiles

- Users can view their own profile from the main menu
- When searching for users, you can view their public profiles
- Profile viewing displays all information in a formatted, readable layout

## Networking Features

### Search for Users

- Search by entering a user's full name (First Name + Last Name)
- System searches through all user profiles
- If found, displays user information and connection options

### Connection Requests

- Users can send connection requests to other users
- Connection requests are stored in the user's profile file
- Users can view pending connection requests from the main menu

### View Network

- Displays all established connections
- Shows connection details including:
  - Connected user's name
  - Their university (if available)
  - Their major (if available)
- Connections are bidirectional (if A connects to B, both see the connection)

### Connection Storage

- Established connections are stored in `bin/established-connections.txt`
- Format: `username1,username2` (one connection per line)

## Job Board

### Features

1. **Post a Job/Internship**
   - Create new job postings
   - Required fields:
     - Job Title
     - Description (max 200 characters)
     - Employer Name
     - Location
     - Salary (optional, enter "NONE" to skip)
   - Job postings are automatically assigned a unique ID
   - The pipe character (`|`) is not allowed in any field

2. **Browse Jobs/Internships**
   - View all available job postings in a numbered list
   - Select a job by number to view full details:
     - Job ID
     - Title
     - Description
     - Employer
     - Location
     - Salary
     - Posted by (username)
   - Apply to jobs directly from the detail view

3. **View My Applications**
   - See all jobs you've applied to
   - Displays:
     - Job ID
     - Job Title
     - Employer
     - Location
   - Shows total application count

### Job Storage

- **Job Listings**: `bin/InCollege_jobListings.txt`
  - Format: `id|title|description|employer|location|salary|poster`
  - One job per line

- **Job Applications**: `bin/InCollege_jobApplications.txt`
  - Format: `username|job-id|title|employer|location`
  - One application per line
  - Automatically created when a user applies

### Job Application Process

1. Browse available jobs
2. Select a job by number to view details
3. Choose to apply to the job
4. Application is saved and associated with your username
5. View your applications from the "View My Applications" menu

## Messaging System

### Send Messages

1. Select "Send a message" from the Messages menu
2. Enter the recipient's full name
3. System verifies the user exists and is in your network
4. Enter your message
5. Message is saved to the recipient's profile file

### View Messages

- Access all messages sent to your account
- Messages are displayed with sender information
- Format: `From [username]: [message content]`

### Message Storage

- Messages are stored in each user's profile file under the `[MESSAGES]` section
- Messages persist across program runs
- Only users in your network can send you messages

## Data Files

### Input File: `bin/InCollege-Input.txt`

This file contains all user input commands, one per line. The program reads from this file sequentially.

**Example Input File:**
```
2
john_doe
MyPass123!
1
2
John Doe
yes
1
6
2
1
```

### Output File: `bin/InCollege-Output.txt`

All program output is written to this file, providing a complete log of the session.

### User Accounts: `bin/inCollege-Users.txt`

Stores user credentials in the format:
```
username1|password1
username2|password2
```

### Profile Files: `bin/profiles/[username].txt`

Structured profile files containing user information, experiences, education, and messages.

### Connections: `bin/established-connections.txt`

Stores bidirectional connections:
```
user1,user2
user3,user4
```

### Job Listings: `bin/InCollege_jobListings.txt`

Job postings in pipe-delimited format:
```
1|Software Engineer|Develop applications|Tech Corp|Remote|$80,000|john_doe
```

### Job Applications: `bin/InCollege_jobApplications.txt`

User applications:
```
john_doe|1|Software Engineer|Tech Corp|Remote
```

## Input/Output Format

### Input Format

The program reads commands from `bin/InCollege-Input.txt` line by line:

- **Menu selections**: Single digit or text (e.g., `1`, `2`, `login`, `createnewaccount`)
- **Text input**: Plain text (e.g., usernames, names, descriptions)
- **Special commands**:
  - `yes` / `done` for profile creation
  - `END` to finish multi-line input
  - `NONE` to skip optional fields

### Output Format

All output is displayed on console and logged to `bin/InCollege-Output.txt`:

- Menu options are numbered
- Prompts clearly indicate required vs. optional fields
- Error messages provide specific feedback
- Success messages confirm actions

## Limitations

1. **User Capacity**: Maximum of 5 users can be registered
2. **Password Length**: Passwords are limited to 12 characters
3. **Profile Fields**:
   - About Me: 90 characters maximum
   - Experience entries: Maximum 3
   - Education entries: Maximum 3
   - Experience description: 300 characters maximum
4. **Job Description**: 200 characters maximum
5. **Input Method**: All input must be pre-written in `InCollege-Input.txt` (no interactive console input)
6. **File Dependencies**: Program requires specific file structure in `bin/` directory
7. **Case Sensitivity**: Usernames and passwords are case-sensitive
8. **Special Characters**: Pipe character (`|`) cannot be used in job postings

## Troubleshooting

### Compilation Issues

**Error: "cobc: command not found"**
- Install GnuCOBOL: `sudo apt-get install gnucobol` (Linux) or `brew install gnucobol` (macOS)

**Error: "File not found"**
- Ensure you're in the correct directory
- Verify `src/InCollege.cob` exists

### Runtime Issues

**Error: "File not found" or "File status error"**
- Ensure `bin/` directory exists: `mkdir -p bin`
- Check file permissions: `chmod +rw bin/`
- Verify input file exists: `ls bin/InCollege-Input.txt`

**Program exits immediately**
- Check that `bin/InCollege-Input.txt` contains valid input
- Ensure the file is not empty

**"No user found" errors**
- Verify users exist in `bin/inCollege-Users.txt`
- Check username spelling (case-sensitive)
- Ensure password matches exactly (case-sensitive)

**Profile not found**
- Create a profile using "Create/Edit Profile" from the main menu
- Verify `bin/profiles/` directory exists

**Connection errors**
- Ensure `bin/established-connections.txt` exists (will be created automatically)
- Check file permissions

### Data File Issues

**Users not persisting**
- Verify `bin/inCollege-Users.txt` is writable
- Check that the file is not corrupted (should be plain text, one user per line)

**Profile data lost**
- Profiles are stored in `bin/profiles/[username].txt`
- Ensure the profiles directory exists and is writable
- Check file permissions: `chmod -R +rw bin/profiles/`

**Job listings not appearing**
- Verify `bin/InCollege_jobListings.txt` exists
- Check file format (pipe-delimited)
- Ensure file is readable

## Sample Workflow

### Creating an Account and Profile

1. Start program → Select `2` (Create New Account)
2. Enter username: `john_doe`
3. Enter password: `MyPass123!`
4. Account created → Automatically logged in
5. Select `8` (Create/Edit Profile)
6. Fill in required fields:
   - First Name: `John`
   - Last Name: `Doe`
   - University: `State University`
   - Major: `Computer Science`
   - Graduation Year: `2025`
7. Add optional About Me
8. Add experiences (up to 3)
9. Add education (up to 3)
10. Profile saved

### Searching and Connecting

1. Select `2` (Search for User)
2. Enter full name: `Jane Smith`
3. If found, view profile
4. Send connection request
5. Other user accepts → Connection established

### Posting and Applying to Jobs

1. Select `6` (Job search/internship)
2. Select `1` (Post a Job/Internship)
3. Enter job details
4. Job posted successfully
5. Select `2` (Browse Jobs/Internships)
6. View job list, select a job
7. Apply to job
8. Select `3` (View My Applications) to see applications

## Additional Notes

- The program uses file-based storage (no database)
- All data persists between program runs
- Input must be prepared in advance in `InCollege-Input.txt`
- Output is logged for debugging and review
- The application is designed for educational purposes as a COBOL demonstration project

## Support

For issues or questions:
1. Check the Troubleshooting section above
2. Review the output log in `bin/InCollege-Output.txt`
3. Verify all required files exist in the `bin/` directory
4. Ensure file permissions are correct

---

**Version**: 1.0
**Language**: COBOL (GnuCOBOL)
**License**: Educational/Project Use
