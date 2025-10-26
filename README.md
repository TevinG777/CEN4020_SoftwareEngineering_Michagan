# CEN4020 Software Engineering – InCollege Project

## Overview
- Console-based LinkedIn-style prototype written in COBOL.
- Supports account creation/login, profile management, networking, and a job board.
- All program input is read from `bin/InCollege-Input.txt`; every message displayed on screen is also written to `bin/InCollege-Output.txt`.

## Development Environment
- Recommended setup: VS Code devcontainer with GnuCOBOL.
- Key directories:
  - `src/` – COBOL source (`InCollege.cob`).
  - `bin/` – compiled binary plus data files (input, output, users, jobs, applications).
  - `epics/` – requirements and planning artifacts.

## Build & Run
- Compile: `cobc -x -o bin/InCollege src/InCollege.cob`
- Populate `bin/InCollege-Input.txt` with the scripted user choices.
- Run the program from `bin/`: `./InCollege` (bash) or `.\InCollege` (PowerShell).
- Review console output and the mirrored log in `bin/InCollege-Output.txt`.

## Week 7 – Job Board Enhancements
- Browse all posted jobs/internships with a numbered summary list.
- Select a posting to view full details (title, description, employer, location, salary).
- Simulate applying to a job; applications persist per user across runs.
- View the "My Applications" report showing each applied position and total count.

## Data Files
- `bin/InCollege_jobListings.txt` – job postings in the format `id|title|description|employer|location|salary|poster`. Seed or edit this file to control what appears when browsing.
- `bin/InCollege_jobApplications.txt` – created automatically; stores applications as `username|job-id|title|employer|location`. Delete or clear to reset the report for a user.

## Sample Scenario
- `bin/InCollege-Input.txt` logs in as a sample user, browses listings, applies to two jobs, and requests the applications report.
- The expected console/output log for that flow is captured in `bin/InCollege-Output.txt`.
