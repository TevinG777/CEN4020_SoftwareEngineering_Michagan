# CEN4020 Software Engineering — InCollege Project

## Overview
- Purpose: Course project for CEN4020 to build a simplified, linkedIn clone called InCollege
- Language/Stack: GNU COBOL (via devcontainer) with a simple console interface.
- Requirements: See `epics/InCollege Software Req - Epic #1.pdf` for the initial scope.

## Development Environment
- Devcontainer: Open the repo in VS Code and “Reopen in Container” to get GNU COBOL and extensions preinstalled.
- Auto-setup: The container creates `bin/` and `src/` on first start.

## Build & Run
From inside the devcontainer (bash):
- Compile: `cobc -x -o bin/app src/main.cob` or `ctrl + shift + b`
- Run: `./bin/main`

## Repository Structure
- `.devcontainer/`: Devcontainer config for a consistent GNU COBOL setup.
- `src/`: Source files (starter `main.cob`).
- `bin/`: Compiled binaries.
- `epics/`: Requirements and planning docs.
- `.vscode/`: Editor settings.

