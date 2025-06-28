# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

tramp-term.el is an Emacs Lisp package that provides automatic setup of directory tracking in SSH sessions. It creates ansi-term buffers with SSH connections that automatically configure TRAMP integration for seamless remote file editing.

## Architecture

**Core Components:**
- `tramp-term.el` - Single-file package containing all functionality
- Main entry point: `tramp-term` interactive function
- SSH connection handling with automatic prompt detection and response
- TRAMP integration via bash shell configuration on remote hosts

**Key Functions:**
- `tramp-term`: Main interactive command to establish SSH connection
- `tramp-term--do-ssh-login`: Handles SSH authentication flow
- `tramp-term--initialize`: Configures bash shell for directory tracking
- `tramp-term--select-host`: Provides host completion from ~/.ssh/config

**Dependencies:**
- Built-in Emacs packages: `term` and `tramp`
- Requires bash shell on remote hosts
- Uses ~/.ssh/config for host completion

## Development Workflow

**Package Structure:**
- Single .el file with proper Emacs Lisp package headers
- Available on MELPA package repository
- Uses `;;;###autoload` for main entry point

**Key Design Patterns:**
- Interactive prompts with host completion
- Async SSH connection handling with regex-based prompt detection
- Shell command injection for TRAMP setup via `term-send-raw-string`
- Hook system via `tramp-term-after-initialized-hook`

**Testing and Validation:**
- Manual testing with various SSH configurations
- Test with different remote hosts and authentication methods
- Verify directory tracking works with TRAMP file operations

## Shell Compatibility

**Current Support:**
- Bash shells only on remote hosts
- Directory tracking via PROMPT_COMMAND and AnSiT escape sequences

**Extension Points:**
- `tramp-term--initialize` function can be modified for other shells
- Example tcsh implementation provided in README.md
- Would require per-host shell configuration for full generalization