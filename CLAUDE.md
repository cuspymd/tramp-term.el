# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

tramp-term.el is an Emacs Lisp package that provides automatic setup of directory tracking in SSH sessions. It creates ansi-term buffers with SSH connections that automatically configure TRAMP integration for seamless remote file editing.

## Architecture

**Core Components:**
- `tramp-term.el` - Single-file package containing all functionality
- Main entry point: `tramp-term` interactive function
- SSH connection handling with automatic prompt detection and response
- Multi-shell support with auto-detection and per-host configuration

**Key Functions:**
- `tramp-term`: Main interactive command to establish SSH connection
- `tramp-term--do-ssh-login`: Handles SSH authentication flow
- `tramp-term--initialize`: Dispatches to shell-specific initialization functions
- `tramp-term--detect-shell`: Auto-detects remote shell using `$0` variable
- `tramp-term--select-host`: Provides host completion from ~/.ssh/config

**Shell Architecture:**
- Shell-specific initialization functions: `tramp-term--initialize-bash`, `tramp-term--initialize-zsh`, `tramp-term--initialize-tcsh`
- Per-shell AnSiT escape sequence injection for directory tracking
- Host-specific shell caching via `tramp-term-host-shells` customization variable

**Dependencies:**
- Built-in Emacs packages: `term` and `tramp`
- Supports bash, zsh, and tcsh shells on remote hosts
- Uses ~/.ssh/config for host completion

## Development Commands

**Testing:**
```bash
# Start test containers with different shells
docker compose up -d

# Run shell compatibility tests
./scripts/test-runner.sh

# Manual testing in Emacs
M-x tramp-term RET tramp-zsh RET
```

**Package Distribution:**
- Only `tramp-term.el` is distributed via MELPA/ELPA
- Test infrastructure and documentation remain in repository only

## Shell Compatibility

**Supported Shells:**
- **Bash**: Uses PROMPT_COMMAND for directory tracking
- **Zsh**: Uses precmd_functions array
- **Tcsh**: Uses aliases for cd/pushd/popd with `/bin/echo -e` for escape sequences

**Auto-Detection Logic:**
- `tramp-term--detect-shell` uses `echo "$MARKER$0"` to identify shell
- Results cached in `tramp-term-host-shells` per hostname
- Falls back to user prompt if detection fails

**Configuration System:**
- `tramp-term-default-shell`: Global default ('bash, 'zsh, 'tcsh, or 'auto)
- `tramp-term-host-shells`: Alist of (hostname . shell-type) pairs
- Automatic persistence via `customize-save-variable`

## Testing Architecture

**Docker Test Environment:**
- Separate containers for bash, zsh, tcsh shells
- SSH key-based authentication via generated test keys
- Clean shell environments for testing auto-detection
- Port mapping: zsh(2222), tcsh(2223), bash(2225)

**Test Verification Points:**
- Shell auto-detection accuracy
- AnSiT escape sequence injection
- Directory tracking functionality
- TRAMP integration with `default-directory` updates