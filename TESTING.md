# Shell Testing Guide for tramp-term.el

## Quick Start

1. **Start test containers:**
   ```bash
   docker compose up -d
   ```

2. **Setup SSH keys and config:**
   ```bash
   # Generate test SSH key
   ./docker/setup-keys.sh
   
   # Add SSH config entries
   cat >> ~/.ssh/config << 'EOF'
   Host tramp-zsh
       HostName localhost
       Port 2222
       User testuser
       IdentityFile ~/.ssh/tramp-term-test
       StrictHostKeyChecking no
       UserKnownHostsFile /dev/null

   Host tramp-tcsh
       HostName localhost
       Port 2223
       User testuser
       IdentityFile ~/.ssh/tramp-term-test
       StrictHostKeyChecking no
       UserKnownHostsFile /dev/null

   Host tramp-fish
       HostName localhost
       Port 2224
       User testuser
       IdentityFile ~/.ssh/tramp-term-test
       StrictHostKeyChecking no
       UserKnownHostsFile /dev/null

   Host tramp-bash
       HostName localhost
       Port 2225
       User testuser
       IdentityFile ~/.ssh/tramp-term-test
       StrictHostKeyChecking no
       UserKnownHostsFile /dev/null
   EOF
   ```

3. **Test in Emacs:**
   ```elisp
   M-x tramp-term RET tramp-zsh RET
   ```
   (No password required with SSH key authentication)

## Testing Procedures

### Basic Functionality Test

For each shell (zsh, tcsh, fish, bash):

1. **Pre-check:** Verify clean environment with `./scripts/test-runner.sh`
2. **Connect:** `M-x tramp-term RET tramp-{shell} RET`
3. **Verify injection:** Look for shell configuration being applied
4. **Navigate:** `cd test-files` 
5. **Verify tracking:** Check that `default-directory` updates in Emacs
6. **Test TRAMP:** `C-x C-f` should show correct remote path
7. **Create/edit files:** Test file operations work seamlessly

### Shell-Specific Tests

#### ZSH (Port 2222)
- Test oh-my-zsh compatibility if installed
- Verify precmd function doesn't conflict
- Test completion and prompt customization

#### TCSH (Port 2223)  
- Test alias-based tracking works
- Verify prompt formatting
- Test variable expansion in aliases

#### Fish (Port 2224)
- Test function-based prompt
- Verify fish-specific syntax works
- Test completion system compatibility

#### Bash (Port 2225)
- Baseline comparison for other shells
- Test standard PROMPT_COMMAND approach

### Advanced Testing

1. **Multiple sessions:** Open multiple terminals to same host
2. **Directory changes:** Test nested directories and symlinks
3. **Error handling:** Test with invalid paths
4. **Reconnection:** Test after network interruption
5. **File operations:** Create, edit, save files via TRAMP

## Expected Behavior

**Before tramp-term connection:**
- Clean shell environments with no AnSiT sequences
- Default shell prompts and configurations

**After tramp-term connection:**
- `tramp-term--initialize` should inject appropriate shell configuration
- Directory changes reflected in Emacs `default-directory`
- TRAMP file operations use correct remote paths
- AnSiT escape sequences visible in terminal output

**Common issues:**
- Shell configuration injection may fail for unsupported shells
- Prompt detection may need adjustment for custom prompts
- Shell-specific syntax variations in escape sequences

## Debugging

1. **Check AnSiT sequences:** Look for `\033AnSiTc` in terminal output
2. **Monitor variables:** Check `default-directory` value
3. **TRAMP debug:** `(setq tramp-verbose 6)` for detailed logging

## Cleanup

```bash
docker compose down
docker system prune -f
```