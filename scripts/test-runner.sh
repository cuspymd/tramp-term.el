#!/bin/bash

# Test runner script for tramp-term shell compatibility

set -e

echo "=== tramp-term Shell Testing ==="

# Setup SSH keys if not exists
if [ ! -f "$HOME/.ssh/tramp-term-test" ]; then
    echo "Setting up SSH keys..."
    ./docker/setup-keys.sh
fi

KEY_PATH="$HOME/.ssh/tramp-term-test"

# Function to test shell connection
test_shell() {
    local shell=$1
    local port=$2
    
    echo "Testing $shell shell (port $port)..."
    
    # Check if container is running
    if ! docker ps | grep -q "tramp-term-$shell"; then
        echo "❌ Container tramp-term-$shell is not running"
        return 1
    fi
    
    # Test SSH connection
    if timeout 10 ssh -i "$KEY_PATH" -o ConnectTimeout=5 -o StrictHostKeyChecking=no \
        -p $port testuser@localhost "echo 'SSH connection successful'" 2>/dev/null; then
        echo "✅ SSH connection to $shell container successful"
    else
        echo "❌ SSH connection to $shell container failed"
        return 1
    fi
    
    # Verify clean shell environment (no pre-configured AnSiT)
    case $shell in
        bash)
            if ssh -i "$KEY_PATH" -o StrictHostKeyChecking=no -p $port testuser@localhost \
                'echo $PROMPT_COMMAND' 2>/dev/null | grep -q "AnSiT"; then
                echo "❌ Bash PROMPT_COMMAND already configured (should be clean)"
            else
                echo "✅ Bash environment clean (ready for tramp-term)"
            fi
            ;;
        zsh)
            if ssh -i "$KEY_PATH" -o StrictHostKeyChecking=no -p $port testuser@localhost \
                'test -f ~/.zshrc && grep precmd ~/.zshrc' 2>/dev/null | grep -q "AnSiT"; then
                echo "❌ Zsh precmd already configured (should be clean)"
            else
                echo "✅ Zsh environment clean (ready for tramp-term)"
            fi
            ;;
        fish)
            if ssh -i "$KEY_PATH" -o StrictHostKeyChecking=no -p $port testuser@localhost \
                'test -f ~/.config/fish/config.fish && cat ~/.config/fish/config.fish' 2>/dev/null | grep -q "AnSiT"; then
                echo "❌ Fish prompt already configured (should be clean)"
            else
                echo "✅ Fish environment clean (ready for tramp-term)"
            fi
            ;;
        tcsh)
            if ssh -i "$KEY_PATH" -o StrictHostKeyChecking=no -p $port testuser@localhost \
                'test -f ~/.tcshrc && grep precmd ~/.tcshrc' 2>/dev/null | grep -q "AnSiT"; then
                echo "❌ Tcsh precmd already configured (should be clean)"
            else
                echo "✅ Tcsh environment clean (ready for tramp-term)"
            fi
            ;;
    esac
    
    echo ""
}

# Start containers if not running
echo "Starting Docker containers..."
docker compose up -d

# Wait for containers to be ready
echo "Waiting for containers to be ready..."
sleep 10

# Test each shell
test_shell "bash" 2225
test_shell "zsh" 2222
test_shell "tcsh" 2223
test_shell "fish" 2224

echo "=== Manual Testing Instructions ==="
echo "1. In Emacs, run: M-x tramp-term"
echo "2. Choose host: tramp-zsh, tramp-tcsh, tramp-fish, or tramp-bash"
echo "3. Test directory tracking: cd test-files"
echo "4. Verify TRAMP integration: C-x C-f sample.txt"
echo ""
echo "To stop containers: docker compose down"