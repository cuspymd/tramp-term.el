#!/bin/bash

# Generate SSH key for testing
KEY_PATH="$HOME/.ssh/tramp-term-test"

if [ ! -f "$KEY_PATH" ]; then
    echo "Generating SSH key for tramp-term testing..."
    ssh-keygen -t rsa -b 2048 -f "$KEY_PATH" -N "" -C "tramp-term-test"
fi

# Copy public key to project root for Docker build context
cp "${KEY_PATH}.pub" test_key.pub

echo "SSH key setup complete. Public key copied to test_key.pub"
echo "Private key location: $KEY_PATH"