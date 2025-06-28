FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    openssh-server \
    tcsh \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Setup SSH
RUN mkdir /var/run/sshd
RUN echo 'root:root' | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -i 's/#PasswordAuthentication yes/PasswordAuthentication yes/' /etc/ssh/sshd_config
RUN sed -i 's/#PubkeyAuthentication yes/PubkeyAuthentication yes/' /etc/ssh/sshd_config

# Create test user
RUN useradd -m -s /bin/tcsh testuser
RUN echo 'testuser:testpass' | chpasswd
RUN usermod -aG sudo testuser

# Setup SSH keys
RUN mkdir -p /home/testuser/.ssh
COPY test_key.pub /home/testuser/.ssh/authorized_keys
RUN chown -R testuser:testuser /home/testuser/.ssh
RUN chmod 700 /home/testuser/.ssh
RUN chmod 600 /home/testuser/.ssh/authorized_keys

# Clean tcsh environment for testuser
USER testuser
WORKDIR /home/testuser

USER root
EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]