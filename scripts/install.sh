#!/bin/bash

# Make sure we are in the scripts directory
cd "$(dirname ${BASH_SOURCE[0]})"

# Stop the crik-server if it is currently running
sudo systemctl stop crik.service

# Build the server
stack install

# Install the server
sudo cp ~/.local/bin/crik-server /usr/bin/

# If this is a clean install make the crik user
if [ ! `id -u crik 2>/dev/null || echo -1` -ge 0 ]; then
# Add the `crik` user
sudo adduser --system --no-create-home --group crik
fi

# Install the service definition 
sudo cp ./crik.service /etc/systemd/system/

# If this is a clean install create config
if [ ! -d /etc/systemd/system/crik.service.d ]; then
# Make a config directory
sudo mkdir /etc/systemd/system/crik.service.d

# Copy default config
sudo cp ./crik.conf /etc/systemd/system/crik.service.d 
fi

# Reload systemd so it picks up crik
sudo systemctl daemon-reload

# Start crik
sudo systemctl start crik.service
