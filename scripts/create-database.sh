#!/bin/bash

# Make sure we are in the scripts directory
cd "$(dirname ${BASH_SOURCE[0]})"

# Create the user and database
sudo -u postgres createuser crik --pwprompt
sudo -u postgres createdb crik --owner crik

# Add the schema
sudo -u postgres psql --host=localhost --username crik -f ./schema.sql
