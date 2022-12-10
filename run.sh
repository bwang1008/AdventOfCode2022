#!/usr/bin/bash

# Run the last modified .ml file in this directory

# get all files that end in ".ml" in this directory, sort by last modified time, get first one
last_modified_file=$(ls -t *.ml | head -1)
echo "Running $last_modified_file"
ocaml $last_modified_file

