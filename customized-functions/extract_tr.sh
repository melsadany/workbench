#!/bin/bash

# Check if the input file path is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 path_to_json_file"
    exit 1
fi

# Input JSON file path
json_file="$1"

# Extract the RepetitionTime (TR) using jq
tr_value=$(jq -r '.RepetitionTime' "$json_file")

# Check if jq was successful
if [ $? -eq 0 ]; then
    echo "$tr_value"
else
    echo "An error occurred while extracting the Repetition Time (TR)." >&2
    exit 1
fi
