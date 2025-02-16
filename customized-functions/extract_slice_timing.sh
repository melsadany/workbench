#!/bin/bash

# Check if the input and output file paths are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 path_to_json_file output_file_path"
    exit 1
fi

# Input JSON file path
json_file="$1"

# Output text file path
output_file="$2"

# Extract the slice timing information and format it
jq -r '.SliceTiming[]' "$json_file" > "$output_file"

# Confirm the output
if [ $? -eq 0 ]; then
    echo "Slice timing information has been saved to $output_file."
else
    echo "An error occurred while extracting slice timing information."
fi
