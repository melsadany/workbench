#!/bin/bash

usage() {
    echo "Usage: $0 -s SOURCE_DIR -d DEST_DIR [-m MIN_SIZE_MB] [-i] [-x] [-n]"
    echo "  -s: Source directory path"
    echo "  -d: Destination directory path"
    echo "  -m: Minimum file size in MB (default: 500)"
    echo "  -i: Interactive mode (will add -i flag to rm commands)"
    echo "  -x: Use xargs for efficient batch deletion"
    echo "  -n: Dry run - only print files that would be deleted"
    exit 1
}

while getopts "s:d:m:ixn" opt; do
    case $opt in
        s) SOURCE_DIR="$OPTARG";;
        d) DEST_DIR="$OPTARG";;
        m) MIN_SIZE="$OPTARG";;
        i) INTERACTIVE=1;;
        x) USE_XARGS=1;;
        n) DRY_RUN=1;;
        *) usage;;
    esac
done

if [ -z "$SOURCE_DIR" ] || [ -z "$DEST_DIR" ]; then
    usage
fi

MIN_SIZE=${MIN_SIZE:-500}
SIZE_BYTES=$((MIN_SIZE * 1024 * 1024))

SOURCE_DIR=$(readlink -f "$SOURCE_DIR")
DEST_DIR=$(readlink -f "$DEST_DIR")

if [ ! -d "$SOURCE_DIR" ] || [ ! -d "$DEST_DIR" ]; then
    echo "Error: Source and destination must be existing directories"
    exit 1
fi

TEMP_DIR=$(mktemp -d)
echo "Temporary directory created at: $TEMP_DIR"

echo "Comparing files using rsync..."
echo "Source: $SOURCE_DIR"
echo "Destination: $DEST_DIR"
echo "Minimum size: ${MIN_SIZE}MB"

# Store raw rsync output first
rsync -rninv --existing --ignore-times --min-size="${MIN_SIZE}M" \
    --itemize-changes "$SOURCE_DIR/" "$DEST_DIR/" > "$TEMP_DIR/rsync_raw.txt"

# Extract matching files, properly stripped of rsync flags
grep '^>f.' "$TEMP_DIR/rsync_raw.txt" | sed 's/^>f..T...... //' > "$TEMP_DIR/matching_files.txt"

echo "Number of lines in raw rsync output: $(wc -l < "$TEMP_DIR/rsync_raw.txt")"
echo "Sample of raw rsync output:"
head -n 5 "$TEMP_DIR/rsync_raw.txt"
echo
echo "Number of lines in matching_files.txt: $(wc -l < "$TEMP_DIR/matching_files.txt")"
echo "Sample of matching files (after stripping rsync flags):"
head -n 5 "$TEMP_DIR/matching_files.txt"

MATCHED_COUNT=$(wc -l < "$TEMP_DIR/matching_files.txt")
echo "Found $MATCHED_COUNT files that exist in both locations"

if [ "$MATCHED_COUNT" -eq 0 ]; then
    echo "No matching files found to delete."
    exit 0
fi

# Generate list of files to delete with full paths, properly single-quoted
echo "Generating list of files to delete..."
cd "$SOURCE_DIR" || exit 1
while IFS= read -r file; do
    if [ -f "$file" ]; then
        echo "Found file: $file"
        echo "'$SOURCE_DIR/$file'" >> "$TEMP_DIR/files_to_delete.txt"
    else
        echo "File not found: $file"
    fi
done < "$TEMP_DIR/matching_files.txt"

echo "Number of files in delete list: $(wc -l < "$TEMP_DIR/files_to_delete.txt")"
echo "Sample of files to delete:"
head -n 5 "$TEMP_DIR/files_to_delete.txt"

# Calculate space savings
echo "Calculating space savings..."
TOTAL_BYTES=0
while IFS= read -r file; do
    # Remove the single quotes for the stat command
    file=$(echo "$file" | sed "s/^'//;s/'$//")
    if [ -f "$file" ]; then
        FILE_SIZE=$(stat --format=%s "$file")
        TOTAL_BYTES=$((TOTAL_BYTES + FILE_SIZE))
        echo "Processing file: $file"
        echo "Size: $FILE_SIZE bytes"
        echo "Running total: $TOTAL_BYTES bytes"
    else
        echo "File not found: $file"
    fi
done < "$TEMP_DIR/files_to_delete.txt"

TOTAL_GB=$(echo "scale=2; $TOTAL_BYTES / 1024 / 1024 / 1024" | bc)

echo
echo "Summary:"
echo "- Number of files identified: $MATCHED_COUNT"
echo "- Potential space savings: ${TOTAL_GB}GB"
echo

if [ "$DRY_RUN" = "1" ]; then
    echo "DRY RUN - The following files would be deleted:"
    echo "----------------------------------------"
    cat "$TEMP_DIR/files_to_delete.txt"
    echo "----------------------------------------"
    echo "No files were actually deleted"
    echo "Temporary files preserved in: $TEMP_DIR"
    exit 0
fi

# Generate deletion script
echo "#!/bin/bash" > "$TEMP_DIR/delete_script.sh"
echo "cd \"$SOURCE_DIR\"" >> "$TEMP_DIR/delete_script.sh"

if [ "$USE_XARGS" = "1" ]; then
    if [ "$INTERACTIVE" = "1" ]; then
        echo "while IFS= read -r file; do rm -i \$file; done < \"$TEMP_DIR/files_to_delete.txt\"" >> "$TEMP_DIR/delete_script.sh"
    else
        echo "cat \"$TEMP_DIR/files_to_delete.txt\" | xargs -r -P 8 -n 100 rm" >> "$TEMP_DIR/delete_script.sh"
    fi
else
    while IFS= read -r file; do
        echo "rm ${INTERACTIVE:+-i} $file" >> "$TEMP_DIR/delete_script.sh"
    done < "$TEMP_DIR/files_to_delete.txt"
fi

chmod +x "$TEMP_DIR/delete_script.sh"

echo "Deletion script generated at: $TEMP_DIR/delete_script.sh"
echo
echo "Next steps:"
echo "1. Review the script: less $TEMP_DIR/delete_script.sh"
echo "2. Test the script: bash -n $TEMP_DIR/delete_script.sh"
echo "3. Run the script: bash $TEMP_DIR/delete_script.sh"
echo
echo "Note: Temporary directory preserved at: $TEMP_DIR"
