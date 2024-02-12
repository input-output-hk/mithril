FILENAME=$1

if [ ! -e "$FILENAME" ]; then
    echo "Error: File '$FILENAME' not found."
    exit 1
fi

echo "Parse headless test results from file: '$FILENAME'"
if grep -q 'title="FAILED"' "$FILENAME"; then
    FAILED_INFO=$(grep -oE '<div id="[^"]+" title="FAILED">([^<]+)' "$FILENAME" | awk 'NR==1 {print substr($0, index($0,$4))}')
    echo "$FAILED_INFO"
    exit 1
elif grep -q 'title="OK"' "$FILENAME"; then
    grep -oE '<div id="[^"]+" title="OK">([^<]+)' "$FILENAME" | awk '{print substr($0, index($0,$4))}'
    echo "Success: all tests passed."
else
    cat "$FILENAME"
    echo "No test results found. Check '$FILENAME' output."
    exit 1
fi
