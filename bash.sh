BIN_PATH="target/x86_64-unknown-linux-musl/release/mithril-client"


if [ ! -f "$BIN_PATH" ]; then
    echo "❌ Binary not found!"
    exit 1
fi

if readelf -d "$BIN_PATH" 2>/dev/null | grep -q NEEDED; then
    echo "❌ $BIN is NOT static (has dynamic dependencies)"
    exit 1
else
    echo "✅ $BIN is fully static"
fi

echo "🎉 All binaries are static!"
