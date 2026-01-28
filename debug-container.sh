#!/bin/bash
# Script to debug Docker container file structure
# Usage: 
#   docker run -it --rm <image-name> /bin/bash
#   Then run the commands below manually, OR
#   docker exec -it <container-id> /bin/bash

echo "========================================"
echo "=== CONTAINER FILE STRUCTURE DEBUG ==="
echo "========================================"
echo ""

echo "=== Current Working Directory ==="
pwd
echo ""

echo "=== Environment Variables ==="
env | grep -E "(HOME|PWD|PATH|SHINY)" || echo "No Shiny-related env vars found"
echo ""

echo "=== Files in /home/shiny-app ==="
ls -la /home/shiny-app
echo ""

echo "=== Checking for app.R ==="
if [ -f "/home/shiny-app/app.R" ]; then
    echo "✓ app.R found at /home/shiny-app/app.R"
    echo "File size: $(stat -c%s /home/shiny-app/app.R) bytes"
    echo "First 5 lines:"
    head -5 /home/shiny-app/app.R
else
    echo "✗ ERROR: app.R NOT FOUND at /home/shiny-app/app.R"
    echo "Searching for app.R in container..."
    find / -name "app.R" 2>/dev/null | head -10 || echo "No app.R found anywhere"
fi
echo ""

echo "=== Directory structure (top 3 levels) ==="
find /home/shiny-app -maxdepth 3 -type d | head -30
echo ""

echo "=== All .R files found ==="
find /home/shiny-app -type f -name "*.R" | head -20
echo ""

echo "=== www directory contents ==="
if [ -d "/home/shiny-app/www" ]; then
    echo "✓ www directory found"
    ls -la /home/shiny-app/www | head -20
else
    echo "✗ ERROR: www directory NOT FOUND"
fi
echo ""

echo "=== data directory contents ==="
if [ -d "/home/shiny-app/data" ]; then
    echo "✓ data directory found"
    ls -la /home/shiny-app/data | head -20
else
    echo "✗ ERROR: data directory NOT FOUND"
fi
echo ""

echo "=== Full directory tree (limited) ==="
if command -v tree &> /dev/null; then
    tree -L 3 /home/shiny-app || find /home/shiny-app -maxdepth 3 -print | sed 's|[^/]*/| |g'
else
    find /home/shiny-app -maxdepth 3 -print | sed 's|[^/]*/| |g'
fi
echo ""

echo "=== Checking if /home/shiny-app/aqbat exists ==="
if [ -d "/home/shiny-app/aqbat" ]; then
    echo "⚠ WARNING: /home/shiny-app/aqbat directory exists!"
    echo "Contents:"
    ls -la /home/shiny-app/aqbat | head -10
    echo ""
    echo "Checking if app.R is in /home/shiny-app/aqbat:"
    if [ -f "/home/shiny-app/aqbat/app.R" ]; then
        echo "✓ app.R found at /home/shiny-app/aqbat/app.R"
    else
        echo "✗ app.R NOT in /home/shiny-app/aqbat"
    fi
else
    echo "✓ /home/shiny-app/aqbat does not exist (this is correct)"
fi
echo ""

echo "=== Process list ==="
ps aux | head -10
echo ""

echo "========================================"
echo "=== DEBUG COMPLETE ==="
echo "========================================"
