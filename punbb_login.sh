#!/bin/bash

# Usage: ./punbb_login.sh USERNAME PASSWORD
USERNAME="$1"
PASSWORD="$2"

if [ -z "$USERNAME" ] || [ -z "$PASSWORD" ]; then
    echo "Usage: $0 USERNAME PASSWORD" >&2
    exit 1
fi

BASE_URL="https://gamestories.clanboard.ru"
LOGIN_PAGE="$BASE_URL/login.php"
LOGIN_POST="$BASE_URL/login.php?action=in"

COOKIE_JAR=$(mktemp)
TEMP_FILE=$(mktemp)

# Clean up on exit
trap 'rm -f "$COOKIE_JAR" "$TEMP_FILE"' EXIT

# Step 1: GET login page to obtain session cookie and hidden fields
curl -k -s -c "$COOKIE_JAR" -o "$TEMP_FILE" \
    -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36" \
    "$LOGIN_PAGE"

# Extract hidden fields
FORM_SENT=$(grep -oP 'name="form_sent"\s+value="\K[^"]+' "$TEMP_FILE" | head -1)
REFERER=$(grep -oP 'name="referer"\s+value="\K[^"]+' "$TEMP_FILE" | head -1)
FORM_SENT="${FORM_SENT:-1}"
REFERER="${REFERER:-/}"

# Step 2: POST login credentials
curl -k -L -b "$COOKIE_JAR" -c "$COOKIE_JAR" -s \
    -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -H "Referer: $LOGIN_PAGE" \
    --data-urlencode "req_username=$USERNAME" \
    --data-urlencode "req_password=$PASSWORD" \
    --data-urlencode "form_sent=$FORM_SENT" \
    --data-urlencode "referer=$REFERER" \
    --data-urlencode "savepassword=1" \
    --data-urlencode "login=Login" \
    "$LOGIN_POST" -o /dev/null

# Step 3: Output the final cookie jar (Netscape format) to stdout
cat "$COOKIE_JAR"
