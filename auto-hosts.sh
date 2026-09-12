#!/usr/bin/env sh

HOSTSFILE="${1:-$(pwd)/hosts}"
SITESDIR="${2:-$HOME/Sites}"

echo "$HOSTSFILE"
cat "$HOSTSFILE"

opener="# LINES CREATED BY AUTO-HOST FILE"
closer="# END LINES CREATED BY AUTO-HOST"

containsOpener=$(grep -c "$opener" "$HOSTSFILE")
containsCloser=$(grep -c "$closer" "$HOSTSFILE")

if [ $containsOpener -eq 0 ]; then
  echo "$containsOpener\n" >> "$HOSTSFILE"
fi

process () {
  $fileName = "$1"
  echo "$fileName"

}

find "$SITESDIR" -maxdepth 1 -mindepth 1 -type d -exec sh -c "
hostRow=\"127.0.0.1 $(basename {}).local\";
echo \"$hostRow\";
contains=$(grep -c \"$hostRow\" \"$HOSTSFILE\")
if [ $contains -eq 0 ]; then
  echo \"$hostRow\" >> \"$HOSTSFILE\"
fi
" \;

