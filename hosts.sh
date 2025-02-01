#!/usr/bin/env sh

HOSTSFILE="./hosts.local"

for f in $HOME/Git/*/
do
  NEWLINE="127.0.0.1 $(basename $f).local"
  if ! grep -q "^$NEWLINE$" "$HOSTSFILE"
  then
    echo "Adding $(basename $f) to $HOSTSFILE"
    echo "$NEWLINE" >> "$HOSTSFILE"
  else
    echo "$(basename $f) already exists in $HOSTSFILE"
  fi
done
