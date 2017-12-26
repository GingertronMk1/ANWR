#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
set -m

folder=$SCRATCHFOLDER   # Where the file things are (that you want copying)

count=0
files=$(find "/Users/Jack/Music/Music" -name "*.flac" | sort)
total=$(find "/Users/Jack/Music/Music" -name "*.flac" | wc -l)

mp3Convert() {
  baseFile=$1
  newFile=$(echo $line | sed -e 's/flac/mp3/g')
  newFile2=$SCRATCHFOLDER$(echo $newFile | sed -e 's/\/Users\/Jack\/Music\/Music//g')
  mkdir -p $(dirname $newFile2)
  echo $newFile2
  avconv -v quiet -i $baseFile -ab 320k -ac 2 -ar 44100 $newFile2 -y
}

for line in $files; do
  while [ $(jobs | wc -l) -ge 4 ] ; do sleep 1 ; done
  count=$((count+1))
  echo "$count/$total"
  mp3Convert $line &
done

wait

echo "Done!"

IFS=$SAVEIFS

# After writing this I'm not sure if sed's my favourite or least favourite function.
