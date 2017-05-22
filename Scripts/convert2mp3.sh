#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#musicPath=/mnt/usbStick/Music
files=$SCRATCHFOLDER/*                               # Where the file things are (that you want copying)

convert() {
  if [ "${1: -4}" == ".m4a" ]; then
    local m4aFile=$1
    mp3File=$(echo $m4aFile | sed -e 's/m4a/mp3/')       # Make a filename for the mp3 version
    echo "Converting $(basename $m4aFile) to $(basename $mp3File)"
    avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File   # Convert from m4a to 320kbps mp3
    echo "Conversion successful!"
    rm $m4aFile
  fi
}

for musicFile in $files; do
  convert $musicFile
  echo -e "Next!\n"
done

IFS=$SAVEIFS

# After writing this I'm not sure if sed's my favourite or least favourite function.
