#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#musicPath=/mnt/usbStick/Music
folder=$SCRATCHFOLDER/                               # Where the file things are (that you want copying)

mp3Convert() {
    mp3File=$(echo $1 | sed -e 's/m4a/mp3/')
    echo "Converting $(basename $1) to $(basename $mp3File)"
    avconv -v quiet -i $1 -ab 320k -ac 2 -ar 44100  $mp3File
    echo "Conversion successful!"
    rm $1
}

flacConvert() {
  flacFile=$(echo $1 | sed -e 's/m4a/flac/')
  echo "Converting $(basename $1) to $(basename $flacFile)"
  avconv -v quiet -i $1 -f flac  $flacFile
  echo "Conversion successful!"
  rm $1
}

read -p "Convert to [1]: mp3 or [2]: flac" -n 1 -r
echo -e "$REPLY\n"

find $folder -name '*.m4a' | while read line; do
  if [ "$REPLY" == "1" ]; then
    mp3Convert $line
  elif [ "$REPLY" == "2" ]; then
    flacConvert $line
  fi
  echo -e "Next!\n"
done

IFS=$SAVEIFS

# After writing this I'm not sure if sed's my favourite or least favourite function.
