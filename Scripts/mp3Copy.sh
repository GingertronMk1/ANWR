#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

musicPath=$MUSIC

files=$musicPath/*                               # Where the file things are (that you want copying)
destFolder=/Volumes/SD\ CARD
#destFolder=/Users/Jack/Desktop/Other

musicFiles=$(find $musicPath/ -name "*.m4a")
destFiles=$(find $destFolder -name "*.mp3")

stripQuery() {
  echo $(echo $(avprobe $1 2>&1 | grep $2) | sed -e "s/$2//") | sed -e "s/[:\/]/_/"
}

for file in $musicFiles; do
  albumQuery="    album           : "             # Queries for grepping the output of avprobe
  albumArtistQuery="    album_artist    : "

  album=$(stripQuery "$file" "$albumQuery")       # Take the output of avprobe and get just the album
  albumArtist=$(stripQuery "$file" "$albumArtistQuery") # Get just the album artist

  albumFolder="$destFolder/$albumArtist/$album"         # Building the name of the album folder
  mp3Name=$(echo $(basename $file) | sed -e 's/m4a/mp3/') # Generating the name of the mp3 version
  mp3File=$albumFolder/$mp3Name       # Make a filename and path for the mp3 version

  if [ ! -e $albumFolder ]
  then          # If a folder doesn't exist for that album:
    echo "$albumFolder doesn't exist. Creating..."
    mkdir -p $albumFolder               # Make it. Make it using -p so it also makes the folder for album artist if needs be
    echo "$albumFolder created, creating $mp3Name there..."
    avconv -v quiet -i $file -ab 320k -ac 2 -ar 44100 $mp3File  # Then put the file there
    echo "$mp3Name created in place."
  elif [ ! -e $mp3File ]
  then
    echo "$albumFolder exists, $mp3Name doesn't. Creating..."   # If the folder exists but the file doesn't, just make the file
    avconv -v quiet -i $file -ab 320k -ac 2 -ar 44100 $mp3File
    echo "$mp3Name created in place."
  else
    echo "Skipping $mp3Name; it already exists in place."    # Otherwise do nothing and move on
  fi
  echo "Next file..."
done

IFS=$SAVEIFS
