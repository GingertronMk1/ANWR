#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

musicPath=/Users/Jack/Desktop/Scratch
#musicPath=/mnt/usbStick/Music
#musicPath=/Volumes/SD\ Card/Other
#files=$musicPath/*                               # Where the file things are (that you want copying)
#destFolder=/Volumes/SD\ CARD/MP3s
destFolder=$musicPath
files=$(find /Users/Jack/Desktop/Scratch -type f)

stripQuery() {                                                                          # Uses `sed` to strip the part of the metadate I don't want,
    echo $(echo $(avprobe $1 2>&1 | grep $2) | sed -e "s/$2//") | sed -e "s/[:\/]/_/"   # as well as replacing the characters that might make folder
}                                                                                       # handling a PITA with underscores

convert() {
    if [ "${1: -4}" == ".m4a" ]; then
        local m4aFile=$1
        outFile=$(echo $m4aFile | sed -e 's/m4a/flac/')       # Make a filename for the mp3 version
        echo "Converting $(basename $m4aFile) to $(basename $outFile)"
        avconv -v quiet -i $m4aFile -f flac $outFile   # Convert from m4a to 320kbps mp3
        echo "Conversion successful!"
        #rm $m4aFile
    fi
}

tidy(){
  if [ 1 ]; then
    #if [ "${1: -4}" == ".mp3" ]; then
        local outFile=$1
        mp3Base=$(basename $outFile)
        echo "Tidying $mp3Base"
        albumQuery="    ALBUM           : "             # Queries for grepping the output of avprobe
        albumArtistQuery="    album_artist    : "

        album=$(stripQuery "$outFile" "$albumQuery")
        albumArtist=$(stripQuery "$outFile" "$albumArtistQuery")

        albumFolder="$destFolder/$albumArtist/$album"

        if [ ! -e $albumFolder ]; then          # If a folder doesn't exist for that album:
            mkdir -p $albumFolder               # Make it. Make it using -p so it also makes the folder for album artist if needs be
            echo "Creating folder for $album"
        fi
        echo "Moving $mp3Base to $albumFolder"
        mv $outFile $albumFolder/$mp3Base   # Move the file from the base directory into the requisite folder
        echo "Moving successful!"
    else
        echo "$(basename $1) is not an MP3, skipping..."
    fi
}

convertAndTidy(){
    tidyFile=$(echo $1 | sed -e 's/m4a/mp3/')
    convert $1
    tidy $tidyFile
    echo "$(ls $musicPath | wc -l) remaining, $(find $destFolder -type f | wc -l) in destination folder"
}

read -p "Do you want to [c]onvert, [t]idy, or [B]oth? [c/t/B]" -n 1 -r          # Convert, copy or both? I can do all three
echo -e "\n"                                                                    # Both is the default

if [ "$REPLY" == "c" ]; then
    thingToDo=convert
elif [ "$REPLY" == "t" ]; then
    thingToDo=tidy
else
    thingToDo=convertAndTidy
fi

for musicFile in $files; do
    $thingToDo $musicFile
    echo -e "Next!\n"
done

IFS=$SAVEIFS

# After writing this I'm not sure if sed's my favourite or least favourite function.
