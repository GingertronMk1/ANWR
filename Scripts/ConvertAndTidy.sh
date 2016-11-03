#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

musicPath=/Users/Jack/Desktop/Scratch
files=$musicPath/*                           # Where the file things are (that you want copying)

convert() {
    if [ "${1: -4}" == ".m4a" ]; then
        local m4aFile=$1
        mp3File=$(echo $m4aFile | sed -e 's/m4a/mp3/')       # Make a filename for the mp3 version
        echo "Converting $m4aFile to $mp3File"
        avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File   # Convert from m4a to 320kbps mp3
        echo "Conversion successful!"
        rm $m4aFile
    fi
}

tidy(){
    if [ "${1: -4}" == ".mp3" ]; then
        local mp3File=$1
        mp3Base=$(basename $mp3File)
        echo "Tidying $mp3Base"
        albumQuery="    album           : "             # Queries for grepping the output of avprobe
        albumArtistQuery="    album_artist    : "

        # This bit takes the metadata outputted by avprobe and filters it to album and album artist, respectively
        # The two "sed"s remove the bit of the line that's not the bit I want and replace : with _, as folders and : don't mix
        album=$(echo $(echo $(avprobe $mp3File 2>&1 | grep $albumQuery) | sed -e "s/$albumQuery//") | sed -e "s/[:\/]/_/")
        albumArtist=$(echo $(echo $(avprobe $mp3File 2>&1 | grep $albumArtistQuery) | sed -e "s/$albumArtistQuery//") | sed -e "s/[:\/]/_/")

        albumFolder="$musicPath/$albumArtist/$album"

        if [ ! -e $albumFolder ]; then          # If a folder doesn't exist for that album:
            mkdir -p $albumFolder               # Make it. Make it using -p so it also makes the folder for album artist if needs be
            echo "Creating folder for $album"
        fi
        echo "Moving $mp3Base to $albumFolder"
        mv $mp3File $albumFolder/$mp3Base   # Move the file from the base directory into the requisite folder
        echo "Moving successful!"
    fi
}

convertAndTidy(){
    tidyFile=$(echo $1 | sed -e 's/m4a/mp3/')
    convert $1
    tidy $tidyFile
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
