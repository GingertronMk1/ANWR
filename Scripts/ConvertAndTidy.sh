#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

musicPath=/Users/Jack/Desktop/Scratch
#files=$(find ~/Music/iTunes/iTunes\ Media/Music -name *.m4a)
files=$musicPath/*                           # Where the file things are (that you want copying)

convert() {
    if [ "${1: -4}" == ".m4a" ]; then
        local m4aFile=$1
        # Converting to MP3
        mp3File=$(echo $m4aFile | sed -e 's/m4a/mp3/')       # Make a filename for the mp3 version
        # echo $mp3File
        echo "Converting $m4aFile to $mp3File"
        avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File   # Convert from m4a to 320kbps mp3
        echo "Conversion successful!"
        rm $m4aFile
    fi
}

tidy(){
    if [ "${1: -4}" == ".mp3" ]; then
        local mp3File=$1
        echo "Tidying $(basename $mp3File)"
        albumQuery="    album           : "             # Queries for grepping the output of avprobe
        albumArtistQuery="    album_artist    : "

        #album=$(echo $(avprobe $mp3File 2>&1 | grep $albumQuery) | sed -e "s/$albumQuery//") | sed -e "s/:/_/"
        #albumArtist=$(echo $(avprobe $mp3File 2>&1 | grep $albumArtistQuery) | sed -e "s/$albumArtistQuery//") | sed -e "s/:/_/"

        album=$(echo $(echo $(avprobe $mp3File 2>&1 | grep $albumQuery) | sed -e "s/$albumQuery//") | sed -e "s/:/_/")
        albumArtist=$(echo $(echo $(avprobe $mp3File 2>&1 | grep $albumArtistQuery) | sed -e "s/$albumArtistQuery//") | sed -e "s/:/_/")

        albumFolder="$musicPath/$albumArtist/$album"

        echo $albumFolder

        if [ ! -e $albumFolder ]; then          # If a folder doesn't exist for that album:
            mkdir -p $albumFolder               # Make it. Make it using -p so it also makes the folder for album artist if needs be
            echo "Creating folder for $album"
        fi
        echo "Moving $mp3File to $albumFolder"
        mv $mp3File $albumFolder/$(basename $mp3File)   # Move the file from the base directory into the requisite folder
        echo "Moving successful!"
    fi
}

convertAndTidy(){
    tidyFile=$(echo $1 | sed -e 's/m4a/mp3/')
    convert $1
    tidy $tidyFile
}

read -p "Do you want to [c]onvert, [t]idy, or [B]oth? [c/t/B]" -n 1 -r
echo -e "\n"

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
