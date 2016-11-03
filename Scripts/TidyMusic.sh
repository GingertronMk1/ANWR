#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#files=$(find ~/Music/iTunes/iTunes\ Media/Music -name *.m4a)
musicPath=/mnt/usbStick/Music
files=$musicPath/*.mp3

function stripQuery {
    echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//"
}

for mp3File in $files
do
    #echo $mp3File
    artistQuery="    artist          : "
    albumQuery="    album           : "
    albumArtistQuery="    album_artist    : "
    artist=$(stripQuery $mp3File $artistQuery)
    album=$(stripQuery $mp3File $albumQuery)
    albumArtist=$(stripQuery $mp3File $albumArtistQuery)
    #echo Artist: $artist / Album: $album / Album Artist: $albumArtist
    albumFolder="$musicPath/$albumArtist/$album"
    echo $albumFolder
    if [ ! -e $albumFolder ]; then
        mkdir -p $albumFolder
        echo "Creating folder for $album"
    fi
    mv $mp3File $albumFolder/$(basename $mp3File)
done

IFS=$SAVEIFS
