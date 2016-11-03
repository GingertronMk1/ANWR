#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#files=$(find ~/Music/iTunes/iTunes\ Media/Music -name *.m4a)
files=~/Desktop/Scratch/*.m4a                           # Where the file things are (that you want copying)

stripQuery() {                                                                      # This is a long one. First it finds an instance of $2 in the output of avprobe
    $(echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//") | sed -e "s/:/_/"    # Then it strips out the $2 part of the line, so you just get the bit of metadata you want
}                                                                                   # Finally, it replaces all : with _, because we're using this for folder names and mkdir hates :

for m4aFile in $files
do
    # Converting to MP3
    mp3File=$(echo $m4aFile |sed -e 's/m4a/mp3/')       # Make a filename for the mp3 version
    # echo $mp3File
    echo "Converting $m4aFile to $mp3File"
    avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File   # Convert from m4a to 320kbps mp3
    echo "Conversion successful!"
    rm $m4aFile

    # Tidying Up
    artistQuery="    artist          : "            # Queries for grepping the output of avprobe
    albumQuery="    album           : "
    albumArtistQuery="    album_artist    : "

    album=$(echo $(stripQuery $mp3File $albumQuery) | sed -e "s/:/_/")  # Get the album name from metadata
    albumArtist=$(stripQuery $mp3File $albumArtistQuery)                # Get the album artist from metadata
    albumFolder="/Users/Jack/Desktop/Scratch/$albumArtist/$album"

    echo $albumFolder
    if [ ! -e $albumFolder ]; then          # If a folder doesn't exist for that album:
        mkdir -p $albumFolder               # Make it. Make it using -p so it also makes the folder for album artist if needs be
        echo "Creating folder for $album"
    fi
    echo "Moving $mp3File to the correct folder"
    mv $mp3File $albumFolder/$(basename $mp3File)   # Move the file from the base directory into the requisite folder
    echo "Moving successful!"

done

IFS=$SAVEIFS
