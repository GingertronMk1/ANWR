#!/bin/bash

SAVEIFS=$IFS            # Filenames with spaces are tricky buggers
IFS=$(echo -en "\n\b")  # This is how you get around it. Just...

files=~/Desktop/Scratch/*.m4a
#destServer="illiac-local"
destServer="illiac"
#destFolder=/mnt/usbStick/Music
destFolder=/mnt/usbStick/Scratch

queryMetadata() {                                                                                       # Strip query out of metadata
    echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//" | sed -e "s/[:\/\.]/_/"                     # Replace FS-problematic chars with _
}                                                                                                       # as well as spaces with escaped spaces

checkDirectory() {
    if [ ! -d "$1" ]; then
        mkdir -p "$1"
    fi
}

for m4aFile in $files
do
    # Convert
    if [ "${m4aFile: -4}" == ".m4a" ]; then                             # If it is an m4a file:
        mp3File=$(echo ${m4aFile} |sed -e 's/m4a/mp3/')                 # Make a filename for the mp3 versions
        mp3Base=$(basename $mp3File)                                    # For debugging, get rid of the file path
        avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File   # Convert m4as to mp3s
    fi
    
    # Query
    album=$(queryMetadata "$m4aFile" "    album           : ")          # Query avprobe for album name
    albumArtist=$(queryMetadata "$m4aFile" "    album_artist    : ")    # and album artist name, then
    albumFolder="$destFolder/$albumArtist/$album"                       # Make a folder path based on these
    destination="$destServer:'$albumFolder'"                            # And append that to the destination server

    # Pre-Tidy
    sshQuery="[ ! -d '$albumFolder' ] && (mkdir -p '$albumFolder'; echo Creating '$albumFolder')"   # See below
    ssh $destServer "$sshQuery"     # SSH into the destination, create album folder if it doesn't exist

    # Copy
    scp "$mp3File" "$destination"                                       # Copy mp3 file to server
    rm $mp3File                                                         # Delete local copy of file
done

IFS=$SAVEIFS    # ...don't forget to put things back where you found them
