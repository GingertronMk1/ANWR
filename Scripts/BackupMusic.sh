#!/bin/bash

SAVEIFS=$IFS            # Filenames with spaces are tricky buggers
IFS=$(echo -en "\n\b")  # This is how you get around it. Just...

destServer="$1"         # The destination is specified as an argument

musicFiles=~/Desktop/Scratch/*.m4a
#destFolder=/mnt/usbStick/Music
destFolder=/mnt/usbStick/Scratch

queryMetadata() {                                                                                       # Strip query out of metadata, and while you're there,
    echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//g" | sed -e "s/[:\/\.]/_/g"                   # replace FS-problematic chars with underscores
}

for m4aFile in $musicFiles
do
    m4aBase=$(basename $m4aFile)

    # Convert
    if [ "${m4aFile: -4}" == ".m4a" ]; then                             # If it is an m4a file:
        mp3File=$(echo "$m4aFile" | sed -e 's/m4a/mp3/')                # Make a filename for the mp3 versions
        echo "Converting $m4aBase to mp3"
        avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File   # Convert m4as to mp3s (320kbps, 2-channel, 44100Hz sample rate)
    else
        echo "$m4aBase is already an mp3. Skipping conversion"          # If it's an mp3, don't bother with all that
        mp3File="$m4aFile"
    fi
    
    # Query
    album=$(queryMetadata "$mp3File" "    album           : ")          # Query avprobe for album name
    albumArtist=$(queryMetadata "$mp3File" "    album_artist    : ")    # and album artist name, then
    albumFolder="$destFolder/$albumArtist/$album"                       # make a folder path based on these
    destination="$destServer:'$albumFolder'"                            # and append that to the destination server

    # Pre-Tidy
    sshQuery="[ ! -d '$albumFolder' ] && (mkdir -p '$albumFolder'; echo '$albumFolder' not found, creating...)"   # See below
    ssh $destServer "$sshQuery"     # SSH into the destination, create album folder if it doesn't exist

    # Copy
    scp "$mp3File" "$destination"                                       # Copy mp3 file to server
    rm $mp3File                                                         # Delete local copy of file
done

IFS=$SAVEIFS    # ...don't forget to put things back where you found them
