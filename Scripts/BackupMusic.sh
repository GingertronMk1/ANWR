#!/bin/bash

SAVEIFS=$IFS            # Back up the current IFS character
IFS=$(echo -en "\n\b")  # Replace it with \n\b so spaces aren't an issue

destServer="$1"         # The destination is specified as an argument

musicFiles=~/Desktop/Scratch/*.m4a
#destFolder=/mnt/usbStick/Music
destFolder=/mnt/usbStick/Scratch

queryMetadata() {
    # Get just the relevant bit from metadata, as well as replacing FS-problematic chars with underscores
    echo $(avprobe "$1" 2>&1 | grep "$2") | sed -e "s/$2//g" | sed -e "s/[:\/\.]/_/g"
}

for m4aFile in $musicFiles
do
    # Preamble
    m4aBase=$(basename $m4aFile)

    # Convert original from m4a to mp3
    if [ "${m4aFile: -4}" == ".m4a" ]; then
        mp3File=$(echo "$m4aFile" | sed -e 's/m4a/mp3/')
        echo "Converting $m4aBase to mp3"
        avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File
    fi
    
    # Query avprobe for artist and album artist, and create a file path from these
    album=$(queryMetadata "$mp3File" "    album           : ")
    albumArtist=$(queryMetadata "$mp3File" "    album_artist    : ")
    albumFolder="$destFolder/$albumArtist/$album"

    # Pre-tidy by SSHing into the destination and creating a folder for the album based on the above querying
    sshQuery="[ ! -d '$albumFolder' ] && (mkdir -p '$albumFolder'; echo '$albumFolder' not found, creating...)"
    ssh $destServer "$sshQuery"

    # Copy mp3 file to the server, into the album folder created above, then delete the local copy
    scp "$mp3File" "$destServer:$albumFolder"
    rm $mp3File
done

IFS=$SAVEIFS    # Finally, put IFS back to how it was before
