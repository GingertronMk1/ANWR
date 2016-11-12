#!/bin/bash

SAVEIFS=$IFS            # Back up the current IFS character
IFS=$(echo -en "\n\b")  # Replace it with \n\b so spaces aren't an issue

destServer="$1"         # The destination is specified as an argument

#musicFiles="find -name /Users/Jack/Music/iTunes/iTunes Media/Music/*.m4a
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

    # Query avprobe for artist and album artist, and create a file path from these
    album=$(queryMetadata "$m4aFile" "    album           : ")
    albumArtist=$(queryMetadata "$m4aFile" "    album_artist    : ")
    albumFolder="$destFolder/$albumArtist/$album"

    # Pre-tidy by SSHing into the destination and creating a folder for the album based on the above querying
    sshQuery="[ ! -d '$albumFolder' ] && (mkdir -p '$albumFolder'; echo '$albumFolder' not found, creating...)"
    ssh $destServer "$sshQuery"

    # Copy mp3 file to the server, into the album folder created above, then delete the local copy
    scp "$m4aFile" "$destServer:$albumFolder"
done

IFS=$SAVEIFS    # Finally, put IFS back to how it was before
