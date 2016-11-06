#!/bin/bash

SAVEIFS=$IFS            # Filenames with spaces are tricky buggers
IFS=$(echo -en "\n\b")  # This is how you get around it. Just...

#files=$(find /Users/Jack/Music/iTunes/iTunes\ Media/Music -name *.m4a)
files=~/Desktop/Scratch/*.m4a
destination="illiac-local:/mnt/usbStick/Music"

for m4aFile in $files
do
    if [ "${m4aFile: -4}" == ".m4a" ]; then
        mp3File=$(echo ${m4aFile} |sed -e 's/m4a/mp3/')             # Make a filename for the mp3 versions
        mp3Base=$(basename $mp3File)                                # For debugging, get rid of the file path
        echo "Converting $(basename $m4aFile) to $mp3Base"
        avconv -v quiet -i $m4aFile -ab 320k -ac 2 -ar 44100 $mp3File        # Convert from m4a to 320kbps mp3
    fi
    echo "Copying $mp3Base to $destination"
    scp $mp3File $destination                                   # Copy file to ssh server
    echo "Deleting local copy of $mp3Base"
    rm $mp3File                                                 # Delete local copy of mp3 version
done

IFS=$SAVEIFS    # ...don't forget to put things back where you found them
