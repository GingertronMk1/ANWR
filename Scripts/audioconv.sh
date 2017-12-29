#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
set -m

threads=4   # Increase/decrease as necessary for how many threads you want of this

#musicPath=/mnt/usbStick/Music
folder=$SCRATCHFOLDER   # Where the file things are (that you want copying)

#files=$(find $folder -name "*.flac")
files=$(find $folder -name "*.flac" | sort)

mp3Convert() {
  baseFile=$1
  newFile=$(echo $line | sed -e 's/flac/mp3/g')
  avconv -v quiet -i $baseFile -ab 320k -ac 2 -ar 44100 $newFile
}

flacConvert() {
  baseFile=$1
  newFile=$(echo $line | sed -e 's/m4a/flac/g')
  avconv -v quiet -i $baseFile -f flac $newFile
}

wavConvert() {
  baseFile=$1
  newFile=$(echo $line | sed -e 's/flac/wav/g')
  avconv -v quiet -i $baseFile -f wav $newFile
}

waveformConvert() {
  newFile=$(echo $line | sed -e 's/flac/png/g')
  ffmpeg -v quiet -i $1 -filter_complex "showwavespic=s=6400x1200" -frames:v 1 $newFile
}

echo -e "Convert to:\n[1]: mp3\n[2]: flac\n[3]: wav\n[4]: waveform png\n[0]: flac & wav"
read -n 1 -r
echo -e "\nConverting using $threads threads..."

STARTTIME=$(date +%s)

for line in $files; do
  while [ $(jobs | wc -l) -ge $threads ] ; do sleep 1 ; done
  case $REPLY in
    1) mp3Convert $line && rm $line &;;
    2) flacConvert $line && rm $line &;;
    3) wavConvert $line && rm $line &;;
    4) waveformConvert $line &;;
    0) flacConvert $line && wavConvert $line &;;
  esac
done

IFS=$SAVEIFS

wait

echo "Done!"

# After writing this I'm not sure if sed's my favourite or least favourite function.
