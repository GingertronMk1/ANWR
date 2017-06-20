#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#musicPath=/mnt/usbStick/Music
folder=$SCRATCHFOLDER   # Where the file things are (that you want copying)

files=$folder/*.m4a

mp3Convert() {
  baseFile=$1
  mp3File=$2
  echo "Converting $1 to $mp3File"
  avconv -v quiet -i $baseFile -ab 320k -ac 2 -ar 44100  $mp3File
  echo "Conversion successful!"
  rm $baseFile
}

flacConvert() {
  baseFile=$1
  flacFile=$2
  echo "Converting $baseFile to $flacFile"
  avconv -v quiet -i $baseFile -f flac  $flacFile
  echo "Conversion successful!"
  rm $baseFile
}

wavconvert() {
  baseFile=$1
  wavFile=$2
  echo "Converting $baseFile to $wavFile"
  avconv -v quiet -i $baseFile -f wav $wavFile
  echo "Conversion successful!"
  rm $baseFile
}

waveformConvert() {
  echo "Generating waveform png for $1"
  ffmpeg -v quiet -i $1 -filter_complex "showwavespic=s=6400x1200" -frames:v 1 $2
  echo "Generation successful!"
}

echo -e "Convert to:\n[1]: mp3\n[2]: flac\n[3]: wav\n[4]: waveform png\n"
read -n 1 -r
echo -e "\n"

for line in $files; do
  case $REPLY in
    1) mp3Convert $line $(echo $line | sed -e 's/m4a/mp3/');;
    2) flacConvert $line $(echo $line | sed -e 's/m4a/flac/');;
    3) wavconvert $line $(echo $line | sed -e 's/m4a/wav/');;
    4) waveformConvert $line $(echo $line | sed -e 's/m4a/png/');;
  esac
  echo -e "Next!\n"
done

IFS=$SAVEIFS

# After writing this I'm not sure if sed's my favourite or least favourite function.
