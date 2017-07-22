#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

#musicPath=/mnt/usbStick/Music
folder=$SCRATCHFOLDER   # Where the file things are (that you want copying)

files=$folder/*.m4a

mp3Convert() {
  baseFile=$1
  newFile=$(echo $line | sed -e 's/m4a/mp3/g')
  echo "Converting $1 to $newFile"
  avconv -v quiet -i $baseFile -ab 320k -ac 2 -ar 44100 $newFile
  echo "Conversion successful!"
}

flacConvert() {
  baseFile=$1
  newFile=$(echo $line | sed -e 's/m4a/flac/g')
  echo "Converting $baseFile to $newFile"
  avconv -v quiet -i $baseFile -f flac $newFile
  echo "Conversion successful!"
}

wavConvert() {
  baseFile=$1
  newFile=$(echo $line | sed -e 's/m4a/wav/g')
  echo "Converting $baseFile to $newFile"
  avconv -v quiet -i $baseFile -f wav $newFile
  echo "Conversion successful!"
}

waveformConvert() {
  echo "Generating waveform png for $1"
  newFile=$(echo $line | sed -e 's/m4a/png/g')
  ffmpeg -v quiet -i $1 -filter_complex "showwavespic=s=6400x1200" -frames:v 1 $newFile
  echo "Generation successful!"
}

echo -e "Convert to:\n[1]: mp3\n[2]: flac\n[3]: wav\n[4]: waveform png\n[0]: flac & wav"
read -n 1 -r
echo -e "\n"

for line in $files; do
  case $REPLY in
    1) mp3Convert $line && rm $line;;
    2) flacConvert $line && rm $line;;
    3) wavConvert $line && rm $line;;
    4) waveformConvert $line;;
    0) flacConvert $line && wavConvert $line;;
  esac
  echo -e "Next!\n"
done

IFS=$SAVEIFS

# After writing this I'm not sure if sed's my favourite or least favourite function.
