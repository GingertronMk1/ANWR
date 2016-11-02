#!/bin/bash

# Points things in the right direction for usefulness

scriptDir="$(pwd)"
dotDir="$(pwd)/Dotfiles"
dotFiles=$(ls -A $scriptDir/Dotfiles)

#echo Script directory: $scriptDir
#echo Dotfile directory: $dotDir
#echo Dotfiles: $dotFiles

cd ~

for file in $dotFiles
do
    [ -e $file ] && (rm $file; echo "Deleting existing copy of $file")
    #echo $dotDir/$file
    ln -s $dotDir/$file $file  # Symlink to this directory's copy
    cd .
    . $file
done

[ ! -e .vim ] && (mkdir -p .vim; echo "Creating .vim folder")        # If the .vim folder doesn't exist, create it
[ -e .vim/colors ] && (rm -rf .vim/colors; echo "Deleting .vim/colors folder")  # If the file ~/.vim/colors exists, delete it
ln -s $scriptDir/vimcolors .vim/colors
echo "Linked .vim/colors to color folder in ANWR"

touch .hushlogin        # Create a .hushlogin file so the system shuts the fuck up when I log in
