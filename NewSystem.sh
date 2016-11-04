#!/bin/bash

# Points things in the right direction for usefulness

scriptDir="$(pwd -P)"
dotDir="$scriptDir/Dotfiles"
dotFiles=$dotDir/*

#echo Script directory: $scriptDir
#echo Dotfile directory: $dotDir
#echo Dotfiles: $dotFiles

cd ~

for file in $dotFiles
do
    [ -e $file ] && (rm $file; echo "Deleting existing copy of $file in ~")
    ln -s $dotDir/$file $file  # Create a symlink to this directory's copy in ~
done

[ ! -e .vim ] && (mkdir -p .vim; echo "Creating .vim folder")        # If the .vim folder doesn't exist, create it
[ -e .vim/colors ] && (rm -rf .vim/colors; echo "Deleting .vim/colors folder")  # If the file/folder .vim/colors exists, delete it
ln -s $scriptDir/vimcolors .vim/colors                      # Symlink to the colors directory in this folder
echo "Linked .vim/colors to color folder in ANWR"           # Tell me what it's done

touch .hushlogin        # Create a .hushlogin file so the system shuts the fuck up when I log in

. .bashrc

cd -
