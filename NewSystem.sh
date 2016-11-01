#!/bin/bash

#Points things in the right direction for usefulness

scriptDir="$(pwd)"

cd ~

for dotFile in .aliases .bash_profile .bashrc .exports .functions .gitconfig .inputrc .prompt .vimrc .xsessionrc
do
    [ -e $dotFile ] && (rm -f $dotFile; echo "Deleted existing copy of $dotFile") # If the dotfile already exists, delete it
    ln -s $scriptDir/$dotFile $dotFile  # Symlink to this directory's copy
    echo "Linked $dotFile to copy in ANWR"
done

[ ! -e .vim ] && (mkdir -p .vim; echo "Creating .vim folder")        # If the .vim folder doesn't exist, create it
[ -e .vim/colors ] && (rm -rf .vim/colors; echo "Deleting .vim/colors folder")  # If the file ~/.vim/colors exists, delete it
ln -s $scriptDir/vimcolors .vim/colors
echo "Linked .vim/colors to color folder in ANWR"

touch .hushlogin        # Create a .hushlogin file so the system shuts the fuck up when I log in
