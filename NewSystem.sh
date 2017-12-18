#!/bin/bash

# Points things in the right direction for usefulness

thisDir=$(pwd -P)

for file in `find ./Dotfiles -type f`
do
  baseFile=$(basename $file)
  fullFile=$(pwd -P)/Dotfiles/$baseFile
  echo File: $fullFile
  [ -e ~/$baseFile ] && (rm ~/$baseFile; echo "Deleting existing copy of $baseFile in ~")
  ln -s $fullFile ~ # Create a symlink to this directory's copy in ~
  echo "Created new link to $fullFile in ~"
done

[ ! -e ~/.vim ] && (mkdir -p ~/.vim; echo "Creating .vim folder")        # If the .vim folder doesn't exist, create it
[ -e ~/.vim/colors ] && (rm -rf ~/.vim/colors; echo "Deleting .vim/colors folder")  # If the file/folder .vim/colors exists, delete it
ln -s $thisDir/vimcolors ~/.vim/colors                      # Symlink to the colors directory in this folder
echo "Linked .vim/colors to color folder in ANWR"           # Tell me what it's done

touch .hushlogin        # Create a .hushlogin file so the system shuts the fuck up when I log in
touch .localexports     # Create a .localexports file so it shuts up when I source

. .bash_profile
