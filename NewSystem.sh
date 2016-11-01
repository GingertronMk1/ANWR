#!/bin/bash

#Points things in the right direction for usefulness

scriptDir="$(pwd)"

cd ~

for dotFile in .aliases .bash_profile .bashrc .exports .functions .gitconfig .inputrc .prompt .vimrc .xsessionrc
do
    if [ -e $dotFile ]; then
        rm -f $dotFile
    fi
    ln -s $scriptDir/$dotFile $dotFile
done

mkdir -p .vim/          # Make vim look right for me
ln -s $scriptDir/vimcolors .vim/colors
echo "Colour schemes added!"

touch .hushlogin        # Create a .hushlogin file so the system shuts the fuck up when I log in
