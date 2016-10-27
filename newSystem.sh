#!/bin/bash

#Points things in the right direction for usefulness

scriptDir="$(pwd)"

cd ~

for file in .bashrc .bash_profile .xsessionrc
do
    touch $file
    echo ". $scriptDir/$file" >> $file
    echo "$file changed!"
done

touch .vimrc        # Point default vimrc to the right place
echo "source $scriptDir/.vimrc" >> .vimrc
echo ".vimrc changed!"

mkdir -p .vim/          # Make vim look right for me
ln -s $scriptDir/vimcolors .vim/colors
echo "Colour schemes added!"


touch .inputrc                  # Point the default inputrc to the right place
echo "\$include $scriptDir/.inputrc" >> .inputrc
echo ".inputrc changed!"

echo "CBA to script it, but don't forget to install sudo, htop, vim, git, and chromium, as well as xinput if you're on a ThinkPad"
