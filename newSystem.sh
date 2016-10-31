#!/bin/bash

#Points things in the right direction for usefulness

scriptDir="$(pwd)"

cd ~

for file in .aliases .bashrc .bash_profile .functions .xsessionrc
do
    touch $file
    echo "Make $file point to the copy in my Github folder"
    echo ". $scriptDir/$file" >> $file
    echo "$file changed!"
done
unset file

touch .vimrc        # Point default vimrc to the right place
echo "source $scriptDir/.vimrc" >> .vimrc
echo ".vimrc changed!"

mkdir -p .vim/          # Make vim look right for me
ln -s $scriptDir/vimcolors .vim/colors
echo "Colour schemes added!"

touch .inputrc                  # Point the default inputrc to the right place
echo "\$include $scriptDir/.inputrc" >> .inputrc
echo ".inputrc changed!"

touch .hushlogin        # Create a .hushlogin file so the system shuts the fuck up when I log in
