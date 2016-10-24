#!/bin/bash

#Points things in the right direction for usefulness

scriptDir="$(pwd)"

cd ~

touch .vimrc
echo "source $scriptDir/.vimrc" >> .vimrc
echo "vimrc changed!"
mkdir -p .vim/colors
cp $scriptDir/molokai.vim .vim/colors/molokai.vim
touch .bash_profile
echo ". $scriptDir/.bashrc" >> .bash_profile
echo "bash_profile changed!"

if uname | grep -q Darwin; then
    touch .inputrc
    echo "\$include $scriptDir/.inputrc" >> .inputrc
    echo "inputrc changed!"
else
    touch .bashrc
    echo ". $scriptDir/.bashrc" >> .bashrc
    echo "bashrc changed!"
    touch .bash_profi    touch .xsessionrc
    echo ". $scriptDir/.xsessionrc" >> .xsessionrc
    echo "xsessionrc changed!"
fi

echo "CBA to script it, but don't forget to install sudo, htop, vim, git, and chromium"
