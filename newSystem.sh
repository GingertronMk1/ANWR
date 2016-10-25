#!/bin/bash

#Points things in the right direction for usefulness

scriptDir="$(pwd)"

cd ~

touch .vimrc        # Point default vimrc to the right place
echo "source $scriptDir/.vimrc" >> .vimrc
echo "vimrc changed!"

mkdir -p .vim/          # Make vim look right for me
ln -s $scriptDir/vimcolors .vim/colors
echo "Colour schemes added!"

touch .bash_profile     # Make bash work right, turns out some Linux systems also use this
echo ". $scriptDir/.bashrc" >> .bash_profile
echo "bash_profile changed!"

if uname | grep -q Darwin; then     # If it's a Mac...
    touch .inputrc                  # Point the default inputrc to the right place
    echo "\$include $scriptDir/.inputrc" >> .inputrc
    echo "inputrc changed!"
else                                # If it's Linux...
    touch .bashrc                   # Point the default bashrc to the right place (redundancy, but you never can tell)
    echo ". $scriptDir/.bashrc" >> .bashrc
    echo "bashrc changed!"
    touch .xsessionrc                               # And do xsessionrc stuff 
    echo ". $scriptDir/.xsessionrc" >> .xsessionrc  # This won't do anything if xinput's not installed,
    echo "xsessionrc changed!"                      # But if it is it makes Debian play nice with ThinkPads
fi

echo "CBA to script it, but don't forget to install sudo, htop, vim, git, and chromium, as well as xinput if you're on a ThinkPad"
