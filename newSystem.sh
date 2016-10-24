#!/bin/bash

#Installs the following:
#sudo, htop, chromium, git, xinput
#As well as pointing various things to the right places

scriptDir="$(pwd)"

cd ~

apt update && apt upgrade -y
apt install sudo && apt install htop && apt install chromium && apt install git && apt install xinput -y

touch .vimrc
echo "source $scriptDir/.vimrc" >> .vimrc

if uname | grep -q Darwin; then
    touch .bash_profile
    echo ". $scriptDir/.bashrc" >> .bash_profile
    touch .inputrc
    echo "\$include $scriptDir/.inputrc" >> .inputrc
else
    touch .bashrc
    echo ". $scriptDir/.bashrc" >> .bashrc
    touch .xsessionrc
    echo ". $scriptDir/.xsessionrc" >> .xsessionrc
fi
