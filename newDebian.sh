#sudo
#htop
#chromium
#git
#xinput


scriptDir="$(dirname ${BASH_SOURCE[0]})"

cd ~

apt install sudo && apt install htop && apt install chromium && apt install git && apt install xinput -y

touch .vimrc
echo "source $scriptDir/.vimrc" >> .vimrc
touch .bashrc
echo ". scriptDir/.bashrc" >> .bashrc
touch .xsessionrc
echo ". $scriptDir/.xsessionrc" >>
