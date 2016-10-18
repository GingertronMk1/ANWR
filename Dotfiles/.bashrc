export GREEN="\[\e[32m\]"
export WHITE="\[\e[97m\]"
export DEFCOL="\[\e[39m\]"
export END="\[\e[0m\]"
printf "\e[32m  _____  ___  _____  _   __
 |___  |/ _ \/  __ \| | / /
     | | /_\ \ /  \/| |/ / 
     | |  _  |(     |    \ 
 /\__/ / | | | \__/\| |\  \\
 \____/\_| |_/\____/\_| \_/
\e[0m\n"

export CLICOLOR=1

#PS1 for days
export HOST="\h"
export UNAME="\u"
export CURRDIR="\w/"
PS1="$GREEN$HOST$END $WHITE|$END $GREEN$UNAME$END $WHITE|$END $GREEN$CURRDIR$END\n> $DEFCOL"

#Aliases for days
alias fuck='eval sudo "$(history -p !!)"'   #fuck = sudo !!
alias ls="ls -aCFGhlp"          #Make ls look nice
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"
alias aptup="sudo apt update && sudo apt upgrade -y"

#Functions for days
md () {
    mkdir "$1"
    cd "$1"
}

rf () {
    rm -r "$1"
}
