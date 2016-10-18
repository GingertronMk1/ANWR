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
\n"

export CLICOLOR=1

#PS1 for days
export HOST="\h"
export UNAME="\u"
export CURRDIR="\w/"
PS1="$GREEN$HOST $WHITE| $GREEN$UNAME $WHITE| $GREEN$CURRDIR\n> $DEFCOL"

#Aliases for days
alias fuck='eval sudo "$(history -p !!)"'   #fuck = sudo !!
if uname | grep -q Darwin; then             #if mac, ls is something else
    alias ls="ls -aCFGhlp"
else                                        #if linux, it's something other else
    alias ls="ls -aCFhlp --color=auto"
fi
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
