echo -e"
  _____  ___  _____  _   __
 |___  |/ _ \/  __ \| | / /
     | | /_\ \ /  \/| |/ / 
     | |  _  |(     |    \ 
 /\__/ / | | | \__/\| |\  \\
 \____/\_| |_/\____/\_| \_/
"

export CLICOLOR=1

#PS1 for days
export HOST="\h"
export UNAME="\u"
export CURRDIR="\w/"
export PS1END="[$(tput sgr0)\]"
PS1="$HOST | $UNAME | $CURRDIR\n> "

#Aliases for days
alias fuck='eval sudo "$(history -p !!)"'   #fuck = sudo !!
export LS_OPTIONS='--color=auto'            #Preamble
eval "`dircolors`"                          #Preamble
alias ls="ls -aCFGhlp $LS_OPTIONS"          #Make ls look nice
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"

#Functions for days
md () {
    mkdir "$1"
    cd "$1"
}

rf () {
    rm -r "$1"
}
