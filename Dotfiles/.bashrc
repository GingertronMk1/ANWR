echo -e "\033[0;32m
  _____  ___  _____  _   __
 |___  |/ _ \/  __ \| | / /
     | | /_\ \ /  \/| |/ / 
     | |  _  |(     |    \ 
 /\__/ / | | | \__/\| |\  \\
 \____/\_| |_/\____/\_| \_/
"

export CLICOLOR=1

export HOST="\h\[$(tput sgr0)\]"
export UNAME="\u\[$(tput sgr0)\]"
export CURRDIR="\w\[$(tput sgr0)\]/"
PS1="$HOST | $UNAME |$CURRDIR\n> "

#Aliases for days
alias fuck='eval sudo "$(history -p !!)"'
alias ls="ls -aCFGhlp"
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"

md () {
    mkdir "$1"
    cd "$1"
}

rf () {
    rm -r "$1"
}
