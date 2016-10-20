# Normal Colors
Black='\[\e[0;30m\]'        # Black
Red='\[\e[0;31m\]'          # Red
Green='\[\e[0;32m\]'        # Green
Yellow='\[\e[0;33m\]'       # Yellow
Blue='\[\e[0;34m\]'         # Blue
Purple='\[\e[0;35m\]'       # Purple
Cyan='\[\e[0;36m\]'         # Cyan
White='\[\e[0;37m\]'        # White

# Bold
BBlack='\[\e[1;30m\]'       # Black
BRed='\[\e[1;31m\]'         # Red
BGreen='\[\e[1;32m\]'       # Green
BYellow='\[\e[1;33m\]'      # Yellow
BBlue='\[\e[1;34m\]'        # Blue
BPurple='\[\e[1;35m\]'      # Purple
BCyan='\[\e[1;36m\]'        # Cyan
BWhite='\[\e[1;37m\]'       # White

# Background
On_Black='\[\e[40m\]'       # Black
On_Red='\[\e[41m\]'         # Red
On_Green='\[\e[42m\]'       # Green
On_Yellow='\[\e[43m\]'      # Yellow
On_Blue='\[\e[44m\]'        # Blue
On_Purple='\[\e[45m\]'      # Purple
On_Cyan='\[\e[46m\]'        # Cyan
On_White='\[\e[47m\]'       # White

NC="\[\e[m\]"               # Color Reset

export CLICOLOR=1

#PS1 for days
export HOST="\h"
export UNAME="\u"
export CURRDIR="\w/"
PS1="${Green}$HOST ${BWhite}| ${Green}$UNAME ${BWhite}| ${Green}$CURRDIR\n> ${NC}"

#Aliases for days
alias fuck='\[eval sudo "$(history -p !!)"'   #fuck = sudo !!
if uname | grep -q Darwin; then             #Change behaviour of ls in different environments
    alias ls="ls -aCFGhlp"                  #On Mac, the --color=auto flag doesn't work because reasons
else
    alias ls="ls -aCFhlp --color=auto"      #On <other UNIX>, the -G flag doesn't work
fi                                          #Hooray for interoperability!
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"
alias aptup="sudo apt update && sudo apt upgrade -y"
alias sudo="sudo -E"        #sudo no longer puts you in /root or wherever it is
alias gadd="git add"
alias gcom="git commit -m"
alias gpush="git push"
alias gstat="git status"

#Functions for days
md () {
    mkdir "$1"
    cd "$1"
}

rf () {
    rm -r "$1"
}
