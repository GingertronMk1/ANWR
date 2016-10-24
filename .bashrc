# Colours for days
# Normal Colours
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

NC="\[\e[m\]"               # Colour Reset

# Exporting for days
export CLICOLOR=1
export EDITOR="vim"

# PS1 for days
Host="\h"
UName="\u"
CurrDir="\w/"
CurrTime="\d, \A"
if [ "$SSH_CLIENT" ]; then      # If I'm SSHing into the machine, make it look a bit different so I know
    PS1="${Cyan}SSH to $Host ${BWhite}| ${Cyan}$UName ${BWhite}| ${Cyan}$CurrTime ${BWhite}| ${Cyan}$CurrDir\n> ${NC}"
else        #Otherwise, make it look'normal'
    PS1="${Green}$Host ${BWhite}| ${Green}$UName ${BWhite}| ${Green}$CurrTime ${BWhite}| ${Green}$CurrDir\n> ${NC}"
fi

# UNIX aliases for days
alias fuck='eval sudo -E "$(history -p !!)"'    # fuck = sudo !!
if uname | grep -q Darwin; then             # Change behaviour of ls in different environments
    alias ls="ls -aCFGhlp"                  # On Mac, the --color=auto flag doesn't work because reasons
else                                        # Whereas...
    alias ls="ls -aCFhlp --color=auto"      # On <other UNIX>, the -G flag doesn't work because reasons
fi                                          # Hooray for interoperability!
if command -v htop >/dev/null; then         # If htop's installed, alias top to it
    alias top="htop"
fi
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"
alias aptup="sudo apt update && sudo apt upgrade -y"    # update and upgrade
alias sudo="sudo -E"        # sudo no longer puts you in /root or wherever it is
alias df="df -h"            # df looks nice
alias vi="vim"              # shouldn't really come in handy, but it might one day
alias ifconfig="sudo ifconfig"  # because Debian doesn't recognise it otherwise

# Git aliases for days
alias gadd="git add"
alias gadda="git add *"     # In da git add * da vida honey...
alias gcom="git commit -m"  # GCOM: Enemy Uncommitted
alias gpush="git push"
alias gpull="git pull"
alias gstat="git status"

# Functions for days
md () {
    mkdir "$1"
    cd "$1"
}

rf () {
    rm -rf "$1"
}
