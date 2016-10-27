# Colours for days
Green='\[\e[0;32m\]'        # Green
Cyan='\[\e[0;36m\]'         # Cyan
BWhite='\[\e[1;37m\]'       # Bold White
NC="\[\e[m\]"               # Colour Reset

# PS1 for days
Host="\h"
UName="\u"
CurrDir="\w/"
CurrTime="\d, \A"
if [ "$SSH_CLIENT" ]; then      # If I'm SSHing into the machine, make it look a bit different so I know
    PS1="${Cyan}SSH to $Host ${BWhite}| ${Cyan}$UName ${BWhite}| ${Cyan}$CurrTime ${BWhite}| ${Cyan}$CurrDir\n> ${NC}"
    PS2="${Cyan}> ${NC}"
else                            #Otherwise, make it look'normal'
    PS1="${Green}$Host ${BWhite}| ${Green}$UName ${BWhite}| ${Green}$CurrTime ${BWhite}| ${Green}$CurrDir\n> ${NC}"
    PS2="${Green}> ${NC}"
fi

# Aliases for days
alias fuck='eval sudo -E "$(history -p !!)"'    # fuck = sudo !!
if uname | grep -q Darwin; then             # Change behaviour of ls in different environments
    Colorflag="-G"                          # On macOS, this makes ls coloured
    export LSCOLORS='exfxcxdxbxegedabagacad'
else                                        # Whereas...
    Colorflag="--color"                     # On Linux, this makes ls coloured
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
fi                                          # Such bullshit that this needs doing
alias ls="ls -aCFhlp ${Colorflag}"
if command -v htop >/dev/null; then         # If htop's installed, alias top to it
    alias top="htop"
fi
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"
alias .....="cd ../../../../.."
alias ~="cd ~"
alias -- -="cd -"
alias aptup="sudo apt update && sudo apt upgrade -y"    # update and upgrade
alias sudo="sudo -E"            # sudo no longer puts you in /root or wherever it is
alias df="df -h"                # df looks nice
alias vi="vim"                  # shouldn't really come in handy, but it might one day
alias ifconfig="/sbin/ifconfig" # Silly ifconfig

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

# Exporting for days
export CLICOLOR=1
export EDITOR='vim'

