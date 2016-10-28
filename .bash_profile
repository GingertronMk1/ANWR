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
    PS1="${Cyan}SSH to $Host ${BWhite}| ${Cyan}$UName ${BWhite}| ${Cyan}$CurrTime ${BWhite}| ${Cyan}$CurrDir\n${Cyan}> ${NC}"
    PS2="${Cyan}> ${NC}"
else                            #Otherwise, make it look'normal'
    PS1="${Green}$Host ${BWhite}| ${Green}$UName ${BWhite}| ${Green}$CurrTime ${BWhite}| ${Green}$CurrDir\n${Green}> ${NC}"
    PS2="${Green}> ${NC}"
fi

# Functions for days
md () {
    mkdir "$1"
    cd "$1"
}

rf () {
    rm -rf "$1"
}

# Sourcing for days
. .aliases

# Exporting for days
export CLICOLOR=1
export EDITOR='vim'

