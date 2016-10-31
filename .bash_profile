# Colours for days
Green='\[\e[0;32m\]'        # Green
Cyan='\[\e[0;36m\]'         # Cyan
BWhite='\[\e[1;37m\]'       # Bold White
NC='\[\e[m\]'               # Colour Reset

# Shell options for days
shopt -s nocaseglob;    # Case insensitive pathname extension
shopt -s histappend;    # Don"t overwrite bash_history
shopt -s cdspell;       # Spell correction in cd
# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr " " "\n")" scp sftp ssh;

# PS1 for days
Host="\h"
UName="\u"
CurrDir="\w/"
CurrTime="\d, \A"
if [ "$SSH_CLIENT" ]; then      # If I"m SSHing into the machine, make it look a bit different so I know
    PS1="${Cyan}SSH to $Host ${BWhite}| ${Cyan}$UName ${BWhite}| ${Cyan}$CurrTime ${BWhite}| ${Cyan}$CurrDir\n${Cyan}> ${NC}"
    PS2="${Cyan}> ${NC}"
else                            #Otherwise, make it look"normal"
    PS1="${Green}$Host ${BWhite}| ${Green}$UName ${BWhite}| ${Green}$CurrTime ${BWhite}| ${Green}$CurrDir\n${Green}> ${NC}"
    PS2="${Green}> ${NC}"
fi

# Sourcing for days
. .aliases
. .functions

# Exporting for days
export CLICOLOR=1
export EDITOR="vim"     # EMACS HERETICS WILL BURN
export HISTSIZE=16384   # Lots of history
export HISTCONTROL=ignoredups
export TERM="xterm-256color"
export LANG="en_GB.UTF-8"
export LC_ALL="en_GB.UTF-8"
