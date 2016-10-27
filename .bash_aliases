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
alias sudo="sudo -E"        # sudo no longer puts you in /root or wherever it is
alias df="df -h"            # df looks nice
alias vi="vim"              # shouldn't really come in handy, but it might one day
alias ifconfig="/sbin/ifconfig" # Silly ifconfig

# Git aliases for days
alias gadd="git add"
alias gadda="git add *"     # In da git add * da vida honey...
alias gcom="git commit -m"  # GCOM: Enemy Uncommitted
alias gpush="git push"
alias gpull="git pull"
alias gstat="git status"
