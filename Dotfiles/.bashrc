    echo -e "\033[0;32m
  _____  ___  _____  _   __
 |___  |/ _ \/  __ \| | / /
     | | /_\ \ /  \/| |/ / 
     | |  _  |(     |    \ 
 /\__/ / | | | \__/\| |\  \\
 \____/\_| |_/\____/\_| \_/
             "

export CLICOLOR=1
HOST="\h\[$(tput sgr0)\]"
PS1="\h|\w\[$(tput sgr0)\] > "
export EDITOR=vim
alias edit="$EDITOR"
alias fuck='eval sudo "$(history -p \!\!)"'
