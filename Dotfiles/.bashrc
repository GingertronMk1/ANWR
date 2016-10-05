if [ $HOST System-X ] ; then
    echo -e "\033[0;32m
     ____ __  __ ____  _____  ___  ___  ___        __  __ 
    / ___|\ \/ // ___||__ __|| __||   \/   |  ___  \ \/ / 
    \___ \ \  / \___ \  | |  | __|| |\__/| | |___|  >  <  
    |____/ /_/  |____/  |_|  |___||_|    |_|       /_/\_\ 

    " ;
elif [ $HOST Watson ] ; then
    echo -e "\033[0;34m
    __       __ __  _____  ____  ___  __   _
    \ \  _  / //  \|_   _|/ ___|/   \|  \ | |
     \ \/_\/ // __ \ | |  \___ \|   || |\\| |
      \_/ \_//_/  \_\|_|  |____/\___/|_| \__|

    " ;
fi

export CLICOLOR=1
HOST="\h\[$(tput sgr0)\]"
PS1="\h|\w\[$(tput sgr0)\] > "
export EDITOR=vim
alias edit="$EDITOR"
alias fuck='eval sudo "$(history -p \!\!)"'
