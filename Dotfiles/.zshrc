###############################################################################
# UNSET ALL ALIASES ###########################################################
###############################################################################

unalias -m '*'

###############################################################################
# SHELL OPTIONS ###############################################################
###############################################################################

setopt EXTENDED_GLOB      # More globbing
setopt GLOB_COMPLETE      # Append to history rather than rewrite it
setopt MENU_COMPLETE      # don't autoselect the first completion entry
setopt APPEND_HISTORY     # Append to history rather than rewrite it
setopt ALWAYS_TO_END      # move cursor to the end of a completed word
setopt AUTO_LIST          # automatically list choices on ambiguous completion
setopt AUTO_MENU          # show completion menu on a successive tab press
setopt AUTO_PARAM_SLASH   # if completed parameter is a directory, add a trailing slash
setopt COMPLETE_IN_WORD   # complete from both ends of a word
setopt NO_CASE_GLOB       # Should make globbing case-insensitive
setopt NO_CASE_MATCH      # Should make globbing case-insensitive
setopt GLOB_DOTS          # Glob dotfiles

[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"      beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"       end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"    overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}" backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"    delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"        up-line-or-beginning-search
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"      down-line-or-beginning-search
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"      backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"     forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"    beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"  end-of-buffer-or-history
[[ -n "${key[ShiftTab]}"  ]] && bindkey -- "${key[ShiftTab]}"  reverse-menu-complete

###############################################################################
# PROMPT ######################################################################
###############################################################################

PROMPT="%F{green}%m"      # Host name
PROMPT+=" %F{white}| "    # Pipe
PROMPT+="%F{green}%n"     # User name
PROMPT+=" %F{white}| "    # Pipe
PROMPT+="%F{green}%w, %*" # Day of the week and month
PROMPT+=" %F{white}| "    # Pipe
PROMPT+="%F{green}%~/"    # Current working directory
PROMPT+=$'\n'             # New line
PROMPT+="╚>"              # Whatever that is
PROMPT+="%F{white} "      # Make the rest white

###############################################################################
# CASE-INSENSITIVE TAB COMPLETION #############################################
###############################################################################

autoload -Uz compinit && compinit

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

###############################################################################
# AND ADDING ALL THE OTHER BITS IN ############################################
###############################################################################

source $HOME/.exports
source $HOME/.functions
source $HOME/.aliases
