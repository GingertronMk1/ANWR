###############################################################################
# UNSET ALL ALIASES ###########################################################
###############################################################################

unalias -m '*'

###############################################################################
# SHELL OPTIONS ###############################################################
###############################################################################

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down


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
setopt NO_BEEP            # SHUT UP

###############################################################################
# PROMPT ######################################################################
###############################################################################

prompt_primary="green"

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  prompt_primary="blue"
fi

PROMPT="%F{$prompt_primary}%m"      # Host name
PROMPT+=" %F{white}| "    # Pipe
PROMPT+="%F{$prompt_primary}%n"     # User name
PROMPT+=" %F{white}| "    # Pipe
PROMPT+="%F{$prompt_primary}%w, %*" # Day of the week and month
PROMPT+=" %F{white}| "    # Pipe
PROMPT+="%F{$prompt_primary}%~/"    # Current working directory
PROMPT+=$'\n'             # New line
PROMPT+="╚>"              # Whatever that is
PROMPT+="%F{white} "      # Make the rest white

###############################################################################
# CASE-INSENSITIVE TAB COMPLETION #############################################
###############################################################################

autoload -Uz compinit && compinit

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

alias res="source $HOME/.zshrc"

###############################################################################
# AND ADDING ALL THE OTHER BITS IN ############################################
###############################################################################

source $HOME/.exports
source $HOME/.functions
source $HOME/.aliases

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env" # cargo-env
