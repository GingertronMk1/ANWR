###############################################################################
# UNSET ALL ALIASES ###########################################################
###############################################################################

unalias -a

###############################################################################
# SHELL OPTIONS ###############################################################
###############################################################################

shopt -s nocaseglob     # Case insensitive pathname extension
shopt -s histappend     # Don"t overwrite bash_history
shopt -s cdspell        # Spell correction in cd
shopt -s checkwinsize   # Checks window size
shopt -s dotglob


###############################################################################
# AUTOCOMPLETE SSH CLIENTS ####################################################
###############################################################################

[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr " " "\n")" scp sftp ssh;

###############################################################################
# ALSO CONTAINED IN .inputrc ##################################################
###############################################################################

# Allow UTF-8 input and output
set convert-meta off
set input-meta on
set output-meta on

# Make Tab autocomplete regardless of filename case
set completion-ignore-case on

# When hitting Tab on ~ expand it to show the full path
set expand-tilde on

# List all matches if multiple completions are possible
#set show-all-if-ambiguous on
set show-all-if-unmodified on

# Show extra file info when completing
set visible-stats on

# Add a trailing slash when autocompleting directory symlinks
set mark-symlinked-directories on

# No bell
set bell-style none

###############################################################################
# PROMPT ######################################################################
###############################################################################

Green="\[\033[0;32m\]"        # Green
Cyan="\[\033[0;36m\]"         # Cyan
BWhite="\[\033[1;37m\]"       # Bold White
NC="\[\033[m\]"               # Colour Reset

Split=" ${BWhite}|${NC} "
if [ "$SSH_CLIENT" ]; then
    ColourStyle=$Cyan         # If I"m SSHing into the machine, make the prompt cyan
else
    ColourStyle=$Green        # Otherwise, make it look green
fi
PS1="${ColourStyle}\h"        # Hostname
PS1+="$Split"                 # An emboldened white pipe
PS1+="${ColourStyle}\u"       # User I'm logged in as
PS1+="$Split"                 # Another emboldened white pipe
PS1+="${ColourStyle}\d, \A"   # The current date and time to the nearest minute
PS1+="$Split"                 # Another emboldened white pipe
PS1+="${ColourStyle}\w/"      # The working directory, with a / at the end just because I like it that way
PS1+="\n╚> ${NC}"             # A new line, with a couple characters for interestingness

PS2="${ColourStyle}╚> ${NC}"  # PS2 prompt meanwhile is just a '>'

alias res=". $HOME/.bashrc" # Re-source .bashrc, which re-sources everything else

###############################################################################
# AND ADDING ALL THE OTHER BITS IN ############################################
###############################################################################

. $HOME/.exports
. $HOME/.functions
. $HOME/.aliases
. "$HOME/.cargo/env"
