# Sourcing for days
. .aliases
. .functions
. .prompt


# Colours for days
Green="\[\033[0;32m\]"      # Green
Cyan="\[\033[0;36m\]"       # Cyan
BWhite="\[\033[1;37m\]"     # Bold White
NC="\[\033[m\]"             # Colour Reset

# Shell options for days
shopt -s nocaseglob;    # Case insensitive pathname extension
shopt -s histappend;    # Don"t overwrite bash_history
shopt -s cdspell;       # Spell correction in cd
shopt -s checkwinsize   # Checks window size
# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr " " "\n")" scp sftp ssh;


# Sourcing for days
. .aliases
. .functions
. .prompt

# Exporting for days
export CLICOLOR=1       # Cos colour in Terminal is useful
export EDITOR="vim"     # EMACS HERETICS WILL BURN
export HISTSIZE=16384   # Lots of history
export HISTCONTROL=ignoredups   # Ignore duplicate entries in history
export TERM="xterm-256color"    # Force 256 colours, mostly for vim
export LANG="en_GB.UTF-8"       # These two force British English and UTF-8
export LC_ALL="en_GB.UTF-8"
