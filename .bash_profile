# Sourcing for days
. .aliases
. .exports
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
