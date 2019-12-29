# Shell options for days
shopt -s nocaseglob     # Case insensitive pathname extension
shopt -s histappend     # Don"t overwrite bash_
shopt -s cdspell        # Spell correction in cd
shopt -s checkwinsize   # Checks window size

# Add tab completion for SSH hostnames based on ~/.ssh/config
# ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr " " "\n")" scp sftp ssh;


if command -v rbenv >/dev/null; then
  eval "$(rbenv init -)"
fi

# And source things
. .exports
. .functions
. .prompt

# This goes at the end for speed of login
. .aliases
