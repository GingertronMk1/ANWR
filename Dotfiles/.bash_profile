# Sourcing for days
. .aliases
. .exports
. .functions
. .prompt

# Shell options for days
shopt -s nocaseglob;    # Case insensitive pathname extension
shopt -s histappend;    # Don"t overwrite bash_history
shopt -s cdspell;       # Spell correction in cd
shopt -s checkwinsize   # Checks window size
shopt -s nullglob       # I think this means using a wildcard won't return the "*" char

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr " " "\n")" scp sftp ssh;

# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
# export X11_PATH="/opt/X11/include:$PATH"
export PATH
