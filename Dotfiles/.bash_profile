# Sourcing for days
. .aliases
. .exports
. .functions
. .prompt

# Shell options for days
shopt -s nocaseglob     # Case insensitive pathname extension
shopt -s histappend     # Don"t overwrite bash_
shopt -s cdspell        # Spell correction in cd
shopt -s checkwinsize   # Checks window size

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr " " "\n")" scp sftp ssh;

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
  PATH="$PATH:$HOME/bin"
fi

# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"

if [ -d "$HOME/.composer" ] ; then
  PATH="$PATH:$HOME/.composer/vendor/bin"
fi

export PATH

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

