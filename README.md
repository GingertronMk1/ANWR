# Jack's Odds and Sods

I'm a Computer Science student, I feel I need a dotfiles repo somewhere. So here it is!

### /
- NewSystem.sh: a shell script that points files in the right direction on a new system.
- .hushlogin: makes Debian systems shut up when I log in.
- README.md: you're reading it.

### /Dotfiles
- .aliases: where my aliases live (there's a few of them).
- .bash_profile: the meat of the dotfiles. Lots of config here.
- .bashrc: I'm certain there's something better I could do with this, but as it stands it just points to bash_profile.
- .exports: all the things I want exported, mostly related to bash history config.
- .functions: functions, really. I've only got the four at the moment, but they still might clutter up the bash_profile.
- .gitconfig: there's probably some security issue with me posting my gitconfig here, but oh well. I don't think it stores passwords, and if it did, I'm pretty certain I'd hear about it before anyone nicked anything.
- .gitignore: file types that aren't here.
- .inputrc: macOS doesn't seem to want to tab-complete, so this is here.
- .prompt: my PS1 and PS2 prompts. PSes 3 and 4 I don't know or care enough about to set.
- .vimrc: my first dotfile, the basis for all my editing. Bane of one of my mates who's used to vanilla vim.
- .xsessionrc: makes Debian play nice with ThinkPads and their nipple mice.

### /Scripts
- ConvertAndTidy.sh: Converts, tidies, or does both to all the mp3s/m4as in a folder. Currently it's pointed at ~/Desktop/Scratch, but it can be wherever.
- MacOS.sh: a shell script that sets sensible defaults on a Mac. Quite why I made this I don't know, as I'm probably not going to get a new Mac for a very long time...
- NewPackages.sh: Installing useful things on a new system.

### /vimcolors
- jack.vim: A modified version of [molokai.vim](https://github.com/tomasr/molokai), stripped down to what I want, and highlighting made bright blue.
