#!/usr/bin/env sh

find Dotfiles -depth 1 -print0 | while read -d $'\0' file
do
  based=$(basename "$file")
  homed="$HOME/$based"
  fullFile="$(pwd)/$file"
  ln -sFv "$fullFile" "$homed"
done
