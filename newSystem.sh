#!/bin/bash

pathhere=$(pwd -L)

for f in Dotfiles/.*
do
  echo "Linking $f"
  ln -sf $pathhere/$f ~
done
