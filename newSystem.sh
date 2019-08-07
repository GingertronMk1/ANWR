#!/bin/bash

pathhere=$(pwd -P)

for f in Dotfiles/.*
do
  echo "Linking $f"
  ln -sf $pathhere/$f ~
done
