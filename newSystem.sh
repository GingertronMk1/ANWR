#!/bin/bash

PATHHERE=$(pwd -L)

for f in Dotfiles/.*
do
  echo "Linking $f"
  ln -sf $PATHHERE/$f ~
done
