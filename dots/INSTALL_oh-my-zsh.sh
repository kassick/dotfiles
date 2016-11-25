#!/bin/sh

REPO="git://github.com/robbyrussell/oh-my-zsh.git"
DEST=~/.oh-my-zsh

cd $DEST
if [ -d .git ]; then
    git pull origin master
else
    git init
    git remote add origin $REPO
    git pull origin master
fi
