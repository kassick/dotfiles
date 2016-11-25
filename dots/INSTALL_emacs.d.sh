#!/bin/sh

REPO="git@bitbucket.org:kassick/emacs.d.git"
DEST=~/.emacs.d

cd $DEST
if [ -d .git ]; then
    git pull origin master
else
    git init
    git remote add origin $REPO
    git pull origin master
fi

git submodule update --init --recursive && bash ./bootstrap.sh
