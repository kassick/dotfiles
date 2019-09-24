#!/bin/bash

REPO="https://github.com/robbyrussell/oh-my-zsh.git"
DEST=~/.oh-my-zsh

if [ -d $DEST ]; then
    cd $DEST
    if [ -d .git ]; then
        if git remote show origin -n | grep -q $REPO; then
            git pull origin master
            exit 0
        fi
    fi

    # not in git or not from the correct repo
    echo "Old install, moving out of the way"
    cd ~
    mv $DEST ~/__oh-my-zsh.bk
fi

mkdir $DEST || exit 1
pushd $DEST
git init && \
    git remote add origin $REPO && \
    git pull origin master || \
        exit 1

popd
