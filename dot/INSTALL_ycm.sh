#!/bin/sh

REPO="https://github.com/Valloric/YouCompleteMe"
DEST=~/.local/dev/ycm

echo "Installing Dependencies"
pkcon install -y python-devel \
      clang-libs clang-devel \
      xbuild \
      golang

echo "Cloning YCM repository"

mkdir -p "$DEST"
cd $DEST

if [ -d .git ]; then
    git pull origin master
else
    git init
    git remote add origin $REPO
    git pull origin master
fi

git submodule update --init --recursive && \
    ./install.py --clang-completer --system-libclang \
                 --omnisharp-completer \
                 --system-boost \
                 --gocode-completer
