#!/bin/sh

cd ~/.emacs.d
git pull remote origin &&
    git submodule update --init --recursive &&
    bash ./bootstrap.sh
