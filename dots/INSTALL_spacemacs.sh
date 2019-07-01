#!/bin/bash

# Install spacemacs in .emacs.d

REPO="https://github.com/syl20bnr/spacemacs"
DEST=~/.emacs.d

if [ -d $DEST ]; then
    cd $DEST
    if [ -d .git ]; then
        if git remote show origin -n | grep -q $REPO; then
            git pull origin master
            exit 0
        fi
    fi

    echo "Old install, moving out of the way"
    cd ~
    mv $DEST ~/__emacs.d.bk
fi

mkdir $DEST || exit 1
pushd $DEST
git init && \
    git remote add origin $REPO && \
    git pull origin master || \
        exit 1
popd

# Needed by pdftools
# for pkg in poppler-devel \
#                poppler-glib-devel \
#                the_silver_searcher ;
# do
#     pkcon install $pkg
# done
