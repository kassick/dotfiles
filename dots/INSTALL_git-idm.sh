#!/bin/sh

REPO=https://github.com/samrocketman/git-identity-manager.git
MAIN_BRANCH=main
DEST=~/.local/share/git-idm
BK_DEST=`echo $DEST | sed -e 's#/#__#g'`

if [ -d "$DEST" ] ; then
    pushd $DEST
    if [ -d .git ]; then
        if git remote show origin -n | grep -q $REPO; then
            git pull origin $MAIN_BRANCH
            ln -sf $DEST/git-idm ~/.local/bin
            exit 0
        else
            echo "git remote does not match repo $REPO"
            exit 1
        fi
    fi
    popd

    # not in git or not from the correct repo
    echo "Old install, moving out of the way"

    if [ -e "$BK_DEST" ]; then
        echo "Backup path $BK_DEST exists; refusing to continue"
        exit 1;
    fi

    mv $DEST $BK_DEST
fi

mkdir $DEST || exit 1
pushd $DEST
git init &&
    git remote add origin $REPO &&
    git pull origin $MAIN_BRANCH ||
        exit 1

[ ! -d ~/.local/bin ] && mkdir -p ~/.local/bin

ln -sf $DEST/git-idm ~/.local/bin
