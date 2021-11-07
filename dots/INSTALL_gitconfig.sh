#!/bin/sh

git config --global user.name 'Rodrigo Kassick'
git config --global user.email kassick@gmail.com
git config --global core.editor 'emacsclient -t --alternate-editor="emacs"'
git config --global diff.tool meld
git config --global merge.tool meld
git config --global pull.rebase true
git config --global init.defaultBranch main
