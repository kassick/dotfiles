#!/bin/sh

echo "Installing python3-dev and python3-pip"
pkcon install -y python3-devel python3-dev
pkcon install -y python3-pip
pkcon install -y redhat-rpm-config

echo "Installing thefuck"
if [ -f $HOME/.local/bin/thefuck ]; then
    pip3 install --user thefuck --upgrade
else
    pip3 install --user thefuck
fi
