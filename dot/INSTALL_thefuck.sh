#!/bin/sh

echo "Installing python3-dev and python3-pip"
pkcon install -y python3-devel
pkcon install -y python3-pip

echo "Installing thefuck"
if [ -f $HOME/.local/bin/thefuck ]; then
    pip3 install --user thefuck --upgrade
else
    pip3 install --user thefuck
fi
