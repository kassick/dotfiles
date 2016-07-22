#!/bin/sh

echo "Pulling changes"

git pull origin master

echo "Updating submodules"

git submodule update --remote --recursive --init
