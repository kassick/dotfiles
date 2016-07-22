#!/bin/sh

echo "Pulling changes"

git pull origin master

echo "Updating submodules"

git submodule foreach 'git checkout master'
git submodule foreach 'git pull origin'
