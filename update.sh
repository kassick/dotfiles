#!/bin/sh

echo "Pulling changes"

git pull origin master

echo "Updating submodules"
git submodule foreach "(git checkout master; git pull; cd ..; git add '$OLDPWD'; git commit -m 'Submodule Sync')"
