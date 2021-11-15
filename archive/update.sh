#!/bin/sh

echo "Pulling changes"

git pull origin master

echo "Updating submodules"
git submodule foreach "(git checkout master; git pull; cd ..; git add '$OLDPWD'; git commit -m 'Submodule Sync')"

for sub in dots/*/.git bundle/*/.git install/*/.git; do
    sub=`dirname "$sub"`
    if [ -d "$sub" ]; then
        pushd "$sub"
        git pull
        popd
    fi

done
