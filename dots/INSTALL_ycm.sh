#!/bin/sh

REPO="https://github.com/Valloric/YouCompleteMe"
DEST=~/.local/dev/ycm

echo "Installing Dependencies"
for pkg in  python-devel \
                clang-libs clang-devel \
                mono-devel \
                xbuild \
                golang \
                mono-xbuild \
                libboost-thread-dev \
                libclang-3.9-dev \
                python-dev \
                python3-dev;
do
    pkcon install -y $pkg
done


echo "Cloning YCM repository"

mkdir -p "$DEST"
cd $DEST

if [ -d .git ]; then
    git pull origin master
else
    git init
    git remote add origin $REPO
    git pull origin master
fi

YCM_CLANG_OPTS="--clang-completer --system-libclang"
YCM_OMNISHARP_OPTS="--omnisharp-completer"
# system boost may create issues with python2/3 compatibility of the generated executable ... --system-boost
YCM_SYSTEM_OPTS=
YCM_GOCODE_OPTS="--gocode-completer"

if [ -f ${DOT_PATH}/ycm_opts.sh ] ; then
    echo " ----- Using options from $DOT_PATH/ycm_opts.sh -----"
    . ${DOT_PATH}/ycm_opts.sh
fi

git submodule update --init --recursive && \
    ./install.py $YCM_CLANG_OPTS $YCM_OMNISHARP_OPTS $YCM_SYSTEM_OPTS $YCM_GOCODE_OPTS
