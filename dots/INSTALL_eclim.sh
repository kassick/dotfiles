#!/bin/sh

ECLIM_MAJOR=2
ECLIM_MINOR=6
ECLIM_REL=0
ECLIM_VERS="${ECLIM_MAJOR}.${ECLIM_MINOR}.${ECLIM_REL}"
ECLIM_JAR="eclim_${ECLIM_VERS}.jar"
ECLIM_URL="https://github.com/ervandew/eclim/releases/download/${ECLIM_VERS}/${ECLIM_JAR}"
DOWNLOAD_DIR=${DOT_PATH}/eclim
ECLIPSE_HOME=~/Dev/Eclipse/eclipse

if [ ! -d ${DOWNLOAD_DIR} ] ; then
    mkdir -p ${DOWNLOAD_DIR}
    if [ $? != 0 ] ; then
        echo "could not create download directory"
        exit 1
    fi
fi

pushd ${DOWNLOAD_DIR}
if [ -f ${ECLIM_JAR} ]; then
    echo Using cached jar for installation
else
    wget ${ECLIM_URL}
    if [ $? != 0 ] ; then
        echo "Error downloading"
        exit 1
    fi
fi


java \
    -Dvim.skip=true \
    -Declipse.home=${ECLIPSE_HOME} \
    -jar eclim_2.6.0.jar install ;

if [ $? != 0 ] ;
then
      echo "Could not install dot eclim"
      exit 1
fi

exit 0
