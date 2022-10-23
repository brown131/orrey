#!/bin/zsh
# Package orrey for docker distribution

IMAGE=brown131/orrey

docker images | grep $IMAGE
if [ $? -ne 0 ]
then docker build . -t $IMAGE
fi

docker save $IMAGE | gzip > ${IMAGE/\//_}.tgz
