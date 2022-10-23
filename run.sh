#!/bin/sh
# Run the app in a dev container

# This may require enabling iglx in XQuartz first:
# defaults write org.xquartz.X11 enable_iglx -bool true  

IMAGE=$(docker images | grep 'vsc-orrey' | awk '$1~"vsc-orrey" {print $1}')
WORKSPACE=/workspaces/orrey
APP=dist-newstyle/build/x86_64-linux/ghc-8.10.7/orrey-1.0.0/build/orrey/orrey
IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')

xhost + $IP
docker run -ti --name orrey --rm -e DISPLAY=$IP:0 -u vscode -v $(pwd):$WORKSPACE $IMAGE $WORKSPACE/$APP

