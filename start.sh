#!/bin/sh
# Start the app in a release container.

# This may require enabling iglx in XQuartz first:
# defaults write org.xquartz.X11 enable_iglx -bool true  

IMAGE=brown131/orrey
IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')

xhost + $IP
docker run -ti --name orrey --rm -e DISPLAY=$IP:0 -u vscode $IMAGE

