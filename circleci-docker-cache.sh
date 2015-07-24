#!/bin/bash

CACHE_DIR="$HOME/cache"
IMAGE="$CACHE_DIR/image.tar"
CABAL_SANDBOX="$CACHE_DIR/cabal-sandbox"
DOCKER_STACK="$CACHE_DIR/docker-stack"
DOCKER_TAG=crogers/stack-ide-atom-docker:stack-0.1.2.0_nightly-2015-06-28

GIT_DIR="$HOME/stack-ide-atom"

mkdir -p $CACHE_DIR

ln -s "$GIT_DIR/haskell/.cabal-sandbox" $CABAL_SANDBOX
ln -s "$GIT_DIR/.docker-stack" $DOCKER_STACK

if [[ -e $IMAGE ]]; then
	echo Loading docker image...
	time docker load -i $IMAGE
else
	echo FROM $DOCKER_TAG >Dockerfile
	echo Downloading docker layers...
	time docker build .
	echo Saving docker image...
	time docker save $DOCKER_TAG >$IMAGE
fi