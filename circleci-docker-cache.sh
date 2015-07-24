#!/bin/bash

CACHE_DIR="$HOME/docker"
IMAGE="$CACHE_DIR/image.tar"
DOCKER_TAG=crogers/stack-ide-atom-docker:stack-0.1.2.0_nightly-2015-06-28

mkdir -p $CACHE_DIR
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