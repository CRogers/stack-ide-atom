#!/bin/sh

SOURCES="$(cabal sandbox list-sources -v0)"

if [ -z "$SOURCES" ]; then
  cabal sandbox add-source ../stack-ide/ide-backend/ide-backend-common
  cabal sandbox add-source ../stack-ide/stack-ide-api
fi