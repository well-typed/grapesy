#!/bin/bash

## Run the protoc compiler
##
## Assumes that `proto-lens-protoc` is installed globally.
##
## You could instead use a custom Cabal setup script (see `proto-lens-setup`),
## or the new Cabal Hooks infrastructure for this.

protoc \
  --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
  --haskell_out=generated \
  --proto_path=proto \
  route_guide.proto
