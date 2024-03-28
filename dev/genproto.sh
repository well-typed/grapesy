#!/bin/bash

###
### NOTE: If you re-generate the Haskell files, you will need to add
###
### {-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
###
### to each generated module (so that we can keep this warning enabled for the
### test suite).
###

##
## Our own .proto files
##

protoc \
  --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
  --haskell_out=proto \
  --proto_path=proto \
  ping.proto

##
## From the gRPC repo
##

for i in dev/grpc-proto/*.proto
do
  protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=proto \
    --proto_path=dev/grpc-proto \
    $i
done
