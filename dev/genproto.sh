#!/bin/bash

##
## grpc-spec, definitions from the main gRPC repo
##

for i in proto/official/grpc-spec/*
do
  protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=grpc-spec/proto \
    --proto_path=proto/official/grpc-spec \
    $i
done

##
## test-grapesy, definitions from the main gRPC repo
##

for i in proto/official/test-grapesy/*
do
  protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=grapesy/proto \
    --proto_path=proto/official/test-grapesy \
    $i
done

##
## test-grapesy, bespoke definitions
##

for i in proto/bespoke/test-grapesy/*
do
  protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=grapesy/proto \
    --proto_path=proto/bespoke/test-grapesy \
    $i
done

##
## Disable some warnings in the generated modules, so that we can keep these
## warnings enabled for the test suite.
##

for i in grpc-spec/proto/Proto/*.hs grapesy/proto/Proto/*.hs
do
  sed -i '1i{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-identities #-}' $i
done