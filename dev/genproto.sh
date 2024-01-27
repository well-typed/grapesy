#!/bin/bash

##
## Our own .proto files
##

protoc \
  --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
  --haskell_out=proto \
  --proto_path=proto \
  ping.proto

##
## Examples and test cases from the gRPC repo
##

if [ -z "${GRPC_REPO}" ]
then
  echo "Please set GPRC_REPO"
  exit
fi

for i in helloworld route_guide
do
  protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=proto \
    --proto_path=${GRPC_REPO}/examples/protos \
    $i.proto
done

for i in test empty messages
do
  protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=proto \
    --proto_path=${GRPC_REPO} \
    src/proto/grpc/testing/$i.proto
done


