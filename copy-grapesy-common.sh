#!/bin/sh

# grapesy-common
##############################################################################

rm -rf grapesy-common/src

# Copy all sources
cp -r grapesy/src grapesy-common/

# ... and then delete HTTP2 specific stuff
for f in Client/Run.hs Client.hs Util/HTTP2.hs Server/Run.hs; do
  rm -f grapesy-common/src/Network/GRPC/$f
done

# grapesy-http2
##############################################################################

rm -rf grapesy-http2/src
mkdir -p grapesy-http2/src/Network/GRPC/Client
mkdir -p grapesy-http2/src/Network/GRPC/Server
mkdir -p grapesy-http2/src/Network/GRPC/Util

for f in Client/Run.hs Client.hs Util/HTTP2.hs Server/Run.hs; do
  cp grapesy/src/Network/GRPC/$f grapesy-http2/src/Network/GRPC/$f
done
