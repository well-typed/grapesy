#!/bin/bash

openssl req -x509 -sha256 -days 365 -nodes -out data/grpc-demo.cert -keyout data/grpc-demo.priv -subj "/CN=localhost"
