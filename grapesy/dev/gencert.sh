#!/bin/bash

openssl req -x509 -sha256 -days 365 -nodes -out data/grpc-demo.pem -keyout data/grpc-demo.key -subj "/CN=127.0.0.1"
