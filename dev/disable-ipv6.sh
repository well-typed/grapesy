#!/bin/bash

##
## Disable IPv6 for Docker
##
## The gRPC interop tests get confused if IPv6 is enabled: they only expect an
## IPv4 port to be listed by `docker port`, and throw a parse error when there
## is also an IPv6 port. Disabling IPv6 in `/etc/docker/daemon.json` instead
## would be cleaner but does not seem to have the desired effect.
##

# Disable IPv6 on the host
sysctl -w net.ipv6.conf.all.disable_ipv6=1
sysctl -w net.ipv6.conf.default.disable_ipv6=1

# Restart docker
systemctl restart docker


