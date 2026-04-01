#!/bin/bash

##
## Update vendored packages to latest version and revision
##

# We construct a reproducible tarball, using the revision date for timestamps.
#
# See https://www.gnu.org/software/tar/manual/html_section/Reproducibility.html

PKGS=(
  "proto-lens-0.7.1.7"                "2026-04-16T04:41:03Z"
  "proto-lens-protobuf-types-0.7.2.3" "2026-04-16T04:41:43Z"
  "proto-lens-protoc-0.9.0.1"         "2026-04-16T04:41:44Z"
  "proto-lens-runtime-0.7.0.8"        "2026-04-16T04:41:45Z"
)

TARFLAGS="
  --sort=name --format=posix
  --pax-option=exthdr.name=%d/PaxHeaders/%f
  --pax-option=delete=atime,delete=ctime
  --clamp-mtime
  --numeric-owner --owner=0 --group=0
  --mode=go+u,go-w
"
GZIPFLAGS="
  --no-name --best
"

rm -f *.tar.gz
for ((i=0; i<4; i++))
do
  PKG=${PKGS[$i * 2]}
  REV=${PKGS[$i * 2 + 1]}
  cabal get -v0 "$PKG"

  find "$PKG" -exec touch -md $REV {} \;
  tar $TARFLAGS --mtime=$REV -cf "$PKG.tar" "$PKG"
  gzip $GZIPFLAGS "$PKG.tar"

  rm -rf "$PKG"
done
