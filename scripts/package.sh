#!/usr/bin/env bash
set -eu

GIT_REF=`git describe --tags`

if [ -z ${GIT_REF+x} ] || [ -z "$GIT_REF" ]
then
  echo "GIT_REF unbound"
  exit 1
else
  DEB="random-sleep"
  mkdir -p $DEB/DEBIAN
  cp scripts/control $DEB/DEBIAN/
  sed -i "2s/.*/Version:\ $GIT_REF/" $DEB/DEBIAN/control
  mkdir -p $DEB/usr/local/bin
  cp $(stack path --local-install-root)/bin/rsleep $DEB/usr/local/bin/
  dpkg-deb --build $DEB
fi
