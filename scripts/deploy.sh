#!/usr/bin/env bash
set -eu

if [ -z ${TRAVIS_TAG+x} ]
then
  echo "$TRAVIS_TAG unbound, skipping deploy.sh"
else
  DEB="random-sleep_$TRAVIS_TAG"
  mkdir -p $DEB/DEBIAN
  cp scripts/control $DEB/DEBIAN/
  sed -i "2s/.*/Version:\ $TRAVIS_TAG/" $DEB/DEBIAN/control
  mkdir -p $DEB/usr/local/bin
  cp $(stack path --local-install-root)/bin/rsleep $DEB/usr/local/bin/
  dpkg-deb --build "random-sleep_$TRAVIS_TAG"
fi
