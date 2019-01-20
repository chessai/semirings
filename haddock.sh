#!/bin/sh
set -e

dir=$(mktemp -d dist-new-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal new-haddock --builddir="$dir" --haddock-for-hackage --haddock-option=--hyperlinked-source
# Starting with cabal 2.0, `--publish` is needed for uploading to non-candidate releases
cabal upload --publish -d $dir/*-docs.tar.gz