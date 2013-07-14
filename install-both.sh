#!/bin/sh -ex

cabal clean
mv gtksourceview2.cabal-renamed gtksourceview2.cabal || true
mv gtksourceview3.cabal gtksourceview3.cabal-renamed || true
cabal-src-install "$@"

cabal clean
mv gtksourceview3.cabal-renamed gtksourceview3.cabal || true
mv gtksourceview2.cabal gtksourceview2.cabal-renamed || true
cabal-src-install "$@"

