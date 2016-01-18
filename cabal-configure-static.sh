#!/bin/sh

set -e

# This configures cabal for a static binary (under linux).

cabal configure \
      --disable-executable-dynamic \
      --ghc-option=-optl=-static \
      --ghc-option=-optl=-pthread
