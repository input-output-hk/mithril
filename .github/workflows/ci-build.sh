#!/usr/bin/env bash
set -e

cd mithril-test-lab

cabal update
cabal build --enable-tests all
