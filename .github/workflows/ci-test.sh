#!/usr/bin/env bash
set -e

# Run tests in sequence as integration tests do collide
cabal test mithril-monitor
cabal test mithril-end-to-end
