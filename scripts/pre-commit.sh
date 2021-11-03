#!/usr/bin/env bash

find src -type f -name "*.hs" | xargs fourmolu --mode check
find test -type f -name "*.hs" | xargs fourmolu --mode check
