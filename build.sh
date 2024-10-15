#!/usr/bin/env bash

ghc -isrc Main.hs -o aec
find . -name "*.o" -delete
find . -name "*.hi" -delete
