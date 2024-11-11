#!/usr/bin/env bash

ghc -isrc Main.hs -o mtc
find . -name "*.o" -delete
find . -name "*.hi" -delete
