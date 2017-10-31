#!/bin/bash

cabal build
cp dist/build/rhine-example-ghcjs-1/rhine-example-ghcjs-1.jsexe/all.js html/all.js
