#!/bin/bash

cabal build
echo "copying..."
cp dist/build/rhine-ghcjs/rhine-ghcjs.jsexe/all.js html/all.js
