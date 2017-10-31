#!/bin/bash

cabal build
cp dist/build/rhine-ghcjs/rhine-ghcjs.jsexe/all.js html/all.js
