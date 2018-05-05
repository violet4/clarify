#!/usr/bin/env bash
elm-package install
elm-make src/App.elm --output js/main.js
