#!/usr/bin/env bash
elm-package install
elm-make src/App.elm --output js/main.js
elm-make slides/Counter.elm --output slides/counter.js
