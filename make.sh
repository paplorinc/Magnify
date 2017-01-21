#!/usr/bin/env bash
elm package install && elm make --output main.js src/Main.elm && elm reactor