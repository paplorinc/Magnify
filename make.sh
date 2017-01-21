#!/usr/bin/env bash
elm package install && elm make --output main.js Main.elm && elm reactor