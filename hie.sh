#!/usr/bin/env bash

exec nix-shell --run "exec /run/current-system/sw/bin/hie $@"
