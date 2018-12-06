#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1

argv=( "$@" )
argv=( "${argv[@]/\'/\'\\\'\'}" )
argv=( "${argv[@]/#/\'}" )
argv=( "${argv[@]/%/\'}" )

echo >&2 "launching nix-shell..."

exec nix-shell --pure --run "exec /run/current-system/sw/bin/hie ${argv[*]}"
