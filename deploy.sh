#!/usr/bin/env bash

target="${1:-rpi}"

export NIX_SSHOPTS="-t"
nixos-rebuild switch --use-remote-sudo --build-host localhost --target-host "$target" --flake ".#rpi-ad-blocker"
