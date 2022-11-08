#!/bin/sh
export NIX_SSHOPTS="-t"
nixos-rebuild switch --use-remote-sudo --build-host localhost --target-host pinto --flake ".#pinto"
