#!/bin/sh
export NIX_SSHOPTS="-t"
nixos-rebuild switch --use-remote-sudo --build-host localhost --target-host "$1" --flake ".#rpi3"
