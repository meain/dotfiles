#!/bin/sh

export ZDOTDIR="$HOME/.config/zsh"
export NOTMUCH_CONFIG="$HOME/.config/notmuch/config"
export _Z_DATA="$HOME/.local/share/z/z"
export XDG_DATA_DIRS=$HOME/.nix-profile/share:$HOME/.share:"${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
