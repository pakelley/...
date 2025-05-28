#!/usr/bin/env sh

echo "[homebrew] Starting homebrew services"
# brew services restart yabai
# brew services restart skhd
yabai --start-service
skhd --start-service
