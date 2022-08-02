#!/usr/bin/sh

# Finder: show hidden files by default
defaults write com.apple.finder AppleShowAllFiles -bool true

# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"

# Disable the sound effects on boot
sudo nvram SystemAudioVolume=" "

### Dock
# Hide dock
defaults write com.apple.dock tilesize -int 1
defaults write com.apple.dock pinning -string start
# Only show open apps
defaults write com.apple.dock static-only -bool TRUE
# Align left
defaults write com.apple.dock pinning -string start

# tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults write com.apple.AppleMultitouchTrackpad Clicking -bool true

# 3-finger drag to select
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true

# Avoid creating .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# disable mission control C-<right> and C-<left> shortcuts
plutil -replace AppleSymbolicHotKeys.79.enabled -bool NO ~/Library/Preferences/com.apple.symbolichotkeys.plist
plutil -replace AppleSymbolicHotKeys.80.enabled -bool NO ~/Library/Preferences/com.apple.symbolichotkeys.plist
plutil -replace AppleSymbolicHotKeys.81.enabled -bool NO ~/Library/Preferences/com.apple.symbolichotkeys.plist
plutil -replace AppleSymbolicHotKeys.82.enabled -bool NO ~/Library/Preferences/com.apple.symbolichotkeys.plist

# Input Sources (programmer dvorak)
if defaults read com.apple.HIToolbox AppleEnabledInputSources | grep "KeyboardLayout Name" | awk '{print $4}' | grep -c '"Brogrammer';
then
    echo "Programmer Dvorak already configured, skipping setup."
else
    defaults write com.apple.HIToolbox AppleEnabledInputSources -array-add '<dict><key>InputSourceKind</key><string>Keyboard Layout</string><key>KeyboardLayout ID</key><integer>6454</integer><key>KeyboardLayout Name</key><string>Programmer Dvorak</string></dict>'
fi
