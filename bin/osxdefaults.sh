#!/usr/bin/sh

# Finder: show hidden files by default, show pathbar at bottom of window
defaults write com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.finder ShowPathbar -bool true

# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"

# Disable the sound effects on boot
sudo nvram SystemAudioVolume=" "

### Dock
# shrink icons
defaults write com.apple.dock tilesize -int 36
# hide dock
defaults write com.apple.dock autohide -bool true
# Only show open apps
defaults write com.apple.dock static-only -bool true
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
if defaults read com.apple.HIToolbox AppleEnabledInputSources | grep "KeyboardLayout Name" | awk '{print $4}' | grep -c '"Programmer';
then
    echo "Programmer Dvorak already configured, skipping setup."
else
    defaults write com.apple.HIToolbox AppleEnabledInputSources -array-add '<dict><key>InputSourceKind</key><string>Keyboard Layout</string><key>KeyboardLayout ID</key><integer>6454</integer><key>KeyboardLayout Name</key><string>Programmer Dvorak</string></dict>'
fi

# hide spotlight from menu bar
defaults -currentHost write com.apple.Spotlight MenuItemHidden -int 1

# get rid of Now Playing icon in menu bar
defaults write com.apple.controlcenter "NSStatusItem Visible NowPlaying" -bool false

# analog clock
defaults write com.apple.menuextra.clock IsAnalog -bool true

# f.lux settings
## set to San Diego
defaults write org.herf.Flux location "32.715330,-117.157260"
defaults write org.herf.Flux locationTextField "San Diego"
defaults write org.herf.Flux locationType L
## set wake time to 7am
defaults write org.herf.Flux wakeTime 420

# bluetooth and sound in menu bar
# defaults write com.apple.controlcenter "NSStatusItem Visible Bluetooth" -bool false
defaults write com.apple.controlcenter "NSStatusItem Visible Sound" -bool true
defaults write com.apple.controlcenter "NSStatusItem Visible WiFi" -bool false
defaults write com.apple.menuextra.clock.plist "DateFormat" "EEE MMM d  h:mm:ss"

# remove siri from menu bar
defaults write com.apple.Siri StatusMenuVisible -bool false
