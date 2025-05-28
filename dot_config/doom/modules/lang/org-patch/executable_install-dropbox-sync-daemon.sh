#!/usr/bin/env bash
TEMPLATE_PATH="$HOME/.config/doom/modules/lang/org-patch/com.user.org-dropbox-sync.plist"
BREW_PREFIX=$(brew --prefix)
PLIST_CONTENT=$(sed -e "s|BREW_PREFIX|$BREW_PREFIX|g" -e "s|HOME|$HOME|g" $TEMPLATE_PATH)

PLIST_OUTPUT_PATH="$HOME/Library/LaunchAgents/com.user.org-dropbox-sync.plist"

echo "$PLIST_CONTENT" > "$PLIST_OUTPUT_PATH"
chown $(whoami) "$PLIST_OUTPUT_PATH"
chmod 644 "$PLIST_OUTPUT_PATH"
chmod +x "$PLIST_OUTPUT_PATH"

# Load the service
launchctl load "$PLIST_OUTPUT_PATH"
