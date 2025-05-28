
sudo rm "/tmp/com.user.org-dropbox-sync.bak.out" || true
mv "/tmp/com.user.org-dropbox-sync.out" "/tmp/com.user.org-dropbox-sync.bak.out"
touch "/tmp/com.user.org-dropbox-sync.out"
sudo rm "/tmp/com.user.org-dropbox-sync.bak.err" || true
mv "/tmp/com.user.org-dropbox-sync.err" "/tmp/com.user.org-dropbox-sync.bak.err"
touch "/tmp/com.user.org-dropbox-sync.err"

launchctl kickstart -k "gui/$(id -u)/com.user.org-dropbox-sync"
