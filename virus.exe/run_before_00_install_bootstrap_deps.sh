#!/usr/bin/env sh

set -euo pipefail

echo "[virus] Installing bootstrap dependencies …"

echo "[homebrew] Ensure homebrew is installed …"
if ! command -v brew >/dev/null 2>&1; then
  echo "[homebrew] Installing Homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

if [[ -d /opt/homebrew ]]; then          # Apple Silicon
  HOMEBREW_PREFIX=/opt/homebrew
elif [[ -d /usr/local/Homebrew ]]; then  # Intel
  HOMEBREW_PREFIX=/usr/local
else
  echo "[homebrew] ERROR: Homebrew installed in an unknown location."
  echo "Expected /opt/homebrew (Apple Silicon) or /usr/local (Intel)."
  echo "Please reinstall Homebrew in a standard prefix and rerun dotfile install."
  exit 1
fi
echo "[homebrew] Using prefix: ${HOMEBREW_PREFIX}"
eval "$("${HOMEBREW_PREFIX}/bin/brew" shellenv)"

echo "[homebrew] Updating formulae"
brew update

echo "[terminal-notifier] Ensure terminal-notifier is installed"
if ! command -v terminal-notifier >/dev/null 2>&1; then
    brew install terminal-notifier
fi

echo "[syncthing] Ensure syncthing is installed"
if ! command -v syncthing >/dev/null 2>&1; then
    brew install syncthing
    echo "[syncthing] Starting syncthing service"
    brew services restart syncthing
    terminal-notifier -message "Set up syncthing (at least for password manager) at http://localhost:8384" -title "dotfiles"
    read -p "[syncthing] Set up syncthing (at least for password manager) at http://localhost:8384, press enter when finished..."
fi

echo "[keepassxc] Ensure KeePassXC is installed"
if ! command -v keepassxc-cli >/dev/null 2>&1; then
    brew install --cask keepassxc
    terminal-notifier -message "Open KeePassXC and open password database" -title "dotfiles"
    read -p "[keepassxc] Open KeePassXC and open password database, press enter when finished..."
fi


echo "[python] Ensure python/uv is installed …"
if ! command -v python &> /dev/null || ! command -v uv &> /dev/null; then
  brew install pyenv
  pyenv install 3.12
  pyenv global 3.12
  eval "$(pyenv init - sh)"
  brew install uv
fi

echo "[python] Install Textual + tomli-w (for config bootstrap script)"
/usr/bin/env python -m pip install --user --upgrade pip
/usr/bin/env python -m pip install --user --upgrade textual tomli-w

echo "[virus] Bootstrap deps installed!"
