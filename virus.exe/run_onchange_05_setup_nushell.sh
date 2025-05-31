#!/usr/bin/env sh

# link nushell for macos
if [ ! -d $HOME/Library/Application\ Support/nushell ]; then
    echo "[nushell] linking nushell config"
    ln -svf $HOME/.config/nushell $HOME/Library/Application\ Support/nushell
fi

if [ ! -f $HOME/.local/share/.zoxide.nu ]; then
    echo "[nushell] setting up zoxide for nushell"
    zoxide init nushell > $HOME/.local/share/.zoxide.nu
fi
