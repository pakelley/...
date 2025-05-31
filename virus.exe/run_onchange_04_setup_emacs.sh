#!/usr/bin/env sh

echo "[emacs] cloning chemacs and doom"
if [ ! -d $HOME/.config/emacs ]; then
    git clone https://github.com/plexus/chemacs2.git $HOME/.config/emacs
fi
if [ ! -d $HOME/.config/doom-emacs ]; then
    git clone https://github.com/hlissner/doom-emacs $HOME/.config/doom-emacs
    $HOME/.config/doom-emacs/bin/doom install -!
fi

echo "[emacs] link doom (for topgrade)"
# link doom so topgrade can find it
if [ ! -f $HOME/.config/emacs/bin/doom ]; then
    mkdir -p $HOME/.config/emacs/bin
    ln -svf $HOME/.config/doom-emacs/bin/doom $HOME/.config/emacs/bin/doom
fi
