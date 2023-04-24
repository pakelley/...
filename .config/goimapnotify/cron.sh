#!/usr/bin/env bash
# to debug: `cat /var/mail/pakelley`, `cat /tmp/gmailieer_patchwork_notmuch.log`
set -e

export NOTMUCH_CONFIG="$HOME/.config/notmuch/default/config"

if [ -d "$HOME/.local/share/mail/account.kelleys-gmail" ]
then
    date >> /tmp/gmailieer_patchwork_notmuch.log
    $HOME/.pyenv/shims/gmi sync -C "~/.local/share/mail/account.kelleys-gmail" >> /tmp/gmailieer_patchwork_notmuch.log 2>&1
fi

if [ -d "$HOME/.local/share/mail/account.heartex" ]
then
    date >> /tmp/gmailieer_patchwork_notmuch.log
    $HOME/.pyenv/shims/gmi sync -C "~/.local/share/mail/account.heartex" >> /tmp/gmailieer_patchwork_notmuch.log 2>&1
fi

# notmuch tag -new -- tag:new >> /tmp/gmailieer_patchwork_notmuch.log 2>&1
/opt/homebrew/bin/notmuch new
