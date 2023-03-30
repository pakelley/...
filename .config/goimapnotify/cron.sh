#!/usr/bin/env bash
set -e

if [ -d "$HOME/.mail/account.kelleys-gmail" ]
then
    cd "$HOME/.mail/account.kelleys-gmail"
    date >> /tmp/gmailieer_patchwork_notmuch.log
    gmi sync >> /tmp/gmailieer_patchwork_notmuch.log 2>&1
fi

if [ -d "$HOME/.mail/account.heartex" ]
then
    cd "$HOME/.mail/account.heartex"
    date >> /tmp/gmailieer_patchwork_notmuch.log
    gmi sync >> /tmp/gmailieer_patchwork_notmuch.log 2>&1
fi

notmuch new
# notmuch tag -new -- tag:new >> /tmp/gmailieer_patchwork_notmuch.log 2>&1
