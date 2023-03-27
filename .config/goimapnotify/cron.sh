#!/usr/bin/env bash

set -e
cd ~/.mail/account.kelleys-gmail

date >> /tmp/gmailieer_patchwork_notmuch.log
gmi sync >> /tmp/gmailieer_patchwork_notmuch.log 2>&1

notmuch new
# notmuch tag -new -- tag:new >> /tmp/gmailieer_patchwork_notmuch.log 2>&1
