#!/usr/bin/env bash

die() { echo "$*" >&2; exit 2; }  # complain to STDERR and exit with error
needs_arg() { if [ -z "$OPTARG" ]; then die "No arg for --$OPT option"; fi; }

while getopts ab:c:-: OPT; do
  # support long options: https://stackoverflow.com/a/28466267/519360
  if [ "$OPT" = "-" ]; then   # long option: reformulate OPT and OPTARG
    OPT="${OPTARG%%=*}"       # extract long option name
    OPTARG="${OPTARG#$OPT}"   # extract long option argument (may be empty)
    OPTARG="${OPTARG#=}"      # if long option argument, remove assigning `=`
  fi
  case "$OPT" in
    m | mailbox )  needs_arg; mailbox_name="$OPTARG" ;;
    ??* )          die "Illegal option --$OPT" ;;  # bad long option
    ? )            exit 2 ;;  # bad short option (error reported via getopts)
  esac
done
shift $((OPTIND-1)) # remove parsed options and args from $@ list

# KLUDGE need to get these set outside this script
export NOTMUCH_CONFIG="$HOME/.config/notmuch/default/config"
# need access to `brew` to use `brew --prefix`, so just hardcode this for now
BREW_BIN_PATH="/opt/homebrew/bin"
export PATH="$PATH:$BREW_BIN_PATH:~/go/bin:~/.pyenv/shims"

# run the sync
gmi sync -C "~/.local/share/mail/account.$mailbox_name";
goimapnotify -conf "$HOME/.config/goimapnotify/$mailbox_name/notify.conf"
