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

# get auth
# first off, if we "snooze"-ed the last attempt to get auth, and we're still in the "snooze" period, exit
SNOOZE_FILE="$HOME/.local/share/goimapnotify/.snooze"
if [ -f "$SNOOZE_FILE" ]; then
  SNOOZE_END=$(cat "$SNOOZE_FILE")
  CURRENT_TIME=$(date +%s)
  if (( CURRENT_TIME < SNOOZE_END )); then
    echo "Snooze active. Exiting script."
    exit 0
  else
    echo "Snooze ended. Deleting snooze file and continuing script."
    rm "$SNOOZE_FILE"
  fi
fi
# otherwise, attempt to get auth creds. Allow user to "snooze" (e.g. if I don't have my yubikey available)
if gpg --card-status > /dev/null 2>&1; then
  osascript -e 'display notification "Retrieving auth for '$mailbox_name'. Please touch your YubiKey." with title "My Mailbox Auth"'
else
  USER_CHOICE=$(osascript -e 'display dialog "Decrypting secret for '$mailbox_name'. Please insert your YubiKey, and be ready to enter pin and/or touch." buttons {"OK", "Snooze for 1 Hour", "Snooze for 24 Hours"} default button 1 with title "My Mailbox Auth"')
  if [[ $USER_CHOICE == *"Snooze for 1 Hour"* ]]; then
    echo $(date -v+1H +%s) > "$HOME/.myscript_snooze"
    exit 0
  elif [[ $USER_CHOICE == *"Snooze for 24 Hours"* ]]; then
    echo $(date -v+24H +%s) > "$HOME/.myscript_snooze"
    exit 0
  fi
fi
SECRET=$(gpg --use-agent --decrypt --armor --local-user 0x7FE626F169E66EFA ~/.local/share/mail/account.protonmail/pass.gpg)
export DECRYPTED_PASS=$SECRET

# run the sync
mbsync --all --config ~/.config/mbsync/mbsyncrc;
goimapnotify -conf "$HOME/.config/goimapnotify/$mailbox_name/notify.conf"
