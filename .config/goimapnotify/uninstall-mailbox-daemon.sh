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

# for reference: alternative when using system-level agent
# PLIST_PATH="$HOME/Library/LaunchDaemons/com.user.goimapnotify-$mailbox_name.plist"

# Write the plist content to the file
PLIST_PATH="$HOME/Library/LaunchAgents/com.user.goimapnotify-$mailbox_name.plist"

# for reference: alternative when using system-level agent
# sudo launchctl bootout system "$PLIST_PATH"
# rm ~/Library/LaunchDaemons/com.user.goimapnotify-$mailbox_name.plist
launchctl unload "$PLIST_PATH"
rm "$PLIST_PATH"

sudo rm /tmp/com.user.goimapnotify.out
touch /tmp/com.user.goimapnotify.out
sudo rm /tmp/com.user.goimapnotify.err
touch /tmp/com.user.goimapnotify.err
