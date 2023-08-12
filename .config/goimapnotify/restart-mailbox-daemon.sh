#!/usr/bin/env sh
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

echo "/tmp/com.user.goimapnotify-$mailbox_name.out"
sudo rm "/tmp/com.user.goimapnotify-$mailbox_name.bak.out"
mv "/tmp/com.user.goimapnotify-$mailbox_name.out" "/tmp/com.user.goimapnotify-$mailbox_name.bak.out"
touch "/tmp/com.user.goimapnotify-$mailbox_name.out"
sudo rm "/tmp/com.user.goimapnotify-$mailbox_name.bak.err"
mv "/tmp/com.user.goimapnotify-$mailbox_name.err" "/tmp/com.user.goimapnotify-$mailbox_name.bak.err"
touch "/tmp/com.user.goimapnotify-$mailbox_name.err"

launchctl kickstart -k "gui/$(id -u)/com.user.goimapnotify-$mailbox_name"
