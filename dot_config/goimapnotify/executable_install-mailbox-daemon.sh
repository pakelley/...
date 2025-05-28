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
    p | provider )  needs_arg; mail_provider="$OPTARG" ;;
    ??* )          die "Illegal option --$OPT" ;;  # bad long option
    ? )            exit 2 ;;  # bad short option (error reported via getopts)
  esac
done
shift $((OPTIND-1)) # remove parsed options and args from $@ list

# Define paths, and generate plist content
PROGRAM_PATH="$HOME/.config/goimapnotify/run-$mail_provider-sync.sh"
TEMPLATE_PATH="$HOME/.config/goimapnotify/$mailbox_name/daemon-template.plist"
PLIST_CONTENT=$(sed -e "s|SCRIPT_PATH_PLACEHOLDER|$PROGRAM_PATH|g" $TEMPLATE_PATH)

# Create/populate plist file
PLIST_PATH="$HOME/Library/LaunchAgents/com.user.goimapnotify-$mailbox_name.plist"
echo "$PLIST_CONTENT" > "$PLIST_PATH"
chown $(whoami) "$PLIST_PATH"
chmod 644 "$PLIST_PATH"

# Load the service
launchctl load "$PLIST_PATH"

# for reference: alternative when using system-level agent
# PLIST_PATH="$HOME/Library/LaunchDaemons/com.user.goimapnotify-$mailbox_name.plist"
# sudo chown root:wheel "$PLIST_PATH"
# sudo chmod 644 "$PLIST_PATH"
# sudo launchctl bootstrap system "$PLIST_PATH"
