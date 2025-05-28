# goimapnotify on macOS

This directory holds configuration and helper scripts for running
[goimapnotify](https://gitlab.com/shackra/goimapnotify) as a macOS
*LaunchAgent* for various mailboxes.

Each mailbox has its own folder (e.g. `kelleys`,
`protonmail`) containing a `daemon-template.plist` and a
`notify.conf`. The template is used to generate the plist that
`launchctl` loads while `notify.conf` is passed directly to
`goimapnotify`.

## Installing a LaunchAgent

Configure the mailbox directory as needed and then run, for example:

```sh
./install-mailbox-daemon.sh --mailbox=kelleys --provider=gmail
```

`install-mailbox-daemon.sh` creates and loads a LaunchAgent for a
  mailbox. It requires the mailbox name and the mail provider which
determines the script used to sync mail.

The script places the generated plist in
`~/Library/LaunchAgents/` and loads it with `launchctl`.

## Managing services

### Restart a running service:

```sh
./restart-mailbox-daemon.sh --mailbox=kelleys
```

`restart-mailbox-daemon.sh` rotates log files and restarts the
  LaunchAgent via `launchctl kickstart`.

### Remove the LaunchAgent entirely:

```sh
./uninstall-mailbox-daemon.sh --mailbox=kelleys
```

`uninstall-mailbox-daemon.sh` unloads and removes the LaunchAgent for
a mailbox.

### Debugging

### Logs

Log output for each mailbox is written to `/tmp/com.user.goimapnotify-<mailbox>.{out,err}`.

### Running Manually

The provider scripts can also be run manually for testing:

```sh
./run-gmail-sync.sh --mailbox=kelleys
./run-protonmail-sync.sh --mailbox=protonmail
```

`run-gmail-sync.sh` is called by Gmail based LaunchAgents. It runs `gmi
sync` for the mailbox then launches `goimapnotify` with the mailbox
configuration.

`run-protonmail-sync.sh` is similar to `run-gmail-sync.sh` but uses
`mbsync` and fetches credentials through age for ProtonMail (mail 
is synced using ProtonMail's Bridge).


