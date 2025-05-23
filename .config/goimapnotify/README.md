# goimapnotify on macOS

This directory holds configuration and helper scripts for running
[goimapnotify](https://github.com/videolabs/goimapnotify) as a macOS
*LaunchAgent* for various mailboxes.

Each mailbox has its own folder (e.g. `human-signal`, `kelleys`,
`protonmail`) containing a `daemon-template.plist` and a
`notify.conf`. The template is used to generate the plist that
`launchctl` loads while `notify.conf` is passed directly to
`goimapnotify`.

## Helper scripts

- `install-mailbox-daemon.sh` – Create and load a LaunchAgent for a
  mailbox. It requires the mailbox name and the mail provider which
determines the script used to sync mail.
- `uninstall-mailbox-daemon.sh` – Unload and remove the LaunchAgent for
a mailbox.
- `restart-mailbox-daemon.sh` – Rotate log files and restart the
  LaunchAgent via `launchctl kickstart`.
- `run-gmail-sync.sh` – Called by Gmail based LaunchAgents. Runs `gmi
  sync` for the mailbox then launches `goimapnotify` with the mailbox
  configuration.
- `run-protonmail-sync.sh` – Similar to `run-gmail-sync.sh` but uses
  `mbsync` and fetches credentials through gpg for ProtonMail.
- `cron.sh` – Example cron script that syncs gmail accounts and runs
  `notmuch new`.

Log output for each mailbox is written to `/tmp/com.user.goimapnotify-<mailbox>.{out,err}`.

## Installing a LaunchAgent

Configure the mailbox directory as needed and then run, for example:

```sh
./install-mailbox-daemon.sh --mailbox=kelleys --provider=gmail
```

The script places the generated plist in
`~/Library/LaunchAgents/` and loads it with `launchctl`.

## Managing services

Restart a running service:

```sh
./restart-mailbox-daemon.sh --mailbox=kelleys
```

Remove the LaunchAgent entirely:

```sh
./uninstall-mailbox-daemon.sh --mailbox=kelleys
```

The provider scripts can also be run manually for testing:

```sh
./run-gmail-sync.sh --mailbox=kelleys
./run-protonmail-sync.sh --mailbox=protonmail
```


