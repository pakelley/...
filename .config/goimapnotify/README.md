# Installation

Still need to automate this... for now, you'll need to:

- move the launch agent using:

```sh
cp ~/.config/goimapnotify/com.pakelley.imapnotify.plist ~/Library/LaunchAgents
```

- load the agent

```sh
launchctl load -w ~/Library/LaunchAgents/com.pakelley.goimapnotify.plist
```
