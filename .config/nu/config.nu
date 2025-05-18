source ~/.config/dots/.config/nu/starship/init.nu

$env.config = {
  keybindings: [
    {
      name: fuzzy_history
      modifier: control
      keycode: char_r
      mode: [emacs, vi_normal, vi_insert]
      event: [
        {
          send: ExecuteHostCommand
          cmd: "commandline edit --insert (
            history
              | get command
              | reverse
              | uniq
              | str join (char -i 0)
              | fzf
                --preview='echo -n {} | nu --stdin -c \'nu-highlight\''
                --preview-window 'right:30%'
                --scheme history
                --read0
                --layout=reverse
                --height=40%
                --tiebreak=index
                --query (commandline)
              | decode utf-8
              | str trim
          )"
        }
      ]
    }
  ]
}


# pyenv
# Typically handled py `pyenv init`, but nu isn't supported so we have to do this manually
pyenv rehash

# zoxide
source ~/.local/share/.zoxide.nu

# custom commands

# Rename the current tab
def rename-tab [
  name: string  # The new name for the current tab
] {
  zsh -c $"echo -ne \"\\x1b]0;($name)\\x1b\\\\\""
}

# listening command from this SE post https://stackoverflow.com/a/30029855/5054505
def listening [
  port?: string  # optional port to filter results by
] {
  if ($port == null) {
     sudo lsof -iTCP -sTCP:LISTEN -n -P
  } else {
    sudo lsof -iTCP -sTCP:LISTEN -n -P | grep -i --color $port
  }
}

def pull-email [] {
  if (not ("DECRYPTED_PASS" in $env)) {
     echo "testing"
     $env.DECRYPTED_PASS = (gpg --use-agent --decrypt --armor --local-user 0x7FE626F169E66EFA ~/.local/share/mbsync/account.protonmail.pass.gpg)
  }
  mbsync -a -c ~/.config/mbsync/mbsyncrc
  gmi sync -C ~/.local/share/mail/account.human-signal
  gmi sync -C ~/.local/share/mail/account.kelleys-gmail
}

def index-email [] {
   notmuch new
   notmuch tag +kelleys -- path:account.kelleys-gmail/**
   notmuch tag +human-signal -- path:account.human-signal/**
   notmuch tag +protonmail -- is:new path:account.protonmail/**
   notmuch tag +calendar -new -- is:new attachment:*.ics
   afew --tag --new
   afew --move-mails
}

def push-email [] {
  if (not ("DECRYPTED_PASS" in $env)) {
     echo "testing"
     $env.DECRYPTED_PASS = (gpg --use-agent --decrypt --armor --local-user 0x7FE626F169E66EFA ~/.local/share/mbsync/account.protonmail.pass.gpg)
  }
  mbsync -a -c ~/.config/mbsync/mbsyncrc
  gmi push -C ~/.local/share/mail/account.human-signal
  gmi push -C ~/.local/share/mail/account.kelleys-gmail
}

def sync-email [] {
   pull-email
   index-email
   push-email
}
