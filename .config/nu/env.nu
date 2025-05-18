# Nushell Environment Config File
#
# version = 0.82.1

def create_left_prompt [] {
    mut home = ""
    try {
        if $nu.os-info.name == "windows" {
            $home = $env.USERPROFILE
        } else {
            $home = $env.HOME
        }
    }

    let dir = ([
        ($env.PWD | str substring 0..($home | str length) | str replace $home "~"),
        ($env.PWD | str substring ($home | str length)..)
    ] | str join)

    let path_color = (if (is-admin) { ansi red_bold } else { ansi green_bold })
    let separator_color = (if (is-admin) { ansi light_red_bold } else { ansi light_green_bold })
    let path_segment = $"($path_color)($dir)"

    $path_segment | str replace --all (char path_sep) $"($separator_color)/($path_color)"
}

def create_right_prompt [] {
    # create a right prompt in magenta with green separators and am/pm underlined
    let time_segment = ([
        (ansi reset)
        (ansi magenta)
        (date now | date format '%Y/%m/%d %r')
    ] | str join | str replace --all "([/:])" $"(ansi green)${1}(ansi magenta)" |
        str replace --all "([AP]M)" $"(ansi magenta_underline)${1}")

    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code, (char space), $time_segment] | str join)
}

# Use nushell functions to define your right and left prompt
$env.PROMPT_COMMAND = {|| create_left_prompt }
# $env.PROMPT_COMMAND_RIGHT = {|| create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
$env.PROMPT_INDICATOR = {|| " > " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| " : " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| " > " }
$env.PROMPT_MULTILINE_INDICATOR = {|| "::: " }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
    # ($nu.default-config-dir | path join 'scripts') # add <nushell-config-dir>/scripts
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
    # ($nu.default-config-dir | path join 'plugins') # add <nushell-config-dir>/plugins
]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# $env.PATH = ($env.PATH | split row (char esep) | prepend '/some/path')

# brew bin path
# NOTE: the simplest way to make this portable between m1 and non-m1 macs seems to be to just check whether the m1 path exists
let brew_prefix = (if ("/opt/homebrew/bin" | path exists) { "/opt/homebrew" }
                            else { "/usr/local" })
$env.PATH = ($env.PATH | prepend [$"($brew_prefix)/bin", $"($brew_prefix)/sbin"])

# pyenv
$env.PYENV_ROOT = ("~/.pyenv" | path expand)
$env.PATH = ($env.PATH | prepend $"($env.PYENV_ROOT)/bin" | prepend ([(pyenv root | str trim) "shims"] | path join))
# may need to add something here for `pyenv shell` to be able to change env vars eventually

# EDITOR
$env.EDITOR = "emacsclient -a ''"

# twitter bin
$env.PATH = ($env.PATH | prepend /opt/twitter_mde/bin)
$env.PATH = ($env.PATH | prepend /opt/twitter_mde/data/gcloud/current/mde_bin)


# doom
$env.PATH = ($env.PATH | prepend ("~/.config/doom-emacs/bin" | path expand))

# dots
$env.PATH = ($env.PATH | prepend ("~/.config/dots/bin" | path expand))

# texbin
$env.PATH = ($env.PATH | append /Library/TeX/texbin)

# rust
$env.PATH = ($env.PATH | append ("~/.cargo/bin" | path expand))

$env.NVM_DIR = ("~/.nvm" | path expand)

# zoxide
# have to comment this out until a new release is cut that includes https://github.com/ajeetdsouza/zoxide/pull/495
# zoxide init nushell | save -f ~/.local/share/.zoxide.nu

# xdg bin
$env.PATH = ($env.PATH | prepend ("~/.local/bin" | path expand))

# pdm
def gen_pdm_pythonpath [] {
  let pdm_path = ("/opt/homebrew/Cellar/pdm/2.4.7/libexec/lib/python3.11/site-packages/pdm/pep582" | path expand)
  if "PYTHONPATH" in $env {
    if (not $pdm_path in $env.PYTHONPATH) {
      ($env.PYTHONPATH | append $pdm_path)
    } else {
    $env.PYTHONPATH
    }
  } else {
     $pdm_path
  }
}
$env.PYTHONPATH = gen_pdm_pythonpath

# m1 docker
# docker
$env.PATH = ($env.PATH | append ("~/.docker/bin" | path expand))

# yubikey for ssh
$env.GPG_TTY = (tty | str trim)
$env.SSH_AUTH_SOCK = (gpgconf --list-dirs agent-ssh-socket | str trim)
gpgconf --launch gpg-agent

# go
$env.GOPATH = ("~/go" | path expand)
$env.PATH = ($env.PATH | append $"($env.GOPATH)/bin")

# notmuch
$env.NOTMUCH_CONFIG = ("~/.config/notmuch/default/config" | path expand)

# scala
$env.PATH = ($env.PATH | append ("~/Library/Application Support/Coursier/bin" | path expand))

# java
$env.JAVA_HOME = (brew --prefix openjdk)
$env.MAVEN_HOME = (brew --prefix maven)
$env.PATH = ($env.PATH | append $"($env.JAVA_HOME)/bin" | append $"($env.MAVEN_HOME)/bin")

# lua, for digestif (LaTeX LSP server)
$env.PATH = ($env.PATH | append ("~/.luarocks/bin" | path expand))
