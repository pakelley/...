# Nushell Environment Config File

def create_left_prompt [] {
    let path_segment = ($env.PWD)

    $path_segment
}

def create_right_prompt [] {
    let time_segment = ([
        (date now | date format '%m/%d/%Y %r')
    ] | str collect)

    $time_segment
}

# Use nushell functions to define your right and left prompt
let-env PROMPT_COMMAND = { create_left_prompt }
let-env PROMPT_COMMAND_RIGHT = { create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
let-env PROMPT_INDICATOR = { "〉" }
let-env PROMPT_INDICATOR_VI_INSERT = { ": " }
let-env PROMPT_INDICATOR_VI_NORMAL = { "〉" }
let-env PROMPT_MULTILINE_INDICATOR = { "::: " }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
let-env ENV_CONVERSIONS = {
  "PATH": {
    from_string: { |s| $s | split row (char esep) }
    to_string: { |v| $v | str collect (char esep) }
  }
  "Path": {
    from_string: { |s| $s | split row (char esep) }
    to_string: { |v| $v | str collect (char esep) }
  }
}

# Directories to search for scripts when calling source or use
#
# By default, <nushell-config-dir>/scripts is added
let-env NU_LIB_DIRS = [
    ($nu.config-path | path dirname | path join 'scripts')
]

# Directories to search for plugin binaries when calling register
#
# By default, <nushell-config-dir>/plugins is added
let-env NU_PLUGIN_DIRS = [
    ($nu.config-path | path dirname | path join 'plugins')
]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# let-env PATH = ($env.PATH | prepend '/some/path')

# homebrew
let-env PATH = ($env.PATH | prepend /opt/homebrew/bin)

# pyenv
let-env PYENV_ROOT = ("~/.pyenv" | path expand)
let-env PATH = ($env.PATH | prepend $"($env.PYENV_ROOT)/bin" | prepend ([(pyenv root | str trim) "shims"] | path join))
# may need to add something here for `pyenv shell` to be able to change env vars eventually

# EDITOR
let-env EDITOR = "emacsclient -a ''"

# twitter bin
let-env PATH = ($env.PATH | prepend /opt/twitter_mde/bin)
let-env PATH = ($env.PATH | prepend /opt/twitter_mde/data/gcloud/current/mde_bin)


# doom
let-env PATH = ($env.PATH | prepend ("~/.config/doom-emacs/bin" | path expand))

# dots
let-env PATH = ($env.PATH | prepend ("~/.config/dots/bin" | path expand))

# usr/local/bin
let-env PATH = ($env.PATH | append /usr/local/bin)

# texbin
let-env PATH = ($env.PATH | append /Library/TeX/texbin)
