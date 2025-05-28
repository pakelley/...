def conda-env [env-name: string] {
    let conda-info = (conda info --envs --json | from json)
    # had to remove extra "envs" dir
    # let suffix = (if $env-name == "base" {""} {(["envs" $env-name] | path join)})
    let suffix = (if $env-name == "base" {""} {($env-name | path expand)})
    # use custom env dir, hard-coded to choose XDG-based path
    # let env-dir = ([$conda-info.root_prefix $suffix] | path join)
    let env-prefix = ($conda-info.envs_dirs | where ('.config' in $it) | path expand)
    let env-dir = ([$env-prefix $suffix] | path join)
    let old-path = ($nu.path | str collect (path-sep))
    let new-path = (if (windows?) { (conda-create-path-windows $env-dir) } { (conda-create-path-unix $env-dir) })
    let new-env = [[name, value];
                   [CONDA_DEFAULT_ENV $env-name]
                   [CONDA_PREFIX $env-dir]
                   [CONDA_PROMPT_MODIFIER $"[($env-name)]"]
                   [CONDA_SHLVL "1"]
                   [CONDA_OLD_PATH $old-path]]
     
    $new-env | append $new-path
}

def conda-create-path-windows [env-dir] {
    # 1. Conda on Windows needs a few additional Path elements
    # 2. The path env var on Windows is called Path (not PATH)
    let env-path = [
        $env-dir
        ([$env-dir "Scripts"] | path join)
        ([$env-dir "Library" "mingw-w64"] | path join)
        ([$env-dir "Library" "bin"] | path join)
        ([$env-dir "Library" "usr" "bin"] | path join)
    ]
    let new-path = ([$env-path $nu.path] | flatten | str collect (path-sep))
    [[name, value]; [Path $new-path]]
}

def conda-create-path-unix [env-dir] {
    let env-path = [
        ([$env-dir "bin"] | path join)
    ]
    let new-path = ([$env-path $nu.path] | flatten | str collect (path-sep))
    [[name, value]; [PATH $new-path]]
}

def windows? [] {
    (sys).host.name == "Windows"
}

def path-sep [] {
    if (windows?) { ";" } { ":" }
}

# find/return a conda env with the same name as the current directory
def conda-env-pwd [] {
    pwd | path basename | each { conda-env $it }
}

# This will stream out a conda env, so you should pipe it into `load-env`,
# e.g. `goto-conda-env my-env-name | load-env`
def goto-conda-env [
    env-name: string  # approx name of the dir your conda env is named after (approx because `z` is used to jump there)
] {
    z $env-name
    conda-env-pwd
}
