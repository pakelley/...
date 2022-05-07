# shell
## Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
    export GUARD_ZSH_RECURSION=true
    source "${ZDOTDIR:-$HOME}/.zprofile"
    unset GUARD_ZSH_RECURSION
fi

## Make special chars show up correctly in emacs
export LC_ALL=en_US.UTF-8 export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color

# dotfiles
export DOTFILES_DIR=$HOME/dotfiles
export PATH=$DOTFILES_DIR/bin:$PATH
export DOTCONF_DIR=$DOTFILES_DIR/config
export DOTMACS_DIR=$DOTCONF_DIR/efreakingmacs/pacemacs.d

export WAKATIME_HOME=$DOTFILES_DIR/config/wakatime

# # Bombora
# export BOMBORA_REPOS_PATH=/Users/pkelley/bombora
# source $BOMBORA_REPOS_PATH/yellow-api-keys/export-yellow-env-vars.sh
# export TESSIE_COMMONS_DIR=$BOMBORA_REPOS_PATH/tessie-group/tessie-commons
# ## gcloud
# export GCLOUD_PROJECT=bombora-dev
# export PATH=/Users/pkelley/Downloads/google-cloud-sdk/bin:$PATH
# export PATH=/Users/pkelley/Downloads/daisy:$PATH
# ## common
# export DATAPROC_DEV_SERVICE_ACCOUNT=dataproc-dev-account@bombora-dev.iam.gserviceaccount.com
# export DATAPROC_TIHA_SERVICE_ACCOUNT=production-tiha-dataproc-job@astute-harbor-103715.iam.gserviceaccount.com
# export PROD_PROJECT=astute-harbor-103715
# export DEV_OUTPUT=gs://bombora-analytics-dev/data/internal/job-output
# export PROD_OUTPUT=gs://bombora-analytics/model-output

# python
export PATH=/usr/local/miniconda3/bin:$PATH
## virtualenvwrapper
# export WORKON_HOME=$HOME/.virtualenvs
# export PROJECT_HOME=$HOME/PyProjs
# source /usr/local/bin/virtualenvwrapper.sh
# export CONDA_BIN_PATH=/opt/conda/bin
# export PATH=$CONDA_BIN_PATH:$PATH
# . /opt/conda/etc/profile.d/conda.sh



# tex
# export PATH=/Library/TeX/texbin:$PATH
export PATH=~/.local/bin:$PATH
export PATH=$PATH:/Users/pakelley/go/bin
export GOPATH=/Users/pakelley/go
PATH=$PATH:~/.playground/bin/

# byobu
export BYOBU_PREFIX=/usr/local

# hadoop
export HADOOP_HOME=/usr/local/Cellar/hadoop/3.1.1/libexec/

# pony
export PATH=/Users/pakelley/.local/share/ponyup/bin:$PATH
if [ -e /Users/pakelley/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/pakelley/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# doom
export PATH=~/.config/doom-emacs/bin:$PATH

# ...
export PATH=~/.config/.../bin:$PATH

# pyenv
export PYENV_ROOT="~/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

export GPG_TTY=$(tty)
