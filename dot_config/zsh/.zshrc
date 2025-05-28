# shell
## For tramp: don't lag like crazy when I open a shell remotely
if [[ $TERM == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '

    source ~/.zshenv

fi
## Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='emacs'
fi

# prezto
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi
## Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"
## Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"
## Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# set prompt
autoload -Uz promptinit
promptinit
## Get rid of extra prompt text
DEFAULT_USER="pakelley"

# aliases
## ls's
alias ll='exa -alFh --git'
alias la='exa -a'
alias l='exa -F'
alias lls='exa -alFhs size'
alias llr='exa -alFhrs size'
alias llS='exa -alFhs date'
alias llR='exa -alFhrs date'
## ..'s
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
## zsh config
alias zshconfig="nvim ~/.zshrc"

alias "docker-run"="docker run --rm -it"
alias mkubectl="kubectl --context=minikube"
alias open-cloud-sql="cat ~/Downloads/dev_env.txt | grep CLOUDSQL_PG_PASSWORD | awk '{ split(\$1,x,\"=\"); print x[2] }' | pbcopy; psql -h 127.0.0.1 -U proxyuser_bf -W -d postgres"
alias whats-my-version-again="cat conda.recipe/meta.yaml | grep 'version:' | awk '{ print \$2 }'"

# bnv
alias bnv="/opt/miniconda3/bin/bnv"


# Google Cloud SDK
## PATH
if [ -f '/Users/pkelley/Downloads/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/pkelley/Downloads/google-cloud-sdk/path.zsh.inc'; fi
## command completion
if [ -f '/Users/pkelley/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/pkelley/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

# shell utils
## autosuggestions
ZSH_AUTOSUGGEST_ACCEPT_WIDGETS+=(end-of-line)
ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS+=(forward-char emacs-forward-word)
## z
# source `brew --prefix`/Cellar/z/1.9/etc/profile.d/z.sh
## iterm
source $DOTFILES_DIR/config/shell/zsh/iterm2_shell_integration.zsh
## fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
 ##zsh syntax highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# antigen
source /usr/local/share/antigen/antigen.zsh
antigen bundle supercrabtree/k
antigen bundle wbingli/zsh-wakatime
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen apply


hackon () {
	  # virtualenvwrapper_load
	  # workon "$@"
    source activate "$@"
    z "$@"
}

hackoff () {
    source deactivate
}

hackout () {
    if [[ -n "$@" ]]; then
        env_name="$@"
    elif [[ -n "$CONDA_DEFAULT_ENV" ]]; then
         env_name="$CONDA_DEFAULT_ENV"
    fi

    if [[ -n $env_name ]]; then
        env_file="$DOTCONF_DIR/python/common-env.yml"
        echo "Updating environment $env_name using environment file $env_file"
        conda env update -f "$env_file" -n "$env_name"
    else
        echo "No conda environment activated or specified, no changes will be made"
    fi
}

checkport() {
    port_num="$@"
    lsof -nP -i4TCP:$port_num
}

find-emacs-pythons() {
    psgrep python | grep anaconda-mode
}

kill-emacs-pythons() {
    for pid in `psgrep python | grep anaconda-mode | awk '{print $2}'`; do kill $pid; done
}


# vi mode
bindkey "^[" vi-cmd-mode

# # autocomplete
# source ~/.zsh_completions.d/binvoke_autocomplete.zsh.sh
# source ~/.zsh_completions.d/helm_autocomplete.zsh.sh


# # bombora
# for f in /keybase/team/bombora/profile-scripts/*; do $f; done

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# zoxide
eval "$(zoxide init zsh)"

# vpn
alias build-vpn="make -C $HOME/Downloads/dev-env build-dev-env"
alias start-vpn="make -C $HOME/Downloads/dev-env run-dev-env"
alias connect-vpn="make -C $HOME/Downloads/dev-env connect-vpn"

export PATH="$HOME/.poetry/bin:$PATH"
