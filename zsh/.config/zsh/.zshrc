#! /bin/zsh

# Basic exports
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM="xterm-256color"

source "$ZDOTDIR/exports"

# zsh settings
export DISABLE_AUTO_TITLE="true"
export COMPLETION_WAITING_DOTS="false"
export HIST_STAMPS="dd.mm.yyyy"
export HISTSIZE=5000
export SAVEHIST=5000
export HISTFILE="$HOME/.local/share/zsh/.zsh_history"
setopt HIST_IGNORE_SPACE
setopt appendhistory
setopt sharehistory
setopt incappendhistory
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS

# cd-ing settings
setopt auto_cd                                         # automatically cd if folder name and no command found
setopt auto_list                                       # automatically list choices on ambiguous completion
setopt auto_menu                                       # automatically use menu completion
setopt always_to_end                                   # move cursor to end if word had one match
setopt interactive_comments                            # allow comments in interactive shells
zstyle ':completion:*' menu select                     # select completions with arrow keys
zstyle ':completion:*' group-name ''                   # group results by category
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' # non case sensitive complete
zstyle ':completion:*' list-colors "$LS_COLORS"
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion

# autocompletions
autoload -Uz compinit
zmodload zsh/complist
compinit

# autosuggestions settings
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=244"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC="true"

# sourcing plugins & themes
source "$ZDOTDIR/.zsh-custom/themes/multi.zsh-theme"
source "$ZDOTDIR/.zsh-custom/plugins/z/z.sh"
source "$ZDOTDIR/.zsh-custom/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh"
source "$ZDOTDIR/.zsh-custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh"
source "$ZDOTDIR/.zsh-custom/plugins/fzf-tab/fzf-tab.plugin.zsh"

# fzf-tab
zstyle ':completion:*:git-checkout:*' sort false # disable sorting for git-checkout
zstyle ':fzf-tab:*' use-fzf-default-opts yes # use fzf default options
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -1 --color=always $realpath' # preview for cd

export EDITOR='nvim'
export BROWSER= # do not set browser
export DIFFTOOL='icdiff'

# Make CTRL-Z background things and unbackground them.
fg-bg() {
	if [[ $#BUFFER -eq 0 ]]; then
		fg
	else
		zle push-input
	fi
}
zle -N fg-bg
bindkey '^Z' fg-bg

ifsource(){
    [ -f "$1" ] && source "$1"
}

# Credentials
ifsource "$HOME/.credentials"

# Source aliases
source "$ZDOTDIR/aliases"
source "$ZDOTDIR/functions"

# source dir hashes
ifsource "$HOME/.local/share/zsh/.zsh_dir_hashes"

# Source fzf
[ -d "$HOME/.nix-profile/share/fzf" ] &&
    source "$HOME/.nix-profile/share/fzf/completion.zsh" &&
    source "$HOME/.nix-profile/share/fzf/key-bindings.zsh"

# Source colors for ls (trapd00r/LS_COLORS)
eval "$(dircolors -b "$ZDOTDIR/dircolors")"

# Use vim mode in zsh
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -v
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^H' backward-kill-word # ctrl+bspc
bindkey '^[^?' backward-kill-word # alt+bspc
# bindkey '^r' history-incremental-search-backward
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
bindkey "${terminfo[kcuu1]}" history-search-backward
bindkey "${terminfo[kcud1]}" history-search-forward
export KEYTIMEOUT=1

preexec() {
	if ! grep -qF "$1" "$DATAFILES_PATH/long_runnable_jobs"; then
		CMD_START_DATE=$(date +%s)
		CMD_NAME=$1
	fi
}

# cd > cd&&ls
list_all() {
  emulate -L zsh
  ls
}
if [[ ${chpwd_functions[(r)list_all]} != "list_all" ]];then
  chpwd_functions=(${chpwd_functions[@]} "list_all")
fi

# set repo root hash
set_repo_root() {
  emulate -L zsh
  hash -d r="$(git rev-parse --show-toplevel 2>/dev/null)" || true
}
if [[ ${chpwd_functions[(r)set_repo_root]} != "set_repo_root" ]];then
  chpwd_functions=(${chpwd_functions[@]} "set_repo_root")
fi

# load nix
ifsource /etc/profile.d/nix.sh
ifsource "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

# setup direnv
eval "$(direnv hook zsh)"
copy_function() {
	test -n "$(declare -f "$1")" || return 
	eval "${_/$1/$2}"
}
copy_function _direnv_hook _direnv_hook__old
_direnv_hook() {
	_direnv_hook__old "$@" 2> >(grep -vE '^direnv: export')
}

# Completions for jj
source <(jj util completion zsh)

# Run jj on CTRL-J
function run_jj_widget() {
  jj
  zle reset-prompt
}
zle -N run_jj_widget
bindkey '^J' run_jj_widget

# Run jd on CTRL-K
function run_jd_widget() {
  jd
  zle reset-prompt
}
zle -N run_jd_widget
bindkey '^K' run_jd_widget

# ,darkmode quiet # set dark or light mode
export ZSH_LOADED=1
