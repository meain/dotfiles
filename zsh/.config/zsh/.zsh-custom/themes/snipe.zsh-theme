#!/bin/zsh

setopt prompt_subst

export VIRTUAL_ENV_DISABLE_PROMPT=1

autoload -Uz colors
colors

# variables
INSERT_COLOR="%{$reset_color%}"
NORMAL_COLOR="%{%F{39}%}"

# https://github.com/clvv/oh-my-zsh/blob/master/modules/git/functions/git-info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{003}"
zstyle ':vcs_info:*' unstagedstr "%F{red}"
zstyle ':vcs_info:*' use-simple true
zstyle ':vcs_info:git+set-message:*' hooks git-untracked
zstyle ':vcs_info:git*:*' formats '%u%c%m%b' # default ' (%s)-[%b]%c%u-'
zstyle ':vcs_info:git*:*' actionformats '%u%c%m%b/%a' # default ' (%s)-[%b|%a]%c%u-'

function _git_time_since_commit() {
  # Only proceed if there is actually a commit.
  if git log -1 > /dev/null 2>&1; then
    # Get the last commit.
    last_commit=$(git log --pretty=format:'%at' -1 2> /dev/null)
    now=$(date +%s)
    seconds_since_last_commit=$((now-last_commit))

    hours=$((seconds_since_last_commit/3600))
    minutes=$((seconds_since_last_commit / 60))

    if [ $hours -gt 24 ]; then
      days=$((seconds_since_last_commit / 86400))
      commit_age="${days}d"
    elif [ $minutes -gt 60 ]; then
      sub_hours=$((hours % 24))
      sub_minutes=$((minutes % 60))
      commit_age="${sub_hours}h${sub_minutes}m"
    else
      commit_age="${minutes}m"
    fi

    # https://gabri.me/blog/custom-colors-in-your-zsh-prompt
    echo "$commit_age"
  fi
}

function _git_pushable() {
  setopt localoptions noshwordsplit
  if git rev-list --count "origin/$(git rev-parse --abbrev-ref HEAD 2>/dev/null).." > /dev/null 2>&1; then
    # git rev-list --left-right --count HEAD...@'{u}'
    git rev-list --count "origin/$(git rev-parse --abbrev-ref HEAD 2>/dev/null).." | sed 's/[1-9][0-9]*/ ↯/;s/[0-9]//'
  fi
}

# TODO: Use https://github.com/jj-vcs/jj/wiki/Shell-Prompt
_vcs_info_wrapper() {
  vcs_info
  if echo "${vcs_info_msg_0_}" | grep -qE 'jj/keep'; then
    line="$(jj log -r @ --no-graph)"
    if echo "$line" | grep -q "(empty)"; then
        echo -e "%F{002}$(echo "$line" | cut -d' ' -f1)%{$reset_color%}"
    elif echo "$line" | grep -q "(no description set)"; then
        echo -e "%F{003}$(echo "$line" | cut -d' ' -f1)%{$reset_color%}"
    else
        echo "$line" | cut -d' ' -f1
    fi
  else
    echo "%F{green}${vcs_info_msg_0_}%{$reset_color%}"
  fi
}

function +vi-git-untracked() {
  emulate -L zsh
  if [[ -n $(git ls-files --exclude-standard --others 2> /dev/null) ]]; then
    hook_com[unstaged]+="%F{blue}"
  fi
}

function _current_kubernetes_namespace() {
  if [ -d helm ] || [ -d charts ] || [ -f deployment.yaml ] || [ -f .namespace ];then
    cluster="$(kubectl config view --minify --output 'jsonpath={..current-context}' 2>/dev/null)"
    namespace="$(kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null)"
    [ -z "$namespace" ] && [ -n "$cluster" ] && namespace="default"
    [ -n "$cluster" ] && printf "%s:%s" "$cluster" "$namespace" || printf "no-cluster"
  fi
}

function _hosthame_custom() {
  hostname | sed 's/\..*//g' | grep -vi -E '^abin' | sed 's/^/\ @/'
}

function _git_total_commits() {
  git rev-list --count HEAD
}

function _git_repo_base(){
  ROOT="$(git remote get-url origin 2>/dev/null | sed 's/\.git$//')"
  if [ -n "$ROOT" ];then
    echo "$(basename "$ROOT")"
  fi
}

_tmux_indicator='!'


if [[ -n "$DISTROBOX_ENTER_PATH" ]] ; then
  local _tmux_indicator="@"
elif [[ -n "$IN_NIX_SHELL" ]] ; then
  local _tmux_indicator="#"
elif [[ -n "$TMUX" ]] ; then
  local _tmux_indicator="="
fi

local _return_status="%(?..%F{red})"

function virtualenv_info {
    [[ -n "$VIRTUAL_ENV" ]] && echo " py:${VIRTUAL_ENV:t}"
}

function _last_two_folders {
  DIR=$(pwd)
  if [ "$DIR" = "$HOME" ];then
    echo "~"
  else
    DIR=$(pwd | sed "s|$HOME|~|")
    D2=$(dirname "$DIR")
    if [ -n "$D2" ] && [[ ! "$D2" == "/" ]];then
      DIRNAME2=$(basename "$D2")/$(basename "$DIR")
    else
      DIRNAME2=$(basename "$DIR")
    fi
    echo "$DIRNAME2"
  fi
}

function _cur_folder_with_git_base {
  DIR=$(_last_two_folders)
  BASE=$(_git_repo_base)

  if [ -n "$BASE" ]; then
    if [[ "$DIR" == *"$BASE"* ]];then
      echo "$DIR" | sed "s/$BASE/%F{240}$BASE%{$reset_color%}/"
    else
      echo "%F{240}$BASE%{$reset_color%} $DIR"
    fi
  else
      echo "$DIR"
  fi
}

PROMPT='${_return_status}${_tmux_indicator}%F{003}%B%(1j.#.) '
RPROMPT='%F{white}%2~ '

function generate_lpropmpt() {
  PROMPT_GREPPER=" "  # will only add this in tmux
  if [[ -n "$TMUX" ]] ; then
    PROMPT_GREPPER=" "
  fi
  echo "${${KEYMAP/vicmd/$NORMAL_COLOR}/(main|viins)/$INSERT_COLOR}${_return_status}${_tmux_indicator}$( _vcs_info_wrapper )%F{yellow}%B%(1j.#.)%{$reset_color%}$PROMPT_GREPPER"
}

function generate_rpropmpt() {
  local custom=""

  if git rev-parse --show-toplevel &>/dev/null; then
    pushd "$(git rev-parse --show-toplevel 2>/dev/null)" > /dev/null
    if [ -f .mscripts/shell-additions ]; then
      custom=$(.mscripts/shell-additions)
    fi
  fi

  echo "${custom}%F{003}$(virtualenv_info)%F{blue}$(_current_kubernetes_namespace)$FG[240]$(_git_pushable)%{$reset_color%} $(_cur_folder_with_git_base)%{%B%F{cyan}%}$(_hosthame_custom)%{$reset_color%}"
}

ASYNC_LPROC=0
ASYNC_RPROC=0
function precmd() {
  function asyncl() {
      mkdir -p /tmp/zp
      printf "%s" "$(generate_lpropmpt)" > "/tmp/zp/zsh_lprompt_$$"  # do not clear, let it persist
      kill -s USR1 $$  # signal parent
  }
  function asyncr() {
      mkdir -p /tmp/zp
      printf "%s" "$(generate_rpropmpt)" > "/tmp/zp/zsh_rprompt_$$"  # save to temp file
      kill -s USR2 $$  # signal parent
  }

  if [[ "${ASYNC_LPROC}" != 0 ]]; then  # kill child if necessary
      kill -s HUP $ASYNC_LPROC >/dev/null 2>&1 || :
  fi
  if [[ "${ASYNC_RPROC}" != 0 ]]; then  # kill child if necessary
      kill -s HUP $ASYNC_RPROC >/dev/null 2>&1 || :
  fi

  asyncl &!  # start background computation
  ASYNC_LPROC=$!  # save pid
  asyncr &!  # start background computation
  ASYNC_RPROC=$!  # save pid

  function zle-keymap-select {
    asyncl &!  # start background computation
    ASYNC_LPROC=$!  # save pid
    asyncr &!  # start background computation
    ASYNC_RPROC=$!  # save pid
  }
  zle -N zle-keymap-select

  # printf '\e[38;5;235m―%.0s' {1..$COLUMNS}  # use if you need a seperator
  echo ""  # newline before prompt

  # notify for long running command
  if ! [[ -z $CMD_START_DATE ]]; then
    CMD_END_DATE=$(date +%s)
    CMD_ELAPSED_TIME=$(($CMD_END_DATE - $CMD_START_DATE))
    CMD_NOTIFY_THRESHOLD=60
    CMD_ELAPSED_TIME_MIN="$(($CMD_ELAPSED_TIME/60))"
    CMD_ELAPSED_TIME_SEC="$(($CMD_ELAPSED_TIME%60))"
    CMD_START_DATE=""

    if [[ $CMD_ELAPSED_TIME -gt $CMD_NOTIFY_THRESHOLD ]]; then
      notify "$CMD_NAME" "Completed in ${CMD_ELAPSED_TIME_MIN}m${CMD_ELAPSED_TIME_SEC}s"
    fi
  fi

}


function TRAPUSR1() {
    # to insert non breaking space in vim: <c-k> <space> <space>
    PS1="$(cat /tmp/zp/zsh_lprompt_$$)"  # read from temp file ( uses non breaking space )
    ASYNC_LPROC=0  # reset proc number
    zle && zle reset-prompt  # redisplay
}
function TRAPUSR2() {
    RPS1="$(cat /tmp/zp/zsh_rprompt_$$)"
    ASYNC_RPROC=0  # reset proc number
    zle && zle reset-prompt  # redisplay
}
