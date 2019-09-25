#!/bin/zsh

setopt prompt_subst

export VIRTUAL_ENV_DISABLE_PROMPT=1

autoload -U colors
colors

# https://github.com/clvv/oh-my-zsh/blob/master/modules/git/functions/git-info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{yellow}"
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
  if git rev-list --count HEAD > /dev/null 2>&1; then
    # git rev-list --left-right --count HEAD...@'{u}'
    git rev-list --count HEAD...@'{u}' | sed 's/[1-9][0-9]*/↯/;s/[0-9]//'
  fi
}

_vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "${vcs_info_msg_0_}"
  fi
}

function +vi-git-untracked() {
  emulate -L zsh
  if [[ -n $(git ls-files --exclude-standard --others 2> /dev/null) ]]; then
    hook_com[unstaged]+="%F{blue}"
  fi
}

fucion _hosthame_custom() {
  hostname | sed 's/\..*//g' | grep -v -E '^plank$' | sed 's/^/\ @/'
}

function _git_total_commits() {
  git rev-list --count HEAD
}

_tmux_indicator='!'
_nesting_level=""

if [[ -n "$TMUX" ]] ; then
  local _tmux_indicator="="
  local _nesting_count=`expr $SHLVL - 3`
else
  local _nesting_count=`expr $SHLVL - 1`
fi

if [[ "$_nesting_count" -ne 0 ]] ; then
  _nesting_level="$(printf '.%.0s' {1..$_nesting_count})"
fi

local _return_status="%(?..%F{red})"

function virtualenv_info {
    [[ -n "$VIRTUAL_ENV" ]] && echo 'py:'${VIRTUAL_ENV:t}' '
}

PROMPT='${_return_status}${_tmux_indicator}%F{yellow}%B%(1j.#.) '
RPROMPT='%F{white}%2~ '

function zle-line-init zle-keymap-select {
NORMAL_COLOR="%{$fg_bold[blue]%}"
INSERT_COLOR="%{$fg_bold[white]%}"
PS1="${_return_status}${_tmux_indicator}%F{green}$( _vcs_info_wrapper )%F{yellow}%B%(1j.#.) "
RPS1="$FG[237]${_nesting_level}%F{yellow} $(virtualenv_info) $FG[240]$(_git_pushable)%{$reset_color%} ${${KEYMAP/vicmd/$NORMAL_COLOR}/(main|viins)/$INSERT_COLOR}%2~%{$reset_color%} %{%B%F{cyan}%}$(_hosthame_custom)" zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
