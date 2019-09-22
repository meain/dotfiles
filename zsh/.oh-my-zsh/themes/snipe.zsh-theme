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
    echo "$FG[240]$commit_age%{$reset_color%}"
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

if [[ -n "$TMUX" ]] ; then
  local _tmux_indicator="="
fi

local _return_status="%(?..%F{red})"

function virtualenv_info {
    [[ -n "$VIRTUAL_ENV" ]] && echo 'py:'${VIRTUAL_ENV:t}' '
}

PROMPT="${_return_status}${_tmux_indicator}%F{yellow}%B%(1j.#.) "
RPROMPT=''

function zle-line-init zle-keymap-select {
NORMAL_COLOR="%{$fg_bold[blue]%}"
INSERT_COLOR="%{$fg_bold[white]%}"
RPS1="$FG[237]${_nesting_level}%F{yellow} $(virtualenv_info)  %f%F{green}$( _vcs_info_wrapper ) ${${KEYMAP/vicmd/$NORMAL_COLOR}/(main|viins)/$INSERT_COLOR}%2~%{$reset_color%} $(_git_time_since_commit)%{%B%F{cyan}%}$(_hosthame_custom)" zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
