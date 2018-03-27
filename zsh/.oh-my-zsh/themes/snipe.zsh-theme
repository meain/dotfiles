setopt prompt_subst

autoload -U colors
colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{yellow}"
zstyle ':vcs_info:*' unstagedstr "%F{red}"
zstyle ':vcs_info:*' use-simple true
zstyle ':vcs_info:git+set-message:*' hooks git-untracked
zstyle ':vcs_info:git*:*' formats '%u%c%m%b' # default ' (%s)-[%b]%c%u-'
zstyle ':vcs_info:git*:*' actionformats '%u%c%m%b|%a' # default ' (%s)-[%b|%a]%c%u-'

function _git_time_since_commit() {
  # Only proceed if there is actually a commit.
  if git log -1 > /dev/null 2>&1; then
    # Get the last commit.
    last_commit=$(git log --pretty=format:'%at' -1 2> /dev/null)
    now=$(date +%s)
    seconds_since_last_commit=$((now-last_commit))

    # Totals
    minutes=$((seconds_since_last_commit / 60))
    hours=$((seconds_since_last_commit/3600))

    # Sub-hours and sub-minutes
    days=$((seconds_since_last_commit / 86400))
    sub_hours=$((hours % 24))
    sub_minutes=$((minutes % 60))

    if [ $hours -gt 24 ]; then
      commit_age="${days}d"
    elif [ $minutes -gt 60 ]; then
      commit_age="${sub_hours}h${sub_minutes}m"
    else
      commit_age="${minutes}m"
    fi

    color="%{$fg[white]%}"
    echo "$color$commit_age%{$reset_color%}"
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

_nesting_count=`expr $SHLVL - 1`
local _nesting_level="$(printf '=%.0s' {1..$_nesting_count})"

local _return_status="%(?..%F{red})"

PROMPT='
${_return_status}${_nesting_level}%f%F{green}$( _vcs_info_wrapper )%F{yellow}%B%(1j.#.) '
RPROMPT='%F{black}%2~ $(_git_time_since_commit)'

function zle-line-init zle-keymap-select {
NORMAL_COLOR="%{$fg_bold[blue]%}"
INSERT_COLOR="%{$fg_bold[black]%}"
RPS1="${${KEYMAP/vicmd/$NORMAL_COLOR}/(main|viins)/$INSERT_COLOR}%2~%{$reset_color%} $(_git_time_since_commit)"
zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
