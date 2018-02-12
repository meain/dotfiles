function _prompt_git() {
    repo_path=$(git rev-parse --git-dir 2>/dev/null)
    ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="$(git rev-parse --short HEAD 2> /dev/null)"
    if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
        dirty=$(parse_git_dirty)
        if [[ -n $dirty ]]; then
            GIT_COLOR=%{$fg[red]%}
        else
            GIT_COLOR=%{$fg[green]%}
        fi
    fi
    echo -n " =$GIT_COLOR${ref/refs\/heads\//$PL_BRANCH_CHAR}${vcs_info_msg_0_%% }%{$reset_color%}"
}

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

    color=$ZSH_THEME_GIT_TIME_SINCE_COMMIT_NEUTRAL
    echo "$color$commit_age%{$reset_color%}"
  fi
}

PROMPT='
$(_prompt_git)%{$reset_color%} '
RPROMPT='%{$fg_bold[black]%}%2~%{$reset_color%} $(_git_time_since_commit)'

function zle-line-init zle-keymap-select {
    NORMAL_COLOR="%{$fg_bold[blue]%}"
    INSERT_COLOR="%{$fg_bold[black]%}"
    RPS1="${${KEYMAP/vicmd/$NORMAL_COLOR}/(main|viins)/$INSERT_COLOR}%2~%{$reset_color%} $(_git_time_since_commit)"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
