function pro_git() {
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

PROMPT='
$(pro_git)%{$reset_color%} '
RPROMPT='%{$fg_bold[black]%}%2~{$reset_color%}'

function zle-line-init zle-keymap-select {
    NORMAL_COLOR="%{$fg_bold[blue]%}"
    INSERT_COLOR="%{$fg_bold[black]%}"
    RPS1="${${KEYMAP/vicmd/$NORMAL_COLOR}/(main|viins)/$INSERT_COLOR}%2~"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
