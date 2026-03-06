#!/bin/zsh

setopt prompt_subst

export VIRTUAL_ENV_DISABLE_PROMPT=1

autoload -Uz colors
colors

# variables
INSERT_COLOR="%{$reset_color%}"
NORMAL_COLOR="%{%F{39}%}"

function sroot(){
    jj workspace root 2>/dev/null ||
        git rev-parse --show-toplevel
}

function dir_from_base() {
    root="$(sroot 2>/dev/null | xargs dirname 2>/dev/null)"
    if [ -n "$root" ]; then
        echo "$(pwd)" | sed "s|$root||;s|^/||"
    else
        echo '%2~'
    fi
}

function virtualenv_info {
    if [[ -n "$VIRTUAL_ENV" ]] ; then
        echo " py:${VIRTUAL_ENV:t}"
    fi
}

function custom_additions() {
    if sroot &>/dev/null; then
        pushd "$(sroot 2>/dev/null)" >/dev/null
        if [ -f .mscripts/shell-additions ]; then
            .mscripts/shell-additions
        fi
    fi
}

function _multi_theme_asyncp() {
    # Alternate: "%(?..%F{red}[%?] )"
    printf "\n%s[%s] %s%s%s\n%s " \
        '%F{white}' \
        "$(dir_from_base)" \
        "$(virtualenv_info)" \
        "$(custom_additions)" \
        "%{$reset_color%}" \
        "%(?.✨.💥️)" \
        > "/tmp/zsh_lprompt_$$"
    kill -s USR1 $$  # signal parent
}

function _multi_theme_precmd() {
    _multi_theme_asyncp &!

    if [[ -n $CMD_START_DATE ]]; then
        CMD_ELAPSED_TIME=$(($(date +%s) - $CMD_START_DATE))
        CMD_START_DATE=""

        if ((CMD_ELAPSED_TIME > 60)); then
            notify "$CMD_NAME" "Completed in $((CMD_ELAPSED_TIME / 60))m$((CMD_ELAPSED_TIME % 60))s"
        fi
    fi
}

autoload -Uz add-zsh-hook
add-zsh-hook precmd _multi_theme_precmd

function TRAPUSR1() {
    PS1="$(cat /tmp/zsh_lprompt_$$)" # read from temp file ( uses non breaking space )
    zle && zle reset-prompt          # redisplay
}

# printf '\e[38;5;235m―%.0s' {1..$COLUMNS} # separator
PROMPT='

✨ '
