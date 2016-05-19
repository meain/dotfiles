function battery_charge() {
    if [ -e ~/bin/batcharge.py ]
        then
            echo `python ~/bin/batcharge.py`
    else
        echo '';
    fi
}
function time_display() {
    if [ -e ~/bin/timedisplay.py ]
        then
            echo `python ~/bin/timedisplay.py`
    else
        echo '';
    fi
}
PROMPT='%(?,%{$fg[green]%},%{$fg[red]%}) x '
# RPS1='%{$fg[blue]%}%~%{$reset_color%} '
RPS1='%{$fg[white]%}%2~$(git_prompt_info) $(time_display) $(battery_charge)'

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} +%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[red]%} x%{$fg[yellow]%}"
