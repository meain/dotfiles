#!/bin/zsh

setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '%b|%a'
zstyle ':vcs_info:*' formats '%b%f'
zstyle ':vcs_info:*' enable git

# or use pre_cmd, see man zshcontrib
vcs_info_wrapper() {
	vcs_info
	[ -n "$vcs_info_msg_0_" ] && echo "${vcs_info_msg_0_}%{$reset_color%}"
}

PROMPT='%(!.#.!) '
RPROMPT='$(vcs_info_wrapper) %2~'
