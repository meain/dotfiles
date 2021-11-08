#! /bin/zsh

[ -n "$ZPROF" ] && zmodload zsh/zprof

# Basic exports
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

source $ZDOTDIR/exports

echo ""
basename `find "$NOTES_PATH/idea" -name '*.md' | sed 's|.md$||' | shuf -n1` | fmt -c -w $COLUMNS
# corpcrap | fmt -c -w $COLUMNS
echo ""

# zsh settings
export DISABLE_AUTO_TITLE="true"
export COMPLETION_WAITING_DOTS="false"
export HIST_STAMPS="dd.mm.yyyy"
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.local/share/zsh/.zsh_history
setopt HIST_IGNORE_SPACE
setopt appendhistory
setopt sharehistory
setopt incappendhistory

# cd-ing settings
setopt auto_cd                                         # automatically cd if folder name and no command found
setopt auto_list                                       # automatically list choices on ambiguous completion
setopt auto_menu                                       # automatically use menu completion
setopt always_to_end                                   # move cursor to end if word had one match
setopt interactive_comments                            # allow comments in interactive shells
zstyle ':completion:*' menu select                     # select completions with arrow keys
zstyle ':completion:*' group-name ''                   # group results by category
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' # non case sensitive complete
zstyle ':completion:*' list-colors $LS_COLORS
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion

# autocompletions
autoload -Uz compinit
zmodload zsh/complist
compinit

# some settings
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=244"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC="true"

# sourcing plugins & themes
source $ZDOTDIR/.zsh-custom/themes/snipe.zsh-theme
source $ZDOTDIR/.zsh-custom/plugins/z/z.sh
source $ZDOTDIR/.zsh-custom/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $ZDOTDIR/.zsh-custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

export EDITOR='emacsclient'
export BROWSER= # do not set browser
export DIFFTOOL='icdiff'

# Make CTRL-Z background things and unbackground them.
function fg-bg() {
	if [[ $#BUFFER -eq 0 ]]; then
		fg
	else
		zle push-input
	fi
}
zle -N fg-bg
bindkey '^Z' fg-bg

# Credentials
[ -f $HOME/.credentials ] && source $HOME/.credentials

# Source aliases
source $ZDOTDIR/aliases
source $ZDOTDIR/functions

# source dir hashes
[ -f ~/.local/share/zsh/.zsh_dir_hashes ] && source ~/.local/share/zsh/.zsh_dir_hashes

# Sorce fzf
[ -f /usr/locale/opt/.fzf.zsh ] && source /usr/locale/opt/.fzf.zsh

# Source colors for ls (trapd00r/LS_COLORS)
# [ "$(uname -s)" == "Darwin" ] && eval $(gdircolors -b $ZDOTDIR/dircolors) || eval $(dircolors -b $ZDOTDIR/dircolors)

# Use vim mode in zsh
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
bindkey "\e[A" history-search-backward
bindkey "\e[B" history-search-forward
export KEYTIMEOUT=1

preexec() {
	if ! grep -q "$1" "$DATAFILES_PATH/long_runnable_jobs"; then
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

# load nix
. $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

# setup direnv
eval "$(direnv hook zsh)"

,darkmode quiet # set dark or light mode
export ZSH_LOADED=1

[ -n "$ZPROF" ] && zprof
