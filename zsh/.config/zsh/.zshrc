#! /bin/zsh

[ -n "$ZPROF" ] && zmodload zsh/zprof

# Basic exports
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=screen-256color-italic

source $ZDOTDIR/.zsh_path

echo ""
if ! onacall;then
  randomidea | fmt -c -w $COLUMNS
else
  corpcrap | fmt -c -w $COLUMNS
fi
echo ""

# zsh settings
export DISABLE_AUTO_TITLE="true"
export COMPLETION_WAITING_DOTS="false"
export HIST_STAMPS="dd.mm.yyyy"
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.cache/zsh/.zsh_history
setopt HIST_IGNORE_SPACE
setopt appendhistory
setopt sharehistory
setopt incappendhistory

# cd-ing settings
setopt auto_cd # automatically cd if folder name and no command found
setopt auto_list # automatically list choices on ambiguous completion
setopt auto_menu # automatically use menu completion
setopt always_to_end # move cursor to end if word had one match
setopt interactive_comments # allow comments in interactive shells
zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
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
source $HOME/.bin/spectrum  # for 256 colors
source $ZDOTDIR/.zsh-custom/themes/snipe.zsh-theme
source $ZDOTDIR/.zsh-custom/plugins/z/z.sh
source $ZDOTDIR/.zsh-custom/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $ZDOTDIR/.zsh-custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

export EDITOR='et'  # emacsclient -t
export BROWSER=  # do not set browser
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
if [ -f $HOME/.credentials ];then
  source $HOME/.credentials
fi

# source exports
source $ZDOTDIR/.exports

# Source aliases
source $ZDOTDIR/.basic_aliases
source $ZDOTDIR/.vim_aliases
source $ZDOTDIR/.emacs_aliases
source $ZDOTDIR/.git_aliases
source $ZDOTDIR/.tmux_aliases
source $ZDOTDIR/.fzf_aliases
source $ZDOTDIR/.macos_aliases

# Source custom functions
source $ZDOTDIR/.zsh_mods
source $ZDOTDIR/.other_functions
source $ZDOTDIR/.coding_functions
source $ZDOTDIR/.npm_functions
source $ZDOTDIR/.git_functions
source $ZDOTDIR/.docker_functions
source $ZDOTDIR/.python_functions
source $ZDOTDIR/.kubectl_functions
source $ZDOTDIR/.gcloud_functions
source $ZDOTDIR/.ffmpeg_functions
source $ZDOTDIR/.imagemagic_functions

# Sorce fzf
[ -f /usr/locale/opt/.fzf.zsh ] && source /usr/locale/opt/.fzf.zsh

if [ -f $HOME/.temp_aliases ];then
  source $HOME/.temp_aliases
fi

# Source any changs for linux
case "$(uname -s)" in
  Linux)
    source $ZDOTDIR/.linux_modifications
    ;;
esac

# Source colors for ls
# case "$(uname -s)" in
#   Darwin)
#     eval $(gdircolors -b $ZDOTDIR/.dircolors)
#     ;;
#   Linux)
#     eval $(dircolors -b $ZDOTDIR/.dircolors)
#     ;;
# esac

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

# updates PATH for Google Cloud SDK && add shell completion for gcloud
export CLOUDSDK_PYTHON="/usr/local/opt/python@3.8/libexec/bin/python"
source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"

preexec () {
  if ! grep -q "$1" "$DATAFILES_PATH/long_runnable_jobs" ; then
    CMD_START_DATE=$(date +%s)
    CMD_NAME=$1
  fi
}

setdarkmode quiet  # set dark or light mode
export ZSH_LOADED=1

[ -n "$ZPROF" ] && zprof
