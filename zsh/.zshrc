#! /bin/zsh

# Basic exports
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=screen-256color-italic

source $HOME/.zsh_path

echo ""
fortune -s | fmt -c -w $COLUMNS
echo ""

# zsh settings
export DISABLE_AUTO_TITLE="true"
export COMPLETION_WAITING_DOTS="false"
export HIST_STAMPS="dd.mm.yyyy"
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zsh_history
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
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion


# autocompletions
autoload -Uz compinit
typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
  compinit -i
else
  compinit -C -i
fi

# sourcing plugins & themes
source $HOME/.bin/spectrum  # for 256 colors
source $HOME/.zsh-custom/themes/snipe.zsh-theme
source $HOME/.zsh-custom/plugins/z/z.sh
source $HOME/.zsh-custom/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $HOME/.zsh-custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

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
source $HOME/.exports

# Source aliases
source $HOME/.basic_aliases
source $HOME/.vim_aliases
source $HOME/.emacs_aliases
source $HOME/.git_aliases
source $HOME/.tmux_aliases
source $HOME/.fzf_aliases
source $HOME/.macos_aliases

# Source custom functions
source $HOME/.zsh_mods
source $HOME/.other_functions
source $HOME/.coding_functions
source $HOME/.npm_functions
source $HOME/.git_functions
source $HOME/.docker_functions
source $HOME/.python_functions
source $HOME/.kubectl_functions

# Sorce fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [ -f $HOME/.temp_aliases ];then
  source $HOME/.temp_aliases
fi

# Source any changs for linux
case "$(uname -s)" in
  Linux)
    source $HOME/.linux_modifications
    ;;
esac

# Source colors for ls
case "$(uname -s)" in
  Darwin)
    eval $(gdircolors -b $HOME/.dircolors)
    ;;
  Linux)
    eval $(dircolors -b $HOME/.dircolors)
    ;;
esac

# Use vim mode in zsh
autoload -U edit-command-line
zle -N edit-command-line
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^e' edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
bindkey "\e[A" history-search-backward
bindkey "\e[B" history-search-forward
export KEYTIMEOUT=1

# updates PATH for Google Cloud SDK && add shell completion for gcloud
GOOGLE_CLOUD='/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
if [ -f '$GOOGLE_CLOUD' ]; then source '$GOOGLE_CLOUD'; fi

# source python workon
case "$(uname -s)" in
  Darwin)
    source '/usr/local/bin/virtualenvwrapper_lazy.sh'
    ;;
  Linux)
    source "$HOME/.local/bin/virtualenvwrapper.sh"
    ;;
esac

preexec () {
  if ! grep "$1" < $HOME/.datafiles/long_runnable_jobs > /dev/null; then
    CMD_START_DATE=$(date +%s)
    CMD_NAME=$1
  fi
}

precmd () {
  # to add a newline after every command, putting newline in the prompt was slowing things down
  print ""

  # Proceed only if we've ran a command in the current shell.
  if ! [[ -z $CMD_START_DATE ]]; then
    CMD_END_DATE=$(date +%s)
    CMD_ELAPSED_TIME=$(($CMD_END_DATE - $CMD_START_DATE))
    CMD_NOTIFY_THRESHOLD=60
    CMD_START_DATE=""

    if [[ $CMD_ELAPSED_TIME -gt $CMD_NOTIFY_THRESHOLD ]]; then
      # print -n '\a'
      osascript -e "display notification \"$CMD_NAME took $CMD_ELAPSED_TIME seconds\" with title \"Job complete\""
    fi
  fi
}

export ZSH_LOADED=1
