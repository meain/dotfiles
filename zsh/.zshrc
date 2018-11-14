#! /bin/zsh
# Basic exports
export ZSH=~/.oh-my-zsh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=screen-256color-italic

# Golang
export GOPATH=~/.go
export GOBIN=$GOPATH/bin

# Rust
PATH=$PATH:~/.cargo/bin

# Python
PATH=/Library/Frameworks/Python.framework/Versions/2.7/bin:$PATH
export WORKON_HOME=~/.virtual_envs

# Latex
PATH=/Library/TeX/texbin:$PATH

# PATH
PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/git/bin:/Users/meain/.bin:$PATH
PATH=$PATH:$GOPATH/bin
export PATH

echo ""
fortune -s | fmt -c -w $COLUMNS
echo ""

# oh-my-zsh settings
export ZSH_THEME="snipe"
export DISABLE_AUTO_TITLE="true"
export COMPLETION_WAITING_DOTS="false"
export HIST_STAMPS="dd.mm.yyyy"
export plugins=(z zsh-syntax-highlighting zsh-autosuggestions)
source $ZSH/oh-my-zsh.sh

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
if [-f $HOME/.credentials];then
  source $HOME/.credentials
fi

# source exports
source $HOME/.exports

# Source aliases
source $HOME/.common_aliases
source $HOME/.other_aliases
source $HOME/.vim_aliases
source $HOME/.emacs_aliases
source $HOME/.git_aliases
source $HOME/.tmux_aliases
source $HOME/.fzf_aliases
source $HOME/.macos_aliases

# Source custom functions
source $HOME/.common_functions
source $HOME/.other_functions
source $HOME/.coding_functions
source $HOME/.npm_functions
source $HOME/.git_functions
source $HOME/.docker_functions

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

# Github
eval "$(hub alias -s)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Use vim mode in zsh
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
export KEYTIMEOUT=1

# updates PATH for Google Cloud SDK && add shell completion for gcloud
GOOGLE_CLOUD='/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
GOOGLE_CLOUD_COMPLETIONS='/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc]'
if [ -f '$GOOGLE_CLOUD' ]; then source '$GOOGLE_CLOUD'; fi
if [ -f '$GOOGLE_CLOUD_COMPLETIONS' ]; then source '$GOOGLE_CLOUD_COMPLETIONS'; fi

# source python workon
source '/usr/local/bin/virtualenvwrapper_lazy.sh'

export ZSH_LOADED=1
