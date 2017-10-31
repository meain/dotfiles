export ZSH=~/.oh-my-zsh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export TERM=screen-256color

# Golang
export GOPATH=~/.config/go

PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/git/bin:$PATH
PATH=/Library/Frameworks/Python.framework/Versions/2.7/bin:$PATH
PATH=/Library/TeX/texbin:$PATH
PATH=$PATH:~/.cargo/bin
PATH=$PATH:$GOPATH/bin
export PATH

echo ""
fortune -s | fmt -c -w $COLUMNS
echo ""

ZSH_THEME="terminalpartyedit"

DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="false"
HIST_STAMPS="dd.mm.yyyy"

# Plugins
plugins=(z zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# Virtualenv ( python )
# export WORKON_HOME=~/.virtual_envs
# source /Library/Frameworks/Python.framework/Versions/2.7/bin/virtualenvwrapper.sh

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi


# Source aliases
source $HOME/.common_aliases
source $HOME/.other_aliases
source $HOME/.vim_aliases
source $HOME/.emacs_aliases
source $HOME/.git_aliases
source $HOME/.tmux_aliases
source $HOME/.fzf_aliases

# Source custom functions
source $HOME/.common_functions
source $HOME/.other_functions

# eval $(thefuck --alias)
eval "$(hub alias -s)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Use vim mode in zsh
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
export KEYTIMEOUT=1
