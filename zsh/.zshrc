# Basic exports
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=screen-256color-italic

# Golang
export GOPATH=~/Documents/Projects/goworkspace
export GOBIN=$GOPATH/bin

# Rust
PATH=$PATH:~/.cargo/bin

# Python
PATH=/Library/Frameworks/Python.framework/Versions/2.7/bin:$PATH
export WORKON_HOME=~/.virtual_envs

# Latex
PATH=/Library/TeX/texbin:$PATH

# PATH
PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/git/bin:$PATH
PATH=$PATH:$GOPATH/bin
export PATH

echo ""
fortune -s | fmt -c -w $COLUMNS
echo ""

# History management
export HISTSIZE=100000
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=$HISTSIZE

# Set some configs
setopt autocd               # .. is shortcut for cd .. (etc)
setopt autoparamslash       # tab completing directory appends a slash
setopt autopushd            # cd automatically pushes old dir onto dir stack
setopt clobber              # allow clobbering with >, no need to use >!
setopt correct              # command auto-correction
setopt correctall           # argument auto-correction
setopt noflowcontrol        # disable start (C-s) and stop (C-q) characters
setopt nonomatch            # unmatched patterns are left unchanged
setopt histignorealldups    # filter duplicates from history
# setopt histignorespace      # do nont record commands starting with a space
setopt histverify           # confirm history expansion (!$, !!, !foo)
setopt ignoreeof            # prevent accidental C-d from exiting shell
setopt interactivecomments  # allow comments, even in interactive shells
setopt printexitvalue       # for non-zero exit status
setopt pushdignoredups      # do not push multiple copies of same dir onto stack
setopt pushdsilent          # do not print dir stack after pushing/popping
setopt sharehistory         # share history across shells

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

# Source shell theme
source ~/.oh-my-zsh/themes/snipe.zsh-theme

# Source plugins
source ~/.zsh/z/z.sh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# Credentials
source $HOME/.credentials

# Source aliases
source $HOME/.basic_aliases
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
source $HOME/.coding_functions

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
if [ -f '/Users/abinsimon/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/abinsimon/google-cloud-sdk/path.zsh.inc'; fi
if [ -f '/Users/abinsimon/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/abinsimon/google-cloud-sdk/completion.zsh.inc'; fi
