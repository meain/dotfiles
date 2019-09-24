#! /bin/zsh

# Basic exports
# export ZSH=$HOME/.oh-my-zsh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=screen-256color-italic
# export DISABLE_AUTO_UPDATE=true

source $HOME/.zsh_path

echo ""
fortune -s | fmt -c -w $COLUMNS
echo ""

# oh-my-zsh settings
# export ZSH_THEME="snipe"
export DISABLE_AUTO_TITLE="true"
export COMPLETION_WAITING_DOTS="false"
export HIST_STAMPS="dd.mm.yyyy"
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zsh_history
setopt appendhistory
setopt sharehistory
setopt incappendhistory

setopt auto_cd
setopt auto_list # automatically list choices on ambiguous completion
setopt auto_menu # automatically use menu completion
setopt always_to_end # move cursor to end if word had one match
setopt interactive_comments # allow comments in interactive shells
zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion


# autocompletions
autoload -Uz compinit
typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
  compinit -i
else
  compinit -C -i
fi

source $HOME/.bin/spectrum  # for 256 colors
source $HOME/.oh-my-zsh/themes/snipe.zsh-theme
source $HOME/.zsh/z/z.sh
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

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

#Source fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Use vim mode in zsh
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
export KEYTIMEOUT=1

# updates PATH for Google Cloud SDK && add shell completion for gcloud
GOOGLE_CLOUD='/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
GOOGLE_CLOUD_COMPLETIONS='/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
if [ -f '$GOOGLE_CLOUD' ]; then source '$GOOGLE_CLOUD'; fi
if [ -f '$GOOGLE_CLOUD_COMPLETIONS' ]; then source '$GOOGLE_CLOUD_COMPLETIONS'; fi

# source python workon
case "$(uname -s)" in
  Darwin)
    source '/usr/local/bin/virtualenvwrapper_lazy.sh'
    ;;
  Linux)
    source "$HOME/.local/bin/virtualenvwrapper.sh"
    ;;
esac

# to add a newline after every command, putting newline in the prompt was slowing things down
precmd() { print "" }

export ZSH_LOADED=1
