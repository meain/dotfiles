# Path to your oh-my-zsh installation.
export ZSH=/Users/abinsimon/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="terminalpartyedit"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git python brew osx extract web-search zsh-autosuggestions zsh-syntax-highlighting)

# User configuration

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/git/bin"
# export MANPATH="/usr/local/man:$MANPATH"


# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

##pipsi install addition to the path variable
export PATH="/Users/abinsimon/.local/bin:$PATH"

. `brew --prefix`/etc/profile.d/z.sh

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

#enabling pandoc and lunx to make reading .md files in terminal
#To read do : rmd file.md
rmd () {
  pandoc $1 | lynx -stdin
}

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

#--------------------------------------------------------------------------------------------

# Custom

echo ""
echo "Hey *meain*, what's up? How you doing?" | fmt -c -w $COLUMNS | lolcat
echo ""

# source ~/.oh-my-zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
eval $(thefuck --alias)

#Alias
alias ss='ls -lht'
alias ll='ls -A -l'
alias l='ls'
alias la='ls -A'
alias lr='ls -lRht'
alias c='clear && echo "\n¯\_(ツ)_/¯\n" | fmt -c -w $COLUMNS | lolcat'
alias q='exit'
alias :q='exit'
alias mkdir='mkdir -p'
alias o='open .'
# alias rm='rm -i'
alias rm='trash'
alias ffind='find . -name'
alias ..='cd ..'
#Check evey single file for a specific text and print surrounding 2 lines
alias here='find . -type f -print0|xargs -0 grep -C 2 -i'
alias server='python -m SimpleHTTPServer '
alias tn='tmux new -s'
alias ta='tmux attach -t'

# Alias to open multimedia folder in finder
alias m='cd ~/Documents/Multimedia && open . && cd -'
# Alias to get into project folder
alias p='cd ~/Documents/Projects && ls'
# Alias to get into desktop folder
alias d='cd ~/Desktop && ls'

# Git alias

# Functions - mostly git
get_git_files_changed(){
    git status --short | sed s/.\ //g | tr '\n' ','  | sed s/,$//g
}
get_git_branch(){
    git branch | grep \* | sed s/^\*\ //g
}
get_change_message(){
    echo "($(git status --short | sed s/.\ //g | tr '\n' ','  | sed s/,$//g | sed s/^.//g)):" | vipe | cat
}
get_change_message_without_filename(){
    echo ":" | vipe | cat
}

# Alias for easier git commits
# Commit message wihout any s***
alias gu='git add -u && git commit'
# Commit message wihout any s*** + push
alias gvp='git add -u && git commit" && git push origin $(get_git_branch)'
# Simple messages
alias gu='git add -u && git commit -m "Updated$(get_git_files_changed)"'
alias gc='git add -u && git commit -m "Clean up$(get_git_files_changed)"'
alias gb='git add -u && git commit -m "Bugfix$(get_git_files_changed)"'
# Simple messge with push
alias gup='git add -u && git commit -m "Updated$(get_git_files_changed)" && git push origin $(get_git_branch)'
alias gcp='git add -u && git commit -m "Clean up$(get_git_files_changed)" && git push origin $(get_git_branch)'
alias gbp='git add -u && git commit -m "Bugfix$(get_git_files_changed)" && git push origin $(get_git_branch)'
# Message with more information without filenmae
alias guv='git add -u && git commit -m "$(get_change_message_without_filename)"'
alias gcv='git add -u && git commit -m "$(get_change_message_without_filename)"'
alias gbv='git add -u && git commit -m "$(get_change_message_without_filename)"'
# More info + push without filename
alias gupv='git add -u && git commit -m "Update$(get_change_message_without_filename)" && git push origin $(get_git_branch)'
alias gcpv='git add -u && git commit -m "Clean up$(get_change_message_without_filename)" && git push origin $(get_git_branch)'
alias gbpv='git add -u && git commit -m "Bugfix$(get_change_message_without_filename)" && git push origin $(get_git_branch)'
# Message with more information with filename
alias guvf='git add -u && git commit -m "$(get_change_message)"'
alias gcvf='git add -u && git commit -m "$(get_change_message)"'
alias gbvf='git add -u && git commit -m "$(get_change_message)"'
# More info + push with filename
alias gupvf='git add -u && git commit -m "Update$(get_change_message)" && git push origin $(get_git_branch)'
alias gcpvf='git add -u && git commit -m "Clean up$(get_change_message)" && git push origin $(get_git_branch)'
alias gbpvf='git add -u && git commit -m "Bugfix$(get_change_message)" && git push origin $(get_git_branch)'
# Git add
alias ga='git add'
# Easier push to origin master
alias gp="git push origin $(git branch | grep \* | sed s/^\*\ //g)"
# Git log and history alias 
alias ggg='git lh|cat'
alias gg='git hm|cat'
alias gh='git hi|cat'
alias gl='git lg'
# Git diff alias
alias gd='git diff'
#Git status alias
alias gs='git status'
alias g='git status -s'
#Git commit 
alias gc='git commit'
#Git checkeout
alias gco='git checkout'
alias gcom='git checkout master'

# More stuff

# Tmux alias
alias tmux='sh ~/bin/tmuxcopyhotfix.sh & tmux && fg'

#eeeks
alias vim="nvim"
#Just because it happens all the time
alias ivm='vim'
alias vm='nvim'
#Open last vim session
alias viml='nvim -c :SLoad\ zPreviousSession'

# Note taking
alias k="python ~/bin/terminalnote.py '"
alias kk='python ~/bin/terminalnote.py k'

# Eywa start
alias eywa='cd ~/Documents/Projects/eywa && source bin/activate && cd eywa && echo "\033[0;31m Do start chromix server using chromix-server \033[0m" && python manage.py runserver localhost:4500'
alias es='~/bin/tmuxeywa.sh'
alias eywaserver='~/bin/tmuxeywavim.sh'

# Youtube-dl
alias ydp='youtube-dl -o "%(playlist_index)s_%(title)s.%(ext)s"'
