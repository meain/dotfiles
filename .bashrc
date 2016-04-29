#!/bin/bash

echo ""
h=`date +%H`
if [ $h -lt 12 ]; then
     echo Good morning, *meain*!|lolcat
elif [ $h -lt 18 ]; then
     echo Good afternoon, *meain*!|lolcat
elif [ $h -lt 24 ]; then
     echo Good evening, *meain*!|lolcat
else
     echo Get some sleep, *meain*!|lolcat
fi
echo ""

#Python sometimes messed up with me when I used matplotlib without the following statements, I don't know why
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

#This is to change the long text to just a %
# PS1="% : "

#Alias
alias ss='ls -lht'
alias ll='ls -A -l'
alias la='ls -A'
alias lr='ls -lRht'
alias c='clear'
alias q='exit'
alias d='less'
alias oo='vim ~/notes.org'
alias mkdir='mkdir -p'
alias rm='rm -i'
alias ffind='find . -name'
alias .='cd ..'
#Check evey single file for a specific text and print surrounding 2 lines
alias here='find . -type f -print0|xargs -0 grep -C 2 -i'
alias server='python -m SimpleHTTPServer '

parse_git_branch() {

    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'

}
# Alias to open multimedia folder in finder
alias fun='cd ~/Documents/Multimedia && open . && cd -'
# Alias to get into project folder
alias meh='cd ~/Documents/Projects'
# Alias to get into desktop folder
alias blah='cd ~/Desktop'

# Git alias

# Alias for easier commit
alias gu='git add -u && git commit'

# Git add
alias ga='git add'

# Easier push to origin master
alias gp='git push origin $(parse_git_branch)'

# Git log and history alias 
alias gg='git lh'
alias gh='git hi'
alias gl='git lg'

# Git diff alias
alias gd='git diff'

#Git status alias
alias gs='git status'
alias g='git status'

#Git commit 
alias gc='git commit'
