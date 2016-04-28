#!/bin/bash

echo ""
echo "*meain* : Just don't code shit" 
echo ""

#Python sometimes messed up with me when I used matplotlib without the following statements, I don't know why
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

#This is to change the long text to just a %
# PS1="% : "

# Alias to get into project folder
alias blah='cd ~/Documents/Projects'

# Alias for easier commit
alias gu='git add -u && git commit'
alias ga='git add * && git commit'
