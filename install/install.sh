#!/bin/bash

execcommandfrompartial() {
    BASE=$1
    FILENAME=$2

    # comments and blank lines will be removed
    CONTENT="$(sed 's/\s*#.*//g; /^$/d' $FILENAME)"

    while read -r item; do
        COMMAND="$BASE $item"
        $COMMAND
    done <<< "$CONTENT"
}

# homebrew packages
if test ! "$( which brew )"; then
    echo "Installing homebrew"
    ruby -e "$( curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install )"
fi
execcommandfrompartial 'brew install' './brew.packages'

# homebrew cask
execcommandfrompartial 'brew cask install' './brew-cask.packages'

# install non package stuff
bash ./non-package.sh

# ruby packages
execcommandfrompartial 'gem install' './ruby.packages'

# npm packages
execcommandfrompartial 'npm install -g' './node.packages'

# python2 packages
execcommandfrompartial 'pip2' './python2.packages'

# python3 packages
execcommandfrompartial 'pip3' './python3.packages'
