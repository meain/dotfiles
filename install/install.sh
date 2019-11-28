#!/bin/sh

# mac settings
defaults write com.apple.screencapture location ~/Documents/Screenshots/
sudo nvram SystemAudioVolume="%00"

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

# npm packages
execcommandfrompartial 'npm install -g' './node.packages'

# python3 packages
execcommandfrompartial 'pip3 install' './python3.packages'

# go packages
execcommandfrompartial 'go get -vu' './go.packages'

# more setup
defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"
