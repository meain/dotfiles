#!/bin/sh

# mac settings
defaults write com.apple.screencapture location ~/Documents/Screenshots/  # change scresnshot dir
defaults write com.apple.Terminal AppleShowScrollBars -string WhenScrolling  # no scrollbar in mac terminal
defaults write -g ApplePressAndHoldEnabled -bool false  # enable key repeat
sudo nvram SystemAudioVolume="%00"  # no boot bell

execcommandfrompartial() {
    BASE=$1
    FILENAME=$2

    # comments and blank lines will be removed
    CONTENT="$(sed 's/\s*#.*//g; /^$/d' "$FILENAME")"

    while read -r item; do
        COMMAND="$BASE $item"
        $COMMAND
    done <<< "$CONTENT"
}

# homebrew packages
if test ! "$( which brew )"; then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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

# tex packages
execcommandfrompartial 'sudo tlmgr install' './go.packages'

# rust packages
execcommandfrompartial 'cargo install' './cargo.packages'

# more setup
defaults write org.hammerspoon.Hammerspoon MJConfigFile "$HOME/.config/hammerspoon/init.lua"

# enable at (https://superuser.com/questions/43678/mac-os-x-at-command-not-working)
sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.atrun.plist
