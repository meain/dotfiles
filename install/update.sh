#! /bin/zsh

# Update homebrew
brew update
brew upgrade

# update oh-my-zsh
zsh $ZSH/tools/upgrade.sh

# update vim plugins
nvim +PlugUpgrade +PlugUpdate +qall

# update mail
offlineimap

# update rust build tools
rustup update stable
rustup update nightly
