#! /bin/zsh

# Update homebrew
brew update
brew upgrade

# update oh-my-zsh
zsh $ZSH/tools/upgrade.sh

# update vim plugins
nvim +PlugUpgrade +PlugUpdate +qall

# update poetry
poetry self update

# update rust build tools
rustup update stable
rustup update nightly

gloc "git fetch" "/Users/meain/Documents/Projects/work/saama"
gloc "git fetch" "/Users/meain/Documents/Projects/projects"

date >> ~/.last_updated_time
