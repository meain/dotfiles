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

# update poetry
poetry self update

# update rust build tools
rustup update stable
rustup update nightly

# update alacritty
cd ~/Documents/Projects/others/clones/alacritty && git pull origin master && make app && cp -r target/release/osx/Alacritty.app /Applications/ && cd -

gloc "git fetch" "/Users/meain/Documents/Projects/work/saama"
gloc "git fetch" "/Users/meain/Documents/Projects/projects"

date > ~/.last_updated_time
