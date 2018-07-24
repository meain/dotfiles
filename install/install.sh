# install homebrew
if test ! "$( which brew )"; then
    echo "Installing homebrew"
    ruby -e "$( curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install )"
fi

# brew
brew install tmux
brew install tree
brew install wget
brew install neovim --HEAD
brew install trash
brew install node
brew install ffmpeg --with-libass --with-fontconfig
brew install imagemagick --with-fontconfig
brew install imagemagick
brew install giflossy
brew install aspell
brew install iproute2mac
brew install offlineimap
brew install axel
brew install irssi
brew install bison
brew install jq
brew install calcurse
brew install pandoc
brew install catimg
brew install cmake
brew install coreutils
brew install cowsay
brew install ctags
brew install emacs
brew install python
brew install python3
brew install fd
brew install figlet
brew install fish
brew install redis
brew install ripgrep
brew install fontconfig
brew install fortune
brew install ruby
brew install freetype
brew install sleepwatcher
brew install fzf
brew install sqlite
brew install stow
brew install tig
brew install tree
brew install mongodb
brew install mpv
brew install urlview
brew install go
brew install vifm
brew install w3m
brew install watch
brew install ncurses
brew install neofetch
brew install neomutt
brew install htop
brew install newsboat
brew install hub

# brew cask
brew cask android-platform-tools
brew cask hammerspoon
brew cask mactex
brew cask google-cloud-sdk
brew cask ngrok

# ruby
gem install lolcat
gem install neovim

# neovim
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# npm
npm install -g neovim
npm install -g nodemon
npm install -g parcel-bundler
npm install -g gifify
npm install -g vscode-css-languageserver-bin
npm install -g typescript
npm install -g typescript-language-server
npm install -g tldr
npm install -g instant-markdown-d
npm install -g flow
npm install -g flow-bin
npm install -g fkill-cli
npm install -g eslint
npm install -g diff-so-fancy
npm install -g bash-language-server
npm install -g ts-node
npm install -g surge
npm install -g prettier
npm install -g http-server
npm install -g stylelint
npm install -g stylelint-config-standard

# python2
pip2 install requests
pip2 install pycodestyle
pip2 install pyflakes
pip2 install pylint
pip2 install virtualenvwrapper
pip2 install wakatime
pip2 install pytest
pip2 install mkdocs
pip2 install ipython
pip2 install googler
pip2 install ipdb
pip2 install flask
pip2 install falcon
pip2 install autopep8
pip2 install neovim

# python3
pip3 install autopep8
pip3 install beautifulsoup4
pip3 install falcon
pip3 install flake8
pip3 install flask
pip3 install flask-Cors
pip3 install googler
pip3 install gunicorn
pip3 install ipdb
pip3 install ipython
pip3 install jedi
pip3 install keras
pip3 install neovim
pip3 install numpy
pip3 install Pillow
pip3 install pycodestyle
pip3 install pydocstyle
pip3 install pyflakes
pip3 install Pygments
pip3 install pymongo
pip3 install python-language-server
pip3 install requests
pip3 install tqdm
pip3 install virtualenv
pip3 install virtualenvwrapper
pip3 install yapf
pip3 install youtube-dl
