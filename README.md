<h1 align="center">Dotfiles</h1>
<p align="center"> If there is a shell, there is a way</p>
<p align="center">
  <img src="https://img.shields.io/badge/Editor-neovim-brightgreen.svg" />
  <img src="https://img.shields.io/badge/Terminal-Alacritty-orange.svg" />
  <img src="https://img.shields.io/badge/Shell-zsh-yellow.svg" />
  <img src="https://img.shields.io/badge/Font-Code%20New%20Roman-lightgrey.svg" />
  <img src="https://img.shields.io/badge/Mail-neomutt-red.svg" />
  <img src="https://img.shields.io/badge/IRC-irssi-blue.svg" />
  <br><br>
  <img src="https://i.imgur.com/pVGr7tX.png">
</p>

**Hey**, these are the dotfiles that I use.

It includes my `[z/ba/fi]sh`, `[v/nv/mv/gv]im`, `emacs`, `tmux`, `git` config files.

## How to use

**Use `gnu-stow` to link the files.**

For example if you need my `nvim` config clone the repo then inside the repo use:
`stow nvim`
This will symlink the necessary files.

```
git clone https://github.com/meain/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
stow nvim
```

## Dependencies

[`oh-my-zsh`](https://github.com/robbyrussell/oh-my-zsh) better zsh

[`pandoc`](http://pandoc.org/index.html) + [`lynx`](http://lynx.browser.org/) view markdown in shell

[`fzf`](https://github.com/junegunn/fzf) the most badass fuzzy find

[`mru.vim`](https://github.com/vim-scripts/mru.vim) need for `v` & `vv`

[`hub`](https://hub.github.com/) better git

[`z`](https://github.com/rupa/z) quickly jump around folders

## Other stuff you might wanna use

[`alacritty`](https://github.com/jwilm/alacritty) The fasters terminal emulator you can get

[`neomutt`](https://www.neomutt.org/) why use a web browser to check mail?

[`irssi`](https://irssi.org/) the best irc client

[`ranger`](https://github.com/ranger/ranger) the better way to manage files

[`coderay`](https://github.com/rubychan/coderay) syntax highlight your code

[`cmus`](https://cmus.github.io/) a beautiful music player in the shell

[`vtop`](https://github.com/MrRio/vtop) a pretty `top` command with ascii art

[`thefuck`](https://github.com/nvbn/thefuck) correct your previous command

[`ripgrep`](https://github.com/BurntSushi/ripgrep) You think ag is fast? Try this

[`colorls`](https://github.com/athityakumar/colorls) Much prettier ls

## Screenshots

*as of `620d4e65`*

> Gone quite a bit minimal after that

### Zsh
![](https://i.imgur.com/oh4DY5e.png)

### Vim
![](https://i.imgur.com/sPVLbzI.png)

### Tmux
![](https://i.imgur.com/YBTlVjK.png)

### irssi
![](https://i.imgur.com/08iF4Ts.png)

### Colors
![](https://i.imgur.com/E9qgsHj.png)

