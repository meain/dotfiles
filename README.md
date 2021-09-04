<h1 align="center">Dotfiles</h1>
<p align="center"> If there is a shell, there is a way</p>
<p align="center">
  <img src="https://img.shields.io/badge/Editor-emacs-brightgreen.svg" />
  <img src="https://img.shields.io/badge/Terminal-alacritty-orange.svg" />
  <img src="https://img.shields.io/badge/Shell-zsh-yellow.svg" />
  <br><br>
  <img src="https://i.imgur.com/pVGr7tX.png">
</p>

**Hey**, these are the dotfiles that I use.

It includes my `[z/ba/fi]sh`, `[v/nv/mv/gv]im`, `emacs`, `tmux`, `git`, `i3`, `karabiner`, `tig`, `newsboat`, `hammerspoon` , ... config files.

## How to use

**Use `gnu-stow` to link the files.**

For example if you need my `nvim` config clone the repo then inside the repo use:
`stow nvim`
This will symlink the necessary files.

```
git clone --recursive https://github.com/meain/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
stow nvim
```

## Stuff you might wanna use

- [`fzf`](https://github.com/junegunn/fzf) Fuzzy finder
- [`ripgrep`](https://github.com/BurntSushi/ripgrep) Much faster ack
- [`mru.vim`](https://github.com/vim-scripts/mru.vim) Needed for `v` & `vv`
- [`alacritty`](https://github.com/jwilm/alacritty) or [`kitty`](https://sw.kovidgoyal.net/kitty/) Best terminal emulators
- [`hub`](https://hub.github.com/) Git plus something else
- [`z`](https://github.com/rupa/z) Quickly jump around folders
- [`bat`](https://github.com/sharkdp/bat) Cat with syntax highlight
- [`lf`](https://github.com/gokcehan/lf) Ranger but faster
- [`jv`](https://github.com/maxzender/jv) and [`jid`](https://github.com/simeji/jid) view/filter json in terminal
- [`scim`](https://github.com/andmarti1424/sc-im) Excel in terminal
- [`lsd`](https://github.com/Peltoche/lsd) `ls` but much more
- [`diff-so-fancy`](https://github.com/so-fancy/diff-so-fancy) Better git diffs
- [`spaceman-diff`](https://github.com/holman/spaceman-diff) Diff images in terminal
- [`icdiff`](https://github.com/jeffkaufman/icdiff) Quick replacement for `diff`
- [`dasht`](https://github.com/sunaku/dasht) Dash docs in terminal
- [`up`](https://github.com/akavel/up) Every piper's dream
- [`newsboat`](https://github.com/newsboat/newsboat) RSS feed in the shell
- [`tig`](https://github.com/jonas/tig) ncurses interface for `git`
- [`pandoc`](http://pandoc.org/index.html) + [`lynx`](http://lynx.browser.org/) View markdown in shell

## Screenshots

| ![simple](https://user-images.githubusercontent.com/14259816/132101085-f7945bae-c37a-40eb-b1d4-7d6c216e3425.png) | ![terminal](https://user-images.githubusercontent.com/14259816/132101090-887420b7-8dc7-4dd1-b509-1814757df7b4.png) | ![rss](https://user-images.githubusercontent.com/14259816/132101092-719da4c2-4651-4cd2-8b13-8d8fa60e60f2.png) |
| :--------------------------------------------------------------------------------------------------------------: | :----------------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------------: |
|                                                  Coding session                                                  |                                                       Shell                                                        |                                                   Rss feeds                                                   |

## More screenshots

<details>
<summary>Screenshots (as of `d4733fb0`)</summary>
<br>

| ![](https://i.imgur.com/EvyYkS9.png) |
| :----------------------------------: |
|             Clean shell              |

| ![](https://i.imgur.com/2ge6Da3.png) |
| :----------------------------------: |
|         Minimal vim session          |

| ![](https://i.imgur.com/iC94NTd.png) |
| :----------------------------------: |
|                Colors                |

| ![](https://i.imgur.com/2GlfOHU.png) |
| :----------------------------------: |
|              Busy tmux               |

| ![](https://i.imgur.com/0EyYmsF.png) |
| :----------------------------------: |
|            Pseudo working            |

| ![](https://i.imgur.com/Jp2sUf0.png) |
| :----------------------------------: |
|                 IRC                  |

| ![](https://i.imgur.com/2FdPs2v.jpg) |
| :----------------------------------: |
|          Kitty icat preview          |

</details>

<details>
<summary>Screenshots (as of `ead4c86c`)</summary>
<br>

### Zsh

![zsh](https://i.imgur.com/0IIq0l3.png)

### Vim

![vim](https://i.imgur.com/hBfeYPe.png)

### IRC

![irc](https://i.imgur.com/UF5fca3.png)

### Colors

![colors](https://i.imgur.com/EB5Chnp.png)

</details>

<details>
<summary>Screenshots (as of `620d4e65`)</summary>
<br>

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

</details>
