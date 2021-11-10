<h1 align="center">Dotfiles</h1>
<p align="center"> If there is a shell, there is a way</p>
<p align="center">
  <img src="https://img.shields.io/badge/OS-NixOS-violet.svg" />
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

## Tools I use a lot

| ...                                                                                   | ...                                                                                     |
| ------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| [fzf](https://github.com/junegunn/fzf) Fuzzy find anything                            | [ripgrep](https://blog.burntsushi.net/ripgrep) Insanely fast grepping                   |
| [z](https://github.com/rupa/z) Quickly jump around common directories                 | [jq](https://github.com/stedolan/jq) Query into json objects                            |
| [jiq](https://github.com/fiatjaf/jiq) `jq` but interactive                            | [isync](https://isync.sourceforge.io/) Mail sync                                        |
| [mpd](https://musicpd.org/) Music player daemon                                       | [parallel](https://www.gnu.org/software/parallel/) Prarllel xargs                       |
| [git-absorb](https://github.com/tummychow/git-absorb) Automatic --fixup               | [restic](https://restic.net/) Sane backup solution                                      |
| [syncthing](https://syncthing.net/) Sane multi device sync tool                       | [KDE Connect](https://kdeconnect.kde.org) Connect your phone and PC                     |
| [axel](https://github.com/axel-download-accelerator/axel) Download accelerator        | [imagemagick](https://imagemagick.org) Image manipulation via cli                       |
| [ffmpeg](https://ffmpeg.org/) Video editing via cli                                   | [todo-txt-cli](https://github.com/todotxt/todo.txt-cli) Simple todo management          |
| [lsd](https://github.com/Peltoche/lsd) `ls` but much more                             | [icdiff](https://github.com/jeffkaufman/icdiff) Colorful side by side diff              |
| [diff-so-fancy](https://github.com/so-fancy/diff-so-fancy) Better git diff            | [dasht](https://dasht.io/) Offline documentation browser                                |
| [pandoc](https://pandoc.org/) Convert from and to multiple formats                    | [hub](https://hub.github.com/) Github extension for git                                 |
| [lf](https://github.com/gokcehan/lf) Better version of ranger                         | [alacritty](https://github.com/alacritty/alacritty) Fast simple terminal emulator       |
| [ddgr](https://github.com/jarun/ddgr) Duckduckgo search from cli                      | [chafa](https://github.com/hpjansson/chafa) Image viewer in terminal                    |
| [tmux](https://github.com/tmux/tmux) Terminal multiplexer                             | [hammerspoon](https://www.hammerspoon.org/) Control mac using lua                       |
| [pixel-picker](https://github.com/acheronfail/pixel-picker/) Color picker for mac     | [Karabiner Elements](https://github.com/pqrs-org/Karabiner-Elements) Keyboard remapping |
| [activity-watch](https://activitywatch.net) Track your computer usage                 | [Next DNS](https://nextdns.io/) A really good dns thingy                                |
| [nix](https://nixos.org/) A sane package manager                                      | [fd](https://github.com/sharkdp/fd) Faster find                                         |
| [notmuch](https://notmuchmail.org/) Simple mail indexer                               | [maccy](https://maccy.app/) Macos clipboard manager                                     |
| [meetingbar](https://github.com/leits/MeetingBar) View next meeting in your statusbar | [insomnia](https://github.com/Kong/insomnia) Simpler postman                            |
| [ledger](https://www.ledger-cli.org/) Plaintext double entry accounting               | [pup](https://github.com/ericchiang/pup) HTML parser for cli                            |
| [entr](https://github.com/clibs/entr) Run commands on file change                     | [mpv](https://mpv.io/) Simple but really powerful media player                          |

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
