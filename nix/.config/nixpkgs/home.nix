# home-manager switch
{ config, pkgs, ... }:

{
  home.stateVersion = "21.05";

  home.username = "meain";
  home.homeDirectory = "/Users/meain";

  programs.home-manager.enable = true;

  home.packages = [
    # core utils
    pkgs.ripgrep
    pkgs.fd
    pkgs.jq
    pkgs.git
    pkgs.fzf
    pkgs.tree

    # packages
    pkgs.notmuch  # mail indexer
    pkgs.isync  # mail syncronize with upstrem
    pkgs.slack-cli  # interact with slack from cli
    pkgs.htop  # process monitor
    pkgs.mpc_cli  # remote for mpd
    pkgs.mpd  # music player
    pkgs.axel  # download manager
    pkgs.bat  # cat with syntax highlight
    pkgs.pandoc  # convert document between different formats
    pkgs.xsv  # view/manage csv from terminal
    pkgs.parallel  # exec things in parallel
    pkgs.fortune  # give me a fortune
    pkgs.surfraw  # search web
    pkgs.lf  # better ranger alternative
    pkgs.diff-so-fancy  # really good diff
    pkgs.icdiff  # simple colorfull diff replacement
    pkgs.ctags  # code tag stuff
    pkgs.pass  # password management
    pkgs.sshfs  # mount vm as fs using ssh
    pkgs.stow  # symlink management
    pkgs.git-absorb  # automatic git commit --fixup
    pkgs.wget  # get stuff from internet
    pkgs.tmux  # terminal multiplexer
    pkgs.aspell  # spell checker
    # pkgs.mpvc  # mpv remote control  # not available on macos

    # apps
    pkgs.kubectl  # kubernetes cli
    pkgs.kubernetes-helm  # helm cli
    pkgs.youtube-dl  # download youtube videos
    pkgs.chafa  # show images in terminal using half blocks
    pkgs.hub  # Github integration for git
    pkgs.lynx  # terminal web browser
    pkgs.w3m  # terminal web browser
    pkgs.imagemagick  # image manip cli
    pkgs.ffmpeg  # video manip cli
    pkgs.gifski  # gif manip cli
    pkgs.httpie  # prettier curl for debugging
    pkgs.gnuplot  # plotting
    pkgs.ddgr  # search ddg from terminal
    pkgs.dasht  # terminal docs
    pkgs.qrencode  # encode data as qr from cli
    pkgs.taskwarrior # task management
    pkgs.todo-txt-cli  # todo management
    pkgs.ts  # task spooler
    pkgs.ncdu  # disk usage viewer tui
    pkgs.pstree  # view process tree
    pkgs.jid  # json incremental digger

    # programming
    pkgs.gist  # create gist
    pkgs.shellcheck  # shell checker
    pkgs.shfmt  # shell code format
    pkgs.html-tidy  # html formatter
    pkgs.hey  # http load generator

    # gui
    pkgs.mpv  # audio/video player

    # others
    pkgs.redis  # key value db
    # pkgs.postgresql  # object db  # need a newer version
    pkgs.mongodb  # document db
    pkgs.sqlite  # better db
    pkgs.minikube  # mini kubernetes
    pkgs.awscli  # manage aws
    pkgs.google-cloud-sdk  # manage google cloud

    # optional
    pkgs.taskell  # kanban board tui
    pkgs.scim  # excel for terminal
    pkgs.jrnl  # journaling
    pkgs.figlet  # make big text
    pkgs.gource  # source tree visualisation
    pkgs.tig  # tui git interface
    pkgs.lazygit  # tui git interface
    pkgs.lazydocker  # tui docker interface
    pkgs.ncmpcpp  # mpd tui client
    pkgs.tokei  # count lines of code
    pkgs.navi  # interactive cli launcher
    pkgs.googler  # search google from terminal
    pkgs.cmatrix  # matrix thingy in shell
    pkgs.graphviz  # draw graphs with code

    # tryout
    pkgs.gforth  # gnu forth interpretter
    pkgs.groff  # gnu troff
  ];
}
