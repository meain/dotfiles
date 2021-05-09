# home-manager switch
{ config, pkgs, ... }:

{
  home.stateVersion = "21.05";

  home.username = "meain";
  home.homeDirectory = "/Users/meain";

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    # core utils
    ripgrep
    fd
    jq
    git
    fzf
    tree

    # packages
    notmuch  # mail indexer
    isync  # mail syncronize with upstrem
    slack-cli  # interact with slack from cli
    htop  # process monitor
    mpc_cli  # remote for mpd
    mpd  # music player
    axel  # download manager
    bat  # cat with syntax highlight
    pandoc  # convert document between different formats
    xsv  # view/manage csv from terminal
    parallel  # exec things in parallel
    fortune  # give me a fortune
    surfraw  # search web
    lf  # better ranger alternative
    diff-so-fancy  # really good diff
    icdiff  # simple colorfull diff replacement
    ctags  # code tag stuff
    pass  # password management
    sshfs  # mount vm as fs using ssh
    stow  # symlink management
    git-absorb  # automatic git commit --fixup
    wget  # get stuff from internet
    tmux  # terminal multiplexer
    aspell  # spell checker
    # mpvc  # mpv remote control  # not available on macos

    # apps
    kubectl  # kubernetes cli
    kubernetes-helm  # helm cli
    youtube-dl  # download youtube videos
    chafa  # show images in terminal using half blocks
    hub  # Github integration for git
    lynx  # terminal web browser
    w3m  # terminal web browser
    imagemagick  # image manip cli
    ffmpeg  # video manip cli
    gifski  # gif manip cli
    httpie  # prettier curl for debugging
    gnuplot  # plotting
    ddgr  # search ddg from terminal
    dasht  # terminal docs
    qrencode  # encode data as qr from cli
    taskwarrior # task management
    todo-txt-cli  # todo management
    ts  # task spooler
    ncdu  # disk usage viewer tui
    pstree  # view process tree
    jid  # json incremental digger

    # programming
    gist  # create gist
    shellcheck  # shell checker
    shfmt  # shell code format
    html-tidy  # html formatter
    hey  # http load generator
    rnix-lsp  # nix language server

    # gui
    mpv  # audio/video player
    alacritty  # terminal emulator

    # others
    redis  # key value db
    # postgresql  # object db  # need a newer version
    mongodb  # document db
    sqlite  # better db
    minikube  # mini kubernetes
    awscli  # manage aws
    google-cloud-sdk  # manage google cloud

    # optional
    taskell  # kanban board tui
    scim  # excel for terminal
    jrnl  # journaling
    figlet  # make big text
    gource  # source tree visualisation
    tig  # tui git interface
    lazygit  # tui git interface
    lazydocker  # tui docker interface
    ncmpcpp  # mpd tui client
    tokei  # count lines of code
    navi  # interactive cli launcher
    googler  # search google from terminal
    cmatrix  # matrix thingy in shell
    graphviz  # draw graphs with code

    # tryout
    gforth  # gnu forth interpretter
    groff  # gnu troff
  ];
}
