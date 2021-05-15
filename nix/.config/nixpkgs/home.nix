# home-manager switch
{ config, pkgs, ... }:

{
  home.stateVersion = "21.05";

  home.username = "meain";
  home.homeDirectory = "/Users/meain";

  programs.home-manager.enable = true;
  # nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    # core utils
    ripgrep
    fd
    jq
    git
    fzf
    tree
    cmake
    coreutils

    # packages
    notmuch # mail indexer
    isync # mail syncronize with upstrem
    slack-cli # interact with slack from cli
    htop # process monitor
    mpc_cli # remote for mpd
    mpd # music player
    axel # download manager
    bat # cat with syntax highlight
    pandoc # convert document between different formats
    xsv # view/manage csv from terminal
    parallel # exec things in parallel
    fortune # give me a fortune
    surfraw # search web
    lf # better ranger alternative
    diff-so-fancy # really good diff
    icdiff # simple colorfull diff replacement
    ctags # code tag stuff
    pass # password management
    sshfs # mount vm as fs using ssh
    stow # symlink management
    git-absorb # automatic git commit --fixup
    wget # get stuff from internet
    tmux # terminal multiplexer
    aspell # spell checker
    # mpvc  # mpv remote control  # not available on macos
    msmtp # smtp client

    # apps
    kubectl # kubernetes cli
    kubernetes-helm # helm cli
    youtube-dl # download youtube videos
    chafa # show images in terminal using half blocks
    hub # Github integration for git
    lynx # terminal web browser
    w3m # terminal web browser
    imagemagick # image manip cli
    ffmpeg # video manip cli
    gifski # gif manip cli
    httpie # prettier curl for debugging
    gnuplot # plotting
    ddgr # search ddg from terminal
    dasht # terminal docs
    qrencode # encode data as qr from cli
    taskwarrior # task management
    todo-txt-cli # todo management
    ts # task spooler
    ncdu # disk usage viewer tui
    pstree # view process tree
    jid # json incremental digger
    tldr # simpler man pages
    # silicon # create pretty code screenshots  # build error

    # programming
    gist # create gist
    hey # http load generator

    # programming-shell
    shellcheck # shell checker
    shfmt # shell code format
    nodePackages.bash-language-server # bash languageserver

    # programming-go
    go # go programming language
    go-langserver # go languageserve
    goimports # go formatter

    # programming-web
    html-tidy # html formatter
    nodejs # nodejs
    nodePackages.neovim # neovim package for js support
    nodePackages.fixjson # much better json formatter
    nodePackages.stylelint # css linter
    nodePackages.prettier # formatting for web stuff
    nodePackages.pnpm # package management
    nodePackages.typescript # typescript
    nodePackages.vscode-css-languageserver-bin # css languageserver
    nodePackages.javascript-typescript-langserver # javascript langserver

    # programming-nix
    rnix-lsp # nix language server
    nixpkgs-fmt # nix formater

    # programming-python
    python39 # python language
    poetry # better package manager
    black # python code formatter
    python39Packages.flake8 # linter
    python39Packages.ipdb # interactive debugging
    python39Packages.pynvim # neovim python support
    python39Packages.pycodestyle # code style check
    python39Packages.pydocstyle # doc style check
    python39Packages.requests # http lib for quick stuff
    python39Packages.virtualenv # virual envs
    python39Packages.bandit # analyze code for security issues
    python39Packages.mypy # check types in code
    python39Packages.isort # fix sort order
    python39Packages.pygments # generic syntax highliht

    # programming-rust
    cargo-edit # dep management
    cargo-bloat # find big chunks
    cargo-udeps # find unnecessary deps
    cargo-watch # continously run cargo check

    # gui
    mpv # audio/video player
    alacritty # terminal emulator

    # others
    redis # key value db
    # postgresql  # object db  # need a newer version
    mongodb # document db
    sqlite # better db
    minikube # mini kubernetes
    awscli # manage aws
    google-cloud-sdk # manage google cloud

    # optional
    taskell # kanban board tui
    scim # excel for terminal
    jrnl # journaling
    figlet # make big text
    gource # source tree visualisation
    tig # tui git interface
    lazygit # tui git interface
    lazydocker # tui docker interface
    ncmpcpp # mpd tui client
    tokei # count lines of code
    navi # interactive cli launcher
    googler # search google from terminal
    cmatrix # matrix thingy in shell
    graphviz # draw graphs with code

    # tryout
    gforth # gnu forth interpretter
    groff # gnu troff
  ];
}
