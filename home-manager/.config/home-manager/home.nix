{ system, personal, bleeding, stable, tree-grepper, firefox-addons, ... }:
{ pkgs, ... }:
let
  ppkgs = personal.packages.${system};
  bpkgs = bleeding.legacyPackages.${system};
  spkgs = stable.legacyPackages.${system};
  utils = import ./utils.nix { inherit pkgs; };
  fonts = import ./fonts.nix { inherit pkgs spkgs; };
  rbm = import ./repo_bookmarks.nix { inherit utils; };
  linuxpkgs = import ./linuxpkgs.nix { inherit pkgs spkgs; };
  firefox = import ./firefox.nix { inherit pkgs ppkgs firefox-addons defaultFont rbm; };
  defaultFont = "Victor Mono";
in
{
  home = {
    stateVersion = "21.05";
    username = "meain";
    # homeDirectory = "/home/meain";
    homeDirectory = "/Users/meain";
  };

  # NOTE: On macOS: Have to manually launch it via finder and not hammerspoon
  # programs.firefox = firefox;
  programs.home-manager.enable = true;

  # # uncomment to use emacs-git
  # # services.emacs.package = pkgs.emacs-git;
  # programs.emacs = {
  #   # package = pkgs.emacs-git;
  #   enable = true;
  #   extraPackages = epkgs: [ epkgs.vterm ];
  # };

  services.syncthing.enable = true;

  fonts.fontconfig.enable = true;
  home.packages = [
    # core utilities
    pkgs.coreutils
    # pkgs.coreutils-prefixed # macOS (for gdircolors)
    pkgs.gnumake
    pkgs.cmake
    pkgs.curl
    # pkgs.zsh
    pkgs.gcc
    pkgs.gitFull # full for send-email
    pkgs.fzf
    pkgs.ripgrep
    pkgs.jq
    pkgs.lsd
    pkgs.fd
    pkgs.lsd
    pkgs.gawk # for tmux-fingers

    # packages
    # ppkgs.notmuch-git # mail indexer
    # pkgs.tree
    pkgs.neovim
    pkgs.notmuch
    pkgs.isync # mail synchronize with upstream
    pkgs.htop # process monitor
    pkgs.mpc_cli # remote for mpd
    spkgs.mpd # music player
    pkgs.pandoc # convert document between different formats
    pkgs.xsv # view/manage csv from terminal
    pkgs.parallel # exec things in parallel
    # pkgs.fortune # give me a fortune
    pkgs.lf # better ranger alternative
    pkgs.diff-so-fancy # really good diff
    pkgs.icdiff # simple colorful diff replacement
    pkgs.difftastic # syntax aware diff (useful for conflicts)
    pkgs.ctags # code tag stuff
    pkgs.pass # password management
    pkgs.gnupg # gpg
    # pkgs.sshfs # mount vm as fs using ssh
    pkgs.stow # symlink management
    pkgs.git-absorb # automatic git commit --fixup
    pkgs.git-crypt # encrypt git stuff
    pkgs.wget # get stuff from internet
    pkgs.tmux # terminal multiplexer
    pkgs.aspell # spell checker
    spkgs.msmtp # smtp client
    pkgs.android-tools # adb and friends  # not available for macos
    # pkgs.restic # backup
    # pkgs.ledger # double entry accounting
    # pkgs.dasel # jq but more versatile
    # pkgs.mosh # better ssh
    # pkgs.haskellPackages.kmonad # key remapping
    # ppkgs.kmonad # key remapping
    # ppkgs.warpd # mouse control
    pkgs.trash-cli # rm -> trash
    pkgs.entr # continuously run stuff
    # pkgs.bandwhich # view network stats (alt: nethogs)
    # pkgs.picotts # for say
    # ppkgs.spaceman-diff # diff images in terminal
    # pkgs.python39Packages.pipx # pipx for installing stuff
    # ppkgs.logseq-doctor # logseq utils
    pkgs.emacs-lsp-booster # lsp json translation proxy

    # aspell dicts
    pkgs.aspellDicts.en

    # apps
    # pkgs.kubectl # kubernetes cli
    # pkgs.kubecolor # colorful kubectl
    # pkgs.kubernetes-helm # helm cli
    # pkgs.stern # better way to fetch kubernetes log
    spkgs.yt-dlp # download youtube videos
    pkgs.chafa # show images in terminal using half blocks
    # pkgs.hub # Github integration for git
    pkgs.gh # Yet another Github integration for git
    pkgs.lynx # terminal web browser
    pkgs.imagemagick # image manip cli
    pkgs.ffmpeg # video manip cli
    pkgs.gnuplot # plotting
    pkgs.ddgr # search ddg from terminal
    # pkgs.dasht # terminal docs
    # pkgs.taskwarrior # task management
    pkgs.todo-txt-cli # todo management
    # pkgs.ts # task spooler
    pkgs.pup # html filtering
    # pkgs.tldr # simpler man pages
    # pkgs.silicon # create pretty code screenshots
    # pkgs.pkgs.transmission_4 # torrent stuff
    # pkgs.kube-prompt # interactive kubernetes cli
    # pkgs.cowsay # useless stuff
    # pkgs.podman # pod manager
    pkgs.typos # typo checker (integrated with flymake)
    # pkgs.k9s # kubernetes tui
    # pkgs.screenkey # show keys (useful when recording screen)
    pkgs.lnav # log viewer

    # programming
    # pkgs.gist # create gist
    # pkgs.hey # http load generator

    # programming-shell
    pkgs.shellcheck # shell checker
    pkgs.shfmt # shell code format
    pkgs.bash-language-server # bash language-server

    # programming-go
    pkgs.go # go programming language
    pkgs.gopls # language server
    pkgs.godef # language server helper
    pkgs.gotools # go formatter
    pkgs.go-tools # installing staticcheck (technically available in golangci-lint, but for use in lsp)
    spkgs.errcheck # available in golangci-lint, but still
    pkgs.golangci-lint # all kinds of linters for go
    pkgs.gomodifytags # modify struct tags
    pkgs.impl # generate implementations for a struct
    # pkgs.golines # long line format golang
    # pkgs.gofumpt # better go formatting
    # pkgs.delve # debugging in go
    # pkgs.rr # record replay debugging
    # pkgs.gotests # test generation for golang

    # programming-web
    pkgs.deno # for running deno binaries (eg: silverbullet)
    pkgs.html-tidy # html formatter
    pkgs.nodejs # nodejs
    # pkgs.nodePackages.neovim # neovim package for js support
    pkgs.nodePackages.stylelint # css linter
    pkgs.nodePackages.prettier # formatting for web stuff
    # pkgs.nodePackages.pnpm # package management
    pkgs.nodePackages.typescript # typescript
    pkgs.vscode-langservers-extracted # css languageserver
    pkgs.nodePackages.typescript-language-server # javascript langserver

    # programming-nix
    # pkgs.nixfmt # nix formatter
    pkgs.nixpkgs-fmt # nix formatter
    pkgs.statix # linter for nix
    pkgs.nix-init # auto create nix expressions
    pkgs.nix-update # update package in nix

    # programming-python
    spkgs.python39 # python language
    pkgs.poetry # better package manager
    spkgs.black # python code formatter
    spkgs.python39Packages.pip
    spkgs.python39Packages.flake8 # linter
    # spkgs.python39Packages.ipdb # interactive debugging
    # pkgs.python39Packages.pynvim # neovim python support
    # spkgs.python39Packages.pycodestyle # code style check
    # spkgs.python39Packages.pydocstyle # doc style check
    # pkgs.python39Packages.requests # http lib for quick stuff
    spkgs.python39Packages.virtualenv # virtual envs
    # pkgs.python39Packages.bandit # analyze code for security issues
    # pkgs.python39Packages.mypy # check types in code
    # pkgs.python39Packages.isort # fix sort order
    # pkgs.python39Packages.pygments # generic syntax highlight
    # spkgs.python39Packages.python-lsp-server # python lsp
    # pkgs.python38Packages.python-language-server # python lsp (using below one as tests are failing)
    # (pkgs.python38Packages.python-language-server.overridePythonAttrs (oldAttrs: { checkPhase = ""; checkInputs = []; }))

    # programming-rust
    # pkgs.rustup # rust toolchain
    # pkgs.cargo-cross # cross platform rust devel
    pkgs.rustc # compiler
    pkgs.cargo # package manager
    pkgs.rustfmt # formatter
    pkgs.clippy # the useful clippy
    pkgs.rust-analyzer # lsp for rust
    # pkgs.cargo-edit # dep management
    # pkgs.cargo-bloat # find big chunks
    # pkgs.cargo-udeps # find unnecessary deps
    # pkgs.cargo-release # for releasing packages
    # pkgs.cargo-watch # continuously run cargo check

    # programming-lua
    # pkgs.nodePackages.lua-fmt

    # programming-haskell
    # pkgs.ghc # haskell compiler
    # pkgs.haskellPackages.brittany # haskell code formatter

    # programming-other
    pkgs.nodePackages.yaml-language-server # language server for yaml
    pkgs.nodePackages.vscode-json-languageserver # language server for json
    # ppkgs.prosemd-lsp # prose lsp
    pkgs.nodePackages.fixjson # much better json formatter
    pkgs.nodePackages.jsonlint # json linting
    pkgs.nodePackages.markdownlint-cli # markdown linter
    pkgs.vale # prose lint
    # pkgs.hadolint # Dockerfile lint
    # pkgs.sqls # lsp server for sql
    # pkgs.sqlfluff # sql linter
    # pkgs.python39Packages.sqlparse # sqlformat
    # pkgs.grpcurl # curl for grpc
    pkgs.zprint # clojure formatter
    tree-grepper.outputs.packages.${system}.tree-grepper # grep with tree-sitter
    pkgs.comby # code mod
    # pkgs.ruby # ruby language
    # pkgs.actionlint # linting for gihtub actions

    # gui
    spkgs.mpv # audio/video player
    # pkgs.kitty
    # pkgs.alacritty # terminal emulator
    # pkgs.firefox # browser
    # pkgs.chromium # because Google hates firefox
    # pkgs.nyxt # browser in common-lisp
    # pkgs.guake # drop down terminal
    # pkgs.insomnia # simpler postman
    # pkgs.beekeeper-studio # db viewer
    pkgs.zathura # pdf viewer
    # pkgs.sxiv # image viewer
    # pkgs.vscode-fhs # vscode

    # others
    # pkgs.redis # key value db
    # pkgs.postgresql_13 # postgres 13 (postgresql is at 11)
    # pkgs.mongodb # document db
    pkgs.sqlite # better db (needed by magit-forge)
    # pkgs.minikube # mini kubernetes
    # pkgs.kind # better minikube
    # pkgs.awscli # manage aws
    # pkgs.google-cloud-sdk # manage google cloud
    # pkgs.lens # kubernetes viewer
    pkgs.qpdf # for zlib-flate
    pkgs.emacs-lsp-booster # helps translate json to emacs objects
    pkgs.cachix # nix external cache server

    # optional
    # pkgs.gdu # disk usage viewer tui (alt: ncdu)
    pkgs.axel # download manager
    # pkgs.bat # cat with syntax highlight
    # pkgs.surfraw # search web
    # pkgs.httpie # prettier curl for debugging
    # pkgs.qrencode # encode data as qr from cli
    # pkgs.w3m # terminal web browser
    # pkgs.scim # excel for terminal
    # pkgs.jrnl # journaling
    # pkgs.figlet # make big text
    # pkgs.gource # source tree visualisation
    pkgs.tig # tui git interface
    pkgs.lazygit # tui git interface
    # pkgs.lazydocker # tui docker interface
    # pkgs.docker-compose # docker-compose
    # pkgs.ncmpcpp # mpd tui client
    # pkgs.tokei # count lines of code
    # pkgs.navi # interactive cli launcher
    # pkgs.googler # search google from terminal
    # pkgs.cmatrix # matrix thingy in shell
    # pkgs.graphviz # draw graphs with code
    # pkgs.groff # gnu troff  ## needed for md->pdf conversion
    # pkgs.pgcli # fancier postgres cli
    # pkgs.trivy # docker vulnerability scanner
    # pkgs.act # github ci locally
    pkgs.pstree # view process tree
    ppkgs.traffic # simple network stats
    # pkgs.comma # run literally anything
    ppkgs.gloc # run stuff in all git repos
    ppkgs.tojson # convert yaml/toml/json
    # pkgs.jo # create json
    pkgs.jiq # interactive jq
    pkgs.jless # json viewer
    pkgs.unixtools.netstat # netstat
    # pkgs.comby # structural search/editing of code
    # pkgs.visidata # data visualization
    # pkgs.nur.repos.renesat.activitywatch-bin  # https://github.com/NixOS/nix/issues/3843
    # pkgs.activitywatch # activity tracking
    # pkgs.gforth # gnu forth interpreter
    pkgs.nodePackages.mermaid-cli # cli for generating mermaid charts
    # pkgs.genact # become a movie "hacker"
    # bpkgs.logseq # tracking life (using flatpak version)
    # pkgs.silverbullet # another simpler PKM (installed from GH)
    # pkgs.obs-studio # video/screen recording
    # pkgs.postman # REST api testing tool
    # pkgs.minicom # connect to rpi
    # pkgs.mitmproxy # mitm proxy
    # pkgs.spotifyd # spotify daemon
    # pkgs.spotify-tui # control spotify
    pkgs.cloc # line count code
    pkgs.helix # alternate editor
    # pkgs.iamb # matrix chat client
    ppkgs.chatgpt-cli # chatgpt cli
    pkgs.mods # another llm cli
    pkgs.llm # package to access multiple llms
    pkgs.shell-gpt # another chatgpt cli
    pkgs.feh # image viewer (for desktop background)
    pkgs.kopia # backup
    pkgs.piper-tts # text to speech
    pkgs.readability-cli # simplify articles
    pkgs.glow # markdown renderer
    pkgs.markdown-oxide # PKM via LSP
    pkgs.poppler_utils # pdf utils
    pkgs.nur.repos.rycee.mozilla-addons-to-nix # package firefox addons
  ]
  # ++ linuxpkgs
  ++ fonts;

  # dconf.settings = import ./dconf.nix;
  # gtk = import ./gtk.nix;
  # systemd.user = import ./systemd.nix;

  # Setup direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
}
