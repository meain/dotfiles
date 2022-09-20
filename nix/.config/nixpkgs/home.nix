{ personal, stable, ... }:
{ pkgs, ... }:
let
  ppkgs = personal.packages.x86_64-linux;
  spkgs = stable.legacyPackages.x86_64-linux;
  utils = import ./utils.nix { inherit pkgs; };
  fonts = import ./fonts.nix { inherit pkgs; inherit spkgs;};
in
{
  home. stateVersion = "21.05";
  home. username = "meain";
  home. homeDirectory = "/home/meain";

  programs.home-manager.enable = true;

  services.emacs.package = pkgs.emacsGit;
  programs.emacs = {
    package = pkgs.emacsGit;
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  services.syncthing.enable = true;

  fonts.fontconfig.enable = true;
  home.packages = [
    # core utilities
    pkgs.coreutils
    pkgs.gnumake
    pkgs.cmake
    pkgs.curl
    # pkgs.zsh
    pkgs.gcc
    pkgs.gitFull # full for send-email
    pkgs.fzf
    pkgs.ripgrep
    pkgs.jq
    pkgs.fd

    # packages
    # ppkgs.notmuch-git # mail indexer
    pkgs.tree
    pkgs.neovim
    pkgs.notmuch
    pkgs.isync # mail synchronize with upstream
    pkgs.htop # process monitor
    pkgs.mpc_cli # remote for mpd
    pkgs.mpd # music player
    pkgs.pandoc # convert document between different formats
    pkgs.xsv # view/manage csv from terminal
    pkgs.parallel # exec things in parallel
    # pkgs.fortune # give me a fortune
    pkgs.lf # better ranger alternative
    pkgs.diff-so-fancy # really good diff
    pkgs.icdiff # simple colorful diff replacement
    pkgs.ctags # code tag stuff
    pkgs.pass # password management
    # pkgs.sshfs # mount vm as fs using ssh
    pkgs.stow # symlink management
    pkgs.git-absorb # automatic git commit --fixup
    pkgs.git-crypt # encrypt git stuff
    pkgs.wget # get stuff from internet
    pkgs.tmux # terminal multiplexer
    pkgs.aspell # spell checker
    pkgs.mpvc # mpv remote control  # not available on macos
    pkgs.msmtp # smtp client
    pkgs.android-tools # adb and friends  # not available for macos
    # pkgs.restic # backup
    # pkgs.ledger # double entry accounting
    # pkgs.dasel # jq but more versatile
    # pkgs.mosh # better ssh
    # pkgs.haskellPackages.kmonad # key remapping
    ppkgs.kmonad # key remapping
    # ppkgs.warpd # mouse control
    pkgs.trash-cli # rm -> trash
    pkgs.entr # continuously run stuff
    pkgs.notify-desktop # desktop notifications
    pkgs.xclip # clipboard (needed for emacs-everywhere)
    pkgs.xsel # clipboard
    # pkgs.wl-clipboard
    # pkgs.bandwhich # view network stats (alt: nethogs)
    pkgs.xdotool # for window switching
    pkgs.picotts # for say
    ppkgs.dmenu # menu stuff (fork for emojis)
    ppkgs.spaceman-diff # diff images in terminal
    pkgs.polybarFull # bar for wm

    # aspell dicts
    pkgs.aspellDicts.en

    # apps
    pkgs.kubectl # kubernetes cli
    pkgs.kubernetes-helm # helm cli
    pkgs.stern # better way to fetch kubernetes log
    pkgs.yt-dlp # download youtube videos
    pkgs.chafa # show images in terminal using half blocks
    # pkgs.hub # Github integration for git
    pkgs.gh # Yet another Github integration for git
    pkgs.lynx # terminal web browser
    spkgs.slop # select region from screen
    pkgs.imagemagick # image manip cli
    pkgs.ffmpeg # video manip cli
    pkgs.gnuplot # plotting
    pkgs.ddgr # search ddg from terminal
    pkgs.dasht # terminal docs
    # pkgs.taskwarrior # task management
    pkgs.todo-txt-cli # todo management
    # pkgs.ts # task spooler
    pkgs.pup # html filtering
    # pkgs.tldr # simpler man pages
    # pkgs.silicon # create pretty code screenshots
    pkgs.pkgs.transmission # torrent stuff
    # pkgs.kube-prompt # interactive kubernetes cli
    # pkgs.cowsay # useless stuff
    # pkgs.podman # pod manager
    pkgs.typos # typo checker (integrated with flymake)
    pkgs.k9s # kubernetes tui
    # pkgs.screenkey # show keys (useful when recording screen)

    # programming
    # pkgs.gist # create gist
    # pkgs.hey # http load generator

    # programming-shell
    pkgs.shellcheck # shell checker
    pkgs.shfmt # shell code format
    pkgs.nodePackages.bash-language-server # bash language-server

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
    # pkgs.delve # debugging in go

    # programming-web
    pkgs.html-tidy # html formatter
    pkgs.nodejs # nodejs
    # pkgs.nodePackages.neovim # neovim package for js support
    pkgs.nodePackages.stylelint # css linter
    pkgs.nodePackages.prettier # formatting for web stuff
    # pkgs.nodePackages.pnpm # package management
    pkgs.nodePackages.typescript # typescript
    pkgs.nodePackages.vscode-css-languageserver-bin # css languageserver
    pkgs.nodePackages.typescript-language-server # javascript langserver

    # programming-nix
    pkgs.rnix-lsp # nix language server
    pkgs.nixpkgs-fmt # nix formatter
    pkgs.statix # linter for nix

    # programming-python
    pkgs.python39 # python language
    pkgs.poetry # better package manager
    pkgs.black # python code formatter
    pkgs.python39Packages.pip
    pkgs.python39Packages.flake8 # linter
    pkgs.python39Packages.ipdb # interactive debugging
    # pkgs.python39Packages.pynvim # neovim python support
    pkgs.python39Packages.pycodestyle # code style check
    pkgs.python39Packages.pydocstyle # doc style check
    # pkgs.python39Packages.requests # http lib for quick stuff
    pkgs.python39Packages.virtualenv # virtual envs
    # pkgs.python39Packages.bandit # analyze code for security issues
    # pkgs.python39Packages.mypy # check types in code
    # pkgs.python39Packages.isort # fix sort order
    # pkgs.python39Packages.pygments # generic syntax highlight
    pkgs.python39Packages.python-lsp-server # python lsp
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
    pkgs.cargo-edit # dep management
    # pkgs.cargo-bloat # find big chunks
    # pkgs.cargo-udeps # find unnecessary deps
    pkgs.cargo-release # for releasing packages
    pkgs.cargo-watch # continuously run cargo check

    # programming-lua
    # pkgs.nodePackages.lua-fmt

    # programming-haskell
    # pkgs.ghc # haskell compiler
    # pkgs.haskellPackages.brittany # haskell code formatter

    # programming-other
    pkgs.nodePackages.yaml-language-server # language server for yaml
    pkgs.nodePackages.vscode-json-languageserver # language server for json
    ppkgs.prosemd-lsp # prose lsp
    pkgs.nodePackages.fixjson # much better json formatter
    pkgs.nodePackages.jsonlint # json linting
    pkgs.nodePackages.markdownlint-cli # markdown linter
    pkgs.vale # prose lint
    pkgs.hadolint # Dockerfile lint
    pkgs.sqls # lsp server for sql
    pkgs.python39Packages.sqlparse # sqlformat
    # pkgs.grpcurl # curl for grpc

    # gui
    pkgs.mpv # audio/video player
    # pkgs.kitty
    # pkgs.alacritty # terminal emulator
    # pkgs.firefox # working OSS browser
    pkgs.chromium # because Google hates firefox
    # pkgs.guake # drop down terminal
    # pkgs.insomnia # simpler postman
    # pkgs.beekeeper-studio # db viewer
    pkgs.sakura # x11 terminal emulator
    # pkgs.foot # wayland terminal emulator
    pkgs.zathura # pdf viewer
    pkgs.sxiv # image viewer

    # others
    # pkgs.redis # key value db
    pkgs.postgresql_13 # postgres 13 (postgresql is at 11)
    # pkgs.mongodb # document db
    pkgs.sqlite # better db (needed by magit-forge)
    # pkgs.minikube # mini kubernetes
    pkgs.kind # better minikube
    # pkgs.awscli # manage aws
    # pkgs.google-cloud-sdk # manage google cloud
    # pkgs.lens # kubernetes viewer

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
    # pkgs.tig # tui git interface
    # pkgs.lazygit # tui git interface
    # pkgs.lazydocker # tui docker interface
    # pkgs.docker-compose # docker-compose
    # pkgs.ncmpcpp # mpd tui client
    # pkgs.tokei # count lines of code
    # pkgs.navi # interactive cli launcher
    # pkgs.googler # search google from terminal
    # pkgs.cmatrix # matrix thingy in shell
    # pkgs.graphviz # draw graphs with code
    pkgs.groff # gnu troff  ## needed for md->pdf conversion
    pkgs.pgcli # fancier postgres cli
    # pkgs.trivy # docker vulnerability scanner
    # pkgs.act # github ci locally
    # pkgs.pkgs.pstree # view process tree
    ppkgs.traffic # simple network stats
    # pkgs.comma # run literally anything
    ppkgs.gloc # run stuff in all git repos
    ppkgs.tojson # convert yaml/toml/json
    # pkgs.jo # create json
    pkgs.jiq # interactive jq
    pkgs.jless # json viewer
    pkgs.blueman # bluetooth control
    pkgs.arandr # screen layout configure
    pkgs.clipmenu # clipboard history
    pkgs.brightnessctl # brightness control
    pkgs.dunst # notifications with buttons (dunstify)
    pkgs.xfce.thunar # gui file manager
    pkgs.unixtools.netstat # netstat
    # pkgs.comby # structural search/editing of code
    # pkgs.visidata # data visualization
    pkgs.xdragon # drag and drop files
    pkgs.sct # redshift ish stuff
    # pkgs.nur.repos.renesat.activitywatch-bin  # https://github.com/NixOS/nix/issues/3843
    ppkgs.activitywatch-bin # activity tracking
    # pkgs.gforth # gnu forth interpreter
    pkgs.nodePackages.mermaid-cli # cli for generating mermaid charts
    pkgs.genact # become a movie "hacker"

    # gnome tweaking
    # pkgs.gnome3.dconf-editor # change dconf settings
    # pkgs.gnome.gnome-tweaks # tweak gnome settings
    # ppkgs.fluent-theme # a good simple theme
    # pkgs.gnomeExtensions.dash-to-panel # move dash and make it a panel
    # pkgs.gnomeExtensions.blur-my-shell # use blurred wallpaper for overview
    # pkgs.gnomeExtensions.clipboard-indicator # clipboard history
    # ppkgs.gnomeExtensions.gsconnect # kdeconnect
    # pkgs.gnomeExtensions.caffeine # don't sleep
    # pkgs.gnomeExtensions.no-overview # no overview on start
    # pkgs.gnomeExtensions.bluetooth-quick-connect # better bluetooth menu
    # pkgs.gnomeExtensions.custom-hot-corners-extended # smokin' hot corners
    # ppkgs.gnomeExtensions.steal-my-focus # just switch, don't say
    # ppkgs.gnomeExtensions.shellout # custom info in bar

    # symlinks (macos polyfills)
    (pkgs.runCommand "open" { } ''mkdir -p $out/bin; ln -s ${pkgs.xdg-utils}/bin/xdg-open $out/bin/open'')
    # (pkgs.runCommand "pbcopy" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-copy $out/bin/pbcopy'')
    # (pkgs.runCommand "pbpaste" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-paste $out/bin/pbpaste'')
    # (pkgs.runCommand "say" { } ''mkdir -p $out/bin; ln -s ${pkgs.espeak}/bin/espeak $out/bin/say'')

    # autostart
    # (pkgs.makeAutostartItem { name = "guake"; package = pkgs.guake; })
    # (pkgs.makeAutostartItem { name = "albert"; package = pkgs.albert; })
  ] ++ fonts;

  dconf.settings = import ./dconf.nix;

  systemd.user.services.activitywatch = {
    Unit.Description = "Start ActivityWatch";
    Service.Type = "simple";
    Service.ExecStart = "${ppkgs.activitywatch-bin}/bin/aw-server";
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "on-failure";
    Service.RestartSec = 5;
  };
  systemd.user.services.activitywatch-afk = {
    Unit.Description = "Start ActivityWatch AFK";
    Service.Type = "simple";
    Service.ExecStart = "${ppkgs.activitywatch-bin}/bin/aw-watcher-afk";
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "on-failure";
    Service.RestartSec = 5;
  };
  systemd.user.services.activitywatch-window = {
    Unit.Description = "Start ActivityWatch Window";
    Service.Type = "simple";
    Service.ExecStart = "${ppkgs.activitywatch-bin}/bin/aw-watcher-window";
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "on-failure";
    Service.RestartSec = 5;
  };
  systemd.user.services.activitywatch-mpd = {
    Unit.Description = "Start ActivityWatch mpd";
    Service.Type = "simple";
    Service.ExecStart = "${ppkgs.aw-watcher-mpd}/bin/aw-watcher-mpd";
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "on-failure";
    Service.RestartSec = 5;
  };
  systemd.user.services.activitywatch-input = {
    Unit.Description = "Start ActivityWatch input";
    Service.Type = "simple";
    Service.ExecStart = "${ppkgs.aw-watcher-input}/bin/aw-watcher-input";
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "on-failure";
    Service.RestartSec = 5;
  };


  # systemd.user.startServices = true;  # enabling this increases switch time a lot
  systemd.user.services.mpd = utils.ss-simple { cmd = "mpd --no-daemon"; wait = 3; };
  systemd.user.services.clipmenud = utils.ss-simple { cmd = "clipmenud"; wait = 3; };
  systemd.user.services.sxhkd = utils.ss-simple { cmd = "sxhkd"; wait = 3; };
  systemd.user.services.wo-info = utils.ss-simple { cmd = "WO_WRITE=1 ,wo-info"; wait = 5; };
  systemd.user.services.emacs = utils.ss-simple { cmd = "emacs --fg-daemon"; wait = 1; };
  systemd.user.services.emacsclient = utils.ss-simple { cmd = "emacsclient -F \\'((title . \"floatingemacs\"))\\' -c"; wait = 1; };
  systemd.user.services.floatingterm = utils.ss-simple { cmd = "sakura --name floatingterm -x \"tmux new -As floating\""; wait = 1; };
  systemd.user.services.mail-watcher = utils.ss-simple { cmd = "find /home/meain/.local/share/mail/.notmuch/xapian|entr -n ,shellout-update"; wait = 5; };

  # code/note sync
  systemd.user.services.note-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/notes"; };
  systemd.user.timers.note-sync = utils.timer-daily;
  systemd.user.services.til-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/til"; };
  systemd.user.timers.til-sync = utils.timer-daily;
  systemd.user.services.work-notes-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/work-notes"; };
  systemd.user.timers.work-notes-sync = utils.timer-daily;
  systemd.user.services.ledger-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/ledger"; };
  systemd.user.timers.ledger-sync = utils.timer-daily;
  systemd.user.services.journal-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/journal"; };
  systemd.user.timers.journal-sync = utils.timer-daily;

  # syncing things
  systemd.user.services.email-sync = utils.ss-timer { cmd = ",mail-sync"; };
  systemd.user.timers.email-sync = utils.timer-min { min = "15"; };
  systemd.user.services.weather-pull = utils.ss-timer { cmd = ",weather-current"; };
  systemd.user.timers.weather-pull = utils.timer-min { min = "30"; };
  systemd.user.services.battery-check = utils.ss-timer { cmd = ",low-battery-notify"; };
  systemd.user.timers.battery-check = utils.timer-min { min = "5"; };
  systemd.user.services.update-sct = utils.ss-timer { cmd = ",update-sct"; };
  systemd.user.timers.update-sct = utils.timer-min { min = "30"; };
  systemd.user.services.update-calendar = utils.ss-timer { cmd = ",upcoming-events"; };
  systemd.user.timers.update-calendar = utils.timer-min { min = "10"; }; # actual pull is hourly
  systemd.user.services.wo-notify = utils.ss-timer { cmd = ",output-notify \"Working on...\" ,wo-info"; };
  systemd.user.timers.wo-notify = utils.timer-min { min = "15"; };
  systemd.user.services.mscripts-backup = utils.ss-timer { cmd = ",projects-config-backup"; };
  systemd.user.timers.mscripts-backup = utils.timer-daily;

  # regular cleanup
  systemd.user.services.cleanup-downloads = utils.ss-cleanup { dir = "/home/meain/Downloads"; };
  systemd.user.timers.cleanup-downloads = utils.timer-daily;
  systemd.user.services.cleanup-scratch = utils.ss-cleanup { dir = "/home/meain/.local/share/scratch"; };
  systemd.user.timers.cleanup-scratch = utils.timer-daily;

  # Setup direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
}
