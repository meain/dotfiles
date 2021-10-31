{ lib, config, pkgs, ... }:
let
  personal = import (builtins.fetchTarball "https://github.com/meain/nix-channel/archive/4780703f9bcb313759aecf11cc231953e77f43f3.tar.gz") { };
in
{
  programs.home-manager.enable = true;
  home.username = "meain";
  home.homeDirectory = "/home/meain";

  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  services.syncthing.enable = true;

  home.packages = [
    # core utils
    pkgs.ripgrep
    pkgs.lsd
    pkgs.neovim
    pkgs.fd
    pkgs.jq
    pkgs.git
    pkgs.fzf
    pkgs.tree
    pkgs.cmake
    pkgs.curl
    pkgs.zsh
    pkgs.coreutils

    # packages
    personal.notmuch-git # mail indexer
    pkgs.isync # mail syncronize with upstrem
    pkgs.htop # process monitor
    pkgs.mpc_cli # remote for mpd
    pkgs.mpd # music player
    pkgs.axel # download manager
    pkgs.pandoc # convert document between different formats
    # pkgs.xsv # view/manage csv from terminal
    pkgs.parallel # exec things in parallel
    pkgs.fortune # give me a fortune
    pkgs.lf # better ranger alternative
    pkgs.diff-so-fancy # really good diff
    pkgs.icdiff # simple colorfull diff replacement
    pkgs.ctags # code tag stuff
    pkgs.pass # password management
    pkgs.sshfs # mount vm as fs using ssh
    pkgs.stow # symlink management
    pkgs.git-absorb # automatic git commit --fixup
    pkgs.git-crypt # encrypt git stuff
    pkgs.wget # get stuff from internet
    pkgs.tmux # terminal multiplexer
    pkgs.aspell # spell checker
    # pkgs.mpvc  # mpv remote control  # not available on macos
    pkgs.msmtp # smtp client
    # pkgs.android-tools # adb and friends  # not available for macos
    pkgs.restic # backup
    pkgs.ledger # double entry accounting
    pkgs.dasel # jq but more versatile
    pkgs.mosh

    # aspell dicts
    pkgs.aspellDicts.en

    # apps
    # pkgs.kubectl # kubernetes cli
    # pkgs.kubernetes-helm # helm cli
    # pkgs.youtube-dl # download youtube videos
    # pkgs.chafa # show images in terminal using half blocks
    pkgs.hub # Github integration for git
    pkgs.lynx # terminal web browser
    pkgs.imagemagick # image manip cli
    pkgs.ffmpeg # video manip cli
    # pkgs.gnuplot # plotting
    pkgs.ddgr # search ddg from terminal
    pkgs.dasht # terminal docs
    # taskwarrior # task management
    pkgs.todo-txt-cli # todo management
    # pkgs.pkgs.ts # task spooler
    pkgs.pkgs.pstree # view process tree
    pkgs.pkgs.jiq # interactive jiq
    pkgs.pkgs.tldr # simpler man pages
    # pkgs.pkgs.silicon # create pretty code screenshots
    # pkgs.pkgs.transmission # torrent stuff
    # pkgs.pkgs.kube-prompt # interactive kubernetes cli
    # pkgs.pkgs.cowsay # useless stuff
    pkgs.podman # pod manager

    # programming
    pkgs.gist # create gist
    # pkgs.hey # http load generator

    # programming-shell
    pkgs.shellcheck # shell checker
    pkgs.shfmt # shell code format
    pkgs.nodePackages.bash-language-server # bash language-server

    # programming-go
    pkgs.go # go programming language
    pkgs.go-langserver # go language-server
    pkgs.goimports # go formatter
    pkgs.golangci-lint # all kinds of linters for go

    # programming-web
    # pkgs.html-tidy # html formatter
    # pkgs.nodejs # nodejs
    # pkgs.nodePackages.neovim # neovim package for js support
    # pkgs.nodePackages.fixjson # much better json formatter
    # pkgs.nodePackages.stylelint # css linter
    # pkgs.nodePackages.prettier # formatting for web stuff
    # pkgs.nodePackages.pnpm # package management
    # pkgs.nodePackages.typescript # typescript
    # pkgs.nodePackages.vscode-css-languageserver-bin # css languageserver
    # pkgs.nodePackages.javascript-typescript-langserver # javascript langserver

    # programming-nix
    pkgs.rnix-lsp # nix language server
    pkgs.nixpkgs-fmt # nix formater

    # programming-python
    pkgs.python39 # python language
    pkgs.poetry # better package manager
    pkgs.black # python code formatter
    pkgs.python39Packages.flake8 # linter
    pkgs.python39Packages.ipdb # interactive debugging
    pkgs.python39Packages.pynvim # neovim python support
    pkgs.python39Packages.pycodestyle # code style check
    pkgs.python39Packages.pydocstyle # doc style check
    pkgs.python39Packages.requests # http lib for quick stuff
    pkgs.python39Packages.virtualenv # virtual envs
    pkgs.python39Packages.bandit # analyze code for security issues
    pkgs.python39Packages.mypy # check types in code
    pkgs.python39Packages.isort # fix sort order
    pkgs.python39Packages.pygments # generic syntax highlight
    pkgs.python38Packages.python-language-server # python lsp (using below one as tests are failing)
    # (pkgs.python38Packages.python-language-server.overridePythonAttrs (oldAttrs: { checkPhase = ""; checkInputs = []; }))

    # programming-rust
    # pkgs.rustc # compiler
    # pkgs.cargo # package manager
    # pkgs.rustfmt # formatter
    # pkgs.clippy # the useful clippy
    # pkgs.rust-analyzer # lsp for rust
    # pkgs.cargo-edit # dep management
    # pkgs.cargo-bloat # find big chunks
    # pkgs.cargo-udeps # find unnecessary deps
    # pkgs.cargo-release # for releasing packages
    # cargo-watch # continuously run cargo check

    # programming-other
    # pkgs.nodePackages.yaml-language-server
    # pkgs.nodePackages.vscode-json-languageserver

    # gui
    # pkgs.mpv # audio/video player
    # pkgs.alacritty # terminal emulator

    # others
    # pkgs.redis # key value db
    # pkgs.postgresql_13 # postgres 13 (postgresql is at 11)
    # pkgs.mongodb # document db
    # pkgs.sqlite # better db
    # pkgs.minikube # mini kubernetes
    # pkgs.awscli # manage aws
    # pkgs.google-cloud-sdk # manage google cloud

    # optional
    # pkgs.ncdu # disk usage viewer tui
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
    # pkgs.ncmpcpp # mpd tui client
    # pkgs.tokei # count lines of code
    # pkgs.navi # interactive cli launcher
    # pkgs.googler # search google from terminal
    # pkgs.cmatrix # matrix thingy in shell
    # pkgs.graphviz # draw graphs with code
    # pkgs.pgcli # fancier postgres cli
    # pkgs.trivy # docker vulnerability scanner
    # pkgs.act # github ci locally

    # tryout
    # pkgs.gforth # gnu forth interpreter
    # pkgs.groff # gnu troff

    # linux-specific
    pkgs.victor-mono
    pkgs.htop
    pkgs.diff-so-fancy
    personal.traffic
    pkgs.trash-cli
    pkgs.entr
    pkgs.notify-desktop
  ];

  dconf.settings = {
    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "icon-view";
    };
    "org/gnome/nautilus/icon-view" = {
      default-zoom-level = "small";
    };

    "org/gnome/shell/keybindings" = {
      toggle-overview = [ "<Super>Space" ];
    };

    "org/gnome/shell/extensions/dash-to-panel" = {
      appicon-margin = 0;
      appicon-padding = 3;
      available-monitors = [ 0 ];
      dot-position = "BOTTOM";
      hotkeys-overlay-combo = "TEMPORARILY";
      leftbox-padding = -1;
      panel-anchors = "{\"0\":\"MIDDLE\"}";
      panel-lengths = "{\"0\":100}";
      panel-positions = "{\"0\":\"TOP\"}";
      panel-sizes = "{\"0\":24}";
      status-icon-padding = -1;
      tray-padding = -1;
      window-preview-title-position = "TOP";
    };

    "org/gnome/desktop/interface" = {
      clock-format = "12h";
    };
    "org/gtk/settings/file-chooser" = {
      clock-format = "12h";
    };
    "org/gnome/desktop/wm/keybindings" = {
      # close = ["<Super>w"];
      switch-to-workspace-left = [ "<Super>u" ];
      switch-to-workspace-right = [ "<Super>o" ];  # M-i/<Super>i has some issues
      move-to-workspace-left = [ "<Super><Shift>u" ];
      move-to-workspace-right = [ "<Super><Shift>o" ];
    };
  };


  systemd.user.services.email-sync = {
    Service.Type = "oneshot";
    Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',mail-sync'";
    Timer.OnCalendar = [ "*:0/15" ];  # every 15min
    Timer.Persistent=true;
  };

  # make this into a function??
  systemd.user.services.note-sync = {
    Service.Type = "oneshot";
    Service.WorkingDirectory = "/home/meain/.local/share/notes";
    Service.ExecStart = "${pkgs.git}/bin/git add . ; ${pkgs.git}/bin/git commit -m 'Updating notes' ; ${pkgs.git}/bin/git push origin master";
    Timer.OnCalendar = [ "*-*-* *:00:00" ];
    Timer.Persistent=true;
  };
  systemd.user.services.ledger-sync = {
    Service.Type = "oneshot";
    Service.WorkingDirectory = "/home/meain/.local/share/ledger";
    Service.ExecStart = "${pkgs.git}/bin/git add . ; ${pkgs.git}/bin/git commit -m 'Updating ledger' ; ${pkgs.git}/bin/git push origin master";
    Timer.OnCalendar = [ "*-*-* *:00:00" ];
    Timer.Persistent=true;
  };
  systemd.user.services.journal-sync = {
    Service.Type = "oneshot";
    Service.WorkingDirectory = "/home/meain/.local/share/journal";
    Service.ExecStart = "${pkgs.git}/bin/git add . ; ${pkgs.git}/bin/git commit -m 'Updating journal' ; ${pkgs.git}/bin/git push origin master";
    Timer.OnCalendar = [ "*-*-* *:00:00" ];
    Timer.Persistent=true;
  };

  # This value determines the Home Manager release that your # configuration is compatible with. This helps avoid breakage # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
