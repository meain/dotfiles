{ personal, bleeding, stable, tree-grepper, firefox-addons, ... }:
{ pkgs, ... }:
let
  ppkgs = personal.packages.x86_64-linux;
  bpkgs = bleeding.legacyPackages.x86_64-linux;
  spkgs = stable.legacyPackages.x86_64-linux;
  utils = import ./utils.nix { inherit pkgs; };
  fonts = import ./fonts.nix { inherit pkgs spkgs; };
in
{
  home.stateVersion = "21.05";
  home.username = "meain";
  home.homeDirectory = "/home/meain";

  programs.home-manager.enable = true;

  services.emacs.package = pkgs.emacs-git;
  programs.emacs = {
    package = pkgs.emacs-git;
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  programs.firefox = {
    enable = true;
    profiles.meain = {
      # TODO: How to set default search engine?
      search.engines = {
        "DuckDuckGo" = {
          urls = [{
            template = "https://duckduckgo.com";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",d" ];
        };
        "Google" = {
          urls = [{
            template = "https://google.com/search";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",g" ];
        };
        "Nix Packages" = {
          urls = [{
            template = "https://search.nixos.org/packages";
            params = [
              { name = "type"; value = "packages"; }
              { name = "query"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",ns" ];
        };
        "YouTube" = {
          urls = [{
            template = "https://www.youtube.com/results";
            params = [
              { name = "search_query"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",yt" ];
        };
        "Wikipedia" = {
          urls = [{
            template = "https://en.wikipedia.org/wiki/Special:Search";
            params = [
              { name = "search"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",w" ];
        };
        "DockerHub" = {
          urls = [{
            template = "https://hub.docker.com/search";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",dh" ];
        };
        "GitHub" = {
          urls = [{
            template = "https://github.com/search";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",gh" ];
        };
      };
      search.force = true;

      bookmarks = [
        {
          name = "GitHub Notifications";
          keyword = "gn";
          url = "https://github.com/notifications";
        }
        {
          name = "Calendar";
          keyword = "cal";
          url = "https://calendar.google.com/calendar/r";
        }
        {
          name = "Email";
          keyword = "mail";
          url = "https://mail.google.com/mail/u/0/#inbox";
        }
        {
          name = "Outlook Calendar";
          keyword = "ocal";
          url = "https://outlook.office.com/calendar/view/month";
        }
        {
          name = "Outlook Mail";
          keyword = "omail";
          url = "https://outlook.office.com/mail/inbox";
        }
      ] ++
      utils.gh-bookmarks {
        repo = "NixOS/nixpkgs";
        basename = "nixpkgs";
        basekeyword = "nip";
      } ++
      utils.gh-bookmarks {
        repo = "alcionai/corso";
        basename = "Corso";
        basekeyword = "cor";
      };

      settings = {
        "dom.security.https_only_mode" = true; # force https
        "browser.download.panel.shown" = true; # show download panel
        "identity.fxaccounts.enabled" = false; # disable firefox accounts
        "signon.rememberSignons" = false; # disable saving passwords
        "extensions.pocket.enabled" = false; # disable pocket
        "app.shield.optoutstudies.enabled" = false; # disable shield studies
        "app.update.auto" = false; # disable auto update
        "browser.bookmarks.restore_default_bookmarks" = false; # don't restore default bookmarks
        "browser.quitShortcut.disabled" = true; # disable ctrl+q
        "browser.shell.checkDefaultBrowser" = false; # don't check if default browser

        # download handling
        "browser.download.dir" = "/home/meain/down"; # default download dir
        "browser.startup.page" = 3; # restore previous session

        # ui changes
        "browser.aboutConfig.showWarning" = false; # disable warning about about:config
        "browser.compactmode.show" = true; # disable compact mode
        "browser.uidensity" = 1;
        "general.autoScroll" = true; # enable autoscroll
        "browser.tabs.firefox-view" = false; # enable firefox view
        "browser.toolbars.bookmarks.visibility" = "never"; # hide bookmarks toolbar
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false; # disable picture in picture button
        "startup.homepage_welcome_url" = ""; # disable welcome page
        "browser.newtabpage.enabled" = false; # disable new tab page

        # privacy
        "browser.contentblocking.category" = "custom"; # set tracking protection to custom
        "browser.discovery.enabled" = false; # disable discovery
        "browser.search.suggest.enabled" = false; # disable search suggestions
        "browser.protections_panel.infoMessage.seen" = true; # disable tracking protection info

        # let me close and open tabs without confirmation
        "browser.tabs.closeWindowWithLastTab" = false; # don't close window when last tab is closed
        "browser.tabs.loadBookmarksInTabs" = true; # open bookmarks in new tab
        "browser.tabs.loadDivertedInBackground" = true; # open new tab in background
        "browser.tabs.loadInBackground" = true; # open new tab in background
        "browser.tabs.warnOnClose" = false; # don't warn when closing multiple tabs
        "browser.tabs.warnOnCloseOtherTabs" = false; # don't warn when closing multiple tabs
        "browser.tabs.warnOnOpen" = false; # don't warn when opening multiple tabs
        "browser.tabs.warnOnQuit" = false; # don't warn when closing multiple tabs

        # other
        "devtools.cache.disabled" = true; # disable caching in devtools
        "devtools.toolbox.host" = "right"; # move devtools to right
        # "browser.ssb.enabled" = true; # enable site specific browser
        "media.autoplay.default" = 0; # enable autoplay on open

        # override fonts (Set tracking protection to custom without "Suspected fingerprinters")
        "font.minimum-size.x-western" = 13;
        "font.size.fixed.x-western" = 15;
        "font.size.monospace.x-western" = 15;
        "font.size.variable.x-western" = 15;
        "font.name.monospace.x-western" = "Monaspace Neon";
        "font.name.sans-serif.x-western" = "Monaspace Neon";
        "font.name.serif.x-western" = "Monaspace Neon";
        "browser.display.use_document_fonts" = 0;
      };

      userChrome = ''
        /* some css */
      '';

      extensions = with firefox-addons.packages."x86_64-linux"; [
        # https://gitlab.com/rycee/nur-expressions/-/blob/master/pkgs/firefox-addons/addons.json?ref_type=heads
        bitwarden
        clearurls
        darkreader
        decentraleyes
        greasemonkey
        multi-account-containers
        privacy-badger
        redirector
        refined-github
        return-youtube-dislikes
        sidebery
        sponsorblock
        stylus
        ublock-origin
        vimium
        wallabagger
        youtube-shorts-block
        youtube-recommended-videos

        # personally packaged
        ppkgs.firefox-addons.awesome-rss
        ppkgs.firefox-addons.aw-watcher-web
        ppkgs.firefox-addons.containerise
        ppkgs.firefox-addons.ghostpage
        ppkgs.firefox-addons.mastodon4-redirect
        ppkgs.firefox-addons.nattynote
        ppkgs.firefox-addons.netflix-prime-auto-skip
        ppkgs.firefox-addons.notifications-preview-github
        ppkgs.firefox-addons.smartreader
        ppkgs.firefox-addons.unofficial-hypothesis
        ppkgs.firefox-addons.watchmarker-for-youtube
        ppkgs.firefox-addons.a-n-i-m-a-t-e-d-kitty-cat
        ppkgs.firefox-addons.global-speed
        ppkgs.firefox-addons.try-another-search-engine
      ];

    };
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
    # pkgs.pass # password management (not using nix version as we cannot make it use nix gpg-agent and we need both to use same version)
    # pkgs.gnupg # gpg
    # pkgs.sshfs # mount vm as fs using ssh
    pkgs.stow # symlink management
    pkgs.git-absorb # automatic git commit --fixup
    pkgs.git-crypt # encrypt git stuff
    pkgs.wget # get stuff from internet
    pkgs.tmux # terminal multiplexer
    pkgs.aspell # spell checker
    pkgs.mpvc # mpv remote control  # not available on macos
    pkgs.playerctl # control any sort of audio playback (mpris cli)
    spkgs.msmtp # smtp client
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
    pkgs.rofi # menu stuff
    ppkgs.spaceman-diff # diff images in terminal
    pkgs.polybarFull # bar for wm
    pkgs.python39Packages.pipx # pipx for installing stuff
    # ppkgs.logseq-doctor # logseq utils

    # aspell dicts
    pkgs.aspellDicts.en

    # apps
    # pkgs.kubectl # kubernetes cli
    # pkgs.kubecolor # colorful kubectl
    # pkgs.kubernetes-helm # helm cli
    # pkgs.stern # better way to fetch kubernetes log
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
    # pkgs.dasht # terminal docs
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
    # pkgs.k9s # kubernetes tui
    # pkgs.screenkey # show keys (useful when recording screen)
    pkgs.lnav # log viewer

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
    # pkgs.golines # long line format golang
    # pkgs.gofumpt # better go formatting
    # pkgs.delve # debugging in go
    # pkgs.rr # record replay debugging
    # pkgs.gotests # test generation for golang

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
    spkgs.python39Packages.python-lsp-server # python lsp
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
    ppkgs.prosemd-lsp # prose lsp
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
    tree-grepper.outputs.packages.x86_64-linux.tree-grepper # grep with tree-sitter
    pkgs.comby # code mod
    # pkgs.ruby # ruby language
    # pkgs.actionlint # linting for gihtub actions

    # gui
    pkgs.mpv # audio/video player
    # pkgs.kitty
    # pkgs.alacritty # terminal emulator
    # pkgs.firefox # browser
    # pkgs.chromium # because Google hates firefox
    # pkgs.guake # drop down terminal
    # pkgs.insomnia # simpler postman
    # pkgs.beekeeper-studio # db viewer
    pkgs.sakura # x11 terminal emulator
    # pkgs.foot # wayland terminal emulator
    pkgs.zathura # pdf viewer
    pkgs.sxiv # image viewer

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
    pkgs.activitywatch # activity tracking
    # pkgs.gforth # gnu forth interpreter
    pkgs.nodePackages.mermaid-cli # cli for generating mermaid charts
    # pkgs.genact # become a movie "hacker"
    # bpkgs.logseq # tracking life (using flatpak version)
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
    pkgs.shell_gpt # another chatgpt cli
    pkgs.feh # image viewer (for desktop background)
    pkgs.kopia # backup
    pkgs.distrobox # run other distros and packages
    pkgs.nur.repos.rycee.mozilla-addons-to-nix # package firefox addons

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

  systemd.user.services = {
    activitywatch = {
      Unit.Description = "Start ActivityWatch";
      Service.Type = "simple";
      Service.ExecStart = "${pkgs.activitywatch}/bin/aw-server";
      Install.WantedBy = [ "default.target" ];
      Service.Restart = "on-failure";
      Service.RestartSec = 5;
    };

    activitywatch-afk = {
      Unit.Description = "Start ActivityWatch AFK";
      Service.Type = "simple";
      Service.ExecStart = "${pkgs.activitywatch}/bin/aw-watcher-afk";
      Install.WantedBy = [ "default.target" ];
      Service.Restart = "on-failure";
      Service.RestartSec = 5;
    };

    activitywatch-window = {
      Unit.Description = "Start ActivityWatch Window";
      Service.Type = "simple";
      Service.ExecStart = "${pkgs.activitywatch}/bin/aw-watcher-window";
      Install.WantedBy = [ "default.target" ];
      Service.Restart = "on-failure";
      Service.RestartSec = 5;
    };
  };


  systemd.user.services.kopia-ui = {
    Unit.Description = "Kopia UI";
    Service.Type = "simple";
    Service.ExecStart = "kopia-ui"; # currently not installed via nix
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "on-failure";
    Service.RestartSec = 5;
  };

  # systemd.user.startServices = true;  # enabling this increases switch time a lot
  systemd.user.services = {
    mpd = utils.ss-simple { cmd = "mpd --no-daemon"; wait = 3; };
    clipmenud = utils.ss-simple { cmd = "clipmenud"; wait = 3; };
    sxhkd = utils.ss-simple { cmd = "sxhkd"; wait = 3; };
    emacs = utils.ss-simple { cmd = "emacs --fg-daemon"; wait = 1; };
    logseq = utils.ss-simple { cmd = "/var/lib/flatpak/app/com.logseq.Logseq/current/active/export/bin/com.logseq.Logseq"; wait = 1; };
    floatingterm = utils.ss-simple { cmd = "sakura --name floatingterm -x \"tt floating\""; wait = 1; };
    mail-watcher = utils.ss-simple { cmd = "find /home/meain/.local/share/mail/.notmuch/xapian|entr -n ,shellout-update"; wait = 5; };
  };

  # code/note sync
  systemd.user.services.note-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/notes"; };
  systemd.user.timers.note-sync = utils.timer-daily;
  systemd.user.services.ledger-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/ledger"; };
  systemd.user.timers.ledger-sync = utils.timer-daily;
  systemd.user.services.journal-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/journal"; };
  systemd.user.timers.journal-sync = utils.timer-daily;
  systemd.user.services.logseq-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/logseq"; };
  systemd.user.timers.logseq-sync = utils.timer-daily;

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
  systemd.user.services.mscripts-backup = utils.ss-timer { cmd = ",projects-config-backup"; };
  systemd.user.timers.mscripts-backup = utils.timer-daily;

  # regular cleanup
  systemd.user.services.cleanup-downloads = utils.ss-cleanup { dir = "/home/meain/down"; };
  systemd.user.timers.cleanup-downloads = utils.timer-daily;
  systemd.user.services.cleanup-scratch = utils.ss-cleanup { dir = "/home/meain/.local/share/scratch"; };
  systemd.user.timers.cleanup-scratch = utils.timer-daily;

  # Setup direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
}
