{
  description = "Home Manager configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/21.11";
    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    unstable.url = "github:nixos/nixpkgs/master";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    personal = {
      url = "path:/home/meain/dev/src/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacsOverlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, homeManager, unstable, nur, personal, emacsOverlay }: {
    homeConfigurations = {
      "meain" = homeManager.lib.homeManagerConfiguration {
        extraSpecialArgs = {
          inherit unstable;
        };

        configuration = { pkgs, ... }:
          let
            ppkgs = personal.packages.x86_64-linux;
            upkgs = unstable.legacyPackages.x86_64-linux;
          in
          {
            programs.home-manager.enable = true;

            services.emacs.package = pkgs.emacsUnstable;
            programs.emacs = {
              package = pkgs.emacsUnstable;
              enable = true;
              extraPackages = epkgs: [ epkgs.vterm ];
            };

            services.syncthing.enable = true;

            fonts.fontconfig.enable = true;
            home.packages = [
              # core utils
              pkgs.ripgrep
              pkgs.gnumake
              # pkgs.lsd # installed from master
              pkgs.fd
              pkgs.jq
              pkgs.git
              pkgs.fzf
              pkgs.tree
              pkgs.cmake
              pkgs.curl
              # pkgs.zsh
              pkgs.coreutils
              pkgs.neovim
              pkgs.gcc

              # packages
              # ppkgs.notmuch-git # mail indexer
              # pkgs.notmuch
              pkgs.notmuch
              pkgs.isync # mail synchronize with upstrem
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
              pkgs.dasel # jq but more versatile
              # pkgs.mosh # better ssh
              ppkgs.kmonad # key remapping
              ppkgs.warpd # mouse control
              pkgs.trash-cli # rm -> trash
              pkgs.entr # continuously run stuff
              pkgs.notify-desktop # desktop notifications
              pkgs.xclip # clipboard
              # pkgs.wl-clipboard
              pkgs.bandwhich # view network stats (alt: nethogs)
              pkgs.xdotool # for window switching
              pkgs.picotts # for say
              # ppkgs.dmenu # menu stuff (fork for emojis)

              # aspell dicts
              pkgs.aspellDicts.en

              # apps
              pkgs.kubectl # kubernetes cli
              pkgs.kubernetes-helm # helm cli
              pkgs.youtube-dl # download youtube videos
              pkgs.chafa # show images in terminal using half blocks
              pkgs.hub # Github integration for git
              pkgs.gh # Yet another Github integration for git
              pkgs.lynx # terminal web browser
              pkgs.slop # select region from screen
              pkgs.imagemagick # image manip cli
              pkgs.ffmpeg # video manip cli
              pkgs.gnuplot # plotting
              pkgs.ddgr # search ddg from terminal
              pkgs.dasht # terminal docs
              # taskwarrior # task management
              pkgs.todo-txt-cli # todo management
              # pkgs.pkgs.ts # task spooler
              pkgs.pup # html filtering
              pkgs.pkgs.jiq # interactive jiq
              # pkgs.pkgs.tldr # simpler man pages
              # pkgs.pkgs.silicon # create pretty code screenshots
              pkgs.pkgs.transmission # torrent stuff
              # pkgs.pkgs.kube-prompt # interactive kubernetes cli
              # pkgs.pkgs.cowsay # useless stuff
              # pkgs.podman # pod manager
              pkgs.typos # typo checker (integrated with flymake)
              pkgs.k9s # kubernetes tui

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
              pkgs.go-langserver # go language-server
              pkgs.goimports # go formatter
              pkgs.go-tools # installing staticcheck (technically available in golangci-lint, but for use in lsp)
              pkgs.errcheck # available in golangci-lint, but still
              pkgs.golangci-lint # all kinds of linters for go
              pkgs.delve # debugging in go

              # programming-web
              pkgs.html-tidy # html formatter
              pkgs.nodejs # nodejs
              # pkgs.nodePackages.neovim # neovim package for js support
              pkgs.nodePackages.stylelint # css linter
              pkgs.nodePackages.prettier # formatting for web stuff
              # pkgs.nodePackages.pnpm # package management
              # pkgs.nodePackages.typescript # typescript
              pkgs.nodePackages.vscode-css-languageserver-bin # css languageserver
              pkgs.nodePackages.javascript-typescript-langserver # javascript langserver

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
              # pkgs.cargo-release # for releasing packages
              pkgs.cargo-watch # continuously run cargo check

              # programming-lua
              # pkgs.nodePackages.lua-fmt

              # programming-other
              pkgs.nodePackages.yaml-language-server # language server for yaml
              pkgs.nodePackages.vscode-json-languageserver # language server for json
              ppkgs.prosemd-lsp
              pkgs.nodePackages.fixjson # much better json formatter
              pkgs.nodePackages.jsonlint # json linting
              pkgs.nodePackages.markdownlint-cli # markdown linter
              pkgs.hadolint # Dockerfile lint
              pkgs.sqls # lsp server for sql
              pkgs.python39Packages.sqlparse # sqlformat

              # gui
              pkgs.mpv # audio/video player
              # pkgs.kitty
              # pkgs.alacritty # terminal emulator
              # pkgs.firefox # working OSS browser
              pkgs.chromium # because Google hates firefox
              # pkgs.guake # drop down terminal
              # pkgs.insomnia # simpler postman
              pkgs.beekeeper-studio # db viewer
              pkgs.sakura # x11 terminal emulator
              # pkgs.foot # wayland terminal emulator
              pkgs.zathura # pdf viewer
              pkgs.sxiv # image viewer

              # others
              # pkgs.redis # key value db
              pkgs.postgresql_13 # postgres 13 (postgresql is at 11)
              # pkgs.mongodb # document db
              # pkgs.sqlite # better db
              # pkgs.minikube # mini kubernetes
              pkgs.kind # better minikube
              # pkgs.awscli # manage aws
              # pkgs.google-cloud-sdk # manage google cloud
              # pkgs.lens # kubernetes viewer

              # optional
              pkgs.gdu # disk usage viewer tui (alt: ncdu)
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
              pkgs.docker-compose # docker-compose
              # pkgs.ncmpcpp # mpd tui client
              # pkgs.tokei # count lines of code
              # pkgs.navi # interactive cli launcher
              # pkgs.googler # search google from terminal
              # pkgs.cmatrix # matrix thingy in shell
              pkgs.graphviz # draw graphs with code
              # pkgs.groff # gnu troff
              pkgs.pgcli # fancier postgres cli
              # pkgs.trivy # docker vulnerability scanner
              # pkgs.act # github ci locally
              # pkgs.pkgs.pstree # view process tree
              ppkgs.traffic # simple network stats
              # pkgs.comma # run literally anything
              ppkgs.gloc # run stuff in all git repos
              ppkgs.tojson # convert yaml/toml/json
              pkgs.jo # create json
              pkgs.jiq
              upkgs.jless # json viewer  // TODO: fix this
              pkgs.blueman # bluetooth control
              pkgs.arandr # screen layout configure
              pkgs.clipmenu # clipboard history
              pkgs.brightnessctl # brightness control
              pkgs.dunst # notifications with buttons (dunstify)
              pkgs.pcmanfm # gui file manager
              pkgs.unixtools.netstat # netstat
              # pkgs.comby # structural search/editing of code
              # pkgs.visidata # data visualization
              pkgs.dragon-drop # drag and drop files
              pkgs.sct # redshift ish stuff

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

              # tryout
              # pkgs.gforth # gnu forth interpreter

              # symlinks (macos polyfills)
              (pkgs.runCommand "open" { } ''mkdir -p $out/bin; ln -s ${pkgs.xdg-utils}/bin/xdg-open $out/bin/open'')
              # (pkgs.runCommand "pbcopy" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-copy $out/bin/pbcopy'')
              # (pkgs.runCommand "pbpaste" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-paste $out/bin/pbpaste'')
              # (pkgs.runCommand "say" { } ''mkdir -p $out/bin; ln -s ${pkgs.espeak}/bin/espeak $out/bin/say'')

              # autostart
              # (pkgs.makeAutostartItem { name = "guake"; package = pkgs.guake; })
              # (pkgs.makeAutostartItem { name = "albert"; package = pkgs.albert; })

              # fonts (set-frame-font  "Anka/Coder 10")
              pkgs.iosevka
              pkgs.hermit
              pkgs.agave
              pkgs.inconsolata
              pkgs.victor-mono
              pkgs.cascadia-code
              pkgs.julia-mono
              pkgs.sudo-font
              pkgs.terminus-nerdfont
              pkgs.fantasque-sans-mono
              pkgs.monoid
              pkgs.ankacoder
              pkgs.jetbrains-mono
              pkgs.go-font
              pkgs.unifont
              # pkgs.tewi-font
              pkgs.hack-font
              pkgs.mononoki
              pkgs.roboto-mono
              pkgs.cozette
              pkgs.fira-code
              # pkgs.dina-font
              # pkgs.envypn-font
            ];

            dconf.settings = {
              # "org/gnome/desktop/background" = {
              #   picture-uri = "file:///home/meain/wallpaper.jpg";
              # };

              "org/gnome/desktop/interface" = {
                clock-format = "12h";
                clock-show-weekday = true;
                font-antialiasing = "grayscale";
                font-hinting = "slight";
                font-name = "Agave 10";
                # gtk-im-module = "gtk-im-context-simple";
                gtk-theme = "Fluent-round-light-compact";
                monospace-font-name = "Anka/Coder 10";
                show-battery-percentage = true;
              };

              "org/gnome/desktop/wm/preferences" = {
                titlebar-font = "Agave 10";
                auto-raise = true;
                focus-new-windows = "smart";
              };

              "org/gnome/shell" = {
                disable-user-extensions = false;
                disabled-extensions = [ ];
                enabled-extensions = [
                  "dash-to-panel@jderose9.github.com"
                  "clipboard-indicator@tudmotu.com"
                  "blur-my-shell@aunetx"
                  "user-theme@gnome-shell-extensions.gcampax.github.com"
                  "caffeine@patapon.info"
                  "gsconnect@andyholmes.github.io"
                  "no-overview@fthx"
                  "bluetooth-quick-connect@bjarosze.gmail.com"
                  "bluetooth-battery@michalw.github.com"
                  "sound-output-device-chooser@kgshank.net"
                  "focus-my-window@varianto25.com"
                  "shellout@meain.io"
                  "custom-hot-corners-extended@G-dH.github.com"
                  "steal-my-focus@kagesenshi.org"
                ];
                favorite-apps = [
                  "org.gnome.Nautilus.desktop"
                  "org.gnome.Terminal.desktop"
                  "slack_slack.desktop"
                  "firefox.desktop"
                  "chromium-browser.desktop"
                  "org.gnome.Calendar.desktop"
                ];
              };

              "org/gnome/shell/extensions/user-theme" = {
                name = "Fluent-dark-compact";
              };

              "org/gnome/nautilus/preferences" = {
                default-folder-viewer = "icon-view";
              };
              "org/gnome/nautilus/icon-view" = {
                default-zoom-level = "small";
              };

              "org/gnome/shell/keybindings" = {
                focus-active-notification = [ ]; # clashes with ,thing-for-today-popup
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
                panel-positions = "{\"0\":\"BOTTOM\"}";
                panel-sizes = "{\"0\":24}";
                status-icon-padding = -1;
                tray-padding = -1;
                window-preview-title-position = "TOP";
              };
              "org/gnome/shell/extensions/clipboard-indicator" = {
                history-size = 100;
                toggle-menu = [ "<Alt>p" ]; # should be alt+shift+p later
              };

              "org/gtk/settings/file-chooser" = {
                clock-format = "12h";
              };
              "org/gnome/desktop/wm/keybindings" = {
                # close = ["<Super>w"];
                switch-to-workspace-left = [ "<Super>u" ];
                switch-to-workspace-right = [ "<Super>o" ]; # M-i/<Super>i has some issues
                move-to-workspace-left = [ "<Super><Shift>u" ];
                move-to-workspace-right = [ "<Super><Shift>o" ];
                move-to-workspace-up = [ "<Super><Shift>u" ]; # for older version of GNOME
                move-to-workspace-down = [ "<Super><Shift>o" ]; # for older version of GNOME
              };
              "org/gnome/settings-daemon/plugins/media-keys" = {
                custom-keybindings = [
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom6/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom7/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom8/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom9/"
                  "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom10/"
                ];
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
                binding = "<Super>semicolon";
                command = "zsh -ic 'guake-toggle'";
                name = "guake";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
                binding = "<Alt>t";
                command = "gnome-terminal";
                name = "spawn-terminal";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
                binding = "<Alt>Space";
                command = "zsh -ic 'albert toggle'";
                name = "albert";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3" = {
                binding = "<Alt>e";
                command = "emacsclient  -a '' --no-wait -c";
                name = "emacs";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4" = {
                binding = "<Alt>Backspace";
                command = "zsh -ic ',open-or-search $(pbpaste)'";
                name = "open-or-search";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5" = {
                binding = "<Super>s"; # this is stupid keybinding, just can't get super+' to work in vm
                command = "zsh -ic ',editor-or-browser'";
                name = "launch-or-focus-editor-browser";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom6" = {
                binding = "<Alt><Shift>e";
                command = "gnome-terminal -- zsh -ic ',mail-quick-read'";
                name = "mail-quick-read";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom7" = {
                binding = "<Alt><Shift>m";
                command = "gnome-terminal -- zsh -ic ',emojipicker'";
                name = "emoji-picker";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom8" = {
                binding = "<Super>n";
                command = "gnome-terminal -- zsh -ic ',thing-for-today-popup'"; # opening via gnome-terminal fixes focus issues
                name = "thing-for-today";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom9" = {
                binding = "<Alt>d";
                command = "dmenu_run";
                name = "dmenu";
              };
              "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom10" = {
                binding = "<Ctrl>t";
                command = "zsh -ic ',browser-newtab'";
                name = "newtab";
              };

              "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-left-0" = {
                action = "toggle-overview";
              };
              "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-right-0" = {
                action = "toggle-overview";
              };

              "apps/guake/general" = {
                abbreviate-tab-names = false;
                compat-delete = "delete-sequence";
                display-n = 0;
                gtk-theme-name = "Default";
                history-size = 10000;
                max-tab-name-length = 100;
                mouse-display = true;
                open-tab-cwd = false;
                prompt-on-quit = false;
                quick-open-command-line = "gedit %(file_path) s";
                restore-tabs-notify = false;
                restore-tabs-startup = false;
                save-tabs-when-changed = false;
                scroll-keystroke = true;
                start-at-login = false;
                use-default-font = true;
                use-popup = false;
                use-scrollbar = false;
                use-trayicon = false;
                window-halignment = 0;
                window-height = 60;
                window-losefocus = false;
                window-refocus = true;
                window-tabbar = false;
                window-width = 100;
              };
              "apps/guake/keybindings/global" = {
                show-hide = "disabled";
              };
              "apps/guake/keybindings/local" = {
                toggle-fullscreen = "<Super>Return";
              };
              "apps/guake/style/background" = {
                transparency = 100;
              };

            };


            # systemd.user.startServices = true;  # enabling this increases switch time a lot
            systemd.user.services.email-sync = {
              Service.Type = "oneshot";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',mail-sync'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.email-sync = {
              Timer.OnCalendar = "*:0/15"; # every 15min
              Install.WantedBy = [ "timers.target" ];
            };

            # update output for shellout gnome extension
            systemd.user.services.mail-watcher = {
              Service.Type = "simple";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic 'find /home/meain/.local/share/mail/.notmuch/xapian|entr -n ,shellout-update'";
              Install.WantedBy = [ "default.target" ];
              Service.Restart = "on-failure";
              Service.RestartSec = 5;
            };

            systemd.user.services.emacs = {
              Unit.Description = "Start emacs server";
              Service.Type = "simple";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic 'emacs --fg-daemon'";
              Install.WantedBy = [ "default.target" ];
              Service.Restart = "on-failure";
              Service.RestartSec = 5;
            };

            # make this into a function??
            systemd.user.services.note-sync = {
              Service.Type = "oneshot";
              Service.WorkingDirectory = "/home/meain/.local/share/notes";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',git-auto-sync'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.note-sync = {
              Timer.OnCalendar = "*-*-* *:00:00";
              Install.WantedBy = [ "timers.target" ];
            };
            systemd.user.services.til-sync = {
              Service.Type = "oneshot";
              Service.WorkingDirectory = "/home/meain/.local/share/til";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',git-auto-sync'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.til-sync = {
              Timer.OnCalendar = "*-*-* *:00:00";
              Install.WantedBy = [ "timers.target" ];
            };
            systemd.user.services.ledger-sync = {
              Service.Type = "oneshot";
              Service.WorkingDirectory = "/home/meain/.local/share/ledger";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',git-auto-sync'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.ledger-sync = {
              Timer.OnCalendar = "*-*-* *:00:00";
              Install.WantedBy = [ "timers.target" ];
            };
            systemd.user.services.journal-sync = {
              Service.Type = "oneshot";
              Service.WorkingDirectory = "/home/meain/.local/share/journal";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',git-auto-sync'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.journal-sync = {
              Timer.OnCalendar = "*-*-* *:00:00";
              Install.WantedBy = [ "timers.target" ];
            };
            systemd.user.services.weather-pull = {
              Service.Type = "oneshot";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',weather-current'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.weather-pull = {
              Timer.OnCalendar = "*:0/30";
              Timer.Persistent = true;
              Install.WantedBy = [ "timers.target" ];
            };

            systemd.user.services.battery-check = {
              Service.Type = "oneshot";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',low-battery-notify'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.battery-check = {
              Timer.OnCalendar = "*:0/5";
              Timer.Persistent = true;
              Install.WantedBy = [ "timers.target" ];
            };

            systemd.user.services.update-sct = {
              Service.Type = "oneshot";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',update-sct'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.update-sct = {
              Timer.OnCalendar = "*:0/30";
              Timer.Persistent = true;
              Install.WantedBy = [ "timers.target" ];
            };

            systemd.user.services.cleanup-downloads = {
              Service.Type = "oneshot";
              Service.WorkingDirectory = "/home/meain/Downloads";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',cleanup-folder'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.cleanup-downloads = {
              Timer.OnCalendar = "*-*-* *:00:00";
              Timer.Persistent = true;
              Install.WantedBy = [ "timers.target" ];
            };
            systemd.user.services.cleanup-scratch = {
              Service.Type = "oneshot";
              Service.WorkingDirectory = "/home/meain/.local/share/scratch";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',cleanup-folder'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.cleanup-scratch = {
              Timer.OnCalendar = "*-*-* *:00:00";
              Timer.Persistent = true;
              Install.WantedBy = [ "timers.target" ];
            };

            systemd.user.services.update-calendar = {
              Service.Type = "oneshot";
              Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',upcoming-events'";
              Install.WantedBy = [ "default.target" ];
            };
            systemd.user.timers.update-calendar = {
              Timer.OnCalendar = "*:0/10"; # the actual update only runs every hour, but this is in case a run fails
              Timer.Persistent = true;
              Install.WantedBy = [ "timers.target" ];
            };

            # Setup direnv
            programs.direnv.enable = true;
            programs.direnv.nix-direnv.enable = true;
          };

        pkgs = import nixpkgs {
          system = "x86_64-linux";
          overlays = [ emacsOverlay.overlay ];
        };

        system = "x86_64-linux";
        homeDirectory = "/home/meain";
        username = "meain";
        stateVersion = "21.05";
      };
    };
  };
}