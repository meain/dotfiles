{pkgs, spkgs}: [
    pkgs.mpvc # mpv remote control  # not available on macos
    pkgs.playerctl # control any sort of audio playback (mpris cli)

    pkgs.notify-desktop # desktop notifications
    pkgs.xclip # clipboard (needed for emacs-everywhere)
    pkgs.xsel # clipboard
    # pkgs.wl-clipboard
    pkgs.xdotool # for window switching
    # ppkgs.dmenu # menu stuff (fork for emojis)
    pkgs.rofi # menu stuff
    pkgs.polybarFull # bar for wm
    spkgs.slop # select region from screen

    # gui
    pkgs.sakura # x11 terminal emulator
    # pkgs.foot # wayland terminal emulator

    # other
    pkgs.arandr # screen layout configure
    pkgs.blueman # bluetooth control
    pkgs.clipmenu # clipboard history
    pkgs.brightnessctl # brightness control
    pkgs.dunst # notifications with buttons (dunstify)
    pkgs.xfce.thunar # gui file manager
    pkgs.xdragon # drag and drop files
    pkgs.sct # redshift ish stuff

    pkgs.distrobox # run other distros and packages
    pkgs.workrave # break reminder
    pkgs.cpulimit # limit cpu usage
    pkgs.pamixer # pulseaudio mixer

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

    # autostart
    # (pkgs.makeAutostartItem { name = "guake"; package = pkgs.guake; })
    # (pkgs.makeAutostartItem { name = "albert"; package = pkgs.albert; })

    # symlinks (macos polyfills)
    # (pkgs.runCommand "open" { } ''mkdir -p $out/bin; ln -s ${pkgs.xdg-utils}/bin/xdg-open $out/bin/open'')
    # (pkgs.runCommand "pbcopy" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-copy $out/bin/pbcopy'')
    # (pkgs.runCommand "pbpaste" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-paste $out/bin/pbpaste'')
    # (pkgs.runCommand "say" { } ''mkdir -p $out/bin; ln -s ${pkgs.espeak}/bin/espeak $out/bin/say'')
]