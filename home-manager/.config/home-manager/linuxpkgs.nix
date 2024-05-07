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

    # symlinks (macos polyfills)
    # (pkgs.runCommand "open" { } ''mkdir -p $out/bin; ln -s ${pkgs.xdg-utils}/bin/xdg-open $out/bin/open'')
    # (pkgs.runCommand "pbcopy" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-copy $out/bin/pbcopy'')
    # (pkgs.runCommand "pbpaste" { } ''mkdir -p $out/bin; ln -s ${pkgs.wl-clipboard}/bin/wl-paste $out/bin/pbpaste'')
    # (pkgs.runCommand "say" { } ''mkdir -p $out/bin; ln -s ${pkgs.espeak}/bin/espeak $out/bin/say'')
]