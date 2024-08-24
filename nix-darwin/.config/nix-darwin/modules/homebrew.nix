{ ... }:
{
  homebrew = {
    enable = true;

    onActivation = {
      cleanup = "uninstall"; # uninstall any not listed here
      # upgrade = true; # note idempotent anymore
    };

    global.brewfile = true;

    masApps = { }; # from mac app store
    taps = [
      "railwaycat/emacsmacport"
    ];
    brews = [
      "emacs-mac"
      "choose-gui" # dmenu alternative
    ];
    casks = [
      "hammerspoon" # make macOS usable
      "hiddenbar" # hide items in bar
      "maccy" # clipboard history
      "caffeine" # caffinate

      "microsoft-teams"
      "slack"
      "signal"

      "logseq"
      "obsidian"
      "ollama"
      "lm-studio"
      "zed"
      "wezterm"
      "vscodium"
      "thunderbird"

      "firefox"
      "google-chrome"
      # "spotify"
      # "overkill" # do not let Apple Music open

      # "http-toolkit" # intercept http traffic
      # "utm" # virtualization
      "rectangle" # window management
      "tailscale" # vpn
    ];
  };
}
