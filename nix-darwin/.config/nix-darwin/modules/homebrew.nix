{ ... }:
{
  homebrew = {
    enable = true;

    onActivation = {
      cleanup = "uninstall"; # uninstall any not listed here
      # upgrade = true; # not idempotent anymore
    };

    global.brewfile = true;

    masApps = { }; # from mac app store
    taps = [
      # "railwaycat/emacsmacport"
      # "d12frosted/emacs-plus"
      "zackelia/formulae" # for bclm
    ];
    brews = [
      # emacs-mac has not been updated in a long time
      # {
      #   name = "emacs-mac";
      #   args = [ "with-natural-title-bar" "with-starter" "with-mac-metal" "with-native-comp"];
      # }
      # {
      #   name = "emacs-plus@31";
      #   args = [ "with-imagemagick" "with-native-comp" ];
      #   link = true;
      # }

      "choose-gui" # dmenu alternative
      "bclm" # limit battery charge
    ];
    casks = [
      "hammerspoon" # make macOS usable
      # "hiddenbar" # hide items in bar
      "maccy" # clipboard history
      "caffeine" # caffinate

      "microsoft-teams"
      "slack"
      "signal"

      # "emacs" # also emacs@pretest
      # "emacs-app" # ???
      # "logseq"
      "obsidian"
      "ollama"
      # "lm-studio"
      # "anythingllm"
      # "cursor"
      "zed"
      "wezterm"
      "vscodium"
      # "thunderbird"

      "firefox"
      "google-chrome"
      # "spotify"
      # "overkill" # do not let Apple Music open

      # "keycastr"
      # "http-toolkit" # intercept http traffic
      # "utm" # virtualization
      # "rectangle" # window management
      "tailscale" # vpn
      "chatgpt" # quick shortcut is awesome
    ];
  };
}
