{ ... }:
{
  homebrew = {
    enable = true;
    global.brewfile = true;
    masApps = { }; # from mac app store
    taps = [
      "jimeh/emacs-builds"
    ];
    casks = [
      # "firefox" # installed via nix
      "hammerspoon"
      "logseq"
      "microsoft-teams"
      "zed"
      "emacs-app-nightly"
      "google-chrome"
      "slack"
      "hiddenbar"
      "spotify"
      "maccy" # clipboard history
      "choose-gui" # dmenu alternative
      "http-toolkit"
    ];
  };
}
