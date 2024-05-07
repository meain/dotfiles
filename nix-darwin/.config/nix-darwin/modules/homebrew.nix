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
      "firefox"
      "hammerspoon"
      "logseq"
      "microsoft-teams"
      "zed"
      "emacs-app"
      "google-chrome"
      "slack"
      "hiddenbar"
      "ext4fuse" # ext4 support
    ];
  };
}
