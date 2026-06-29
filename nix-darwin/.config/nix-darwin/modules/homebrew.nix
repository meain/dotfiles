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
      {
        name = "d12frosted/emacs-plus";
        trusted = true;
      }
      {
        name = "jimeh/emacs-builds";
        trusted = true;
      }
      {
        name = "keith/formulae"; # for reminders-cli
        trusted = true;
      }
      {
        name = "databricks/tap";
        trusted = true;
      }
    ];
    brews = [
      "choose-gui" # dmenu alternative
      "reminders-cli" # manage apple reminders from cli

      "ispell" # Emacs does not pick up nix version
      "ical-buddy" # Read from macos calendar
      "databricks" # databricks cli
      "pi-coding-agent" # pi coding agent
    ];
    casks = [
      # "emacs-plus-app" # stable emacs
      "emacs-app-nightly"

      "hammerspoon" # make macOS usable
      # "hiddenbar" # hide items in bar
      "maccy" # clipboard history
      "caffeine" # caffinate
      "monitorcontrol" # control brightness of external monitors

      "microsoft-teams"
      "slack"
      "signal"

      "obsidian"
      # "ollama"
      # "lm-studio"
      "zed"
      "vscodium" # a gui editor
      # "visual-studio-code" # copilot
      "orbstack" # better docker
      "kitty" # terminal emulator

      "firefox"
      "google-chrome"
      "spotify"
      # "overkill" # do not let Apple Music open

      # "http-toolkit" # intercept http traffic
      # "utm" # virtualization
      # "rectangle" # window management
      # "tailscale" # vpn
      # "chatgpt" # quick shortcut is awesome
      # "meetingbar" # next meeting in bar
      "obs" # screen recording
      "keycastr" # show keys being typed (for recording)

      # ai agents
      "claude"
      "claude-code"
    ];
  };
}
