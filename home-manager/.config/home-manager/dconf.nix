{
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

}
