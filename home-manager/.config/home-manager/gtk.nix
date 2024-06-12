{
  enable = true;
  font.name = "${defaultFont}"; # gui font
  theme.name = "Clearlooks";
  iconTheme.name = "breeze";
  cursorTheme = {
    name = "Graphite light Cursors"; # original: DMZ-White
    package = pkgs.graphite-cursors;
  };

  gtk3.extraConfig = {
    gtk-menu-images = 1;
    gtk-xft-hinting = 1;
    gtk-xft-rgba = "rgb";
    gtk-application-prefer-dark-theme = 0;
    gtk-decoration-layout = ":";
    gtk-toolbar-style = "GTK_TOOLBAR_ICONS";
    gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
    gtk-enable-even-sounds = 1;
    gtk-enable-input-feedback-sounds = 1;
    gtk-button-images = 1;
  };
}
