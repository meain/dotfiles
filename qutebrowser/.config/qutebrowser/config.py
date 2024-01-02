# References
# https://www.qutebrowser.org/doc/help/configuring.html
# https://github.com/neeasade/dotfiles/blob/master/net/.config/qutebrowser/config.py

config.load_autoconfig(False)

c.fonts.default_family = "Sarasa Mono SC Nerd"
c.fonts.default_size = "10pt"

c.content.javascript.can_access_clipboard = True  # enable clipboard access

# Load user stylesheet for all web pages
c.content.user_stylesheets = ['~/.config/qutebrowser/userscripts/override_font.css']
