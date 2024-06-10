local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config = {
  hide_tab_bar_if_only_one_tab  = true,
  window_decorations = "RESIZE",
  font = wezterm.font('Dank Mono', { weight = 'Regular', italic = false }),
  font_size = 14.0,
  initial_cols = 150,
  initial_rows = 35,

  colors = {
    foreground = '#000000',
    background = '#FFFFFF',

    cursor_bg = '#8C8C8C',
    cursor_fg = '#FFFFFF',
    cursor_border = '#8C8C8C',

    selection_fg = '#FFFFFF',
    selection_bg = '#365B8D',

    scrollbar_thumb = '#333333',

    split = '#333333',

    ansi = {
      '#333333',   -- black
      '#850031',   -- red
      '#5D7300',   -- green
      '#8E4400',   -- yellow
      '#2F537E',   -- blue
      '#6F4E87',   -- magenta
      '#25767A',   -- cyan
      '#CCCCCC',   -- white
    },

    brights = {
      '#5A5958',   -- black
      '#D5004C',   -- red
      '#7F9D00',   -- green
      '#D46000',   -- yellow
      '#3F73CC',   -- blue
      '#946DAB',   -- magenta
      '#409FA1',   -- cyan
      '#E6E6E6',   -- white
    },
}
}


return config
