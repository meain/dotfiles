# Tmux theme!

# panes
set -g pane-border-fg black
set -g pane-active-border-fg brightred

# status line
set -g status-justify left
set -g status-bg colour232
set -g status-fg colour12
set -g status-interval 1

# messaging
set -g message-fg white
set -g message-bg yellow
set -g message-command-fg blue
set -g message-command-bg black

#window mode
setw -g mode-bg colour6
setw -g mode-fg colour232

# window status
setw -g window-status-format " #F#I:#W#F "
setw -g window-status-current-format " #F#I:#W#F "
setw -g window-status-format "#[fg=magenta]#[bg=colour232] #I #[bg=colour232]#[fg=colour231] #W "
setw -g window-status-current-format "#[bg=color8]#[fg=colour231] #I #[fg=colour231]#[bg=colour232] #W "
setw -g window-status-current-bg colour232
setw -g window-status-current-fg colour11
setw -g window-status-current-attr dim
setw -g window-status-bg green
setw -g window-status-fg white
setw -g window-status-attr reverse

# loud or quiet?
set-option -g visual-activity off
set-option -g visual-bell off
set-option -g visual-silence off
set-window-option -g monitor-activity on
set-option -g bell-action none

set -g default-terminal "screen-256color"

# mode
setw -g clock-mode-colour colour135
setw -g mode-attr bold
setw -g mode-fg colour196
setw -g mode-bg colour238

# pane
set -g pane-border-bg colour232
set -g pane-border-fg colour238
set -g pane-active-border-bg colour232
set -g pane-active-border-fg colour51

# statusbar
set -g status-position top
set -g status-bg colour232
set -g status-fg colour137
set -g status-attr dim

setw -g window-status-current-fg colour81
setw -g window-status-current-bg colour232
setw -g window-status-current-attr bold
setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '

setw -g window-status-fg colour138
setw -g window-status-bg colour232
setw -g window-status-attr none
setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '

setw -g window-status-bell-attr bold
setw -g window-status-bell-fg colour255
setw -g window-status-bell-bg colour232

# messages
set -g message-attr bold
set -g message-fg colour232
set -g message-bg colour232

# status bar left and right
tm_color_active=colour39
tm_color_inactive=colour32
tm_active_border_color=colour39
tm_color_feature=colour198
tm_color_music=colour245
tm_tunes="#[fg=$tm_color_music]#(osascript ~/.applescripts/tunes.scpt)"
tm_battery="#(python ~/bin/batchargetmux.py)"
tm_date="#[fg=$tm_color_inactive] %R %d %b"
tm_host="#[fg=$tm_color_feature,bold]#h"
tm_session_name="$tm_icon #S"
set -g status-left '#[fg=colour231,bg=colour232,bold] *meain* #[bg=black] '
set -g status-right $tm_tunes' #[fg=colour15,bg=colour232,bold] '$tm_battery'#[fg=colour15,bg=colour232,bold] %d/%m #[fg=colour15,bg=colour232,bold] %r |'$tm_session_name' '
set -g status-right-length 100
set -g status-left-length 20
