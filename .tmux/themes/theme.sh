# Theme!

# panes
set -g pane-border-fg black
set -g pane-active-border-fg brightred

# status line
set -g status-utf8 on
set -g status-justify left
set -g status-bg default
set -g status-fg colour12
set -g status-interval 2

# messaging
set -g message-fg black
set -g message-bg yellow
set -g message-command-fg blue
set -g message-command-bg black

#window mode
setw -g mode-bg colour6
setw -g mode-fg colour0

# window status
setw -g window-status-format " #F#I:#W#F "
setw -g window-status-current-format " #F#I:#W#F "
setw -g window-status-format "#[fg=magenta]#[bg=black] #I #[bg=cyan]#[fg=colour8] #W "
setw -g window-status-current-format "#[bg=brightmagenta]#[fg=colour8] #I #[fg=colour8]#[bg=colour14] #W "
setw -g window-status-current-bg colour0
setw -g window-status-current-fg colour11
setw -g window-status-current-attr dim
setw -g window-status-bg green
setw -g window-status-fg black
setw -g window-status-attr reverse

# loud or quiet?
set-option -g visual-activity on
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
set -g pane-border-bg colour235
set -g pane-border-fg colour238
set -g pane-active-border-bg colour236
set -g pane-active-border-fg colour51

# statusbar
set -g status-position bottom
set -g status-bg colour234
set -g status-fg colour137
set -g status-attr dim

setw -g window-status-current-fg colour81
setw -g window-status-current-bg colour238
setw -g window-status-current-attr bold
setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '

setw -g window-status-fg colour138
setw -g window-status-bg colour235
setw -g window-status-attr none
setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '

setw -g window-status-bell-attr bold
setw -g window-status-bell-fg colour255
setw -g window-status-bell-bg colour1

# messages
set -g message-attr bold
set -g message-fg colour232
set -g message-bg colour166

# status bar left and right
tm_color_active=colour39
tm_color_inactive=colour241
tm_color_feature=colour198
tm_color_music=colour245
tm_active_border_color=colour39
tm_tunes="#[fg=$tm_color_music]|#(osascript ~/.applescripts/tunes.scpt)|"
tm_battery="#(~/bin/battery_indicator.sh)"
tm_date="#[fg=$tm_color_inactive] %R %d %b"
tm_host="#[fg=$tm_color_feature,bold]#h"
tm_session_name="$tm_icon #S"
set -g status-left '#[fg=colour233,bg=colour2,bold] *meain* '
set -g status-right $tm_tunes' #[fg=colour233,bg=colour1,bold] '$tm_battery'#[fg=colour233,bg=colour241,bold] %d/%m #[fg=colour233,bg=colour245,bold] %H:%M:%S |'$tm_session_name' '
set -g status-right-length 100
set -g status-left-length 20

# tm_icon="*meain* "

# # separators
# tm_separator_left_bold="◀"
# tm_separator_left_thin="❮"
# tm_separator_right_bold="▶"
# tm_separator_right_thin="❯"

# set -g status-left-length 32
# set -g status-right-length 150
# set -g status-interval 5


# # default statusbar colors
# # set-option -g status-bg colour0
# set-option -g status-fg $tm_color_active
# set-option -g status-bg default
# set-option -g status-attr default

# # default window title colors
# set-window-option -g window-status-fg $tm_color_inactive
# set-window-option -g window-status-bg default
# set -g window-status-format "#I #W"

# # active window title colors
# set-window-option -g window-status-current-fg $tm_color_active
# set-window-option -g window-status-current-bg default
# set-window-option -g  window-status-current-format "#[bold]#I #W"

# # pane border
# set-option -g pane-border-fg $tm_color_inactive
# set-option -g pane-active-border-fg $tm_active_border_color

# # message text
# set-option -g message-bg default
# set-option -g message-fg $tm_color_active

# # pane number display
# set-option -g display-panes-active-colour $tm_color_active
# set-option -g display-panes-colour $tm_color_inactive

# clock
# set-window-option -g clock-mode-colour $tm_color_active

