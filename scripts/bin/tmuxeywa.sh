#!/bin/bash
SESSION=$USER

tmux -2 new-session -d -s $SESSION

tmux split-window -h
tmux select-pane -t 0
tmux resize-pane -R 23
tmux send-keys "c && eywa" C-m
tmux select-pane -t 1
tmux send-keys "c && chromix-server" C-m
tmux split-window -v
tmux resize-pane -U 17
tmux send-keys "c && cd ~/Documents/Projects/eywa/ && source bin/activate && cd eywa && gg" C-m

# Attach to session
tmux -2 attach-session -t $SESSION
