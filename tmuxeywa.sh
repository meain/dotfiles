#!/bin/bash
SESSION=$USER

tmux -2 new-session -d -s $SESSION

tmux split-window -h
tmux select-pane -t 0
tmux resize-pane -R 30
tmux send-keys "eywa" C-m
tmux select-pane -t 1
tmux send-keys "chromix-server" C-m
tmux split-window -v
tmux send-keys "pagekite.py 4500 meain.pagekite.me" C-m

# Attach to session
tmux -2 attach-session -t $SESSION
