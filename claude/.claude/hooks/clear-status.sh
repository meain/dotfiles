#!/usr/bin/env bash
# Remove the pid-keyed status file on /clear so stale status doesn't linger.
# Triggered by SessionStart (matcher "clear").

INPUT=$(cat)

[ -z "${TMUX_PANE:-}" ] && exit 0

# This hook runs as a child of the claude process itself, but walk up a few
# levels just in case an intermediate shell/wrapper is in between.
find_claude_pid() {
  p="$PPID"
  for _ in 1 2 3 4 5; do
    [ -z "$p" ] && break
    base=$(basename "$(ps -o comm= -p "$p" 2>/dev/null)" 2>/dev/null)
    if [ "$base" = "claude" ]; then
      echo "$p"
      return
    fi
    p=$(ps -o ppid= -p "$p" 2>/dev/null | tr -d ' ')
    [ -z "$p" ] || [ "$p" = "1" ] && break
  done
  echo "$PPID"
}
PID=$(find_claude_pid)
[ -z "$PID" ] && exit 0

rm -f "$HOME/.local/share/aistatus/pid-${PID}.json"
