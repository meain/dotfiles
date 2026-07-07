#!/usr/bin/env bash
# Update Claude session status for tmux window status display.
# Triggered by Stop, Notification, UserPromptSubmit, and PostToolUse hooks.
# Writes to ~/.local/share/aistatus/pid-<pid>.json
#
# Readers discover live pi/claude processes from the tmux pane tree and use
# this file only as a status/project lookup keyed by pid, so it stays
# accurate across pane splits, moves, and restarts.

INPUT=$(cat)

SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
[ -z "$SESSION_ID" ] && exit 0

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

HOOK_EVENT=$(echo "$INPUT" | jq -r '.hook_event_name // empty')
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')
PROJECT=$(basename "$CWD")

case "$HOOK_EVENT" in
  Stop)         STATUS="idle" ;;
  Notification) STATUS="waiting" ;;
  *)            STATUS="working" ;;
esac

STATUS_DIR="$HOME/.local/share/aistatus"
mkdir -p "$STATUS_DIR"

TMP=$(mktemp "$STATUS_DIR/.tmp.XXXXXX")
jq -n \
  --arg session_id "$SESSION_ID" \
  --argjson pid "$PID" \
  --arg status "$STATUS" \
  --arg hook_event "$HOOK_EVENT" \
  --arg project "$PROJECT" \
  --argjson timestamp "$(date +%s)" \
  '{session_id: $session_id, pid: $pid, status: $status, hook_event: $hook_event, project: $project, timestamp: $timestamp}' \
  > "$TMP" && mv "$TMP" "$STATUS_DIR/pid-${PID}.json"
