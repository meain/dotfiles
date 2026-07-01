#!/usr/bin/env bash
# Update Claude session status for tmux window status display.
# Triggered by Stop, Notification, UserPromptSubmit, and PostToolUse hooks.
# Writes to ~/.local/share/claudestatus/window-<window_id>.json

INPUT=$(cat)

SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
[ -z "$SESSION_ID" ] && exit 0

# Resolve the tmux window this Claude session is running in
WINDOW_ID=""
if [ -n "${TMUX_PANE:-}" ]; then
  WINDOW_ID=$(tmux display-message -p -t "$TMUX_PANE" '#{window_id}' 2>/dev/null || true)
fi
[ -z "$WINDOW_ID" ] && exit 0

HOOK_EVENT=$(echo "$INPUT" | jq -r '.hook_event_name // empty')
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')
PROJECT=$(basename "$CWD")

case "$HOOK_EVENT" in
  Stop)         STATUS="idle" ;;
  Notification) STATUS="waiting" ;;
  *)            STATUS="working" ;;
esac

STATUS_DIR="$HOME/.local/share/claudestatus"
mkdir -p "$STATUS_DIR"

TMP=$(mktemp "$STATUS_DIR/.tmp.XXXXXX")
jq -n \
  --arg session_id "$SESSION_ID" \
  --arg window_id "$WINDOW_ID" \
  --arg status "$STATUS" \
  --arg hook_event "$HOOK_EVENT" \
  --arg project "$PROJECT" \
  --argjson timestamp "$(date +%s)" \
  '{session_id: $session_id, window_id: $window_id, status: $status, hook_event: $hook_event, project: $project, timestamp: $timestamp}' \
  > "$TMP" && mv "$TMP" "$STATUS_DIR/window-${WINDOW_ID}.json"
