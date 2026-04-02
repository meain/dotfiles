#!/usr/bin/env bash
set -euo pipefail

INPUT=$(cat)

SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')

# Try to read task summary for this session
SUMMARY=""
SUMMARY_DIR="$HOME/.claude/task-summaries"
if [ -n "$SESSION_ID" ] && [ -f "$SUMMARY_DIR/$SESSION_ID" ]; then
  SUMMARY=$(cat "$SUMMARY_DIR/$SESSION_ID" 2>/dev/null || true)
fi

if [ -n "$SUMMARY" ]; then
  echo "$SUMMARY"
fi
