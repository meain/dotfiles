#!/usr/bin/env bash
set -euo pipefail

INPUT=$(cat)

CTX=$(echo "$INPUT" | jq -r '.context_window.used_percentage // 0 | round')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
# USD to INR rate as of 2026-04-24 (~94.13); update periodically
COST=$(echo "$INPUT" | jq -r '.cost.total_cost_usd // 0 | . * 94.13 | round')

PARTS=""

if [ "$CTX" -gt 0 ] 2>/dev/null; then
  PARTS="ctx:${CTX}%"
fi

if [ "$COST" != "0" ] 2>/dev/null; then
  PARTS="${PARTS:+$PARTS }₹${COST}"
fi

SUMMARY_DIR="$HOME/.claude/task-summaries"
if [ -n "$SESSION_ID" ] && [ -f "$SUMMARY_DIR/$SESSION_ID" ]; then
  SUMMARY=$(cat "$SUMMARY_DIR/$SESSION_ID" 2>/dev/null || true)
  if [ -n "$SUMMARY" ]; then
    PARTS="${PARTS:+$PARTS }$SUMMARY"
  fi
fi

if [ -n "$PARTS" ]; then
  echo "$PARTS"
fi
