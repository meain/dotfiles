#!/usr/bin/env bash
set -euo pipefail

INPUT=$(cat)

CTX=$(echo "$INPUT" | jq -r '.context_window.used_percentage // 0 | round')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
CURRENT_COST_USD=$(echo "$INPUT" | jq -r '.cost.total_cost_usd // 0')
MODEL=$(echo "$INPUT" | jq -r '.model.display_name // empty')
DURATION_MS=$(echo "$INPUT" | jq -r '.cost.total_duration_ms // 0')
LINES_ADDED=$(echo "$INPUT" | jq -r '.cost.total_lines_added // 0')
LINES_REMOVED=$(echo "$INPUT" | jq -r '.cost.total_lines_removed // 0')

# Track cost baseline so /clear resets the displayed cost.
# We detect /clear by a significant drop in context window usage.
STATE_DIR="$HOME/.claude/statusline-state"
mkdir -p "$STATE_DIR"
LAST_CTX_FILE="$STATE_DIR/last_ctx"
COST_BASELINE_FILE="$STATE_DIR/cost_baseline"

LAST_CTX=$(cat "$LAST_CTX_FILE" 2>/dev/null || echo "0")

# If context dropped significantly (>15% → <5%), treat it as a /clear
if [ "$LAST_CTX" -gt 15 ] && [ "$CTX" -lt 5 ]; then
  echo "$CURRENT_COST_USD" > "$COST_BASELINE_FILE"
fi

echo "$CTX" > "$LAST_CTX_FILE"

BASELINE=$(cat "$COST_BASELINE_FILE" 2>/dev/null || echo "0")

# USD to INR rate as of 2026-04-24 (~94.13); update periodically
COST=$(echo "$CURRENT_COST_USD $BASELINE 94.13" | awk '{v=($1-$2)*$3; printf "%d", (v<0)?0:v}')

# Format duration: show as Xm or Xs
DURATION=""
if [ "$DURATION_MS" -gt 0 ] 2>/dev/null; then
  DURATION=$(echo "$DURATION_MS" | awk '{
    s = int($1/1000)
    d = int(s/86400); s -= d*86400
    h = int(s/3600);  s -= h*3600
    m = int(s/60);    s -= m*60
    out = ""
    if (d > 0) out = out d "d"
    if (h > 0) out = out h "h"
    if (m > 0) out = out m "m"
    if (out == "") out = s "s"
    print out
  }')
fi

PARTS=""

if [ -n "$MODEL" ]; then
  PARTS="${MODEL} |"
fi

if [ "$CTX" -gt 0 ] 2>/dev/null; then
  PARTS="${PARTS:+$PARTS }${CTX}%"
fi

if [ "$COST" != "0" ] 2>/dev/null; then
  PARTS="${PARTS:+$PARTS }₹${COST}"
fi

if [ -n "$DURATION" ]; then
  PARTS="${PARTS:+$PARTS }${DURATION}"
fi

if [ "$LINES_ADDED" -gt 0 ] || [ "$LINES_REMOVED" -gt 0 ] 2>/dev/null; then
  PARTS="${PARTS:+$PARTS }+${LINES_ADDED}/-${LINES_REMOVED}"
fi

SUMMARY_DIR="$HOME/.claude/task-summaries"
SUMMARY=""
if [ -n "$SESSION_ID" ] && [ -f "$SUMMARY_DIR/$SESSION_ID" ]; then
  SUMMARY=$(cat "$SUMMARY_DIR/$SESSION_ID" 2>/dev/null || true)
fi

if [ -n "$PARTS" ]; then
  echo "$PARTS"
fi

if [ -n "$SUMMARY" ]; then
  echo "$SUMMARY"
fi
