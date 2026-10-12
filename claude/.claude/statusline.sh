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

# State is kept per session so concurrent sessions don't clobber each other.
# Context drops are NOT used to detect /clear: auto-compaction drops context
# just as far and would zero the cost. If the reported cost ever goes down,
# the counter was reset, so the baseline is discarded.
STATE_DIR="$HOME/.claude/statusline-state"
mkdir -p "$STATE_DIR"
STATE_KEY="${SESSION_ID:-default}"
LAST_COST_FILE="$STATE_DIR/$STATE_KEY.last_cost"
COST_BASELINE_FILE="$STATE_DIR/$STATE_KEY.cost_baseline"

LAST_COST=$(cat "$LAST_COST_FILE" 2>/dev/null || echo "0")
if awk -v c="$CURRENT_COST_USD" -v l="$LAST_COST" 'BEGIN{exit !(c<l)}'; then
  echo "0" > "$COST_BASELINE_FILE"
fi
echo "$CURRENT_COST_USD" > "$LAST_COST_FILE"

BASELINE=$(cat "$COST_BASELINE_FILE" 2>/dev/null || echo "0")

# Display cost in USD (no conversion needed)
COST=$(echo "$CURRENT_COST_USD $BASELINE" | awk '{v=($1-$2); printf "%.2f", (v<0)?0:v}')

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
  PARTS="${PARTS:+$PARTS }\$${COST}"
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
