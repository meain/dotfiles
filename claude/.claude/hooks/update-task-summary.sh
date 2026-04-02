#!/usr/bin/env bash
# Claude Code hook (UserPromptSubmit): Summarize current task(s) via Haiku, write per-session file
set -euo pipefail

INPUT=$(cat)

SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
TRANSCRIPT=$(echo "$INPUT" | jq -r '.transcript_path // empty')
if [ -z "$SESSION_ID" ]; then
  exit 0
fi

SUMMARY_FILE="$HOME/.claude/task-summaries/${SESSION_ID}"

# Collect all user prompts from the transcript for full context
USER_PROMPTS=""
if [ -n "$TRANSCRIPT" ] && [ -f "$TRANSCRIPT" ]; then
  USER_PROMPTS=$(jq -r 'select(.type == "user") | .message.content | if type == "array" then [.[] | if type == "string" then . elif .type == "text" then .text else empty end] | join("\n") else tostring end' "$TRANSCRIPT" 2>/dev/null | head -80)
fi

# Also append the current prompt (it may not be in the transcript yet)
CURRENT_PROMPT=$(echo "$INPUT" | jq -r '.prompt // empty')
if [ -n "$CURRENT_PROMPT" ]; then
  USER_PROMPTS="${USER_PROMPTS}
${CURRENT_PROMPT}"
fi

if [ -z "$USER_PROMPTS" ]; then
  exit 0
fi

# Truncate to ~3000 chars to keep the API call small
CONTEXT="${USER_PROMPTS:0:3000}"

PAYLOAD=$(jq -n --arg text "$CONTEXT" '{
  model: "claude-haiku-4-5-20251001",
  max_tokens: 150,
  messages: [{
    role: "user",
    content: ("Below are ALL user prompts from a coding session, in chronological order. Summarize the OVERARCHING task the user is working on.\n\nRules:\n- Identify the main goal/task from the session — follow-up questions (\"what does X do?\", \"show me Y\", \"can you check Z?\") are part of the same task, not separate tasks\n- Write one short phrase per distinct GOAL (max 8 words each), not per prompt\n- A follow-up question about error types, implementation details, or sub-components is NOT a new task — it is research for the main task\n- A new task is when the user explicitly starts something unrelated (\"now let us work on...\", \"switching to...\", or a clearly unrelated request)\n- If the user pivoted to a genuinely new task, show only current task(s) — drop completed/abandoned ones\n- If working on multiple unrelated things concurrently, list each on its own line\n- If it is all one task (even with many follow-up questions), just one line describing the main goal\n- No bullets, no numbering, no quotes, no preamble — just the phrase(s), one per line\n\nExamples:\n- Prompts: \"add retry logic to cosmosdb\" then \"what error types does cosmosdbutils expose?\" → \"Adding retry logic to CosmosDB store\"\n- Prompts: \"fix the earn e2e test\" then \"now update the deploy script\" → \"Updating deploy script\"\n- Prompts: \"fix earn e2e\" then \"also the deploy script is broken\" → \"Fixing earn e2e test\\nFixing deploy script\"\n\nUser prompts:\n" + $text)
  }]
}')

SUMMARY=$(curl -s --max-time 5 https://api.anthropic.com/v1/messages \
  -H "x-api-key: $ANTHROPIC_API_KEY" \
  -H "anthropic-version: 2023-06-01" \
  -H "content-type: application/json" \
  -d "$PAYLOAD" | jq -r '.content[0].text // empty') || true

if [ -n "$SUMMARY" ]; then
  echo "$SUMMARY" > "$SUMMARY_FILE"
fi
