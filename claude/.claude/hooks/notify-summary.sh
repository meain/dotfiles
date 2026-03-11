#!/usr/bin/env bash
# Claude Code hook: Summarize last assistant message via Haiku and send notification
set -euo pipefail

INPUT=$(cat)

# Get last assistant message: directly from Stop hook, or from transcript for Notification
TEXT=$(echo "$INPUT" | jq -r '.last_assistant_message // empty')

if [ -z "$TEXT" ]; then
  TRANSCRIPT=$(echo "$INPUT" | jq -r '.transcript_path // empty')
  if [ -n "$TRANSCRIPT" ] && [ -f "$TRANSCRIPT" ]; then
    TEXT=$(tac "$TRANSCRIPT" | while IFS= read -r line; do
      role=$(echo "$line" | jq -r '.type // empty')
      if [ "$role" = "assistant" ]; then
        echo "$line" | jq -r '
          [.message.content[]? |
            if type == "string" then .
            elif .type == "text" then .text
            else empty end
          ] | join("\n")'
        break
      fi
    done)
  fi
fi

if [ -z "$TEXT" ]; then
  notify "Claude Code" "Waiting for input"
  exit 0
fi

PAYLOAD=$(jq -n --arg text "$TEXT" '{
  model: "claude-haiku-4-5-20251001",
  max_tokens: 120,
  messages: [{
    role: "user",
    content: ("Summarize the following Claude Code assistant message in one short sentence (max 15 words) for a desktop notification. If the assistant is asking the user a question or waiting for input, mention that in the summary. No quotes or preamble, just the summary:\n\n" + $text)
  }]
}')

SUMMARY=$(curl -s https://api.anthropic.com/v1/messages \
  -H "x-api-key: $ANTHROPIC_API_KEY" \
  -H "anthropic-version: 2023-06-01" \
  -H "content-type: application/json" \
  -d "$PAYLOAD" | jq -r '.content[0].text // "Task update"') || SUMMARY="Task update"

notify "Claude Code" "$SUMMARY"
