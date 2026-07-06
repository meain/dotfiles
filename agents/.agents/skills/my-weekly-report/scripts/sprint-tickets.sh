#!/usr/bin/env bash
set -e

# Fetch the current user's tickets in the active EARN sprint, grouped by status.
# Returns JSON: { "sprint": {id, name}, "done": [...], "in_progress": [...], "to_do": [...] }
# Each ticket is { "key", "status", "summary" }.
# Statuses map: Done -> done, In Progress -> in_progress, everything else
# (To Do, Blocked, ...) -> to_do (original status preserved on each ticket).

me=$(jira me)

# Find the active sprint whose name contains "EARN" (the user's primary sprint).
# The --table sprint list ignores --delimiter, so parse the tab-separated output.
sprint_line=$(jira sprint list --table --plain --no-headers --state active \
    --columns id,name 2>/dev/null | grep -i "earn" | head -1)

if [ -z "$sprint_line" ]; then
    echo "No active EARN sprint found" >&2
    exit 1
fi

sprint_id=$(printf '%s' "$sprint_line" | cut -f1)
sprint_name=$(printf '%s' "$sprint_line" | cut -f2)

# Fetch this user's tickets in that sprint (pipe-delimited to survive spaces in
# summaries; a To Do status contains a space, so a custom delimiter is required).
tickets=$(jira sprint list "$sprint_id" -a"$me" --plain --no-headers --no-truncate \
    --columns key,status,summary --delimiter '|' 2>/dev/null || true)

printf '%s\n' "$tickets" | jq -R -s \
    --arg id "$sprint_id" --arg name "$sprint_name" '
    split("\n")
    | map(select(length > 0) | split("|") | {
        key: .[0],
        status: .[1],
        summary: (.[2:] | join("|"))
      })
    | {
        sprint: { id: $id, name: $name },
        done:        map(select(.status == "Done")),
        in_progress: map(select(.status == "In Progress")),
        to_do:       map(select(.status != "Done" and .status != "In Progress"))
      }'
