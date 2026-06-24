#!/usr/bin/env bash
set -e

# Fetch current Jira sprint tickets assigned to current user
# Returns JSON with tickets grouped by status: done, in_progress, to_do

# Find active EARN sprint
sprint_id=$(jira sprint list --plain 2>/dev/null | grep -i earn | grep -i active | head -n 1 | awk '{print $1}')

if [ -z "$sprint_id" ]; then
    echo '{"error": "No active EARN sprint found"}' >&2
    exit 1
fi

# Get tickets in sprint assigned to current user (raw JSON output)
tickets=$(jira issue list -q "assignee = currentUser() AND sprint = $sprint_id" --raw 2>/dev/null)

if [ "$tickets" = "[]" ] || [ -z "$tickets" ]; then
    echo '{"done": [], "in_progress": [], "to_do": []}'
    exit 0
fi

# Parse and group by status
echo "$tickets" | jq '{
    done: (map(select(.fields.status.name == "Done")) | map({
        key,
        status: .fields.status.name,
        summary: .fields.summary
    })),
    in_progress: (map(select(.fields.status.name | test("In Progress|IN REVIEW"))) | map({
        key,
        status: .fields.status.name,
        summary: .fields.summary
    })),
    to_do: (map(select(.fields.status.name == "To Do")) | map({
        key,
        status: .fields.status.name,
        summary: .fields.summary
    }))
}'
