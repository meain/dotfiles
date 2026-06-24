#!/usr/bin/env bash
set -e

# Fetch PRs authored by user in given repos and org-wide
# Usage: user-prs.sh <username> <repo1> <repo2> ...
# Returns JSON array of PRs created or merged this week

username="$1"
shift
repos=("$@")

week_start=$(date -d "last monday" "+%Y-%m-%dT00:00:00Z" 2>/dev/null || date -v-mon -v0H -v0M -v0S "+%Y-%m-%dT%H:%M:%SZ")

# Fetch from specific repos in parallel
pids=()
tmpfiles=()
for repo in "${repos[@]}"; do
    tmpfile=$(mktemp /tmp/prs-XXXXXX.json)
    tmpfiles+=("$tmpfile")
    (
        GIT_DIR=$(jj git root 2>/dev/null || echo .git)
        gh pr list --repo "$repo" --author "@me" --state all --limit 20 \
            --json number,title,state,mergedAt,createdAt,url 2>/dev/null || echo "[]"
    ) > "$tmpfile" &
    pids+=($!)
done

# Org-wide search
org_tmpfile=$(mktemp /tmp/prs-org-XXXXXX.json)
tmpfiles+=("$org_tmpfile")
(
    gh search prs --author "@me" --owner Veeam-VDC --state all --limit 20 \
        --json number,title,repository,state,mergedAt,createdAt,url 2>/dev/null || echo "[]"
) > "$org_tmpfile" &
pids+=($!)

# Wait for all
for pid in "${pids[@]}"; do
    wait "$pid"
done

# Combine and filter to this week
jq -s --arg week_start "$week_start" 'add | unique_by(.url) | map(
    select(
        (.createdAt >= $week_start) or 
        (.mergedAt and .mergedAt >= $week_start)
    )
)' "${tmpfiles[@]}"

# Cleanup
rm -f "${tmpfiles[@]}"
