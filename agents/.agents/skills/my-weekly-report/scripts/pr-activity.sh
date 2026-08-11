#!/usr/bin/env bash
set -euo pipefail

# Find PR activity (authored + reviewed) for the user within a date range,
# across a list of repos plus an org-wide search fallback for authored PRs.
#
# Usage: pr-activity.sh <start-date> <end-date> <repo1> <repo2> ...
#   dates in YYYY-MM-DD format (inclusive)
#
# Requires dangerouslyDisableSandbox: true (network calls to GitHub).
#
# Outputs JSON: { "authored": [...], "reviewed": [...] }
#   authored: PRs created or merged within the range
#   reviewed: PRs where the user left a review or comment within the range
#
# Known gh gotchas this script works around:
#   - `gh search prs --state all` is invalid (search only accepts
#     open/closed, not "all") -- for the org-wide authored search, --state
#     is omitted entirely, which searches every state.
#   - `gh pr list --state all` IS valid and used for per-repo authored PRs.
#   - `gh ... --jq EXPR` takes exactly one argument -- it does NOT accept
#     `--arg` like real jq does. To pass variables into the jq filter, pipe
#     the JSON to a separate `jq` invocation instead.
#   - A PR's own `updatedAt` is not proof the *user's* activity on it falls
#     in the target window (someone else may have touched it since) -- each
#     reviewed-PR candidate is fetched individually and its actual
#     review/comment timestamps are checked against the range.

start="$1"; shift
end="$1"; shift
repos=("$@")

start_ts="${start}T00:00:00Z"
end_ts="${end}T23:59:59Z"
me=$(gh api user --jq '.login')

# ---- Authored PRs ----
tmpfiles=()
pids=()

for repo in "${repos[@]}"; do
    f=$(mktemp); tmpfiles+=("$f")
    (
        gh pr list --repo "$repo" --author "@me" --state all --limit 30 \
            --json number,title,state,mergedAt,createdAt,url 2>/dev/null > "$f" || echo "[]" > "$f"
    ) &
    pids+=($!)
done

org_f=$(mktemp); tmpfiles+=("$org_f")
(
    gh search prs --author "@me" --owner Veeam-VDC --limit 30 \
        --json number,title,repository,state,mergedAt,createdAt,url 2>/dev/null > "$org_f" || echo "[]" > "$org_f"
) &
pids+=($!)

for pid in "${pids[@]}"; do wait "$pid"; done

authored=$(jq -s --arg s "$start_ts" --arg e "$end_ts" '
    add | unique_by(.url) | map(select(
        (.createdAt >= $s and .createdAt <= $e) or
        (.mergedAt and .mergedAt >= $s and .mergedAt <= $e)
    ))' "${tmpfiles[@]}")

rm -f "${tmpfiles[@]}"

# ---- Reviewed PRs ----
# Phase 1: candidates -- any PR where the user is a reviewer, touched since
# `start`. Phase 2: fetch each candidate's actual review/comment timestamps
# and keep only the ones with real activity by the user inside [start, end].
cand_files=()
pids=()
for repo in "${repos[@]}"; do
    f=$(mktemp); cand_files+=("$f")
    (
        gh search prs --repo "$repo" --reviewed-by "@me" --updated ">=$start" --limit 30 \
            --json number,title,url 2>/dev/null | jq --arg repo "$repo" '[.[] | . + {repo: $repo}]' > "$f" \
            || echo "[]" > "$f"
    ) &
    pids+=($!)
done
for pid in "${pids[@]}"; do wait "$pid"; done

candidates=$(jq -s 'add | unique_by(.url)' "${cand_files[@]}")
rm -f "${cand_files[@]}"

reviewed="[]"
while IFS= read -r pr; do
    [ -z "$pr" ] && continue
    num=$(jq -r '.number' <<<"$pr")
    repo_name=$(jq -r '.repo' <<<"$pr")
    url=$(jq -r '.url' <<<"$pr")
    title=$(jq -r '.title' <<<"$pr")
    last_activity=$(gh pr view "$num" --repo "$repo_name" --json reviews,comments 2>/dev/null \
        | jq -r --arg me "$me" \
            '[(.reviews[], .comments[]) | select(.author.login == $me) | (.submittedAt // .createdAt)] | sort | last // empty' \
        || echo "")
    if [ -n "$last_activity" ] && [[ "$last_activity" > "$start_ts" || "$last_activity" == "$start_ts" ]] \
        && [[ "$last_activity" < "$end_ts" || "$last_activity" == "$end_ts" ]]; then
        reviewed=$(jq --arg url "$url" --arg title "$title" --arg at "$last_activity" \
            '. + [{url:$url, title:$title, at:$at}]' <<<"$reviewed")
    fi
done < <(jq -c '.[]' <<<"$candidates")

jq -n --argjson authored "$authored" --argjson reviewed "$reviewed" '{authored:$authored, reviewed:$reviewed}'
