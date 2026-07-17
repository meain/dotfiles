---
name: vibediff-review
description: >
  Run /code-review on the current diff and post the findings as inline
  comments in VibeDiff, tagged as agent:reviewer. Use when the user says
  /vibediff-review, "review and post to vibediff", "code review this into
  vibediff", or asks for a code review to show up as VibeDiff comments
  instead of a chat summary.
---

# VibeDiff Review Workflow

Run the `code-review` skill against the current diff, then post each surviving finding as an inline VibeDiff comment authored as `agent:reviewer`.

## When to use

- User says "/vibediff-review", "review and post to vibediff", "code review this into vibediff"
- User wants review findings to land as VibeDiff threads instead of (or in addition to) a chat summary

## Workflow

### 1. Run the code review

Invoke the `code-review` skill on the current diff (pass through any effort level or args the user gave, e.g. `/vibediff-review high`). Do **not** pass `--comment` (that posts to GitHub PRs) or `--fix` — this skill owns posting the output, to VibeDiff.

This produces a set of verified findings (file, line, summary, failure_scenario, category, verdict).

If there are no findings, tell the user and stop — don't post anything to VibeDiff.

### 2. Verify VibeDiff instance and find the project

```bash
DIR="/path/to/repo"

curl -s http://localhost:8888/api/directories | grep -q "$DIR" || \
  curl -s -X POST http://localhost:8888/api/directories \
    -H 'Content-Type: application/json' \
    -d "{\"directory\":\"$DIR\"}"

curl -s "http://localhost:8888/api/directory?directory=$DIR"
```

### 3. Resolve the revision

Leave `revision` empty to attach comments to the working-copy diff (the common case right after a review). If the user asked to review a specific commit, resolve it to a jj change ID first — VibeDiff uses jj change IDs, not git hashes:

```bash
REV=$(jj log --no-graph -r '<revision>' --template 'change_id' -R $DIR)
```

### 4. Post each finding as a comment

For every finding, post one comment at its `file`/`line`. Combine `summary` and `failure_scenario` into the content so the reader gets both the defect and a concrete failure case. Always set `"author": "agent"` and `"authorName": "reviewer"` — the UI renders this as `agent:reviewer`, distinguishing it from other agent comment types (e.g. `agent:explain`).

```bash
curl -s -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d "{
    \"directory\": \"$DIR\",
    \"revision\": \"$REV\",
    \"file\": \"services/example-svc/internal/store/example.go\",
    \"line\": 88,
    \"author\": \"agent\",
    \"authorName\": \"reviewer\",
    \"content\": \"<summary>\\n\\n<failure_scenario>\"
  }"
```

If `revision` is empty (working-copy review), omit it from the payload rather than sending an empty string.

### 5. Report back

Tell the user how many findings were posted and to which file/line pairs. Don't repeat the full finding text in chat — that's what the VibeDiff comments are for — a one-line-per-finding recap is enough.

## Notes

- `directory` is required for root comments; `author` must be `"user"` or `"agent"` — always `"agent"` here.
- `authorName` is capped at 50 chars — `"reviewer"` fits and is the fixed tag for this skill; don't vary it per invocation.
- Reuse the same `revision` value across all comments from one review run so they group into a single pass in the VibeDiff UI.
- If `code-review` reports zero findings, do not create a placeholder "no issues found" comment — just tell the user in chat.
