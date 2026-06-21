# VibeDiff Explain Workflow

Explain a specific jj revision by fetching its diff and adding explanatory comments in VibeDiff to document the changes.

## When to use

- User says "explain this commit in vibediff", "add comments to vibediff", "explain this revision in vibediff"
- User provides a jj revision ID and asks to document or explain changes
- User asks to annotate a diff with explanatory comments

## How it works

1. Verify VibeDiff is running and identify the project directory
2. Fetch the revision diff to understand the changes
3. Add explanatory comments to key lines explaining WHY changes were made
4. Focus on non-obvious rationale, cross-file impacts, and design decisions

## Workflow

### 1. Verify VibeDiff instance and find the project

```bash
# List registered directories
curl -s http://localhost:8888/api/directories

# Get backend info for the target repo
curl -s "http://localhost:8888/api/directory?directory=/path/to/repo"
```

If the target directory isn't registered yet, register it first:

```bash
curl -s -X POST http://localhost:8888/api/directories \
  -H 'Content-Type: application/json' \
  -d '{"directory":"/path/to/repo"}'
```

### 2. Get revision information

In a jj repo, VibeDiff uses **jj change IDs** (not git commit hashes). Always resolve the revision to a jj change ID:

```bash
DIR="/path/to/repo"

# Get the jj change ID for a revision (use @ for working copy, or a change ID prefix)
REV=$(jj log --no-graph -r '<revision>' --template 'change_id' -R $DIR)

# Or for the working copy:
REV=$(jj log --no-graph -r '@' --template 'change_id' -R $DIR)

# Show the commit to understand context
jj show $REV -R $DIR

# Get the diff from VibeDiff (directory is required)
curl -s "http://localhost:8888/api/diff?directory=$DIR&revision=$REV" | jq '.'
```

You can also list recent revisions from VibeDiff to confirm the correct ID:
```bash
curl -s "http://localhost:8888/api/revisions?directory=$DIR&limit=5"
```

### 3. Analyze the changes

Look at the diff structure:
- Which files changed
- What kind of changes (additions, modifications, deletions)
- Size of changes (addition/deletion counts)

### 4. Add explanatory comments

`directory` is **required** in the comment body. Add comments to key lines explaining:
- **WHY** the change was made (not what — the code shows that)
- Non-obvious design decisions
- Cross-file or cross-service impacts
- Constraints or invariants being maintained
- Workarounds for specific issues

```bash
curl -s -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "directory": "/path/to/repo",
    "revision": "<revision-id>",
    "file": "path/to/file.go",
    "line": 42,
    "content": "Explanation of why this change was made",
    "author": "EXPLAIN"
  }'
```

## Comment placement strategy

### Focus on:
- New files: Add overview comment at line 1 explaining purpose
- Key architectural changes: Explain the design decision
- Non-obvious logic: Clarify intent
- Cross-cutting changes: Explain how pieces connect
- Refactoring patterns: Document the transformation

### Avoid commenting:
- Self-explanatory changes
- Mechanical refactors (unless the pattern needs explanation)
- Every single line (signal over noise)

## Example flow

```bash
DIR="/Users/meain/dev/veeam/control-plane-backend"

# 1. Ensure directory is registered
curl -s http://localhost:8888/api/directories | grep -q "$DIR" || \
  curl -s -X POST http://localhost:8888/api/directories \
    -H 'Content-Type: application/json' \
    -d "{\"directory\":\"$DIR\"}"

# 2. Resolve jj change ID (VibeDiff uses change IDs, not git commit hashes)
REV=$(jj log --no-graph -r 'npuuvppmqwuksxpzwwrkovtsusvnvvrn' --template 'change_id' -R $DIR)

# 3. Get revision info
jj show $REV -R $DIR

# 4. Fetch diff
curl -s "http://localhost:8888/api/diff?directory=$DIR&revision=$REV" \
  | jq '.files[] | {path, additions, deletions}'

# 5. Add comment to new test file
curl -s -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d "{
    \"directory\": \"$DIR\",
    \"revision\": \"$REV\",
    \"file\": \"services/earn/tests/earn-e2e/org-anchoring_test.go\",
    \"line\": 1,
    \"author\": \"EXPLAIN\",
    \"content\": \"E2E test suite validating EARN's org anchor geo routing behavior...\"
  }"

# 6. Add comment explaining a key change
curl -s -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d "{
    \"directory\": \"$DIR\",
    \"revision\": \"$REV\",
    \"file\": \"services/earn/tests/earn-e2e/api.go\",
    \"line\": 53,
    \"author\": \"EXPLAIN\",
    \"content\": \"Added variadic opts parameter to allow passing additional HTTP client options...\"
  }"
```

## Notes

- Always set `"author": "EXPLAIN"` in comment payloads (not "agent" — this is the user's preferred identifier)
- `directory` and `file` are required for root comments
- Comments are persisted in `~/.config/vibediff/comments/<sha256-of-path>.json`
- Focus on WHY over WHAT — the code shows what changed
- Aim for 10–20 comments per revision depending on complexity
- Use the commit message and PR context to inform comment content
- **In jj repos, always use the jj change ID (from `jj log --template 'change_id'`) as the `revision` field — never use the git commit hash. VibeDiff's `/api/revisions` endpoint lists change IDs; use it to confirm.**
