# VibeDiff Explain Workflow

Explain a specific jj revision by fetching its diff and adding explanatory comments in VibeDiff to document the changes.

## When to use

- User says "explain this commit in vibediff", "add comments to vibediff", "explain this revision in vibediff"
- User provides a jj revision ID and asks to document or explain changes
- User asks to annotate a diff with explanatory comments

## How it works

1. Verify VibeDiff is running and check the current project
2. Fetch the revision diff to understand the changes
3. Add explanatory comments to key lines explaining WHY changes were made
4. Focus on non-obvious rationale, cross-file impacts, and design decisions

## Workflow

### 1. Verify VibeDiff instance

```bash
# Check VibeDiff is running and on correct project
curl -s http://localhost:8888/api/directory
```

### 2. Get revision information

```bash
# Show the commit to understand context
jj show <revision-id>

# Get the diff from VibeDiff
curl -s "http://localhost:8888/api/diff?revision=<revision-id>" | jq '.'
```

### 3. Analyze the changes

Look at the diff structure:
- Which files changed
- What kind of changes (additions, modifications, deletions)
- Size of changes (addition/deletion counts)

### 4. Add explanatory comments

Add comments to key lines explaining:
- **WHY** the change was made (not what - the code shows that)
- Non-obvious design decisions
- Cross-file or cross-service impacts
- Constraints or invariants being maintained
- Workarounds for specific issues

```bash
curl -s -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "revision": "<revision-id>",
    "file": "path/to/file.go",
    "line": 42,
    "content": "Explanation of why this change was made",
    "author": "agent"
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
# 1. Check VibeDiff
curl -s http://localhost:8888/api/directory

# 2. Get revision info
jj show npuuvppmqwuksxpzwwrkovtsusvnvvrn

# 3. Fetch diff
curl -s 'http://localhost:8888/api/diff?revision=npuuvppmqwuksxpzwwrkovtsusvnvvrn' \
  | jq '.files[] | {path, additions, deletions}'

# 4. Add comment to new test file
curl -s -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "revision": "npuuvppmqwuksxpzwwrkovtsusvnvvrn",
    "file": "services/earn/tests/earn-e2e/org-anchoring_test.go",
    "line": 1,
    "author": "agent",
    "content": "E2E test suite validating EARN'\''s org anchor geo routing behavior..."
  }'

# 5. Add comment explaining a key change
curl -s -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "revision": "npuuvppmqwuksxpzwwrkovtsusvnvvrn",
    "file": "services/earn/tests/earn-e2e/api.go",
    "line": 53,
    "author": "agent",
    "content": "Added variadic opts parameter to allow passing additional HTTP client options..."
  }'
```

## Notes

- Always set `"author": "agent"` in comment payloads
- The `file` field is required for root comments (comments without `parentId`)
- Comments are persisted in `~/.config/vibediff/comments/<sha256-of-path>.json`
- Focus on WHY over WHAT - the code shows what changed
- Aim for 10-20 comments per revision depending on complexity
- Use the commit message and PR context to inform comment content
