# VibeDiff Integration

Interact with VibeDiff code review instances - fetch review comments, reply to threads, resolve comments, and manage review state.

## When to use

- User says "check vibediff", "get review comments", "vibediff review", "reply to review comment"
- User asks to resolve/close VibeDiff comments
- Working on code changes that have active VibeDiff review comments
- User mentions VibeDiff by name or asks about review feedback

## How it works

VibeDiff runs as a local HTTP server (default port 8888) that can review **multiple repositories** at once. Directory is a **per-request parameter** — there is no server-side "current directory" state. Each API call must include the `directory` query param (or body field for POSTs).

Each instance provides:
- REST API for comments, diffs, and revisions (all require `directory`)
- Directory registry for managing known repos
- WebSocket for real-time updates (messages include `directory`)
- MCP server for agent integration

### Finding the running instance and active directories

```bash
# Check VibeDiff is up and list registered directories
curl http://localhost:8888/api/directories

# Instance-specific docs (always check if API behavior seems wrong)
curl http://localhost:8888/docs

# If on a different port, search all listening ports
lsof -iTCP -sTCP:LISTEN
```

The `/docs` endpoint provides instance-specific API documentation with the current base URL, registered projects, and endpoint reference.

## Directory registry

### List registered directories

```bash
curl http://localhost:8888/api/directories
# ["\/path\/to\/repo-a", "\/path\/to\/repo-b"]
```

### Register a new directory

```bash
curl -X POST http://localhost:8888/api/directories \
  -H 'Content-Type: application/json' \
  -d '{"directory":"/path/to/repo"}'
# Returns: {"directory":"/path/to/repo","backend":"jj"}
```

### Get backend info for a directory

```bash
curl 'http://localhost:8888/api/directory?directory=/path/to/repo'
# {"directory":"/path/to/repo","backend":"git"}
```

## Common operations

### Fetch review comments

All comment endpoints require `?directory=...`.

```bash
DIR="/path/to/repo"

# All comments for a directory
curl "http://localhost:8888/api/review/comments?directory=$DIR"

# Open comments only (across all directories — no dir param needed)
curl http://localhost:8888/api/review/comments/open

# Filter by revision
curl "http://localhost:8888/api/review/comments?directory=$DIR&revision=abc123xyz"

# Filter by file
curl "http://localhost:8888/api/review/comments?directory=$DIR&file=path/to/file.go"
```

### Reply to comments

**Important**: When replying (setting `parentId`), only provide `content`, `author`, and `parentId`. The API automatically inherits `file`, `line`, `lineEnd`, `revision`, and `directory` from the parent comment.

```bash
curl -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "content": "Your reply text here",
    "author": "agent",
    "parentId": "parent-comment-id"
  }'
```

Always set `"author": "agent"` when creating replies.

### Create new comment

```bash
curl -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "directory": "/path/to/repo",
    "file": "path/to/file.go",
    "line": 42,
    "lineEnd": 44,
    "content": "Comment text",
    "author": "agent",
    "revision": ""
  }'
```

`directory` is required for root comments. Leave `revision` empty for working-copy comments. Always set `"author": "agent"`.

### Resolve/reopen comments

```bash
# Resolve
curl -X POST http://localhost:8888/api/review/comment/{id}/resolve

# Reopen
curl -X POST http://localhost:8888/api/review/comment/{id}/reopen
```

No `directory` param needed — the server looks it up from the stored comment.

### Get diff and revisions

All diff/revision endpoints require `?directory=...`.

```bash
DIR="/path/to/repo"

# Current working copy diff
curl "http://localhost:8888/api/diff?directory=$DIR"

# Diff for specific revision
curl "http://localhost:8888/api/diff?directory=$DIR&revision=abc123"

# List recent revisions
curl "http://localhost:8888/api/revisions?directory=$DIR&limit=10"
```

**jj repos**: VibeDiff uses **jj change IDs**, not git commit hashes. Always resolve a revision to its change ID before using it as a `revision` parameter:

```bash
# Get change ID for a specific revision
jj log --no-graph -r '<rev>' --template 'change_id' -R $DIR

# Get change ID for working copy
jj log --no-graph -r '@' --template 'change_id' -R $DIR
```

Confirm via `GET /api/revisions` — the `id` field is always the jj change ID.

## Comment object structure

```json
{
  "id": "a1b2c3d4",
  "directory": "/path/to/repo",
  "file": "internal/git/parser.go",
  "line": 42,
  "lineEnd": 44,
  "content": "Why is this offset by one?",
  "author": "user",
  "status": "open",
  "parentId": "",
  "revision": "",
  "commit": "abc1234",
  "createdAt": "2026-06-16T15:04:05Z"
}
```

Replies have `parentId` set. All comments in the API response are flat — group by `parentId` to build threads.

## Workflow patterns

### Address review comments

1. List registered directories to find the repo: `GET /api/directories`
2. Fetch open comments: `GET /api/review/comments?directory=...`
3. Make code changes to address feedback
4. Reply to each comment explaining the fix
5. Optionally resolve the thread

### Review summary

1. Get registered directories
2. Fetch current diff and open comments for the target directory
3. Provide summary of changes and outstanding feedback

## Notes

- VibeDiff stores comments in `~/.config/vibediff/comments/<sha256-of-path>.json`
- Comments are scoped to revisions via the `revision` field
- The `directory` field on a comment tells the server where to save it — always include it for root comments
- Only one MCP session allowed per instance
- WebSocket at `/api/ws` — messages include a `directory` field so clients can filter by repo
- Full API docs at `/docs` endpoint
