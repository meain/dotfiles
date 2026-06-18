# VibeDiff Integration

Interact with VibeDiff code review instances - fetch review comments, reply to threads, resolve comments, and manage review state.

## When to use

- User says "check vibediff", "get review comments", "vibediff review", "reply to review comment"
- User asks to resolve/close VibeDiff comments
- Working on code changes that have active VibeDiff review comments
- User mentions VibeDiff by name or asks about review feedback

## How it works

VibeDiff runs as a local HTTP server (default port 8888) serving one repository at a time. Each instance provides:
- REST API for comments, diffs, and revisions
- WebSocket for real-time updates
- MCP server for agent integration

### Finding the running instance

```bash
# VibeDiff defaults to port 8888 - check if it's responding
curl http://localhost:8888/api/directory

# If on a different port, search all listening ports
lsof -iTCP -sTCP:LISTEN

# Check instance-specific docs (always check if API behavior seems wrong)
curl http://localhost:8888/docs
```

The `/docs` endpoint provides instance-specific API documentation with the current base URL, project path, and VCS backend.

## Common operations

### Connect to instance and switch project

```bash
# Check current project
curl http://localhost:8888/api/directory

# Switch to different repo
curl -X POST http://localhost:8888/api/directory \
  -H 'Content-Type: application/json' \
  -d '{"directory":"/path/to/repo"}'
```

### Fetch review comments

```bash
# All comments
curl http://localhost:8888/api/review/comments

# Open comments only
curl http://localhost:8888/api/review/comments/open

# Filter by revision
curl 'http://localhost:8888/api/review/comments?revision=abc123xyz'

# Filter by file
curl 'http://localhost:8888/api/review/comments?file=path/to/file.go'
```

### Reply to comments

**Important**: When replying (setting `parentId`), only provide `content`, `author`, and `parentId`. The API automatically inherits `file`, `line`, `lineEnd`, and `revision` from the parent comment.

```bash
curl -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "content": "Your reply text here",
    "author": "agent",
    "parentId": "parent-comment-id"
  }'
```

If you provide `file`, `line`, etc. they will override the inherited values. Always set `"author": "agent"` when creating replies.

### Create new comment

```bash
curl -X POST http://localhost:8888/api/review/comment \
  -H 'Content-Type: application/json' \
  -d '{
    "file": "path/to/file.go",
    "line": 42,
    "lineEnd": 44,
    "content": "Comment text",
    "author": "agent",
    "revision": ""
  }'
```

Leave `revision` empty for working-copy comments. Always set `"author": "agent"` when creating comments as the agent.

### Resolve/reopen comments

```bash
# Resolve
curl -X POST http://localhost:8888/api/review/comment/{id}/resolve

# Reopen
curl -X POST http://localhost:8888/api/review/comment/{id}/reopen
```

### Get diff and revisions

```bash
# Current working copy diff
curl http://localhost:8888/api/diff

# Diff for specific revision
curl 'http://localhost:8888/api/diff?revision=abc123'

# List recent revisions
curl 'http://localhost:8888/api/revisions?limit=10'
```

## Comment object structure

```json
{
  "id": "a1b2c3d4",
  "file": "internal/git/parser.go",
  "line": 42,
  "lineEnd": 44,
  "content": "Why is this offset by one?",
  "author": "user",          // "user" or "agent"
  "status": "open",           // "open" or "resolved"
  "parentId": null,           // set for replies
  "revision": "",             // empty for working-copy
  "commit": "abc1234",
  "createdAt": "2026-06-16T15:04:05Z"
}
```

Replies have `parentId` set and are listed as separate entries (not nested), linked via the parent ID.

## Workflow patterns

### Address review comments

1. Fetch open comments for current revision
2. Make code changes to address feedback
3. Reply to each comment explaining the fix
4. Resolve the comment thread

### Review summary

1. Switch to target repo
2. Fetch current diff and open comments
3. Provide summary of changes and outstanding feedback

## Notes

- VibeDiff stores comments in `~/.config/vibediff/comments/<sha256-of-path>.json`
- Comments are scoped to revisions via the `revision` field
- Only one MCP session allowed per instance
- WebSocket available at `/api/ws` for push notifications
- Full API docs at `/docs` endpoint
