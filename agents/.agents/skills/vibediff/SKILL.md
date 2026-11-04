---
name: vibediff
description: >
  Work with VibeDiff code review instances: fetch and reply to review comments,
  resolve or reopen threads, post findings from a code review as inline comments
  (agent:reviewer), and annotate a jj revision with explanatory comments
  (agent:explain). Use when the user says /vibediff, /vibediff-review,
  /vibediff-explain, "check vibediff", "get review comments", "reply to review
  comment", "resolve vibediff comment", "review and post to vibediff", "explain
  this commit in vibediff", "add comments to vibediff", or when working on code
  changes that have active VibeDiff review comments.
---

# VibeDiff

VibeDiff is a local review server (default `http://localhost:8888`) that can hold
several repositories at once. Everything below works the same whether you are
answering comments, posting a review, or writing explanations.

## Helper script

`vd.py` in this skill directory wraps the common chores. It defaults `--dir` to the
cwd and uses `VIBEDIFF_URL` (default `http://localhost:8888`). Use it first; fall back to curl only for things it does not cover.

```bash
VD=~/.claude/skills/vibediff/vd.py

$VD comments                         # open threads in the cwd repo, with replies
$VD comments --rev trswtrqn          # filter by revision prefix (jj change id)
$VD comments --author user           # only threads a human started
$VD comments --all --json            # include resolved threads, as JSON (with replies)
$VD comments --reaction down         # threads the user thumbed down (also: up)
$VD reply <id> "Fixed in wip abc"    # reply as agent:claude; --resolve also resolves
$VD reply <id> - < msg.txt           # reply text from stdin (multi-line)
$VD post --rev <change-id> --file path/to/x.go --line 42 [--line-end 44] [--name reviewer] "text"
$VD post --rev <change-id> --file path/to/x.go "file-level text"   # no --line => file-level
$VD react <id> up|down|clear
$VD resolve <id>... / $VD reopen <id>...
$VD revisions [--limit 10]
```

`post` defaults `--name` to `reviewer`; pass `--name explain` for explanations. `reply` defaults to `claude`.

A human reply can sit under an agent's root comment, so `comments --author user` (which filters on the root author) can miss it. To catch every thread a human touched, check `comments` without `--author`.

## Shared rules

- Every comment endpoint needs `directory` (query param for GET, body field for POST), except reactions, resolve and reopen, which look it up from the stored comment. Replies must also include `directory`; without it the server answers `400 directory is required`. The script always sends it and exits non-zero with the server's message on HTTP errors.
- Replies (`parentId` set) should send only `content`, `author`, `authorName` and `parentId`. The server inherits `file`, `line`, `lineEnd`, `revision` and `directory` from the parent.
- `author` is always `"agent"` for anything this skill posts. `authorName` (max 50 chars) is the tag the UI shows as `agent:<name>`: `reviewer` for review findings, `explain` for explanations, `claude` for replies.
- Leave `revision` out (or empty) for working-copy comments. Otherwise pass a jj change ID.
- In jj repos VibeDiff uses jj change IDs, never git hashes. Resolve first:

  ```bash
  REV=$(jj log --no-graph -r '<rev>' --template 'change_id' -R $DIR)   # or -r '@'
  curl -s "http://localhost:8888/api/revisions?directory=$DIR&limit=5"  # confirm ids
  ```

- File-level comments: omit `line` (or send `0`). Use them for overviews and new files instead of pinning to line 1.
- Directory registration: if the repo is not in `GET /api/directories`, register it with `POST /api/directories {"directory": "..."}`.
- Thumbs: the user can thumb a comment up or down (`reaction`). 👎 on an agent comment means wrong or unwanted. Reactions do not resolve a thread. Before posting, check `GET /api/review/comments?directory=$DIR` for 👎 on earlier agent comments and do not repeat that kind of comment.
- Discovery: `curl http://localhost:8888/api/directories`, or `curl http://localhost:8888/docs` for instance-specific API docs (check there first if behaviour looks wrong). If the port differs, `lsof -iTCP -sTCP:LISTEN`.

## Fetch and answer comments

1. `$VD comments` (or `GET /api/review/comments?directory=$DIR`, with `&revision=` or `&file=` to filter).
2. Make the code changes.
3. `$VD reply <id> "<what changed>" --resolve` for each thread you addressed.

Replies should be one or two sentences: the fix, nothing more.

## Post review findings

Use this after a code review run when findings should show up in VibeDiff instead of only in chat.

1. Run the `code-review` skill on the current diff. Pass through any effort level the user gave. Do not pass `--comment` (that posts to GitHub) or `--fix`; this workflow owns the output.
2. If there are no verified findings, say so in chat and stop. Do not post a "no issues" comment.
3. Post one comment per finding at its file and line, with `$VD post ... --name reviewer`. Put the summary and failure scenario together in the text.
4. Use one `revision` value (or none for working copy) across the whole run so the comments group together.
5. In chat, report the count and the file:line pairs, one line each. Do not repeat the finding text.

## Explain a revision

Use this when the user asks to document WHY a revision changed, not to review it.

1. Resolve the change ID and look at the commit: `jj show $REV -R $DIR`, and the diff from `GET /api/diff?directory=$DIR&revision=$REV`.
2. Pick the files and lines where a reader needs the reason. Aim for roughly 10 to 20 comments per revision, fewer for small changes.
3. Post with `$VD post --rev $REV --file <path> [--line N] --name explain "<text>"`.

What to comment on:

- New files: a file-level comment on purpose.
- Non-obvious design decisions, constraints, invariants, workarounds.
- Cross-file or cross-service impact.
- Refactors where the pattern needs naming.

What to skip: self-explanatory changes, mechanical refactors, and every single line. Explain WHY, not WHAT. Use the commit message and PR context for content.

## Comment object

```json
{
  "id": "a1b2c3d4",
  "directory": "/path/to/repo",
  "file": "internal/git/parser.go",
  "line": 42,
  "lineEnd": 44,
  "content": "Why is this offset by one?",
  "author": "user",
  "authorName": "",
  "status": "open",
  "reaction": "down",
  "parentId": "",
  "revision": "",
  "commit": "abc1234",
  "createdAt": "2026-06-16T15:04:05Z"
}
```

All comments come back flat. Group by `parentId` to build threads. `line` is `0` or absent for file-level comments.

## Notes

- Comments persist in `~/.config/vibediff/comments/<sha256-of-path>.json`.
- The server allows one MCP session per instance. WebSocket is at `/api/ws`; messages carry `directory`.
