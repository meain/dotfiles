---
name: address-review-comments
description: Work through review comments across a stack of jj-managed PRs, one PR at a time — pull comments, present them with proposed fixes, apply only what's approved, and manage the commit/rebase/push flow.
---

Address code review comments on a stack of jj-managed PRs/commits, one PR at
a time, keeping every fix reviewable before it's folded into history.

## Scope: which PRs to process

Only process PRs that have actual **human** review activity. Comments from
bots (`coderabbitai`, `github-actions`, `claude`) do not count as "reviewed"
for this filtering purpose — skip a PR with only bot comments unless the
user explicitly asks to address bot comments too. When they do ask about a
bot's comment, check it against current code before doing anything: bot
reviews often describe code that has since changed shape (e.g. an earlier
architectural fix already made the comment moot) — say so plainly if the
finding no longer applies, don't reflexively "fix" something already fixed.

If preparing research for several PRs ahead of time, background subagents
work well — one per PR, each writing findings to
`.martifacts/pr-<number>-review.md` — so the user can go through them one by
one without waiting on research each time.

## Per-PR loop

For each PR, in order:

### 1. Create a new commit on top of the PR's commit

```bash
jj new <pr-change-id> -m "wip: address pr #<number> review comments"
```

Never work by directly `jj edit`-ing the PR's own commit to add new logical
changes — that buries the diff inside an existing commit before the user has
seen it. The one exception is resolving a genuine rebase conflict (see step
6) — that's mechanical reconciliation of two existing diffs, not new work,
so it's fine to resolve inline and squash immediately.

### 2. Pull the PR's review comments

```bash
GIT_DIR=$(jj git root) gh pr view <number> --json reviews,comments
```

`gh pr view` does not show whether an inline thread is resolved, so also
pull the threads with their state and full reply chain:

```bash
GIT_DIR=$(jj git root) gh api graphql -f query='query{repository(owner:"<owner>",name:"<repo>"){pullRequest(number:<number>){reviewThreads(first:100){nodes{id isResolved isOutdated path line originalLine comments(first:50){nodes{author{login} body createdAt}}}}}}}'
```

Only work on what is actually still open — the goal is never to re-address
a comment that has already been dealt with:

- Drop every thread with `isResolved: true`.
- If the last comment in an unresolved thread is the PR author's, it is
  waiting on the reviewer — list it as "replied, awaiting reviewer", no new
  work.
- For what's left, including outdated threads, check the current code (at
  the PR's commit and in descendants that rewrote the same lines) before
  proposing anything. If it's already fixed, say so and quote the code
  rather than proposing a change.
- A `CHANGES_REQUESTED` review state stays until the reviewer re-reviews,
  even when every thread behind it is resolved. That needs a re-review
  request, not more code.

### 3. Research and present as a numbered list

For each comment: re-check it against the *current* shape of the code (line
numbers and even the code itself may have moved since the review was
posted), then propose a concrete fix. Present all comments as a numbered
list with the proposed solution before changing anything — do not edit code
first and explain after.

If a comment is architectural rather than mechanical (e.g. "why build a
second client instead of extending the shared one"), lay out the tradeoffs
and flag that it needs a decision, rather than just picking an approach.

If two comments' proposed fixes contradict each other, or a proposed fix
conflicts with a decision already made elsewhere in the stack, stop and
explain the contradiction — don't silently resolve it one way.

Right after showing the list, open the PR in the browser so the user can
work through the comments visually alongside it:

```bash
open https://github.com/<owner>/<repo>/pull/<number>
```

### 4. Apply only what's approved

Wait for the user to pick which comments to act on (they may say "do 2, 3,
5", ask follow-up questions about specific ones first, or ask for a
different fix than the one proposed). Only touch what's explicitly
approved. Re-verify build and tests after every change:

```bash
go build ./... && go test ./<affected-packages>/...
```

Watch for formatting tools (`make fmt`, gofumpt/goimports hooks) sweeping
unrelated files when run repo-wide — check `jj diff --stat` afterward and
`jj restore --from @-` anything outside the intended scope before
describing the commit.

### 5. Handle ripple effects across the stack

A rename or signature change made at PR N's commit often breaks compilation
at PR N+k downstream, because later commits reference the old name. Fix each
broken descendant as its **own** new commit (`jj new <descendant-rev> -m
"wip: ..."`), scoped to just that ripple — not folded into the fix commit,
and not directly edited into the descendant's existing commit. This lets the
user review the fix and its ripple separately, and keeps each PR's diff
matching what it's actually supposed to contain.

Sequence: fix at the PR's commit → `jj describe` it → `jj rebase -s
<next-commit> -d <fix-commit>` → check for breakage/conflicts at each
affected descendant → fix each with its own `jj new`/`jj describe` → rebase
the remaining tail forward → repeat until the whole stack builds clean.

### 6. Resolve real rebase conflicts inline

If `jj rebase` reports actual conflicts (not just compile breakage — look
for `CONFLICT` in `jj log` or `<<<<<<<` markers in files), resolve them by
editing the conflicted file directly, then:

```bash
jj squash --use-destination-message
```

This is expected/mechanical — reconciling two sides of a real merge — and
distinct from squashing new work into an existing commit. Do it as soon as
the conflict is resolved; no need to hold it for approval.

### 7. Verify the whole stack before finalizing

Move to the tip (`jj edit <tip-change-id>` or the bookmark furthest along)
and run the full build + test suite there — passing at an individual
commit doesn't guarantee the assembled stack still builds:

```bash
go build ./... && go test ./...
```

### 8. Show the diffs, then squash only with explicit approval

Show the user the diff of every new fix/ripple commit created in this pass
(`jj diff -r <rev>` for each). Default suggestion is to squash each into its
target PR commit, but **never squash without the user explicitly saying so
for this batch** — a prior "yes" does not carry over to the next PR's
squashes. Once approved:

```bash
jj squash --from <fix-commit> --into <target-commit> --use-destination-message
```

Re-verify build/test at the tip after squashing (squashing can itself
surface new conflicts if two independent fixes touched overlapping lines).

### 9. Push

```bash
GIT_DIR=$(jj git root) jj git push -b <bookmark1> -b <bookmark2> ...
```

List every bookmark in the stack from the PR just fixed through the tip —
squashing rewrites commit IDs for every descendant, so all of them need
re-pushing, not just the one that changed.

### 10. Move to the next PR

Repeat from step 1 for the next PR with human review comments.

## Rebasing this stack against upstream main

A long-running review stack will need to be rebased onto upstream main more
than once as other work lands. This is a distinct situation from step 6's
same-stack ripple conflicts — here the conflicting side is *someone else's*
merged work, so treat every conflict as needing a review step, not an
immediate squash:

1. `jj rebase -d main` (or whatever the trunk bookmark is) to pull the whole
   stack onto the new base.
2. Find every commit the rebase left conflicted:
   ```bash
   jj log -r 'descendants(<stack-base>) & mutable()' -T 'change_id.shortest(8) ++ " " ++ if(conflict, "CONFLICT ", "") ++ description.first_line() ++ "\n"'
   ```
3. Process conflicted commits **oldest first** — resolving an older commit's
   conflict often auto-resolves its descendants' conflicts too (they were
   the same root cause propagating downstream), so re-run the check above
   after each fix before assuming there's more work.
4. For each: `jj new <conflicted-rev>` (a new commit on top of it), edit out
   the `<<<<<<<`/`|||||||`/`=======`/`>>>>>>>` markers by hand (usually
   both sides are additive — keep both, don't just pick one), build/test/lint,
   then show the resulting diff and **wait for explicit approval before
   squashing** — unlike step 6's same-stack ripple conflicts, a wrong merge
   here can silently drop real work from someone else's landed PR, so it's
   worth the extra pause even though the mechanics are the same
   (`jj squash --into <conflicted-rev>`).
5. Once every conflict is resolved, do a full build/test/lint sweep at the
   tip (not just the commits you touched) before pushing — a clean merge at
   each individual commit doesn't guarantee the assembled stack still
   builds against the new base.

## Standing rules

- Show before changing: present the comment list and proposed fix before
  editing code, and show the diff before squashing — every time, not just
  the first time.
- Default to a new commit per fix; squashing always needs explicit,
  per-batch approval.
- If a comment's fix belongs conceptually to an earlier PR in the stack
  (even though the conflict/need surfaced later), put the fix at that
  earlier PR's commit and rebase forward, rather than patching it wherever
  is most convenient.
- If addressing a comment reveals the PR's design needs to change (not just
  a mechanical fix), stop and explain the situation before proceeding —
  this is a decision for the user, not something to resolve unilaterally.
- Skip a PR/comment entirely if the user says so ("skip this one for now")
  — don't leave partial edits behind; `jj restore`/`jj abandon` anything
  speculative.
