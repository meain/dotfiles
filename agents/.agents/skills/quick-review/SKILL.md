---
name: quick-review
description: >
  Lightweight code review via subagent. The main agent identifies commits and
  passes them to a subagent that retrieves the changes and reviews for
  correctness issues, consistency problems, and gaps. No praise, no style nits.
  Use when the user says
  /quick-review, "review my changes", "review this diff", "sanity check my
  code", or after finishing a feature/fix and wanting a second opinion before
  pushing.
---

Spin up a subagent to review the relevant commits. Run in background so the
main thread isn't blocked.

## Step 1: Identify commits to review

The main agent (you) knows which commits are relevant — use that context. Do
NOT assume the working copy is empty or that everything above `trunk()` is
in scope. Common cases:

- User just made one or more commits → pass those specific commit IDs
- User is working on a branch → pass the commits that make up the feature
- User provides a description → infer from context which commits apply

If unclear, ask which commits to review before spawning the subagent.

## Step 2: Build context

Build a brief with:
- **What**: what the change does (from commit messages or user args)
- **Why**: why the change was made — the problem being solved, the incident that prompted it, the constraint being addressed. Pull this from the conversation context; don't just restate the what.
- **Background**: any domain context the reviewer needs to assess correctness — e.g. how the system works, what the replaced code did, relevant constraints.

The richer this brief, the more useful the review. A reviewer who understands why a change was made can spot gaps the diff alone won't reveal.

## Step 3: Spawn the review subagent (background)

Pass the commit IDs and context. The subagent will retrieve the changes itself
and may read any files or run any checks it needs.

Prompt template:

```
Review the following commits in <repo-path>:
<commit IDs, one per line>

What: <what the change does>
Why: <why the change was made — problem, incident, constraint>
Background: <any domain context needed to assess correctness>

Retrieve the changes, then review for: correctness bugs, broken invariants,
consistency issues with surrounding code, clear gaps or missing cases. You may
read any files in the repo or run any checks needed to assess correctness.

Return only actionable findings — one bullet per finding.
No praise. No style nits. No commentary on what the code does.
If nothing to flag, say "Nothing to flag."
```

Set `run_in_background: true`.

## Step 4: Report

Wait for the background subagent to complete, then return its findings verbatim.
