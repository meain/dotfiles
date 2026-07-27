---
name: concern-review
description: >
  Generate an interactive local HTML review page for a large PR or diff,
  grouping the changed files by logical concern (not just by file) so a
  reviewer can go through one theme at a time instead of a flat file list.
  Each concern gets its own section with a description, per-file summaries,
  the real diff (sliced to just the hunks relevant to that concern when a
  file serves more than one), and callouts for anything genuinely non-obvious.
  Triggers: /concern-review, "review this PR by concern", "review this large
  PR", "build a review dashboard", "group this diff by concern", "concern
  review", "help me review this PR".
user_invocable: true
---

# concern-review

Large PRs are easier to review grouped by *what the change is doing* (e.g.
"new retry logic", "bug fix", "reporting additions") than by a flat list of
files — a file that serves two purposes should show up, sliced, in both
places. This skill produces that as a static HTML page: a left sidebar to
jump between concerns, one concern's files shown at a time as a single
scrolling list, real unified diffs with GitHub-style coloring, and a short
summary above each file's diff.

## The one rule that makes this fast and safe

**Your output is a small JSON blob — never the diff content itself.** You
decide *which* hunks belong to *which* concern and write short summaries; a
Python script pulls the actual diff text from the real diff file and builds
the HTML. This means:

- Your generation is small and fast (summaries + hunk-index lists, not
  reproduced code).
- The rendered diff can never be wrong/hallucinated — it's sliced
  mechanically from the same diff you read.

Do not paste diff hunks into the JSON. Reference them by index instead.

## Steps

### 1. Get the diff into a file

Pick whichever applies:

```bash
# GitHub PR (set GIT_DIR if the repo uses jj)
GIT_DIR=$(jj git root 2>/dev/null || echo .git) gh pr diff <number> > /tmp/<slug>.diff

# local working diff (jj)
jj diff --git -r <revision> > /tmp/<slug>.diff

# local working diff (git)
git diff <base>...<head> > /tmp/<slug>.diff
```

Use a short `<slug>` you'll reuse for the other temp files (e.g. `pr5276`).

### 2. Get the authoritative hunk map

```bash
python3 ~/.claude/skills/concern-review/scripts/list_hunks.py /tmp/<slug>.diff [optional-path-substring-filter]
```

This prints, per file, every hunk's 0-based index, its `@@ ... @@` header, and
its +/- line count. **These indices are what you'll reference in the JSON —
don't hand-count `@@` lines from the raw diff, use this output.** Filter by a
path substring if the diff is huge and you want one file at a time.

### 3. Read the diff to understand the change

Read `/tmp/<slug>.diff` (or grep/sed specific line ranges for very large
files) to actually understand what changed — the hunk map from step 2 tells
you *where* things are, not *what* they do. This is normal input-token
reading; the efficiency goal is about what you *write*, not what you read.

### 4. Decide the concerns

Aim for 2-6 concerns that are genuinely distinct themes — not "misc" or one
concern per file. Good concerns read like a PR description's bullet points:
"new retry logic for X", "bug fix: Y", "reporting/logging additions". Each
concern needs:

- `key`: short kebab-slug, used in DOM ids/anchors (e.g. `"vpn-retry"`)
- `title`: human title, shown as the section heading
- `desc`: one or two sentences

### 5. Assign files to concerns

For each file touched by a concern, add an entry:

```json
{ "path": "exact/path/as/printed/by/list_hunks.py", "summary": "...", "hunks": "all" }
```

- If **all** of a file's changes serve this one concern, use `"hunks": "all"`
  (or omit the key — that's the default).
- If a file's hunks serve **multiple** concerns, list only the relevant
  0-based hunk indices for each concern, e.g. `"hunks": [0, 1, 4, 5]`. Every
  hunk in the file must be assigned to exactly one concern — check the total
  hunk count from `list_hunks.py` against your combined lists so none are
  silently dropped or duplicated.
- If a single hunk's lines genuinely mix two concerns (rare — usually only
  happens with an unrelated one-line addition riding inside a bigger hunk),
  assign the whole hunk to whichever concern dominates it. Don't try to split
  a hunk line-by-line; that produces an invalid diff.
- `summary`: one to three sentences on what changed **in this file, for this
  concern** and why — not a restatement of the diff. Write it like you're
  telling a colleague what to look for.

### 6. Add notes only for genuinely confusing things

Each concern may have a `"notes"` array (rendered as an amber "Might be
confusing" callout). Reserve these for things a careful reviewer could easily
miss or misjudge: a hidden invariant, duplicated logic that could drift, an
intentional-looking tradeoff, a migration/compatibility implication, two call
sites solving the same problem differently. **Do not** add navigational notes
like "the rest of this file is in another section" — the UI already makes
that obvious (files are listed per-concern in the sidebar), and a prior user
explicitly asked for these to be removed. If nothing is genuinely confusing,
omit `notes` or leave it empty.

### 7. Write the concerns JSON

```json
{
  "pr": { "label": "org/repo #123", "subtitle": "TICKET-123 — short title" },
  "pathPrefix": "optional/common/path/prefix/to/strip/in/the/sidebar/",
  "categories": [
    {
      "key": "vpn-retry",
      "title": "VPN-resilience (new)",
      "desc": "One or two sentences.",
      "notes": ["Non-obvious point worth flagging.", "..."],
      "files": [
        { "path": "internal/cosmosretry/retry.go", "summary": "...", "hunks": "all" },
        { "path": "main.go", "summary": "...", "hunks": [0, 1, 4, 5] }
      ]
    }
  ]
}
```

Save it to `/tmp/<slug>-concerns.json`. `pathPrefix` is optional — set it to
whatever common prefix (e.g. a monorepo module path) should be stripped when
showing filenames in the sidebar, so `internal/foo/bar.go` shows instead of
`tools/some-service/internal/foo/bar.go`.

### 8. Build the HTML

```bash
python3 ~/.claude/skills/concern-review/scripts/build_review.py \
  --diff /tmp/<slug>.diff \
  --concerns /tmp/<slug>-concerns.json \
  --out /tmp/<slug>-review.html
```

The script errors out with a clear message (and the list of valid paths) if a
`path` doesn't match anything in the diff, or if a `hunks` index is out of
range — fix the JSON and re-run rather than guessing.

### 9. Open it and report

```bash
open /tmp/<slug>-review.html
```

(use `dangerouslyDisableSandbox: true` for the `open` call). Tell the user
how many concerns/files it covers in a sentence or two — don't re-narrate the
whole PR in chat, the page is the review surface now.

## What the template already handles

You never touch `template.html`'s CSS/JS — it already implements everything
validated across real review sessions:

- Sidebar lists concerns and their files; clicking a concern loads only that
  concern's files as one continuous scroll (not a wall of every concern at
  once).
- Clicking a file scrolls to it and briefly highlights it, switching concern
  first if needed.
- Sidebar filenames truncate from the **front** (so the actual filename stays
  visible, not the directory prefix) — don't fight this by pre-truncating
  paths yourself.
- Sidebar width is user-resizable (drag the divider) and persists across
  reloads.
- Diffs render GitHub-style (green/red, line numbers, hunk headers) with
  per-file expand/collapse and page-wide expand/collapse-all buttons.
- No stat tiles, charts, or metrics — a past iteration of this had a
  dashboard-style summary view and it was explicitly rejected in favor of
  seeing the actual code. Don't reintroduce metrics/charts here.

## Testing changes to this skill

If you modify `template.html` or the scripts, verify with a headless browser
before considering it done — a static review of the JS isn't enough to catch
layout bugs (this skill's design went through several rounds of exactly that
kind of bug). Playwright works well for this (`npm install playwright` in a
scratch dir, then drive it with a small node script); screenshot at least the
default concern and one concern with a long file path, and check for errors
via `page.on("console", ...)` and `page.on("pageerror", ...)`.

Note when scripting clicks: `data-cat-select="<key>"` appears on **every**
file link in a concern's sidebar list, not just once on the concern's header
link — that's intentional (clicking any file jumps to it within that
concern), but it means a plain `page.click('[data-cat-select="..."]')` or an
nth-match click can silently hit the same concern twice instead of switching.
Scope the selector to the header when you want an unambiguous single click:
`.cat-link[data-cat-select="<key>"]`.
