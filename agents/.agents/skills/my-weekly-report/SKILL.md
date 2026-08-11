---
name: my-weekly-report
description: >
  Generate a concise weekly status update in team format. "What I worked on"
  is built from the most recently completed Monday-Sunday week in the
  personal backlog archive (Backlog/Backlog.md, "## Before" section),
  cross-referenced with Jira, GitHub PRs (authored + reviewed), and
  Confluence edits from that same window. "What's next" comes strictly from
  the current Jira sprint's To Do tickets. Ends by opening the team's
  Confluence status page for that week and handing back its plain link.
  Triggers: /my-weekly-report, "my weekly update", "my weekly status",
  "generate my weekly", "write my weekly", "my weekly report"
user_invocable: true
---

# /my-weekly-report — Weekly Status Update Generator

Generate a concise weekly update in the team's standard format.

## Format

```
## Abin Simon

**What I worked on:**
- <bullet per notable thing done in the reporting week>

**What's next:**
- <bullet for each To Do ticket in the current sprint>

**Blockers:** None
**Upcoming leaves:** None
**Unplanned leaves last week:** None
```

Always output the report directly in the chat — do NOT write it to a file.

## Critical: sandbox

Every command below hits a network API (Jira, GitHub, Confluence) and will
fail with a TLS/certificate error under the default sandbox. Run **every**
Bash call in this skill with `dangerouslyDisableSandbox: true`. This is the
single most common failure mode when running this skill — if a command
fails with `tls: failed to verify certificate` or similar, that's the
sandbox, not a real problem.

## Steps

### 1. Compute the reporting window

"What I worked on" covers the most recently **completed** Monday-Sunday
week — not "since Monday of this week." Reports are typically generated a
day or more into the new week, so the prior full week is what has actual
completed work to report; the current week in progress rarely does.

```bash
~/.agents/skills/my-weekly-report/scripts/last-week-backlog.sh
```

With no argument this returns last week (Mon-Sun). Pass `0` instead to get
the current week-to-date if the user explicitly asks for that.

The output labels every day with its weekday name and date, including
Saturday/Sunday (shown as "(no entries)" if empty) — this lets you see the
full week shape and decide whether weekend work is worth including.

**Do not filter items out on the assumption they're personal.** If it's
ambiguous whether something is personal or work (e.g. "Apply for leave",
"Schedule 1-on-1s"), leave it in your working notes and let the final
"What I worked on" draft skip only the obviously-personal ones (banking,
health, shopping). When genuinely unsure, include it — the user filters
the final draft, not you.

### 2. Get the exact date range as YYYY-MM-DD

You'll need this for steps 3 and 4. Compute it the same way the script
does:

```bash
dow=$(date +%u)
this_monday=$(date -d "-$((dow - 1)) days" "+%Y-%m-%d")
start=$(date -d "$this_monday -7 days" "+%Y-%m-%d")   # last Monday
end=$(date -d "$this_monday -1 days" "+%Y-%m-%d")     # last Sunday
```

### 3. Check GitHub PRs (authored + reviewed)

```bash
~/.agents/skills/my-weekly-report/scripts/pr-activity.sh "$start" "$end" $(cat ~/.config/datafiles/prs-repos)
```

Returns `{ "authored": [...], "reviewed": [...] }` for that date range.

- `authored`: PRs you opened or that got merged in the window — evidence of
  completed work, use to enrich or add bullets.
- `reviewed`: PRs where you left a review or comment in the window (checked
  per-PR, not just by the PR's own `updatedAt` — a stale-looking PR can
  still have a fresh review from you). Roll these into a single "PR
  reviews" bullet with short refs (e.g. `cpb#5152, cpb#5372`) rather than
  listing each as its own bullet.

If this returns mostly empty, don't pad the report — a light week is a
light week. But cross-check step 1's output first: PR review activity in
particular is easy to under-report from GitHub data alone if the backlog
already names who/what was reviewed.

### 4. Check Confluence for docs touched in the window

```bash
~/.agents/skills/my-weekly-report/scripts/confluence-updates.sh "$start" "$end"
```

If any design docs, runbooks, or notable pages were created or edited,
add a bullet (e.g. "Continued Org Anchoring design — updated Tech Spec
doc").

### 5. Get current sprint's To Do tickets

```bash
jira sprint list --plain --state active --table --no-headers --columns id,name
```

(Or just run `sprint-tickets.sh`, below — it does this lookup internally.)

```bash
~/.agents/skills/my-weekly-report/scripts/sprint-tickets.sh
```

Returns JSON grouped by status: `done`, `in_progress`, `to_do`. **Only the
`to_do` list feeds "What's next."** Do not pull "What's next" from the
backlog's Tomorrow/This Cycle/Next Cycle sections — those are personal
planning, not the sprint board, and mixing them in was a mistake corrected
in a past run of this skill. If `done`/`in_progress` are non-empty, they can
supplement "What I worked on," but the backlog archive (step 1) is usually
the richer source for that.

### 6. Build the report

**What I worked on:**
- Synthesize step 1 (backlog), step 3 (PRs), and step 4 (Confluence) into
  concise, deduplicated bullets — don't list the same underlying piece of
  work twice just because it showed up in two sources.
- Group related items (e.g. all Org Anchoring work in one bullet, all PR
  reviews in one bullet) rather than one bullet per raw backlog line.
- Include a Jira link where a ticket ID is known.

**What's next:**
- One bullet per `to_do` sprint ticket (step 5), with Jira link.

**Blockers / leaves:**
- Default all three fields to "None" unless the backlog explicitly says
  otherwise (leave requests, blocker notes).

Keep bullets as short phrases, not full sentences — match the style in
`/Users/meain/.local/share/sbdb/InfraCloud/Weekly Updates Submitted/`.

### 7. Copy to clipboard

```bash
printf '%s' "<report text>" | pbcopy
```

### 8. Find and open the team's Confluence status page

```bash
~/.agents/skills/my-weekly-report/scripts/confluence-doc-link.sh
```

Prints the page title on line 1 and its plain URL on line 2. **The page
title's date is the Sunday before the current week's Monday, not the
Monday itself** — e.g. for the week starting Monday 2026-08-10, the page is
titled "Updates for week of 2026-08-09." Searching for the Monday's date
directly returns nothing; this was a bug in an earlier version of this
skill. The script handles this automatically, with a fallback pattern
search if the exact page doesn't exist yet.

Open it:

```bash
open "<url from the script>"
```

Requires `dangerouslyDisableSandbox: true` (it launches a GUI browser).

### 9. Show the final draft

Print the report block, then the doc link **on its own, as a plain URL —
not embedded in the report content and not markdown-formatted** (no
`[text](url)`), so the user can copy-paste the bare link directly. The
report content itself should never contain the doc link.

Do NOT save the report locally — do not write to any file under
`/Users/meain/.local/share/sbdb/InfraCloud/Weekly Updates Submitted/`. The
user pastes and saves it manually in Confluence.

## Known gh/jira/confluence gotchas (read before debugging a "broken" script)

- **`gh search prs --state all` is invalid.** `gh search prs` only accepts
  `--state open` or `--state closed` — there is no "all". Omit `--state`
  entirely to search every state. (`gh pr list --state all` is fine — the
  two commands have different flag validation.) An earlier version of
  `pr-activity.sh`'s predecessor used `--state all` here and it silently
  returned an empty array every time because of a `|| echo "[]"` fallback
  swallowing the error.
- **`gh ... --jq EXPR` takes exactly one argument.** It does NOT support
  jq's own `--arg` flag the way piping into a real `jq` does. If you need
  variables in the filter, do `gh ... --json X | jq --arg foo "$foo" '...'`
  instead of `gh ... --json X --jq --arg foo "$foo" '...'` (the latter
  errors with "accepts at most 1 arg(s)").
- **A PR's `updatedAt` doesn't prove *your* activity is recent.** Someone
  else's comment updates it too. To confirm you reviewed something in a
  specific window, fetch that PR's `reviews`/`comments` and check the
  timestamp of the entry where `author.login` matches you specifically.
- **`jira sprint list` (and any jira/gh/confluence call) fails under the
  sandbox** with a TLS certificate error — always disable the sandbox for
  these, per the note at the top of this file.

## Reference Format

See `/Users/meain/.local/share/sbdb/InfraCloud/Weekly Updates Submitted/2026/W17.md`
for the exact style. The Abin Simon section there is the canonical example:

```
## Abin Simon

**What I worked on:**
- Worked on separating earn query and ingestion service
- Working on pushing JSON schema validation to prod
- Security fixes in multiple repos
- Working with Rushikesh's (Veeam Employee) onboarding
- ADK SKU change and dropping DR for eventhub
- PR reviews
**What's next:**
- Org Anchoring
- Help with onboarding Rushikesh and work on synthetic monitoring

**Blockers:** None
**Upcoming leaves:** None
**Unplanned leaves last week:** April 14, 15
```

Note: bullets are short phrases, not full sentences. Jira links are optional
but preferred for tickets — include them when the ticket ID is known.
