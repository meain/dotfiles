---
name: backlog
description: >
  Daily backlog management — full planning review OR add a single entry from a URL.
  Full review: gathers Jira sprint, GitHub PRs, checked-off items, and collaboratively
  plans the day. Add from URL: looks up a Teams message, Jira ticket, GitHub PR/issue,
  or incident.io alert and creates a concise backlog entry with a [ref] link.
  Triggers: /backlog-review, /update-backlog, "update my backlog", "plan my backlog",
  "what should I work on today", "refresh backlog", "backlog update",
  /backlog-add, "add this link to backlog", "backlog from link", "look up and add to backlog"
user_invocable: true
---

# /backlog — Daily Backlog Management

Two modes: **full daily review** (default) or **add single entry from URL** (when a URL is provided).

## Backlog Location

**File**: `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`
Sections: Today, Tomorrow, This Week, Weekend, Next Week, Whenever, Ongoing, and dated "Before" sections.

---

## Mode A: Add Entry from URL

When given a URL, classify the source, fetch content, and add a task to the backlog.

### Step 1: Classify

| Pattern | Type |
|---------|------|
| `teams.microsoft.com/l/message/` | Teams message |
| `veeam-vdc.atlassian.net/browse/` | Jira ticket |
| `github.com/.../pull/` | GitHub PR |
| `github.com/.../issues/` | GitHub issue |
| `app.incident.io/` | incident.io alert/incident |

### Step 2: Fetch Content

- **Teams**: Use MS365 MCP `read_resource` with URI `teams:///teams/{groupId}/channels/{channelId}/messages/{messageId}`. Extract sender name, message body.
- **Jira**: `jira issue view PROJ-123 --plain` — extract summary, status, assignee, type.
- **GitHub PR**: `gh pr view <url> --json title,author,body,state`
- **GitHub issue**: `gh issue view <url> --json title,author,body,state`
- **incident.io**: Use incident.io MCP tools (`alert_show` or `incident_show`).

### Step 3: Compose Entry

Format: `- [ ] <concise description> [ref](<original-url>)`

Guidelines:
- Keep description **short** — a few words, enough to identify the task. No inline rationale or detail.
- If more context is needed, create a task note and link it with a wiki-link instead of expanding the description.
- Include person's first name if someone is asking/assigning something
- Jira: `Fix auth token storage bug [DP-1234](url)`
- GitHub PR: `Review [repo#number](url): summary`
- Teams: `Review topic from Name [ref](url)`
- Use `[x]` for completed or won't-do items. Never use `[-]` or any other checkbox state.

### Step 4: Add to Backlog

Use `/vault` skill to add to the specified section (default: Today). If unavailable, directly edit `Backlog.md` — find the target section heading and insert at the end of that section.

---

## Mode B: Full Daily Review

Collaboratively update the Today section of the backlog by pulling data from external sources and working with the user to decide what's realistic.

### Step 0: Verify Today's Date

Always run `date "+%Y-%m-%d (%A)"` and treat its output as authoritative.

**Monday cleanup:** `/Users/meain/.local/share/ignored-prs-for-review.yaml` is a YAML file with three top-level list keys: `prs` (specific PR path fragments, e.g. `owner/repo/pull/123`), `repos` (bare repo names — blocks every PR from that repo), and `people` (author logins — blocks every PR by that person). Only the `prs` list resolves to real PRs, so Monday cleanup only touches that section — leave `repos` and `people` entries untouched.

If today is Monday, extract just the `prs` entries and check each one's state:
```bash
awk '/^prs:/{f=1;next} /^[a-zA-Z_]+:/{f=0} f' /Users/meain/.local/share/ignored-prs-for-review.yaml | sed -E 's/^[[:space:]]*- //'
```
For each non-empty result line:
```bash
export GIT_DIR=$(jj git root 2>/dev/null || echo .git)
result=$(gh pr view "https://github.com/$prpath" --json state 2>&1)
state=$(echo "$result" | grep -o 'OPEN\|MERGED\|CLOSED')
```
(use a variable name other than `path` — zsh aliases lowercase `path` to `$PATH`, and overwriting it breaks every subsequent command in the shell).

Remove any `prs` entries where state is not `OPEN` (merged, closed, or unknown/deleted), rewriting only that list in the YAML file — leave `repos` and `people` untouched. Report how many were pruned.

### Step 1: Read Backlog

Read the full backlog file first.

### Step 2: Handle Checked-Off Items

Note the checked-off items from Today — they will be archived to Before during Step 5. Format: `### YYYY-MM-DD (DayOfWeek)`. Get the date with:
```bash
~/.agents/skills/backlog/scripts/date-before.sh
```

Insert the new date block at the **end (bottom)** of the Before section — Before is chronologically ascending (oldest at top, newest at bottom).

### Step 3: Gather External Data (parallel)

> **All Jira and GitHub commands must be run with `dangerouslyDisableSandbox: true`** — the sandbox blocks TLS certificate verification needed for these hosts.

**3a. Jira sprint:**
```bash
jira issue list -q "sprint in openSprints() AND assignee = currentUser()" --raw 2>&1
```
Returns JSON array with raw Jira issue data. Parse with `jq` to extract key, status, summary.

Also fetch the sprint end date for the "This Cycle" section header:
```bash
jira sprint list --project DP | awk '/active/ {print $6}' | head -1
```
Returns a date like `2026-06-29`. Use it in the section header as `### This Cycle (ends YYYY-MM-DD)`. If the backlog already has a "This Cycle" header with a different date, update it to reflect the current sprint.

**3b. GitHub PRs — pending reviews:**
```bash
~/.agents/skills/backlog/scripts/pending-prs.sh
```
Returns JSON array of PRs where user is a direct reviewer (not via team), excluding approved PRs, drafts, and `Veeam-VDC/vdc-shared-data-plane` PRs (never surface data-plane PRs — not the user's area). Also exclude dependabot PRs from Veeam-VDC repos — they are auto-reviewed and merged.

After fetching, filter out ignored PRs/repos/people. Flatten the YAML blocklist to one pattern per line, then filter the one-PR-per-line JSON with `grep -vFf`:
```bash
~/.agents/skills/backlog/scripts/pending-prs.sh | jq -c '.[]' | grep -vFf <(grep -E '^[[:space:]]*- ' /Users/meain/.local/share/ignored-prs-for-review.yaml | sed -E 's/^[[:space:]]*- //')
```

Apply the same flatten-and-filter pattern (jq per-line, then `grep -vFf` against the flattened YAML) whenever filtering PR lists in this skill, including the meain/* repos list in 3b-ii.

**3b-ii. PRs from meain/* repos needing review:** Open PRs in repos owned by the `meain` GitHub org (user's personal projects where others open PRs):
```bash
GIT_DIR=$(jj git root 2>/dev/null || echo .git) gh search prs --state=open --owner=meain --json number,title,url,repository,updatedAt,isDraft,author 2>&1
```
Include all non-draft PRs not authored by meain. Filter out ignored PRs/repos/authors using the same flatten-and-filter pattern as 3b. Surface these in a separate "meain/* repos" subsection of the candidates file.

**3c. PRs authored by user:** Open PRs authored by the user across Veeam-VDC org.

**3d. Calendar:** Use MS365 MCP `outlook_calendar_search` to fetch today's meetings.

**3e. This Cycle candidates:** From the Jira sprint results (Step 3a), identify items that are **not Done** and **not already present anywhere in the backlog file** (check all sections — Today, Tomorrow, This Week, This Cycle, Next Week, Whenever). These are sprint commitments that have fallen off the radar.

### Step 4: Select Candidates via Emacs

Write all candidates to `/tmp/backlog-candidates-YYYY-MM-DD.md` with `- [ ]` lines, grouped by:
- Morning routine (Apply rosemary water, Apply sunscreen, Check emails, Check Slack, Check Teams, Charge devices)
- Today's meetings (from calendar — list as info, no checkboxes; don't add to backlog)
- My PRs — approved (ready to merge)
- My PRs — needs action
- PRs needing direct review — split into subsections:
  - **Recent (this week)**: PRs updated within the last ~7 days (only where `login: meain` is a direct reviewer, not team; exclude PRs with "local-dev" in the title, never include `vdc-shared-data-plane` PRs, and never include dependabot PRs from Veeam-VDC repos — they are auto-reviewed)
  - **Older (sitting for a while)**: PRs more than ~7 days old that are still open and awaiting review (same exclusion rules apply) — useful for surfacing stale reviews that may have been overlooked
  - **meain/* repos**: Open PRs in `meain`-owned repos (from Step 3b-ii), grouped separately — always include these regardless of age
- This Cycle — sprint items not yet in the backlog (from Step 3e); selected items go into the `### This Cycle` section, not Today
- Candidates from other sections (Tomorrow/This Week/Whenever worth pulling in)

The "Checked-off items to move to Before" header must include the day name, e.g. `(2026-06-03, Wednesday)`.

**Inline Jira details**: For any candidate line that references a Jira ticket (e.g. `[DP-1234](url)` or a `DP-1234` in a PR title), fetch the ticket with `jira issue view DP-1234 --plain` (run in parallel for all tickets, `dangerouslyDisableSandbox: true`) and add a concise detail line immediately below the candidate, indented and prefixed with `↳`. Format: `↳ DP-1234 (Type, Status, assignee/mine): one-line summary of the ask`. This lets the reader glance at ticket context while selecting. These `↳` lines are candidate-file only — do NOT carry them into `Backlog.md`, which stays single-line per item.

Invoke the `/edit-in-emacs` skill with the file path (blocking, 600s timeout). After it returns, treat remaining `- [ ]` lines as selections.

**Handling `(ignored)` markers:** After the emacs edit returns, scan the candidates file for any lines containing `(ignored)`. For each such line, extract the GitHub URL and append it (as a path fragment like `owner/repo/pull/N`) to `/Users/meain/.local/share/ignored-prs-for-review`. Do not add these PRs to the backlog.

**Never use AskUserQuestion / multi-select prompts** — always go straight to the emacs file.

### Step 5: Commit Then Update Backlog

Before writing any changes to the backlog file, commit the current state:

```bash
cd /Users/meain/.local/share/sbdb && git add -A && git commit -m "Before backlog updates"
```

Then:

1. Move items from other sections to Today (don't duplicate)
2. Add new "This Cycle" candidates to the `### This Cycle` section (not Today)
3. Add new items with reference links
4. Preserve priority emojis near top of Today

### Step 6: Generate Code Review Summaries (background, best-effort)

After the backlog is committed and usable, spin off **parallel background agents** — one per PR selected for review. Each agent should:

1. Fetch the PR diff and details from GitHub
2. Run `/explain-flow` on the PR to generate a code flow summary
3. Write the result to:
   ```
   /Users/meain/.local/share/sbdb/Artifacts/Code Reviews/<org>/<repo>/<pr-number>.md
   ```
   For example: `Artifacts/Code Reviews/Veeam-VDC/control-plane-backend/5046.md`
4. Update the matching backlog line to append a markdown link:
   ```
   - [ ] Review [cpb#5046](https://github.com/...): title (author) [summary](Artifacts/Code%20Reviews/Veeam-VDC/control-plane-backend/5046.md)
   ```
   Use `[summary]` as link text. Path is vault-root-relative (no `../`). URL-encode spaces (`Code%20Reviews`).
5. Commit the updated backlog file.

This step is best-effort — the backlog is already complete and usable from Step 5. Summaries enrich the review items but are not blocking.

### Formatting Rules

- Prefer inline links over trailing links (e.g. `Merge [repo#123](url)`)
- Trailing `[ref](url)` only for URLs without a natural inline anchor (Teams links)
- Each PR must be on its own line
- Never remove section headers (Today, Tomorrow, etc.) even when empty
- Show meeting times in IST (UTC+5:30)
- Keep all backlog entries short — a few words per line. Use task notes for detail.
- Only two checkbox states: `- [ ]` (open) and `- [x]` (done/won't-do). Never use `[-]`.
