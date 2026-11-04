---
name: daily-todo-dashboard
description: >
  Build the day's todo dashboard: gather calendar, PRs, Jira sprint, backlog and Teams
  follow-ups, turn them into ranked todos, render one self-contained HTML page into the
  vault's .martifacts, and end the reply with a clickable link (never open it). Used by the
  scheduled Dashy agent `day-plan` and by hand.
  Triggers: /daily-todo-dashboard, "build my todo dashboard", "day plan dashboard",
  "plan my day", "regenerate the todo dashboard"
user_invocable: true
---

# Daily todo dashboard

Output: `/Users/meain/.local/share/sbdb/.martifacts/<YYYY-MM-DD>-todo-dashboard.html`.
Display (the default template, `references/dashboard-alt.html`):

- A "Today" strip of all of today's events with start and end times (past ones dimmed, next
  one highlighted). Show the events as the calendar gives them; never decide when I do a
  todo or how long it takes, so there is no scheduler and no time estimates anywhere.
- A list grouped by priority (High priority, Normal, Low, On hold), with Table and Cards
  views switchable in the header. Titles are plain weight, not bold.
- Colored category pills per todo (`src`): Merge green, Chase review orange, PR review blue,
  Teams purple, Backlog grey, Blocked red. The same colored buttons above the list filter by
  category (multi-select, with counts, "All" clears). No colored row edges, pills only.
- Each todo can be ignored (x on the row, or "Ignore for today" in the detail panel). Ignored
  items move to a collapsed "Ignored (n)" section with Restore.
- A right-hand detail panel: why, links, notes, "Copy agent prompt", "Mark done".
- "Copy backlog update prompt" in the header (done items, ignored-on-purpose items, open).
- State (done, ignored, notes, view, filter) lives in the browser per day, keyed by todo `id`.

Other templates: `render.py --board` (columns by action with a detail drawer) and
`--classic` (old two-pane page with the scheduler). Use them only when I ask.

Do not open the page. Do not edit the backlog or any other file. The only file you write
is the dashboard (plus the data JSON in `/tmp`).

When I ask for design alternatives or mock-ups, follow the global rule (an HTML options page
in `.martifacts`), then apply the pick to the template here.

## Steps

1. Run `date` first and use that date everywhere (`day`, the file name, "last working day").
2. Calendar: `,calendar-today` with `dangerouslyDisableSandbox: true` (prints today's events
   in IST AM/PM, one per line). Don't use icalBuddy. If it fails unsandboxed, say so and
   leave `events` empty. Put every event in `events` as-is. The output has no "optional"
   marker, so never set `opt` from the title (that was a past mistake). Convert to 24h.
   An event that shows a different timezone label (e.g. an all-shift on-call block) is
   an all-day style block: leave it out and mention it.
   On-call check: call `schedule_list` with `user: "me"` on the incident.io MCP
   (`mcp__claude_ai_Incident_io_--_Veeam_CT_Gateway__schedule_list`, load with `ToolSearch select:`).
   A non-empty `user_current_shift` means I am on call (the calendar's on-call block is
   labelled UTC but the shift times in the schedule are authoritative). If I am on call
   on a weekday, add two todos: promote earn from stage to prod first, then dev to
   stage/qa. On a Friday add only dev to stage (no prod promotion on Fridays). Weekend:
   no promotion todos. Say in `ctx` when the shift ends. If incident.io is unavailable,
   say so in the reply.
3. `,backlog-review` with `dangerouslyDisableSandbox: true`: open PRs, PRs needing my
   review, Jira sprint tickets. It prints the PR URLs. Ignore its calendar section. Its
   "PRs to review" list is incomplete (it showed about 29 when 68 PRs requested me), so
   reconcile it against GitHub with `dangerouslyDisableSandbox: true`:
   `gh search prs --review-requested=@me --state=open --owner Veeam-VDC --limit 100 --json number,title,repository,author,isDraft,updatedAt`
   then keep the ones that name me directly (the search also returns team-owner requests):
   `gh pr view <n> -R Veeam-VDC/<repo> --json reviewRequests --jq '[.reviewRequests[]|select(.login=="meain")]|length'`.
   Drop drafts, bot PRs, `vdc-shared-data-plane`, and titles with "local-dev". Check the
   state of any PR a Teams message asks me to approve (`gh pr view --json state,latestReviews`);
   merged PRs are not todos. Group old requests too (a stack from three weeks ago still counts)
   and show their age.
   Also check feedback on my own open PRs (`gh pr list --author @me --state open`, then
   `gh pr view --json reviews,comments,reviewDecision,mergeable` plus unresolved
   `reviewThreads` via `gh api graphql`). Unresolved human review threads, changes
   requested, merge conflicts and failing checks go into the `ctx` of that PR's Chase
   review or Merge todo; a human comment I haven't answered is its own `dev` todo.
   Bot comments (coderabbit, claude, github-actions) count only when unresolved and real.
4. Read `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`: the dated section for today
   (Today list), plus Backlog, Blocked and High priority backlog. If the newest dated
   section is an earlier day (e.g. Monday morning, newest is Sunday), carry its open items
   over and say so in `ctx`. Read linked task notes (`Tasks/YYYY-MM/...`) only when they
   change how a todo ranks or what its context is.
5. Teams, via the Microsoft 365 MCP tools (`mcp__claude_ai_Microsoft_365__chat_message_search`
   and `read_resource`; load with `ToolSearch select:` if deferred):
   - `chat_message_search`, `afterDateTime` = last working day's start (Monday: Friday).
     Query `*` alone is flooded by alert and bot channels, so also search by `sender`
     for the people who usually ask things of me (Tim Ford, Ryan Keepers, Rabee) and by
     `recipient` = my address, and by `sender` = my address (my own replies). Run these in
     parallel. Page every query to the end (`offset` = `nextOffset` until no `moreResults`
     or results predate the window); do not stop at a page cap. If a query still cannot be
     finished, say which dates were covered.
   - Also read the team's PR review request channel `project.EARN.backend.CodeReviews`
     (`teams_list_channel_messages`, teamId `7e735ea1-c2fb-430e-a0d3-395e63680899`, channelId
     `19:5729597e716c441b8d30bcc3e4240b86@thread.tacv2`, page to the window start; load with
     `ToolSearch select:`). It holds all of the team's review requests. Only posts that tag
     me (check `mentions` via `read_resource`, also in thread replies) are directed at me;
     make review todos only for those, and list untagged ones as `fyi` at most. Still verify
     PR state with `gh`.
   - Skip incident.io and Azure alert posts, all-hands/meeting-chat chatter, workflow
     cards, and anything I sent. Use `read_resource` when a preview is cut off (channel
     messages sometimes return NOT_FOUND; skip them and say so).
   - Before creating any Teams todo, check whether I already handled it: run one more
     search with `sender` = my address over the same window, and match by chat or topic
     (`chatId` of the ask, ticket or PR numbers). If I replied after the ask, drop it. A
     past miss: a todo to answer Rabee's INTUNE_ROLE question when I had already replied in
     that DM with the ticket. Approvals and merges happen on GitHub, so for asks like "needs
     a re-approval" Teams can't tell; keep those and say so in `ctx`.
   - Keep only real todos: things asked of me, things I said I'd do, review or follow-up
     requests, unanswered questions to me. Skip anything already covered by the backlog or
     step 3.
   - If the tools are missing or unauthenticated, say so in one line and carry on. After
     the connector gets authorized the tools only appear in a new session.
6. Build the data JSON (schema below) at `/tmp/todo-dashboard-<date>.json`.
7. Render:
   `python3 ~/.agents/skills/daily-todo-dashboard/scripts/render.py /tmp/todo-dashboard-<date>.json /Users/meain/.local/share/sbdb/.martifacts/<date>-todo-dashboard.html`
   The script validates the data; fix what it reports and re-run.
8. Reply (format below).

## Data JSON

See `references/example-data.json` for a full real example. Top level:

```json
{ "day": "2026-10-29", "title": "Todo dashboard: 2026-10-29",
  "subtitle": "Thu 2026-10-29, generated 8:02 AM IST",
  "events": [ {"s": "12:00", "e": "13:00", "t": "Meeting title"} ],
  "todos": [ ... ] }
```

- `events`: today's meetings, 24h IST. An end past midnight is `24:30`. Skip all-day events.
  Only the classic template reads `"opt": true`; do not set it unless I say a meeting is
  optional.
- Each todo:

```json
{ "id": "stack-kqlcheck", "src": "Merge", "proj": "EARN",
  "title": "Merge the approved kqlcheck stack in earn (10 PRs)",
  "urg": 5, "imp": 3, "age": 7, "blocks": false, "held": false,
  "tags": ["merge", "kqlcheck"],
  "ctx": "One to three sentences: why it is on the list and what matters.",
  "links": [ {"t": "Run kqlcheck tests in CI", "u": "https://github.com/..."} ] }
```

Fields:

- `id`: unique and stable. Regenerating on the same day must give the same ids so ticks and
  ordering survive. Use `pr-<number>` for one PR, `stack-<slug>` for a stack,
  `bl-<title-slug>` for backlog items, `tm-<message id>` for Teams items.
- `src` must be one of `Backlog`, `Merge`, `Chase review`, `PR review`, `Teams`, `Blocked`.
  `proj` is free text (EARN, Platform, KaaS, People, ...).
- `urg` and `imp` 1 to 5 set the priority group. `age` = days open (0 = new today).
  `eff` is optional and ignored by the default template; do not estimate durations.
- `blocks`: finishing it unblocks someone else. `held`: on hold or blocked (goes to the
  On hold group).
- `tags` drive the agent-prompt type: include `review` for review todos and `dev` for
  implementation todos. Other tags are free (`fyi`, `reporting`, `tim`, ...).
- `links`: PRs, Teams messages (`webUrl`), Jira, task notes. A task note link is
  `{"t": "Task note", "u": "obsidian://open?path=<url-encoded absolute path>"}`.

Rank honestly rather than evenly. Rough guide: urgency 5 = overdue or blocking a deadline
today, 4 = due today, 3 = this week, 2 = soon, 1 = whenever. Impact 5 = unblocks several
people or ships something, 3 = normal, 1 = nice to have.

### Grouping

- One todo per stack or batch, with each PR as a link: approved PRs to merge (`Merge`),
  my open PRs awaiting review (`Chase review`, split backend and cp-api companions), and a
  collaborator's stack that should be reviewed together.
- One todo per standalone review (`PR review`), with the author in the title in
  parentheses: `Query view by name (rs-malik)`. If a stack should wait (author restacking,
  depends on another PR), set `held` and say why in `ctx`.
- Backlog Today items keep the backlog wording. Pull their context from the task note and
  link related PRs.
- Teams items: one line of `ctx` with who asked and what, link the message. Mark
  unclear-ownership ones as `fyi`.
- Review candidates: skip draft PRs, PRs titled "local-dev", and anything in the
  `vdc-shared-data-plane` repo. Don't filter by author.

### Verify before adding (be thorough)

A todo on the list that is already done costs trust, and a missed PR costs more. Before a
todo goes in, check it against current state, and finish each source before summarizing it.

- Teams ask: did I already reply (search `sender` = me), and is the PR or ticket it names
  still open? Past misses: a todo to answer Rabee's INTUNE_ROLE question (I had replied) and
  one to re-approve Tim's API PRs 865, 876 and 878 (all merged and approved by others).
- PRs: take the state from `gh`, not from a chat message or the tool's summary.
- Coverage: page every search to the end and reconcile `,backlog-review` with the GitHub
  query in step 3. Report what was not fully covered instead of assuming it was.
- If something cannot be verified, keep the todo only when missing it would hurt, and say
  in `ctx` what could not be confirmed.

### Environment notes

- Sandboxed Bash can write only `$TMPDIR`, not `/tmp`. Write the data JSON with the Write
  tool (that works at `/tmp`), and send any scratch output from Bash to `$TMPDIR`.
  `render.py` writes the dashboard into the vault `.martifacts`, which is allowed.
- `chat_message_search` date filters are not obviously IST. Use an explicit offset such
  as `afterDateTime: 2026-10-30T00:00:00+05:30`, and note the results' `createdDateTime`
  values are UTC.
- `gh` needs `dangerouslyDisableSandbox: true` (outbound network).

## Reply format

Short and direct. No file edits to report.

- One line on what's in it: counts of open todos, how many are overdue or aging (5d+), how
  many Teams follow-ups.
- "Couldn't fetch or check": always include this, even if it is only "nothing". Name each
  gap: calendar failed, Teams unavailable or only partly paged (give the dates covered),
  messages that couldn't be opened, ask-already-answered unknowns, events left out.
- "Do first": the top 3 todos, one line each.
- "At risk": overdue or stuck items, one line each.
- Whenever a PR is mentioned show its title as a markdown link to the PR URL, followed by
  the author in parentheses for PRs I'm reviewing. No repo names or bare PR numbers as
  text. Every PR mention gets a link.
- Last line, nothing after it:
  `[Open the dashboard](file:///Users/meain/.local/share/sbdb/.martifacts/<date>-todo-dashboard.html)`
