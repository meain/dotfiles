---
name: daily-todo-dashboard
description: >
  Find work that is not tracked in the backlog (my open PRs by state, Jira tickets, Teams
  messages tagged to me, review requests tagged to me in the EARN code-review channel) and
  post it as one Dashy board entry, then archive the agent. Used by the scheduled Dashy
  agent `day-plan` and by hand.
  Triggers: /daily-todo-dashboard, "what am I not tracking", "untracked work", "plan my day",
  "check what I'm missing"
user_invocable: true
---

# What am I not tracking

Output: one Dashy board entry, nothing else. No HTML page, no widget, no long chat report.
I read the entry, then decide what goes into the backlog. Do not edit the backlog or any
other file. After the entry is posted, call `archive_when_done` so the agent archives
itself, then finish the turn.

## Board entry

Post with `board_post`: kind `result`, key `untracked-work` (a repeat run replaces the old
entry), title `Untracked work, <YYYY-MM-DD>`, a one-sentence `summary` with counts. The body
is markdown: bold section headers (not `#` headings, no umbrella title), then
`- [ ] [title](url), detail` lines, one item per line. Skip a section with nothing in it
except Review requests and Teams, which say "none" so I know they were checked.
Do not set the `links` field of `board_post`: every URL is already linked inline in the
body, so it only adds a redundant list of links at the bottom.

Sections, in this order:

- Own PRs by state: Needs review, Blocked needs review, Changes requested, Approved ready
  to merge, Merge conflict. Each PR is its own line with how long ago it was opened.
- Review requests tagged to me in the channel.
- Jira, open and not in the backlog, one section per status: To Do, In Progress, In Review,
  then any other status by its own name (Blocked and so on). Each line is key, title, and
  type when it is a bug or epic. Skip a status with no tickets.
- Teams: messages tagged to me, and replies to my posts that I haven't answered.
- Calendar: only a meeting that needs prep or an action from me that the backlog doesn't
  cover. Skip the section otherwise.
- Already in your backlog: last section of the body, after all the others, set off from
  them by a `---` divider line. One line per PR
  or Jira ticket you dropped because the backlog or a task note already covers it, as
  `- [repo#n](url)` or `- [DP-123](https://veeam-vdc.atlassian.net/browse/DP-123)`, with
  the title if you have it. Every key is a link. Skip the section if nothing was dropped.

Keep it short. Only things the backlog does not already track.

## Steps

1. Run `date` first and use that date everywhere.
2. Read `/Users/meain/.local/share/sbdb/Backlog/Backlog.md` (all sections) so you know what
   is already tracked. Search linked task notes (`Tasks/YYYY-MM/...`) when deciding if a
   Jira key or PR is covered. Never edit it.
3. Own PRs: `gh search prs --author=@me --state=open --owner Veeam-VDC --limit 100 --json number,title,repository,url,isDraft`
   (`gh pr list` fails outside a GitHub checkout), then per PR
   `gh pr view <n> -R <owner>/<repo> --json state,isDraft,reviewDecision,mergeStateStatus,createdAt,url,title`.
   Skip drafts. Map to a state: REVIEW_REQUIRED means Needs review (BLOCKED means Blocked,
   needs review), CHANGES_REQUESTED, APPROVED, merge state DIRTY means Merge conflict. Note
   unresolved human review threads or failing checks on the PR's line. Leave out PRs already
   covered by a backlog item.
4. Review requests: read the EARN backend code reviews channel with `teams_list_channel_messages`
   (teamId `7e735ea1-c2fb-430e-a0d3-395e63680899`, channelId
   `19:5729597e716c441b8d30bcc3e4240b86@thread.tacv2`), paging back to the last working
   day's start. The list preview shows only text, so call `read_resource` on a candidate to
   see its `mentions`, including thread replies. Only a post whose mentions include me counts.
   Do not list every pending GitHub review request. Verify each linked PR with `gh pr view`
   (state, isDraft); drop merged, closed and draft PRs and bot PRs.
5. Jira: `jira issue list -a$(jira me) -s~Done --plain`. Keep tickets whose key does not
   appear in the backlog or its task notes. Drop canceled ones. Group the rest by status
   (the status column of the list output). If a ticket may be the same work as a backlog
   item under a different name, keep it and say so in the detail.
6. Teams: search from the last working day's start with an explicit offset
   (`afterDateTime: 2026-11-12T00:00:00+05:30`; result times are UTC). Run in parallel
   `recipient` = my address and my own posts in the review channel (read their thread
   replies with `parentMessageId`). Page every query to the end. Skip incident.io and Azure
   alerts, workflow cards, all-hands and meeting-chat chatter, and anything I sent. Before
   adding an ask, check whether I already replied after it or whether someone else answered
   in the same chat.
7. Calendar (optional section): `timeout 90 ,calendar-today` with
   `dangerouslyDisableSandbox: true`. It can hang; if it fails, say so in the reply.
8. Post the board entry, call `archive_when_done`, and finish.

## Verify before adding

A stale item costs trust, and a missed one costs more. Take PR state from `gh`, not from a
chat message. Write an age, date or weekday only if a source you read this run gives it
(`createdAt` for "opened N days ago", a message timestamp). Never use a PR's last-updated
time as its age, and never estimate. If something cannot be confirmed, keep it only when
missing it would hurt and say what could not be confirmed.

## Environment notes

- `gh`, `jira` and `,calendar-today` need `dangerouslyDisableSandbox: true` (network or
  socket access).
- Sandboxed Bash writes only to `$TMPDIR`; scratch output goes there.
- Microsoft 365 tools may be missing or unauthenticated. Say so in one line and carry on.
  A connector authorized mid-session only appears in a new session.
- `chat_message_search` can fail with ECONNRESET. Retry once, then report which query is
  not covered.

## Reply

After posting, the reply is short: what was found (counts per section), what could not be
fetched or fully paged (name each gap and the dates covered), then the board entry id.
