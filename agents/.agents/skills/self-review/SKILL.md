---
name: self-review
description: >
  Generate a comprehensive work self-review by pulling data from GitHub PRs,
  Jira tickets, and Confluence pages. Covers accomplishments, patterns, areas
  for improvement, and current focus. Accepts an optional time range argument
  (e.g., "last quarter", "Q1 2026", "last 6 months") defaulting to last quarter.
  Triggers: /self-review, "generate self review", "introspect my work", "quarterly review"
user_invocable: true
---

# Work Self-Review Generator

Generate a comprehensive self-review of your work by aggregating data from GitHub (PRs authored & reviewed), Jira (tickets completed), and Confluence (goals pages, design docs).

## Arguments

`$ARGUMENTS` is an optional time range string (e.g., "last quarter", "Q1 2026", "last 6 months", "oct 2025 to apr 2026"). Default to "last quarter" if empty.

## Workflow

### 1. Setup

```bash
# Get GitHub username
gh api user --jq '.login'

# Get repo list
cat ~/.dotfiles-private/datafiles/.config/datafiles/prs-repos
```

### 2. Gather GitHub PRs Authored

For each repo in the repos file, fetch PRs authored by the user within the time range. Run these in parallel across repos:

```bash
gh pr list --repo <REPO> --author <USERNAME> --state all --limit 100 \
  --json number,title,state,createdAt,mergedAt,url
```

Filter results to PRs whose `createdAt` falls within the requested time range.

### 3. Gather GitHub PRs Reviewed

Search for PRs reviewed by the user across all repos in the org:

```bash
gh search prs --reviewed-by=<USERNAME> --owner=Veeam-VDC --merged \
  --json title,number,repository,updatedAt --limit 100
```

Filter to the requested time range.

### 4. Gather Jira Tickets

Fetch tickets both assigned to and created/reported by the user. Run both in parallel:

```bash
# Tickets assigned to me
jira issue list --assignee="$(jira me)" --order-by "updated" --reverse \
  --paginate 100 --plain --no-headers \
  --columns key,status,summary,type,created,updated

# Tickets I created/reported
jira issue list --reporter="$(jira me)" --order-by "updated" --reverse \
  --paginate 100 --plain --no-headers \
  --columns key,status,summary,type,created,updated
```

Merge and deduplicate the two lists by ticket key. Filter to the requested time range. Include all statuses (Done, In Progress, Canceled, To Do) for a complete picture.

### 5. Gather Confluence Context

Search Confluence for pages the user created, contributed to, or is mentioned in. Use CQL queries with `--cql` flag for precise filtering, and text search for broader context. Run these in parallel:

```bash
# Pages I created (most recent first)
confluence search 'creator = currentUser() order by lastmodified desc' --cql --limit 20

# Pages I contributed to (edited/commented on)
confluence search 'contributor = currentUser() order by lastmodified desc' --cql --limit 20

# Quarterly goals pages — stored under "VDC Data Pipelines Team Goals" in Gokul's space
# Search directly for the user's goals page for the relevant quarter:
confluence search 'space = "~71202041711d825b9c4649b13c777b34245dc2" AND title ~ "Abin" AND title ~ "goals" order by lastmodified desc' --cql --limit 10
```

Merge and deduplicate results by page ID. Filter to pages modified within the requested time range where possible.

Read relevant pages (goals for the matching quarter, design docs, runbooks authored) using:

```bash
confluence read <PAGE_ID>
```

Focus on reading:
- Quarterly goals pages (to compare stated goals vs. actual delivery) — these live under the "VDC Data Pipelines Team Goals" parent page (ID: 294783303) in Gokul Ramanan Subramanian's personal Confluence space, structured as: Parent → Quarter folder (e.g. "Q1'26 goals") → Individual page (e.g. "Abin Simon - Q1'26 goals")
- Design docs authored during the period
- Runbooks or guides created
- Weekly reports with notable contributions

### 5b. Gather On-Call Data

Search Confluence for weekly on-call reports that mention the user during the time range:

```bash
confluence search 'title ~ "Weekly Report" AND text ~ "<USERNAME full name>" order by created desc' --cql --limit 20
```

For each matching weekly report, read it and look for:
- Weeks where the user was listed as on-call staff
- Incidents/alerts the user investigated or mitigated
- Any customer-impacting or event-producer-impacting incidents handled

Also cross-reference with Jira tickets of type Bug/Task that mention "on-call", "investigate", or "prod" in the summary.

### 6. Analyze & Categorize

Group the collected data into themes. Look for:

- **Major features/projects:** Multi-PR efforts that deliver a capability end-to-end
- **Bug fixes & investigations:** Production incidents, on-call work
- **Operational improvements:** Dashboards, alerts, DLQs, reliability work
- **Developer tooling:** CLI tools, CI improvements, nix/build improvements
- **Testing:** E2E tests, integration tests, test infrastructure
- **Code reviews:** Notable reviews, cross-team contributions (repos outside the user's core set show breadth)
- **On-call & incident response:** On-call weeks, incidents handled, production investigations
- **Design & documentation:** Design docs, runbooks, Confluence pages created

For each major theme, include:
- A summary of what was accomplished and why it matters
- Key PR references (repo#number format)
- Key Jira ticket references

### 7. Goals vs. Delivery Comparison

If quarterly goals pages were found in step 5, produce an explicit comparison. For each stated goal, determine its status based on the collected PR/Jira/Confluence data:

- **DONE** — Goal fully delivered, evidence in merged PRs / completed Jira tickets
- **PARTIAL** — Some work delivered but goal not fully met
- **NOT STARTED** — No evidence of work toward this goal
- **DROPPED/CHANGED** — Goal was canceled or scope changed (note why if apparent)

Present this as a checklist table in the report. This is one of the most valuable parts of the self-review — it shows commitment vs. delivery clearly.

### 8. Identify Strengths

Based on the data, identify 3-5 things that are working well. Look for:
- Patterns of end-to-end ownership
- Testing discipline
- Operational mindset
- Cross-repo/cross-team work
- Tooling investments

### 9. Identify Areas for Improvement

Based on the data, identify 3-5 areas for improvement. Look for:
- Stale backlog items (tickets in To Do for extended periods)
- Features that went to code without design docs
- Knowledge concentration risks
- Missing operational capabilities (monitoring gaps, etc.)
- PR size patterns

### 10. Current Focus

List tickets currently In Progress or recently moved to In Progress.

### 11. Generate Report

Write the report as markdown to `/tmp/self-review-<period>.md` where `<period>` is a slug of the time range.

Structure:

```markdown
# Work Self-Review: <Period>

**Author:** <Full Name>
**Date:** <Today's Date>
**Team:** <Team Name>

---

## Summary

<2-3 sentence high-level summary of work focus areas>

---

## Key Accomplishments

### 1. <Project/Theme Name>
<Description of what was done and why>

**Key PRs:** #<num>, #<num>, ...
**Key Jira:** DP-<num>, DP-<num>, ...

### 2. <Next Theme>
...

---

## Goals vs. Delivery

*If quarterly goals page was found, include this section.*

| Goal | Status | Evidence |
|------|--------|----------|
| <Goal from Confluence> | DONE / PARTIAL / NOT STARTED / DROPPED | <Brief evidence: PRs, tickets> |
| ... | ... | ... |

---

## On-Call & Incident Response

- **On-call weeks:** <N weeks>
- **Incidents handled:** <list with brief description and Jira links>

---

## What's Working Well

1. **<Strength>:** <Evidence>
...

---

## Areas for Improvement

1. **<Area>:** <Evidence and suggestion>
...

---

## Current Focus

- <In-progress item 1>
- <In-progress item 2>
...

---

## By the Numbers

| Metric | Count |
|--------|-------|
| PRs authored | <N> |
| PRs reviewed | <N> |
| Jira tickets completed | <N> |
| Confluence pages created/contributed | <N> |
| On-call weeks | <N> |
| Production incidents investigated | <N> |
| Design docs authored | <N> |
```

## Important Notes

- Fetch data in parallel across repos wherever possible
- Use `gh api` and `gh search` for GitHub data, `jira` CLI for Jira, `confluence` CLI for Confluence
- Do NOT use MCP tools — use CLI tools only
- Cross-reference Jira tickets with PRs to build richer narratives around projects
- Confluence goals pages are valuable for understanding intent vs. delivery — compare stated goals with actual work
- Keep the report focused on themes and patterns, not exhaustive PR-by-PR listings
- Include PR numbers in repo#number format so they render as clickable links
- Do not include the report content in the chat response — just confirm it was opened
- The user's full name can be inferred from Confluence search results or `gh api user`
