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

Generate a comprehensive self-review by aggregating GitHub PRs, Jira tickets, and Confluence data.

## Arguments

`$ARGUMENTS` — optional time range (e.g. "last quarter", "Q1 2026", "last 6 months"). Defaults to last quarter.

## Workflow

### 1. Setup
```bash
gh api user --jq '.login'
cat ~/.dotfiles-private/datafiles/.config/datafiles/prs-repos
```

### 2. Gather GitHub PRs Authored (parallel across repos)
```bash
gh pr list --repo <REPO> --author <USERNAME> --state all --limit 100 \
  --json number,title,state,createdAt,mergedAt,url
```
Filter to createdAt within time range.

### 3. Gather GitHub PRs Reviewed
```bash
gh search prs --reviewed-by=<USERNAME> --owner=Veeam-VDC --merged \
  --json title,number,repository,updatedAt --limit 100
```
Filter to time range.

### 4. Gather Jira Tickets (parallel)
```bash
jira issue list --assignee="$(jira me)" --order-by "updated" --reverse --paginate 100 --plain --no-headers --columns key,status,summary,type,created,updated
jira issue list --reporter="$(jira me)" --order-by "updated" --reverse --paginate 100 --plain --no-headers --columns key,status,summary,type,created,updated
```
Merge by ticket key, filter to time range.

### 5. Gather Confluence Context (parallel)
```bash
confluence search 'creator = currentUser() order by lastmodified desc' --cql --limit 20
confluence search 'contributor = currentUser() order by lastmodified desc' --cql --limit 20
confluence search 'space = "~71202041711d825b9c4649b13c777b34245dc2" AND title ~ "Abin" AND title ~ "goals" order by lastmodified desc' --cql --limit 10
```
Merge by page ID. Read relevant pages: quarterly goals (under parent ID 294783303), design docs, runbooks.

### 5b. On-Call Data
```bash
confluence search 'title ~ "Weekly Report" AND text ~ "<full name>" order by created desc' --cql --limit 20
```
Also cross-reference Jira bugs/tasks mentioning "on-call", "investigate", or "prod".

### 6. Analyze & Categorize
Group into themes: major features, bug fixes, ops improvements, tooling, testing, code reviews, on-call, design/docs. For each: summary, key PRs (repo#number), key Jira tickets.

### 7. Goals vs. Delivery
If quarterly goals found, produce a comparison table:

| Goal | Status | Evidence |
|------|--------|----------|
| ... | DONE / PARTIAL / NOT STARTED / DROPPED | PRs, tickets |

### 8-10. Strengths, Areas for Improvement, Current Focus
Identify 3-5 each. For gaps: look for stale tickets, missing docs, knowledge concentration, PR size patterns.

### 11. Generate Report
Write to `/tmp/self-review-<period>.md`:

```markdown
# Work Self-Review: <Period>
**Author, Date, Team**

## Summary

## Key Accomplishments (per theme)
### 1. <Theme> — description, key PRs, key Jira

## Goals vs. Delivery (if applicable)

## On-Call & Incident Response

## What's Working Well

## Areas for Improvement

## Current Focus

## By the Numbers
| Metric | Count |
|--------|-------|
| PRs authored | N |
| PRs reviewed | N |
| Jira tickets completed | N |
| ... | ... |
```
Do not include report content in chat — just confirm it was opened.
