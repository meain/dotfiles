---
name: jira-lookup
description: >
  Look up Jira issue information using the jira CLI.
  Triggers: /jira, "look up jira issue", "check jira ticket",
  "what's the status of PROJ-123", "get issue details",
  any mention of a Jira issue key like PROJ-123
---

# Jira Issue Lookup

Use the `jira` CLI to look up issue information, list issues, and provide context from Jira to the user.

## When to Use

- The user mentions a Jira issue key (e.g., `PROJ-123`)
- The user asks to look up, check, or get details on a Jira ticket
- The user wants to list or search for issues in a project
- The user needs context from a Jira issue to inform their work

## Instructions

### Phase 1: Identify the Request

Determine what the user needs:
- **Single issue lookup**: They mentioned a specific issue key (e.g., `PROJ-123`)
- **Issue search/list**: They want to find issues matching criteria (status, assignee, type, etc.)
- **Issue context for coding**: They want to understand a ticket before implementing it

### Phase 2: Fetch Issue Information

#### Viewing a single issue
```bash
jira issue view ISSUE-KEY --plain
```

To include recent comments:
```bash
jira issue view ISSUE-KEY --plain --comments 5
```

To get full raw JSON data for deeper inspection:
```bash
jira issue view ISSUE-KEY --raw
```

#### Listing/searching issues
```bash
# List issues in the default project
jira issue list --plain --no-truncate

# Filter by status
jira issue list --plain -s "In Progress"

# Filter by assignee
jira issue list --plain -a "user@example.com"

# Filter by type and status
jira issue list --plain -t Bug -s "To Do"

# Filter by priority
jira issue list --plain -y High

# Filter by label
jira issue list --plain -l backend

# Search by text
jira issue list "search query" --plain

# Combine filters
jira issue list --plain -s "In Progress" -a "me" -y High

# Use raw JQL for complex queries
jira issue list --plain -q "assignee = currentUser() AND status = 'In Progress'"

# Specify a different project
jira issue list --plain -p PROJECT_KEY

# Limit results
jira issue list --plain --paginate 20
```

### Phase 3: Present the Information

Summarize the issue information clearly for the user. Include:
- **Key and Summary**: The issue identifier and title
- **Status**: Current workflow state
- **Type**: Bug, Story, Task, Epic, etc.
- **Priority**: The issue priority level
- **Assignee**: Who is responsible
- **Description**: What the issue is about (summarize if very long)
- **Comments**: Recent discussion if relevant

When the user needs issue context for coding work, focus on:
- What needs to be done (acceptance criteria, description)
- Any technical details mentioned in comments
- Related issues or blockers

### Phase 4: Follow-up (if applicable)

If the user wants to take action after viewing the issue:
- To open the issue in a browser: `jira open ISSUE-KEY`
- To add a comment: `jira issue comment add ISSUE-KEY`
- To transition the issue: `jira issue move ISSUE-KEY "Status Name"`
- To assign the issue: `jira issue assign ISSUE-KEY "user@example.com"`

## Guidelines
- Always use `--plain` flag when listing issues so output is parseable (avoid the interactive TUI)
- Use `--no-truncate` with list commands to show full field contents when details matter
- If the jira CLI is not configured or authentication fails, inform the user and suggest running `jira init`
- Do NOT modify issues (move, assign, comment, edit) unless the user explicitly asks
- If a project key is ambiguous or missing, ask the user which project to query
- When an issue key is mentioned in a branch name or commit message, offer to look it up
- Summarize long descriptions and comment threads instead of dumping raw output
