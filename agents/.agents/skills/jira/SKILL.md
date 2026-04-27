---
name: jira
description: >
  Manage Jira issues using the jira CLI — lookup, create, edit, transition, assign, comment, link, and search.
  Triggers: /jira, "look up jira issue", "check jira ticket", "create jira issue", "update jira",
  "move ticket to", "assign issue", "add comment to", "what's the status of PROJ-123",
  any mention of a Jira issue key like PROJ-123
argument-hint: "[ISSUE-KEY or action description]"
---

# Jira Issue Management

Use the `jira` CLI to look up, create, edit, transition, assign, comment on, and link Jira issues.

## When to Use

- The user mentions a Jira issue key (e.g., `DP-123`)
- The user asks to look up, check, or get details on a Jira ticket
- The user wants to list or search for issues in a project
- The user wants to create, edit, transition, assign, comment on, or link issues
- The user needs context from a Jira issue to inform their work

## Instructions

### Phase 1: Identify the Request

Determine which operation the user needs:

| Intent | Operation |
|--------|-----------|
| View issue details | **view** |
| Search/list issues | **list** |
| Create a new issue | **create** |
| Edit fields on an issue | **edit** |
| Change issue status | **move** |
| Assign/unassign an issue | **assign** |
| Add a comment | **comment** |
| Link two issues | **link** |
| Open in browser | **open** |

### Phase 2A: View / Search (read operations)

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

# Filter by assignee (use "me" for current user)
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

### Phase 2B: Create (write operation)

Always use `--no-input` to avoid interactive prompts.

```bash
# Basic creation (always specify Story type)
jira issue create -tStory -s "Summary here" -b "Description here" --no-input

# Other issue types when explicitly needed
jira issue create -tBug -s "Bug title" -yHigh -lbug -lurgent -a "user@example.com" -b "Bug description" --no-input
jira issue create -tTask -s "Task title" -b "Description here" --no-input

# Create subtask under a parent
jira issue create -tSubtask -P PARENT-KEY -s "Subtask title" -b "Details" --no-input

# In a specific project
jira issue create -p PROJECT -tStory -s "Story title" -b "As a user..." --no-input

# With custom fields
jira issue create -tStory -s "Story" --custom story-points=3 --no-input

# Pipe description from stdin for long bodies
echo "Long description here" | jira issue create -tStory -s "Summary" --no-input
```

**Important**: When creating or editing issue descriptions:
- Use markdown format for descriptions
- Start with a descriptive paragraph, not section headings like "Context" or "Background"
- Structure content naturally with markdown formatting (lists, code blocks, etc.)
- **Always explicitly specify `-tStory` when creating issues unless told to use a different type**

**Issue types:** Task, Bug, Story, Epic, Subtask

### Phase 2C: Edit (write operation)

Always use `--no-input` to avoid interactive prompts.

```bash
# Edit summary
jira issue edit ISSUE-KEY -s "New summary" --no-input

# Edit description
jira issue edit ISSUE-KEY -b "New description" --no-input

# Change priority
jira issue edit ISSUE-KEY -y High --no-input

# Add labels (append)
jira issue edit ISSUE-KEY -l newlabel --no-input

# Remove a label (prefix with -)
jira issue edit ISSUE-KEY --label -oldlabel --no-input

# Change assignee
jira issue edit ISSUE-KEY -a "user@example.com" --no-input

# Edit custom fields
jira issue edit ISSUE-KEY --custom story-points=5 --no-input

# Combine multiple edits
jira issue edit ISSUE-KEY -s "Updated title" -yHigh -l backend --no-input
```

### Phase 2D: Transition / Move

```bash
# Move issue to a new status
jira issue move ISSUE-KEY "In Progress"
jira issue move ISSUE-KEY "Done"

# Move with a comment
jira issue move ISSUE-KEY "Done" --comment "Completed in PR #123"

# Move and reassign
jira issue move ISSUE-KEY "In Review" -a "reviewer@example.com"
```

### Phase 2E: Assign

```bash
# Assign to a user
jira issue assign ISSUE-KEY "user@example.com"

# Assign to self
jira issue assign ISSUE-KEY $(jira me)

# Unassign
jira issue assign ISSUE-KEY x
```

### Phase 2F: Comment

Always use `--no-input` to avoid interactive prompts.

```bash
# Add a comment
jira issue comment add ISSUE-KEY --no-input "Comment text here"

# Multi-line comment
jira issue comment add ISSUE-KEY --no-input $'First line\n\nSecond paragraph'

# Internal comment (not visible to external users)
jira issue comment add ISSUE-KEY --no-input "Internal note" --internal
```

### Phase 2G: Link

```bash
# Link two issues
jira issue link ISSUE-1 ISSUE-2 "Blocks"
jira issue link ISSUE-1 ISSUE-2 "Duplicate"

# Unlink
jira issue unlink ISSUE-1 ISSUE-2
```

### Phase 2H: Sprint & Epic

```bash
# List sprints
jira sprint list --plain

# Add issues to a sprint
jira sprint add SPRINT_ID ISSUE-KEY

# Add issues to an epic
jira epic add EPIC-KEY ISSUE-KEY

# Remove from epic
jira epic remove ISSUE-KEY
```

### Phase 2I: Open in Browser

```bash
jira open ISSUE-KEY
```

### Phase 3: Present Results

For **read operations**, summarize clearly:
- **Key and Summary**: The issue identifier and title
- **Status**: Current workflow state
- **Type**: Bug, Story, Task, Epic, etc.
- **Priority**: The issue priority level
- **Assignee**: Who is responsible
- **Description**: What the issue is about (summarize if very long)
- **Comments**: Recent discussion if relevant

For **write operations**, confirm what was done:
- State the action taken and the issue key
- Show the new state if relevant (e.g., new status after a move)

When the user needs issue context for coding work, focus on:
- What needs to be done (acceptance criteria, description)
- Any technical details mentioned in comments
- Related issues or blockers

## Guidelines

- Always use `--plain` flag when listing/viewing issues so output is parseable (avoid the interactive TUI)
- Always use `--no-input` flag on create/edit/comment to avoid interactive prompts
- Use `--no-truncate` with list commands to show full field contents when details matter
- If the jira CLI is not configured or authentication fails, inform the user and suggest running `jira init`
- **Confirm before mutating**: For write operations (create, edit, move, assign, comment, link, delete), confirm the action with the user before executing unless they gave an explicit and unambiguous instruction
- If a project key is ambiguous or missing, ask the user which project to query
- When an issue key is mentioned in a branch name or commit message, offer to look it up
- Summarize long descriptions and comment threads instead of dumping raw output
- The default project is `DP` (Data Pipelines) — only specify `-p` when targeting a different project
