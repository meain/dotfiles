---
name: tw
description: Manage tasks using Taskwarrior (task CLI) - create, list, update, complete, annotate, and manage dependencies between tasks.
triggers:
  - /tw
  - "create task"
  - "create issue"
  - "add task"
  - "list tasks"
  - "show task"
  - "close task"
  - "complete task"
  - "what tasks"
  - "what's ready"
  - "task status"
  - "update task"
  - "blocked tasks"
  - "blocking tasks"
---

# Taskwarrior Issue Management

Use the `task` CLI to manage issues with dependency tracking.

## When to Use

- The user mentions a task ID or wants to manage work items
- The user asks to create, view, update, close, or search tasks
- The user wants to see what work is ready or blocked
- The user wants to manage dependencies between tasks
- The user asks about project status

## Instructions

### Phase 1: Identify the Request

**IMPORTANT**: Always use UUID (short prefix is fine) instead of numeric IDs when referencing tasks. Numeric IDs are unstable and shift when tasks are completed or deleted — **never use numeric IDs in commands**, even if they appear in the output. IDs are shown in reports for the user's convenience only. After creating a task, note its UUID from the output and use that for all subsequent operations.

| Intent | Operation |
|--------|-----------|
| View task details | `task <uuid> information` |
| List/filter tasks | `task list`, `task project:<name> list` |
| See available work | `task ready` |
| Create a new task | `task add` |
| Update task fields | `task <uuid> modify` |
| Complete a task | `task <uuid> done` |
| Delete a task | `task <uuid> delete` |
| Add a comment/annotation | `task <uuid> annotate "text"` |
| Remove annotation | `task <uuid> denotate "text"` |
| Manage dependencies | `task <uuid> modify depends:<other_uuid>` |
| See blocked tasks | `task blocked` |
| See blocking tasks | `task blocking` |
| Project summary | `task summary` or `task projects` |
| Start working | `task <uuid> start` |
| Stop working | `task <uuid> stop` |
| Active tasks | `task active` |

### Phase 2A: Creating Tasks

```bash
# Basic task with project
task add "Task description" project:cpb

# With priority (H=high, M=medium, L=low)
task add "Fix bug" project:cpb priority:H

# With due date
task add "Deploy" project:cpb due:friday

# With tags
task add "Review" project:cpb +urgent +review

# With dependencies (use UUID of the dependency)
task add "Deploy to prod" project:cpb depends:a1b2c3d4

# Sub-projects using dots
task add "Fix API" project:cpb.earn
```

After creating a task, note the UUID from `task list` output for future reference.

### Phase 2B: Annotations (Comments)

```bash
# Add annotation (use UUID)
task a1b2c3d4 annotate "Spoke with Ryan, needs API change first"
task a1b2c3d4 annotate "See PR #1234 for context"

# Remove annotation
task a1b2c3d4 denotate "Spoke with Ryan"
```

### Phase 2C: Dependencies

```bash
# Add dependency using UUIDs (task abc depends on tasks def and ghi)
task a1b2c3d4 modify depends:d5e6f7a8,b9c0d1e2

# Remove dependency
task a1b2c3d4 modify depends:-d5e6f7a8

# View blocked/blocking
task blocked
task blocking
```

### Phase 2D: Filtering

```bash
# By project
task project:cpb list

# By priority
task priority:H list

# By tag
task +urgent list

# By status
task +BLOCKED list
task +OVERDUE list
task +ACTIVE list

# Text search
task /deploy/ list

# Combined filters
task project:cpb priority:H due.before:eow list
```

### Phase 2E: Modifying Tasks

```bash
task a1b2c3d4 modify priority:H
task a1b2c3d4 modify project:cpb.earn
task a1b2c3d4 modify due:eow
task a1b2c3d4 modify +urgent        # add tag
task a1b2c3d4 modify -urgent        # remove tag
task a1b2c3d4 modify depends:d5e6f7a8  # add dependency
```

### Phase 2F: Completing and Deleting

```bash
task a1b2c3d4 done                  # mark complete
task a1b2c3d4 delete                # mark deleted
task undo                    # revert most recent change
```

### Phase 2G: Reports and Views

```bash
task list                    # pending tasks
task next                    # most urgent
task ready                   # actionable (not blocked/waiting)
task all                     # everything
task completed               # done tasks
task summary                 # project summary
task projects                # project list with counts
```

### Phase 2H: Export/Import

```bash
task export                  # all as JSON
task project:cpb export      # filtered
task import tasks.json       # import
```

### Phase 3: Present Results

For **read operations**, summarize clearly:
- **UUID and Description**: Task identifier (UUID) and description
- **Status**: pending, completed, deleted, waiting
- **Priority**: H/M/L
- **Project**: Project hierarchy
- **Dependencies**: What blocks/is blocked by
- **Annotations**: Comments/notes
- **Due**: Due date if set

For **write operations**, confirm what was done:
- State the action taken and the task UUID
- Show the new state if relevant

## Guidelines

- Use `project:cpb` as the default project for control-plane-backend work
- Use sub-projects with dots for specificity (e.g., `project:cpb.earn`)
- Priority levels: H (high/critical), M (medium), L (low/backlog)
- Use annotations for context, links, and notes — but NOT for dependency info (use `depends:` instead)
- Always use taskwarrior's native `depends:` for dependency chains, never annotations
- When creating multi-environment rollouts (dev -> stage -> prod), create separate tasks and chain them with `depends:`
- Use `task ready` to find unblocked work (excludes blocked tasks)
- Use `task next` when the user asks "what should I work on"
- Use `task blocked` / `task blocking` to inspect dependency state
- Reference tasks in backlog using backtick format: `tw:<uuid>`
- Date shorthands: today, tomorrow, monday, eod, eow, eom, eoq, eoy, som, sow
- The default `list` report shows dependencies in the Deps column

ARGUMENTS: $ARGUMENTS
