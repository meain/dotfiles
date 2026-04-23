---
name: taskwarrior
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

## Important Notes

### Sandbox
The `task` CLI needs read/write access to `~/.task/` which is typically outside the sandbox. Commands will fail with "unable to open database file" errors. Use `dangerouslyDisableSandbox: true` for all `task` commands.

### Output Formatting
Use `rc.verbose=label` on list/report commands to keep column headers but strip footer noise (task count, config overrides, sync messages). Prefer this over `rc.verbose=nothing` which strips headers too and makes output harder to parse.

For programmatic use or when you need structured data (e.g., to build a markdown table), use `task export` which outputs JSON. You can filter it the same way: `task project:cpb export`, `task +BLOCKED export`, etc.

## Instructions

### Phase 1: Identify the Request

**IMPORTANT**: Always use UUID (short prefix is fine) instead of numeric IDs when referencing tasks. Numeric IDs are unstable and shift when tasks are completed or deleted — **never use numeric IDs in commands**, even if they appear in the output. IDs are shown in reports for the user's convenience only. After creating a task, use `rc.verbose=new-uuid` to get the UUID directly (see Phase 2A).

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

Always use `rc.verbose=new-uuid` when creating tasks — this makes `task add` output the UUID directly instead of the unstable numeric ID.

**Task titles**: Write clean, concise descriptions — don't just copy-paste alert names or ticket titles verbatim. Distill the key info: service, symptom, environment. For example, instead of `Elastic - [prod] [swedencentral] API Error - earn-svc - 5xx PrdeuswEARNQueryOrg`, use `Investigate earn-svc 5xx errors in prod swedencentral (PrdeuswEARNQueryOrg)`.

**Tags and projects**: Do NOT invent tags or projects. Only use tags and projects that already exist in the task database. Before creating a task, check existing tasks (`task rc.verbose=label projects` and `task rc.verbose=label tags`) to see what's in use. If no matching tag/project exists, create the task without tags/projects and let the user decide. The only exception is `project:on-call` and `priority:H` for on-call investigation tasks — these are always correct.

```bash
# Basic task with project
task rc.verbose=new-uuid add "Task description" project:cpb

# With priority (H=high, M=medium, L=low)
task rc.verbose=new-uuid add "Fix bug" project:cpb priority:H

# With due date
task rc.verbose=new-uuid add "Deploy" project:cpb due:friday

# With tags
task rc.verbose=new-uuid add "Review" project:cpb +urgent +review

# With dependencies (use UUID of the dependency)
task rc.verbose=new-uuid add "Deploy to prod" project:cpb depends:a1b2c3d4

# Sub-projects using dots
task rc.verbose=new-uuid add "Fix API" project:cpb.earn
```

The output will be like `Created task a1b2c3d4-...` — use that UUID for all subsequent operations.

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

**IMPORTANT**: Always use `tag:` attribute syntax when adding tags — the `+tag` syntax is unreliable with `modify` and can be misinterpreted as a description. Use `-tag` to remove tags.

```bash
task a1b2c3d4 modify priority:H
task a1b2c3d4 modify project:cpb.earn
task a1b2c3d4 modify due:eow
task a1b2c3d4 modify tag:urgent          # add/set tag
task a1b2c3d4 modify tag:on-call,prod    # set multiple tags
task a1b2c3d4 modify -urgent             # remove tag
task a1b2c3d4 modify depends:d5e6f7a8    # add dependency
```

### Phase 2F: Task Notes

Detailed notes for tasks live in `/Users/meain/.local/share/tasknotes/`. Each note is a markdown file named `<uuid-prefix> - <task name>.md`.

**All task-related resources go here** — not just notes, but also drafts, templates, support ticket messages, or any other artifacts tied to a task. Name them with the task UUID prefix for discoverability (e.g., `<uuid> - Azure support ticket draft.md`). Never save task-related files to `/tmp` or other locations.

**When completing a task**, always write detailed notes summarizing what was done, findings, and outcomes. If you learned something new during the task (new patterns, gotchas, field types, service behaviors, etc.), also create a separate learning note named `<uuid-prefix> learning.md` with bullet points of key learnings.

After writing notes, annotate the task with the filenames:

```bash
# Write the note file
# /Users/meain/.local/share/tasknotes/<uuid> - <task name>.md

# Annotate the task to reference the note
task a1b2c3d4 annotate "notes: <uuid> - <task name>.md"

# If there are learnings too
task a1b2c3d4 annotate "notes: <uuid> - <task name> - learning.md"
```

Notes should include:
- Summary of what was done
- Root cause / findings (for investigations)
- Evidence and key data points
- Links to PRs, runbooks, or other artifacts
- Next steps if applicable

Learning notes should be concise bullet points covering:
- New patterns or conventions discovered
- Gotchas and pitfalls encountered
- Service behaviors or field types that weren't obvious
- Useful queries or commands found

### Phase 2G: Completing and Deleting

```bash
task a1b2c3d4 done                  # mark complete
task a1b2c3d4 delete                # mark deleted
task undo                    # revert most recent change

# Revert a completed task back to pending (works for any task, not just the last action)
task a1b2c3d4 modify status:pending
```

**On completion**: Always write task notes (and learning notes if applicable) before marking done. See Phase 2F.

### Phase 2H: Reports and Views

Use `rc.verbose=label` on all list/report commands for clean output with headers.

```bash
task rc.verbose=label list                    # pending tasks
task rc.verbose=label next                    # most urgent
task rc.verbose=label ready                   # actionable (not blocked/waiting)
task rc.verbose=label all                     # everything
task rc.verbose=label completed               # done tasks
task rc.verbose=label summary                 # project summary
task rc.verbose=label projects                # project list with counts
```

### Phase 2I: Export/Import

Use `task export` for structured JSON output — useful for building markdown tables or programmatic processing. Supports the same filters as other commands.

```bash
task export                  # all as JSON
task project:cpb export      # filtered by project
task +BLOCKED export         # filtered by virtual tag
task status:pending export   # filtered by status
task import tasks.json       # import
```

### Phase 3: Present Results

For **read operations**, present as a markdown table when showing multiple tasks:

| # | UUID | Task | Status |
|---|------|------|--------|
| 8 | `30bd26f9` | Check with Juan about schema validation | **ready** |
| 9 | `5466d0bb` | Fix schema validation in dev | blocked by #8 |

Include relevant columns: UUID, description, status/blockers, priority, due date, project. For single tasks, a brief summary is fine.

For **write operations**, confirm what was done:
- State the action taken and the task UUID
- Show the updated task list if multiple tasks were affected

## External Lookups

When the user references external links or identifiers in task arguments, look them up to extract context (title, status, summary) for use in task descriptions and annotations.

| Source | How to look up |
|--------|---------------|
| GitHub PR/issue URL | `GIT_DIR=$(jj git root 2>/dev/null \|\| echo .git) gh pr view <number> --repo <owner/repo> --json title,state` or `gh issue view` |
| Jira ticket (e.g. `PROJ-123`) | `jira issue view PROJ-123` |
| Confluence page URL | `confluence page view --url <url>` or use WebFetch |
| Generic webpage URL | Use WebFetch to retrieve the page title/summary |

After looking up, use the extracted info to:
1. Write a clean task title (don't just paste the URL)
2. Annotate the task with the URL for reference
3. Create dependency tasks if the task depends on the external item (e.g., "merge PR#X" as a blocker)

## Guidelines

- Tasks tagged `on-call` should always have `priority:H` — on-call items are top priority
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
- Use `rc.confirmation=0` to suppress confirmation prompts on destructive operations (delete, done on multiple tasks)
- When task changes reflect broader project state changes (e.g., a revert, a new blocker), consider updating auto-memory to keep project context current

ARGUMENTS: $ARGUMENTS
