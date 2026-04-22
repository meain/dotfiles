---
name: vault
description: >
  Manage the Obsidian vault at ~/.local/share/sbdb -- backlog tasks, meeting notes, journal entries,
  task notes, and general vault lookups. Triggers: /vault, "add to backlog", "add to my tasks",
  "check my backlog", "what's on my plate", "create meeting note", "save to vault", "look up in vault",
  "add to journal", "what did I discuss with", "find in my notes", "add to task", "meeting notes for"
---

# /vault -- Obsidian Vault Operations

Manage the user's Obsidian vault as a personal knowledge base and task system.

## Vault Location

**Root**: `/Users/meain/.local/share/sbdb`

## Vault Structure

| Path | Purpose |
|------|---------|
| `Backlog/Backlog.md` | **Primary backlog** -- sections: Today, Tomorrow, day names (Monday, Tuesday...), This Week, Weekend, Next Week, Whenever, Ongoing |
| `Backlog/{Work,Personal,InfraCloud,Books,Blog Ideas,Checkout}.md` | Legacy/archived backlogs (no longer actively used -- all tasks go in Backlog.md) |
| `Backlog/Refiled/YYYY/MM/` | Archived completed tasks (automated, do not manually manage) |
| `Tasks/YYYY-MM/<Task Name>.md` | Detailed task notes with research, context, links |
| `Meeting/YYYY/MM/DD/<Meeting Name>.md` | Meeting notes |
| `Journal/Day/YYYY/MM/YYYY-MM-DD.md` | Daily journal entries |
| `Templates/` | Templates for meetings, interviews, status updates |
| `My/` | Personal info (banking, health, investments) |
| `Veeam/` | Work notes for Veeam/VDC projects |
| `InfraCloud/` | InfraCloud interviews, weekly updates |
| `Resources/` | Reference materials (papers, books, articles) |
| `TIL/` | "Today I Learned" entries |
| Root level | ~435 files including people (~80), technical (~130), work (~40) notes |

## Task System Format

```markdown
- [ ] Task description                          # unchecked task
- [x] Task description                          # completed task
- [/] Task description (reason)                 # partially done / handed off
- [ ] Task description 🔺                       # high priority
- [ ] Task description ➕                       # major priority
- [ ] Task description 🏁                       # end-of-day deadline
- [ ] Task description ➡                       # follow-up needed
- [ ] Task description 📅 YYYY-MM-DD            # due date
- [x] Task description ✅ YYYY-MM-DD            # completion date
- [ ] [[Tasks/YYYY-MM/Task Name]]               # linked task note
- [ ] Description [PROJ-123](https://jira-url)  # linked to Jira
- [ ] Description [ref](https://teams-url)      # linked to Teams message
```

## Operations

### 1. View Backlog

Read `Backlog/Backlog.md` and present the relevant section(s).

- If user asks "what's on my plate" or "check my backlog" -- show Today + Tomorrow + This Week
- If user asks about a specific section -- show that section
- If user asks about ongoing work -- show the Ongoing section
- Filter out completed (`- [x]`) items unless specifically asked

### 2. Add Task to Backlog

Append a task line to the appropriate section in `Backlog/Backlog.md`.

**Defaults:**
- Section: **Today** (unless user specifies otherwise)
- Format: `- [ ] <description>`
- If user provides a Jira ticket, include the link: `[PROJ-123](https://veeam-vdc.atlassian.net/browse/PROJ-123)`
- If user provides a Teams/PR/URL reference, include as `[ref](url)`
- If the task needs a detailed note, also create the task note file (see below)

**Section keywords mapping:**
- "today", "now", "next" -> Today
- "tomorrow" -> Tomorrow
- "this week" -> This Week
- "weekend" -> Weekend
- "next week" -> Next Week
- "whenever", "someday", "low priority" -> Whenever
- "ongoing" -> Ongoing
- Day names ("monday", "tuesday", etc.) -> Match the corresponding `### Monday`, `### Tuesday` etc. section heading in the backlog. These are used for day-specific planning within the current or next week.

**Important:** Insert the new task at the END of the target section (before the next `###` heading), not at the beginning.

### 3. Complete Task

Find the task line in `Backlog/Backlog.md` and change `- [ ]` to `- [x]`.
Match by substring -- the user may give a partial description.
If multiple matches, show them and ask which one.

### 4. Create Task Note

Create a detailed task note at `Tasks/YYYY-MM/<Task Name>.md` where YYYY-MM is the current month.

**Format:** Free-form markdown. Start with a `## Summary` section if saving research or context.
Include references, links, bullet points as appropriate.

**Backlog linking:** If a task already exists in the backlog, update it to include the wiki-link. The task description and note name must match. For example, if the backlog has `- [ ] Design for Org Anchoring`, create `Tasks/2026-04/Design for Org Anchoring.md` and update the backlog line to `- [ ] [[Tasks/2026-04/Design for Org Anchoring]]`. If no backlog entry exists yet, add one: `- [ ] [[Tasks/YYYY-MM/<Task Name>]]`.

### 5. Add to Existing Task Note

Read the existing task note, then append or update content.
Task notes are at `Tasks/YYYY-MM/<Task Name>.md`.
If unsure which file, search with Glob: `Tasks/**/<partial-name>*.md`

### 6. Create Meeting Note

Create at `Meeting/YYYY/MM/DD/<Meeting Name>.md` using this template:

```markdown
---
date: YYYY-MM-DD
presenter:
participants:
tags:
---
### Agenda
### Notes
### Follow-ups
```

- Fill in the date automatically
- Fill in participants and tags if provided by the user
- Add agenda items if provided

**Also:** If the user wants it on the backlog, add: `- [ ] [[Meeting/YYYY/MM/DD/<Meeting Name>]]`

### 7. Update Meeting Note

Find and update an existing meeting note.
Search pattern: `Meeting/YYYY/MM/**/<partial-name>*.md`
Common updates:
- Add notes under `### Notes`
- Add follow-up items under `### Follow-ups`
- Extract follow-ups and add them to `Backlog/Backlog.md`

### 8. Journal Entry

Create or append to `Journal/Day/YYYY/MM/YYYY-MM-DD.md`.
Journal entries are freeform markdown. Each entry is a new paragraph prefixed with a timestamp in `HH:MM` format (24h).
When appending to an existing file, add a blank line before the new entry.
Create the directory structure if it doesn't exist.

**Example:**
```
14:30 Had a good sync with Rabee about org anchoring.

15:45 Finished reviewing the design doc for self-service schemas.
```

### 9. Vault Search / Lookup

Search the vault for information:

- **By person:** Check root-level files first (`<Person Name>.md`), then search in Meeting/ and Backlog/
- **By topic:** Use Grep across the vault
- **By date:** Navigate to the appropriate date-based directory
- **By meeting:** Search `Meeting/` directories
- **Recent activity:** Check recent entries in Backlog/Backlog.md, recent Meeting/ files, recent Journal/ entries
- **Wiki-link resolution:** `[[Page Name]]` links refer to files -- resolve by searching for `Page Name.md`

### 10. Save Content to Vault

When the user says "save this to my vault" or "add this to my notes":

1. Determine the best location based on content type:
   - Research/plans for a work task -> `Tasks/YYYY-MM/<name>.md`
   - Meeting output -> `Meeting/YYYY/MM/DD/<name>.md`
   - Person info -> root level `<Person Name>.md`
   - Book/article/blog idea/personal item -> `Backlog/Backlog.md` (Whenever section)
   - Technical reference -> `TIL/<topic>.md` or root level
2. Write using existing conventions (wiki-links, task format, templates)
3. Cross-link from backlog if actionable

### 11. Add to Specialized Backlog

**Specialized backlogs are no longer used.** All tasks go into `Backlog/Backlog.md` under the appropriate section.
The old files (`Books.md`, `Blog Ideas.md`, `Checkout.md`, `Personal.md`, `InfraCloud.md`, `Work.md`) exist but are legacy/archived.

## Conventions

- **Wiki-links:** Use full path for Tasks and Meetings: `[[Tasks/2026-04/Name]]`, `[[Meeting/2026/04/21/Name]]`. Use bare name for root-level pages: `[[Person Name]]`.
- **Dates:** Always use YYYY-MM-DD format.
- **File naming:** Use the natural name with spaces, not kebab-case. Match existing patterns.
- **Directory creation:** Create intermediate directories as needed (e.g., `Meeting/2026/04/22/` for a new date).
- **No frontmatter for tasks or backlog items.** Only meeting notes and journal entries use YAML frontmatter.
- **Preserve existing content:** When editing files like `Backlog.md`, only modify the specific lines needed. Never rewrite the whole file.

## Arguments

The skill accepts an optional argument describing what to do:

- `/vault` -- show today's backlog
- `/vault add <task>` -- add task to today's backlog
- `/vault add tomorrow <task>` -- add to tomorrow's section
- `/vault meeting <name>` -- create meeting note for today
- `/vault task <name>` -- create task note for current month
- `/vault search <query>` -- search the vault
- `/vault journal <content>` -- add to today's journal
- Free-form: `/vault what meetings did I have last week`

If no argument is provided, show Today + Tomorrow sections from the backlog.
