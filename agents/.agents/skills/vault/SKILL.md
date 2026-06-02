---
name: vault
description: >
  Manage the Obsidian vault at ~/.local/share/sbdb -- backlog tasks, meeting notes, journal entries,
  task notes, and general vault lookups. Triggers: /vault, "add to backlog", "add to my tasks",
  "check my backlog", "what's on my plate", "create meeting note", "save to vault", "look up in vault",
  "add to journal", "what did I discuss with", "find in my notes", "add to task", "meeting notes for"
---

# /vault -- Obsidian Vault Operations

**Root**: `/Users/meain/.local/share/sbdb`

## Vault Structure

| Path | Purpose |
|------|---------|
| `Backlog/Backlog.md` | **Primary backlog** — sections: Today, Tomorrow, day names, This Week, Weekend, Next Week, Whenever, Ongoing |
| `Tasks/YYYY-MM/<Task Name>.md` | Detailed task notes |
| `Meeting/YYYY/MM/DD/<Name>.md` | Meeting notes |
| `Journal/Day/YYYY/MM/YYYY-MM-DD.md` | Daily journal |
| `Templates/` | Templates |
| Root level | ~435 files: people (~80), technical (~130), work (~40) notes |

## Task Format

```
- [ ] Task description                        # unchecked
- [x] Task description                        # completed
- [ ] Description 🔺                          # high priority
- [ ] Description 📅 YYYY-MM-DD               # due date
- [ ] [[Tasks/YYYY-MM/Task Name]]             # linked task note
- [ ] Description [PROJ-123](url)             # linked to Jira
- [ ] Description [ref](url)                  # linked to external ref
```

## Operations

### 1. View Backlog
Read `Backlog/Backlog.md`. Show Today + Tomorrow + This Week by default. Filter out completed items unless asked.

### 2. Add Task to Backlog
Append `- [ ] <description>` to the end of the target section in `Backlog/Backlog.md`.

**Section keywords:**
- "today"/"now"/"next" → Today | "tomorrow" → Tomorrow | "this week" → This Week
- "weekend" → Weekend | "next week" → Next Week | "whenever"/"someday" → Whenever | "ongoing" → Ongoing
- Day names → match the `### DayName` section heading

Include links: `[PROJ-123](https://...)` for Jira, `[ref](url)` for Teams/PRs.
If a detailed task note is needed, create it first then link with wiki-link.

### 3. Complete Task
Find the line by substring match in `Backlog.md`, change `- [ ]` to `- [x]`. If multiple matches, show and ask.

### 4. Create Task Note
Save to `Tasks/YYYY-MM/<Task Name>.md`. Free-form markdown. If a backlog entry exists, add the wiki-link: `[[Tasks/YYYY-MM/Name]]`. The task name in the backlog and the note filename must match.

### 5. Add to Existing Task Note
Read and append. Search with `Tasks/**/<partial-name>*.md` if unsure of the exact name.

### 6. Create Meeting Note
Save to `Meeting/YYYY/MM/DD/<Name>.md`:
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
Optionally add to backlog: `- [ ] [[Meeting/YYYY/MM/DD/<Name>]]`

### 7. Update Meeting Note
Find with `Meeting/YYYY/MM/**/<partial-name>*.md`. Add to Notes, Follow-ups, and optionally extract follow-ups to backlog.

### 8. Journal Entry
Create or append to `Journal/Day/YYYY/MM/YYYY-MM-DD.md`. Each entry is a new paragraph prefixed with `HH:MM` timestamp. Create dirs as needed.

### 9. Vault Search
- **By person**: check root-level `<Name>.md` files, then Meeting/ and Backlog/
- **By topic**: grep across the vault
- **By date**: navigate date directories
- **By meeting**: search Meeting/ subdirs
- **Recent**: check recent backlog entries, meeting files, journal entries
- **Wiki-links**: `[[Page Name]]` → search for `Page Name.md`

### 10. Save Content to Vault
Determine best location based on content type:
- Research/plans → `Tasks/YYYY-MM/<name>.md`
- Meeting output → `Meeting/YYYY/MM/DD/<name>.md`
- Person info → root `<Name>.md`
- Technical reference → `TIL/<topic>.md` or root
- Book/article/blog idea → `Backlog/Backlog.md` (Whenever)
- Cross-link from backlog if actionable

## Conventions
- Wiki-links: full path for Tasks/Meetings (`[[Tasks/2026-04/Name]]`), bare name for root-level (`[[Person Name]]`)
- Dates: YYYY-MM-DD
- File naming: natural name with spaces, not kebab-case
- Create intermediate dirs as needed
- No frontmatter for tasks/backlog items — only meeting notes and journal use YAML
- Only modify specific lines when editing Backlog.md — never rewrite the whole file

## Arguments
`/vault` — show Today backlog
`/vault add <task>` — add to Today
`/vault add tomorrow <task>` — add to Tomorrow
`/vault meeting <name>` — create meeting note
`/vault task <name>` — create task note
`/vault search <query>` — search vault
`/vault journal <content>` — add journal entry
Free-form: `/vault what meetings did I have last week`
