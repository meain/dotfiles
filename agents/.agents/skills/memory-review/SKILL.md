---
name: memory-review
description: >
  Review and clean up Claude auto-memory for the current project. Opens the memory list in
  Emacs for annotation, then applies deletions, updates, and relocations based on edits.
  Triggers: /memory-review, "review my memories", "clean up memories", "prune memories",
  "update my memories"
user_invocable: true
---

# Memory Review

Interactively prune and update the Claude auto-memory for the current project.

## Step 1: Find the Memory Directory

Derive the memory path from `pwd`:

```bash
memory_dir="$HOME/.claude/projects/$(pwd | sed 's|/|-|g')/memory"
```

Verify it exists. If not, tell the user there are no memories for this project.

## Step 2: Write the Review File

Read `$memory_dir/MEMORY.md`. For each entry, write a review file to
`/tmp/memory-review-YYYY-MM-DD.md` containing the MEMORY.md bullet list exactly as-is,
with instructions at the top:

```markdown
# Memory Review — YYYY-MM-DD
# Instructions:
#   - Delete a line to remove that memory file entirely
#   - Add an inline note to request changes, e.g.:
#       (move to AGENTS.md)   → extract content into AGENTS.md, delete memory
#       (move to <skill>)     → add to the named skill's SKILL.md, delete memory
#       (update: <note>)      → update the memory file with the note
#       (delete)              → same as deleting the line
#   - Leave a line unchanged to keep it as-is
# Save and C-x # when done.

<paste MEMORY.md bullet list here>
```

## Step 3: Open in Emacs

```bash
emacsclient /tmp/memory-review-YYYY-MM-DD.md
```

Use `timeout: 600000` and `dangerouslyDisableSandbox: true`.

## Step 4: Parse the Edits

After Emacs returns, read the edited file and compare against the original list.
Use the diff (from the harness system-reminder if present, otherwise compare manually).

For each change:

### Deleted line
- Delete the corresponding memory file from `$memory_dir/`
- Remove its entry from `$memory_dir/MEMORY.md`

### Line with `(delete)` annotation
- Same as deleted line.

### Line with `(move to AGENTS.md)` or `(move to ~/.agents/AGENTS.md)`
- Read the memory file content
- Find the appropriate section in `/Users/meain/.dotfiles/agents/.agents/AGENTS.md`
  (note: AGENTS.md is a symlink — always edit the dotfiles path directly)
- Insert the rule/fact (distilled to a single bullet) in the relevant section
- Delete the memory file and remove from MEMORY.md

### Line with `(move to <skill name>)`
- Find the skill: check `~/.claude/skills/<skill>/SKILL.md` first,
  then `~/dev/veeam/oncall-copilot-plugins/plugins/*/skills/<skill>/SKILL.md`
- Read the memory file content
- Add a concise rule to the appropriate place in the skill's SKILL.md
- Delete the memory file and remove from MEMORY.md

### Line with `(update: <note>)` or freeform annotation
- Read the memory file
- Update the body to incorporate the annotation (add the note, correct a fact, etc.)
- Update the MEMORY.md one-liner if the description changed

### Unchanged line
- No action.

## Step 5: Confirm

After applying all changes, output a brief summary:
- N memories deleted
- N memories updated
- N memories relocated (to where)
- N unchanged
