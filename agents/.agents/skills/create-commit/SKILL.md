---
name: create-commit
description: Create Jujutsu (jj) commits — simple one-shot or split across logical groups. Also suggest splitting when changes span multiple unrelated areas or when user explicitly requests it.
---

Create one or more jj commits from the working copy. If the user asked to split changes or if the diff spans clearly unrelated areas, use the split workflow. Otherwise, default to a single commit.

## Important Context
- Uses **Jujutsu (jj)**, not git
- All changes in the working copy are automatically tracked

## Steps

### 1. Understand Current State
Run these in parallel:
```bash
jj status           # See all changes
jj diff             # See detailed changes
jj log -r 'ancestors(@, 3)'  # Recent commits for style reference
```

### 2. Assess Changes
Check if changes should be a single commit or split:
- **Single commit**: changes are few, logically one unit (feature, bug fix, refactor)
- **Suggest split**: changes span multiple distinct areas (e.g. unrelated features, tests + code changes + config). Present the proposed groups to the user and let them decide.
- **User requested split**: skip suggestion, go straight to split workflow

### 3a. Simple Commit
```bash
jj describe -m "$(cat <<'EOF'
brief description (1-2 sentences)

Optional body if reasoning isn't obvious from the diff
EOF
)"
jj new
```

Guidelines:
- Keep first line under 72 chars
- Do NOT commit secrets
- Do NOT push unless requested
- Do NOT add `Co-Authored-By` lines unless the user asks

### 3b. Split into Logical Commits
1. Group changed files into logical sets — by feature/purpose, layer (code vs tests vs config), or file relationships
2. Present the proposed grouping to the user and wait for confirmation
3. For N groups, do N-1 splits:
   ```bash
   jj split -m "$(cat <<'EOF'
<commit message for this group>
EOF
)" <file1> <file2> ...
   ```
4. After all splits, describe the remaining commit:
   ```bash
   jj describe -m "$(cat <<'EOF'
<commit message for last group>
EOF
)"
   ```
5. Create new working revision:
   ```bash
   jj new
   ```

### 4. Verify
```bash
jj log -r 'ancestors(@, 3)'
```

## Branch/Bookmark Naming
- Veeam repos: prefix with `meain/` (e.g. `meain/earn-prod-adx-sku-downsize`)
- Other repos: follow project conventions

## Notes
- If changes within a single file need splitting, mention this limitation and suggest `jj split -i` done manually
- Never use interactive split (`-i` flag) in scripts
- Prefer meaningful groups over many tiny commits
