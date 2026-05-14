---
name: split-commit
description: "Analyze changed files and split them into separate jj commits for each logical group of changes. Triggers: /split-commit, 'split into commits', 'split changes', 'commit each change separately', 'split this into logical commits'"
---

Split the current working copy changes into separate Jujutsu (jj) commits, one per logical group.

## Steps

### 1. Gather Current State

Run in parallel:
```bash
jj status
jj diff
jj log -r 'ancestors(@, 3)'
```

### 2. Analyze and Group Changes

Look at the full diff and group changes into logical hunks. A logical group is a set of changes that belong together conceptually. Consider:

- **By feature/purpose**: Changes implementing the same feature or fixing the same bug
- **By layer**: Separating refactors from new functionality from test changes
- **By file relationship**: Files that are tightly coupled (e.g., a module and its tests)
- **Config/infra vs code**: Keep build config, dependency changes, or CI changes separate

For each group, determine:
1. A short descriptive commit message
2. The list of files (or file patterns) belonging to that group

If changes within a single file belong to different logical groups, note this — but prefer splitting by whole files when possible since `jj split` with filesets operates at the file level.

Present the proposed split plan to the user as a numbered list:
```
1. <commit message> — file1, file2
2. <commit message> — file3, file4
3. <commit message> — file5
```

Wait for user confirmation before proceeding. If the user says "go ahead" or similar without the initial plan being shown, skip confirmation — they want autonomous execution.

### 3. Execute the Split

Use `jj split` to peel off one group at a time. For N groups, do N-1 splits (the last group stays in the current commit).

For each group (except the last):
```bash
jj split -m "$(cat <<'EOF'
<commit message for this group>
EOF
)" <file1> <file2> ...
```

This puts the selected files into a new parent commit with the given message, and the remaining changes stay in `@`.

After all splits, describe the final remaining commit:
```bash
jj describe -m "$(cat <<'EOF'
<commit message for last group>
EOF
)"
```

### 4. Create Empty Working Revision

After all splits and describes are done, create a new empty revision on top:
```bash
jj new
```

This ensures `@` is a clean working copy on top of the split commits.

### 5. Verify

```bash
jj log -r 'ancestors(@, <N+2>)'
```

Show the resulting commit stack to the user.

## Guidelines

- Keep commit messages brief (1-2 sentences)
- Prefer fewer, meaningful groups over many tiny commits
- If all changes are logically one unit, tell the user and suggest using `/commit` instead
- Do NOT push unless explicitly requested
- NEVER use interactive mode (`-i` flag) — always use filesets
- If changes within a single file need splitting, mention this limitation and suggest the user do that part manually with `jj split -i`
