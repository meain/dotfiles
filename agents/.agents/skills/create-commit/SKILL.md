---
name: create-commit
description: Create a Jujutsu (jj) commit with a concise message
---

When the user asks you to create a new commit, follow these steps to create a commit using Jujutsu (jj):

## Important Context
- This project uses **Jujutsu (jj)**, NOT git
- All changes in the working copy are automatically tracked
- Use `jj describe` to set commit messages (opens interactive editor)

## Steps to Create a Commit

### 1. Understand Current State
Run these jj commands in parallel:
```bash
jj status           # See all changes in working copy
jj diff             # See detailed changes
jj log -r 'ancestors(@, 3)'  # See recent commits for style reference
```

### 2. Analyze and Draft Message
- Summarize the nature of changes (feature, enhancement, bug fix, refactoring, test, docs, etc.)
- Ensure accuracy of the summary
- Do NOT commit secrets (.env, credentials.json, etc.)

### 3. Create the Commit
Draft a concise commit message (1-2 sentences explaining what changed and why), then use:
```bash
jj describe -m "$(cat <<'EOF'
[Your drafted message here]
EOF
)"
```

### 4. Create New Revision
After describing the commit, always create a new empty revision on top:
```bash
jj new
```

### 5. Verify
Run `jj log -r 'ancestors(@, 3)'` to confirm the commit and new revision were created successfully.

## Branch/Bookmark Naming
- For Veeam repos (`/Users/meain/dev/veeam/`), use `meain/` prefix for branch names
- Example: `meain/earn-prod-adx-sku-downsize`, `meain/fix-auth-bug`
- For other repos, follow the project's naming conventions

## Guidelines
- Keep the first line (header) under 72 characters; use a blank line + body for details if needed
- Keep the message brief (1-2 sentences)
- NEVER use TodoWrite or Task tools for commits
- DO NOT push unless explicitly requested
- If commit fails: fix the issue and try again
