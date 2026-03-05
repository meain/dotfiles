---
name: create-commit
description: Create a Jujutsu (jj) commit with a detailed reproduction prompt
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
- **CRITICAL**: Create a "Reproduction Prompt" that an agent could use to recreate the changes
  - For simple changes: may match the original user request
  - For complex changes: consolidate my prompts or reverse engineer one
- Ensure accuracy of the summary
- Do NOT commit secrets (.env, credentials.json, etc.)

### 3. Prepare Commit Message
Draft a message in this format:
```
Brief summary of what changed and why

## Reproduction Prompt
[Prompt that an AI assistant could use to recreate
these changes from scratch]
```

### 4. Create the Commit
Once your message is ready, use:
```bash
jj describe -m "$(cat <<'EOF'
[Your drafted message here]
EOF
)"
```

### 5. Verify
Run `jj status` and `jj log -r @` to confirm the commit was created successfully.

## Guidelines
- Keep summary brief (1-2 sentences)
- Make reproduction prompt clear and actionable for an AI agent
- The reproduction prompt helps maintain project history and enables future recreation
- NEVER use TodoWrite or Task tools for commits
- DO NOT push unless explicitly requested
- If commit fails: fix the issue and try again
