---
name: commit
description: Create git commits for dotfiles changes, grouping by component with conventional prefix style
---

When the user asks you to commit changes in this dotfiles repo, follow these steps:

## Important Context
- This is a **stow-based dotfiles** repo using **git** (not jj)
- Changes are organized by component directories: `emacs/`, `zsh/`, `home-manager/`, `scripts/`, etc.
- Commits follow the format: `[component] brief description`
- Each component's changes should be a **separate commit**
- Do NOT add `Co-Authored-By` lines unless the user asks for it

## Steps

### 1. Understand Current State
Run these in parallel:
```bash
git status
git diff --staged
git diff
git log --oneline -10
```

### 2. Group Changes by Component
- Look at modified file paths and group them by their top-level directory (e.g. `emacs/`, `zsh/`, `home-manager/`)
- Files in the same component that are part of the same logical change go in one commit
- If a component has unrelated changes, split into separate commits

### 3. Draft Commit Messages
For each group, draft a message following the existing style:
- Format: `[component] brief-area: what changed`
- Examples from this repo:
  - `[emacs] fix dired block brackets`
  - `[zsh] tweak how reporoot and gitroot is set`
  - `[scripts] ,markdown-to-pdf: use gfm input format and fix parameter handling`
  - `[home-manager] add emacs-lsp-booster package`
- Use a sub-area (e.g. `utils:`, `web:`) when it clarifies which part of the component changed
- Keep the first line short and lowercase
- Add a detailed body (separated by blank line) when the reasoning behind the change is not obvious from the diff

### 4. Create Commits
For each group, stage the relevant files and commit:
```bash
git add <specific-files> && git commit -m "$(cat <<'EOF'
[component] brief description

Optional detailed explanation of why this change was made,
if not obvious from the diff itself.
EOF
)"
```

- **Stage specific files** — never use `git add -A` or `git add .`
- Issue all `git add && git commit` calls as **parallel tool calls in a single response** so the user can approve them all at once

### 5. Verify and Report
Run `git log --oneline -5` to confirm all commits were created, then show the output to the user.

## Guidelines
- Do NOT push unless explicitly requested
- Do NOT commit files containing secrets
- If the user has both staged and unstaged changes, ask which to include
- Prefer fewer commits when changes are closely related within a component
- **Skip `claude/.claude/settings.json` model changes** — the `"model"` field changes frequently and is not worth tracking; omit it from commits unless other meaningful settings changed alongside it
