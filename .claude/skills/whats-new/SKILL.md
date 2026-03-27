---
name: whats-new
description: List notable new tools, scripts, aliases, and config changes from recent git history
---

Generate a concise list of notable new features from recent git history. Focus on things the user would actively use, not passive improvements or fixes.

## Steps

### 1. Get Recent Commits
```bash
git log --oneline --since="1 month ago"
```
If the user specifies a different time range, use that instead.

### 2. Identify Alias Changes
Check for new aliases that map to scripts:
```bash
git log --since="1 month ago" -p -- 'zsh/.config/zsh/aliases' 'zsh/.config/zsh/.zsh-custom/*alias*'
```
Look for added alias lines (lines starting with `+alias`) to pair with new scripts.

### 3. Filter and Categorize
Only include items the user would actively invoke or benefit from knowing about. Skip:
- Bug fixes, refactors, passive improvements
- Lock file updates, flake input updates
- Gitignore changes
- Minor config tweaks

Categorize into:
- **New tools** — new scripts or skills with their alias if one exists (format: `,script-name` (`alias`))
- **Config changes worth remembering** — non-obvious behavior changes the user should know about

### 4. Output Format
Use this exact format:

```
**New tools:**
- `,script-name` (`alias`) — one-line description

**Config changes worth remembering:**
- brief description of the change
```

Omit a section if empty. Keep descriptions to one line each.
