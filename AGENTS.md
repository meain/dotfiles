## What This Is

A personal dotfiles repository managed with [GNU Stow](https://www.gnu.org/software/stow/). Each top-level directory is a stow package that mirrors the home directory structure. To install a package: `stow <package>` (e.g., `stow nvim` symlinks `nvim/.config/nvim/` to `~/.config/nvim/`).

## Repository Structure

- Each top-level directory is a stow package containing config files in their XDG-relative paths (e.g., `emacs/.config/emacs/`, `zsh/.config/zsh/`)
- `scripts/.local/bin/` — custom scripts organized by category (`git/`, `utils/`, `tmux/`, `ai/`, `jj/`, etc.). Scripts are prefixed with `,` (comma) to distinguish them from system commands
- `_install/` — package lists (brew, cargo, go, node, python) and a macOS setup script
- `nix-darwin/.config/nix-darwin/` — nix-darwin system config (flake.nix, system.nix, homebrew.nix)
- `home-manager/.config/home-manager/` — home-manager config (home.nix and modules)
- `agents/.agents/skills/` — shared Claude Code skills (commit, recall, etc.)
- `esa/.config/esa/` — esa (CLI AI tool) agent configs

## Commit Convention

Commits follow the format: `[component] brief description`

- Component is the top-level stow package directory (e.g., `emacs`, `zsh`, `scripts`, `home-manager`)
- Use sub-areas when helpful: `[emacs] web: refactor github-url helpers`
- Keep first line short and lowercase
- Group changes by component into separate commits — stage specific files, never `git add -A`
- Never combine multiple scripts into a single commit — each script gets its own commit
- Always use a single component name in `[component]` — pick the primary one if changes span multiple
- Do NOT add `Co-Authored-By` lines unless asked
- Use the `/commit` skill which handles all of this automatically
- Git index locking breaks parallel commits — issue `git add && git commit` calls sequentially, never in parallel
- Commit `home-manager/.config/home-manager/flake.lock` and `nix-darwin/.config/nix-darwin/flake.lock` separately with `[home-manager]` and `[nix-darwin]` prefixes, not together as `[nix]`
- Commits that don't belong to a specific stow package (root-level meta files, shared configs) use `[]` with no component name
- If unsure about conventions or component names, check `git log` for similar past changes

## Key Config Files

- **Emacs**: `emacs/.config/emacs/init.el` — primary editor config (uses elpaca package manager, evil mode)
- **Zsh**: `zsh/.config/zsh/.zshrc` and `zsh/.config/zsh/.zsh-custom/`
- **Git**: `git/.config/git/config`
- **Jujutsu**: `jj/.config/jj/config.toml`
- **Ghostty**: `ghostty/.config/ghostty/config`
- **Tmux**: `tmux/.config/tmux/tmux.conf`

## Important: File Paths

When config files reference scripts or other dotfiles (e.g., shell aliases calling `,some-script`), those files live in this repo, not at their home directory symlink targets. Always look for and edit files here in the repo — the home directory paths are just symlinks back to this repo.

## Finding Commands

When the user references a command, use `which <command>` to resolve it. If it's an alias, follow through with `which` again on the resolved name to get the script path, then find it in this repo. Example:

```
$ which bb
bb: aliased to ,bm
$ which ,bm
/Users/meain/.local/bin/random/,bm
```

That path is a symlink into this repo at `scripts/.local/bin/random/,bm`.

## Conventions

- Use `,picker` instead of `fzf` directly in scripts — it's a wrapper that supports both terminal and GUI picker backends
- Prefer fewer commits when changes are closely related within a component — the "one commit per script" rule applies to independent scripts, not tightly coupled edits

## Shell Scripts

When creating or modifying shell scripts:

- **Shebang**: Use `#!/bin/sh` by default (not `#!/bin/bash`). Only use `#!/usr/bin/env bash` when you need bash-specific features (e.g., `read -ei` for editable input, arrays, `[[`, `set -euo pipefail`). Add a comment on line 2 explaining why bash is needed.
- **Second line**: After the shebang, line 2 should be either:
  - A descriptive comment explaining what the script does (for sh scripts)
  - A comment explaining why bash is required (for bash scripts)
- **Error handling**: Use `set -e` on line 3 or 4 (after the initial comment). For bash scripts that need stricter error handling, use `set -euo pipefail`.
- **Structure**: Follow the pattern from existing scripts in `scripts/.local/bin/` — check shebang, `set -e` usage, and argument parsing patterns in the same category.
- **Naming**: Scripts are prefixed with `,` (comma) to distinguish them from system commands.

## Version Control

This repo uses **git** (not jj) for its own version control. The `jj/` directory contains jj config for use in other repos.
