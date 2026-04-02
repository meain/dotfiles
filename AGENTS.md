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

## Key Config Files

- **Emacs**: `emacs/.config/emacs/init.el` — primary editor config (uses elpaca package manager, evil mode)
- **Zsh**: `zsh/.config/zsh/.zshrc` and `zsh/.config/zsh/.zsh-custom/`
- **Git**: `git/.config/git/config`
- **Jujutsu**: `jj/.config/jj/config.toml`
- **Ghostty**: `ghostty/.config/ghostty/config`
- **Tmux**: `tmux/.config/tmux/tmux.conf`

## Important: File Paths

When config files reference scripts or other dotfiles (e.g., shell aliases calling `,some-script`), those files live in this repo, not at their home directory symlink targets. Always look for and edit files here in the repo — the home directory paths are just symlinks back to this repo.

## Conventions

- Use `,picker` instead of `fzf` directly in scripts — it's a wrapper that supports both terminal and GUI picker backends

## Version Control

This repo uses **git** (not jj) for its own version control. The `jj/` directory contains jj config for use in other repos.
