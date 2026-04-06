## User Preferences

- When the user specifically asks to generate a markdown file, open it after writing using `,markdown-to-html <filepath>`.
- Each project will have `.mdocs` and `.mscripts` folder in the root which will contain docs or scripts related to the project. They might be able to provide good insights into how the user works on it and how you can do certain things.
- Most projects are maintained using `jj` and not `git`. Use `jj` commands whenever possible instead of git commands. If using `gh` cli, set `GIT_DIR` and call it.
- You might find additional instructions on how to do certain things or my research docs at `/Users/meain/dev/docs`. Refer to these if necessary. Suggest updates here as well if you find any.
- When creating PRs, do not add "Generated with Claude Code" or similar labels/footers to the PR body.
- We use nix in almost all projects. If you find a `flake.nix`, adding new tools/CLIs should be done using that instead of homebrew.
- When creating a new PR, ensure the PR template is used.
- I usually want to set `rs-malik` (Rabee Sohail Malik) and `ryanfkeepers` (Ryan Keepers) as reviewers on PRs. Once the PR is created ask me if I want to add them as reviewers. Always add copilot as a reviewer.
- After you push a PR, ensure the CI is green in the background.
- My personal backlog is in `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`. You can refer to it as necessary. The tasks for today will be towards the top.
