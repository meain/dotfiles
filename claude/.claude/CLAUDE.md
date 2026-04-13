## User Preferences

- When the user specifically asks to generate a markdown file, open it after writing using `,markdown-to-html <filepath>`. This only works for markdown files. For HTML files, use `open <filepath>`.
- Each project will have `.mdocs` and `.mscripts` folder in the root which will contain docs or scripts related to the project. They might be able to provide good insights into how the user works on it and how you can do certain things. Do not reference this in documentation outside of taskwarriror tasks.
- Most projects are maintained using `jj` and not `git`. Use `jj` commands whenever possible instead of git commands. If using `gh` cli, set `GIT_DIR` to `jj git root 2>/dev/null || echo .git` and call it.
- You might find additional instructions on how to do certain things or my research docs at `/Users/meain/dev/docs`. Refer to these if necessary. Suggest updates here as well if you find any.
- When creating PRs, do not add "Generated with Claude Code" or similar labels/footers to the PR body.
- We use nix in almost all projects. If you find a `flake.nix`, adding new tools/CLIs should be done using that instead of homebrew.
- When creating a new PR, ensure the PR template is used.
- I usually want to set `rs-malik` (Rabee Sohail Malik) and `ryanfkeepers` (Ryan Keepers) as reviewers on PRs from repos in /Users/meain/dev/veeam. Once the PR is created ask me if I want to add them as reviewers. Always add `Copilot` as a reviewer (note: capital C, it's a GitHub user not a bot).
- After you push a PR, ensure the CI is green in the background.
- My personal backlog is in `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`. You can refer to it as necessary. The tasks for today will be towards the top.
- When creating a PR, always respect the PR template and there is nothing include a summary of the changes and why it was changed as two paragraphs. Include examples of code flow if it makes sense.
- When creating html resources like dashboards or slides, prefer light mode unless explicitly stated.
- When creating temporary resources, create it in /tmp and not the repo root
- If the user suggests corrections or you find issues in how you work through issues, suggest adding changes in skills/CLAUDE.md to the user. Do not add it without user confirmation.
- Ask me before you do action on a public forum like Jira or GitHub PRs/issues unless I have already explicitly asked you to do it.
