## File & Output Conventions

- When the user specifically asks to generate a markdown file, open it after writing using `,markdown-to-html <filepath>`. This only works for markdown files. For HTML files, use `open <filepath>`.
- When creating HTML resources like dashboards or slides, prefer light mode unless explicitly stated.
- When creating temporary resources, create it in /tmp and not the repo root.

## Project Structure

- Each project will have `.mdocs` and `.mscripts` folders in the root containing docs or scripts related to the project. These provide insights into how the user works on it and how you can do certain things.
- Do not reference `.mdocs`/`.mscripts` in documentation outside of taskwarrior tasks. Use the `save-research` skill when working with `.mdocs`.
- You might find additional instructions or research docs at `/Users/meain/dev/docs`. Refer to these if necessary. If you find outdated or incorrect information in those docs, suggest updates to them.
- We use nix in almost all projects. If you find a `flake.nix`, adding new tools/CLIs should be done using that instead of homebrew.

## Version Control & PRs

- Most projects are maintained using `jj` and not `git`. Use `jj` commands whenever possible instead of git commands. If using `gh` cli, set `GIT_DIR` to `jj git root 2>/dev/null || echo .git` and call it.
- When creating PRs, do not add "Generated with Claude Code" or similar labels/footers to the PR body.
- When creating a new PR, ensure the PR template is used. If there is none, include a summary of the changes and why they were made as two paragraphs. Include examples of code flow if it makes sense.
- I usually want to set `rs-malik` (Rabee Sohail Malik) and `ryanfkeepers` (Ryan Keepers) as reviewers on PRs from repos in /Users/meain/dev/veeam. Once the PR is created ask me if I want to add them as reviewers. Always add `Copilot` as a reviewer (note: capital C, it's a GitHub user not a bot).
- After you push a PR, ensure the CI is green in the background.

## External Tools

- Use CLI tools to access external services: `confluence` for Confluence pages, `jira` for Jira tickets, and `gh` (with GIT_DIR set) for GitHub. Prefer these over MCP tools, web fetching, or asking the user to look things up.

## Workflow

- Ask me before you do action on a public forum like Jira or GitHub PRs/issues unless I have already explicitly asked you to do it.
- When the user corrects your approach or points out issues in how you work, suggest updating relevant instructions (CLAUDE.md, skills, or both) to prevent recurrence. Always ask for confirmation before making the update.

## Personal Notes

- My personal backlog is in `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`. You can refer to it as necessary. The tasks for today will be towards the top.
- Task notes with detailed research/context live in `/Users/meain/.local/share/sbdb/Tasks/YYYY-MM/<Task Name>.md`. They are referenced from the backlog using `[[Tasks/YYYY-MM/Task Name]]` wiki-links.
- Use the `/vault` skill when working with these notes.
