## File & Output Conventions

- When the user specifically asks to generate a markdown file, open it after writing. Always open markdown files using `,markdown-to-html <filepath>`. For HTML or other file formats, use `open <filepath>` with `dangerouslyDisableSandbox: true` — the sandbox blocks `open` from launching GUI apps.
- When creating HTML resources like dashboards or slides, prefer light mode unless explicitly stated.
- When creating temporary resources, create it in /tmp and not the repo root.

## Project Structure

- When working in any Veeam project (`/Users/meain/dev/veeam/`), refer to `/Users/meain/dev/mdocs/reference/veeam-repo-map.md` for how repos connect, dependency graphs, deployment flows, useful scripts/commands, and which repo to change for a given task. If you find any inaccuracies or missing information in that doc, update it.
- Each project will have `.mdocs` and `.mscripts` folders in the root containing docs or scripts related to the project. These provide insights into how the user works on it and how you can do certain things.
- Do not reference `.mdocs`/`.mscripts` in documentation outside of taskwarrior tasks. Use the `save-research` skill when working with `.mdocs`.
- Cross-project research, guides, runbooks, reference docs, and workflows live in `/Users/meain/dev/mdocs/` (the global mdocs directory). It follows the same category structure as project-level `.mdocs/` folders. Refer to these if necessary. If you find outdated or incorrect information, suggest updates.
- Workflows in `/Users/meain/dev/mdocs/workflows/` define structured processes that combine multiple data sources (Jira, Confluence, vault) to provide intelligent recommendations and automation.
- We use nix in almost all projects. If you find a `flake.nix`, adding new tools/CLIs should be done using that instead of homebrew.

## Version Control & PRs

- Most projects are maintained using `jj` and not `git`. Use `jj` commands whenever possible instead of git commands. If using `gh` cli, set `GIT_DIR=$(jj git root 2>/dev/null || echo .git)` inline when calling it.
- Use the `/create-commit` skill when making jj commits in non-dotfiles repos.
- Do not squash changes or push to upstream without explicit user request.
- Branch names in Veeam repos should start with `meain/` (e.g., `meain/earn-prod-adx-sku-downsize`).
- When creating PRs, do not add "Generated with Claude Code" or similar labels/footers to the PR body.
- When creating a new PR in Veeam repos, look up the associated Jira ticket and link it in the PR description body (not as a comment). Use the `jira` CLI to fetch ticket details.
- When creating a new PR, look up the repo's PR template first (`.github/pull_request_template.md`) and use it as the structure for the PR body. If there is none, include a summary of the changes and why they were made as two paragraphs. Include examples of code flow if it makes sense.
- I usually want to set `rs-malik` (Rabee Sohail Malik), `ryanfkeepers` (Ryan Keepers), and `rami-veeam` as reviewers on PRs from repos in /Users/meain/dev/veeam. Once the PR is created ask me if I want to add them as reviewers.
- After you push a PR, ensure the CI is green in the background. If CI fails, notify the user proactively.

## Networking & TLS

- TLS/connection failures (curl exit 35, HTTP:000) when making HTTPS calls from Bash are caused by the sandbox blocking outbound network. Use `dangerouslyDisableSandbox: true` on the Bash tool call for any command that makes outbound network requests (curl, az CLI, etc.).

## External Tools

- Use the `/jira` and `/confluence` skills when working with Jira tickets and Confluence pages. These skills wrap the CLI tools with best practices and correct flags. Use `gh` (with GIT_DIR set) for GitHub. Prefer these over MCP tools, web fetching, or asking the user to look things up.
- Use the `/web-search` skill for web searches when the built-in WebSearch tool is unavailable.
- To open a URL in the browser, use `open <url>` (no sandbox needed). Reserve the `/firefox` skill for interactions that require UI scripting — tab switching, keyboard shortcuts, getting tab info.
- When debugging Emacs issues, connect to the running instance via `emacsclient --eval '(...)'` to inspect state, evaluate expressions, or test fixes without restarting.

## Workflow

- Ask me before you do action on a public forum like Jira or GitHub PRs/issues unless I have already explicitly asked you to do it.
- When the user corrects your approach or points out issues in how you work, suggest updating relevant instructions (AGENTS.md, skills, or both) to prevent recurrence. Always ask for confirmation before making the update. Note: CLAUDE.md is a symlink to AGENTS.md — always edit `/Users/meain/.agents/AGENTS.md` directly.
- Keep commit message subject lines short and descriptive — avoid listing every change in the summary. One concise phrase is enough.

## Personal Notes

- My personal backlog is in `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`. You can refer to it as necessary. The tasks for today will be towards the top.
- Task notes with detailed research/context live in `/Users/meain/.local/share/sbdb/Tasks/YYYY-MM/<Task Name>.md`. They are referenced from the backlog using `[[Tasks/YYYY-MM/Task Name]]` wiki-links.
- Use the `/vault` skill when working with these notes.
- My dotfiles are managed at `/Users/meain/.dotfiles` and stowed from there to the appropriate locations in my home directory.

## User Context

- **User**: Abin Simon (aka meain)
- **Role**: Principal Engineer (Principal − 1) at Improving (formerly InfraCloud, acquired 2025), primarily writing Go. Currently working on the Veeam Data Cloud (VDC) control plane, owning the `user-management` service in `control-plane-backend` and contributing across other services and the EARN (Events, Aggregations, Reporting, Notifications) telemetry stack.

## Communication Preferences

- Prefer concise, high-signal communication over lengthy explanations
- Use direct, no-nonsense tone
- Strip away unnecessary complexity and focus on what's immediately useful
- Default to brief responses unless specifically asked for detail