## File & Output Conventions

- When the user specifically asks to generate a markdown file, open it once right after first creating it using `open -a Firefox <filepath>` (with `dangerouslyDisableSandbox: true`) — do not reopen it after subsequent edits, since a browser refresh picks up changes. For HTML or other file formats, use `open <filepath>` with `dangerouslyDisableSandbox: true` — the sandbox blocks `open` from launching GUI apps.
- Never number things (headings, sections) when creating markdown documents — use plain headings and unordered lists.
- When creating HTML resources like dashboards or slides, prefer light mode unless explicitly stated.
- When creating temporary resources, create it in /tmp and not the repo root.
- Screenshots are stored in `/Users/meain/Documents/Screenshots/` with format `Screenshot YYYY-MM-DD at H.MM.SS AM.png`. Latest screenshot however can be found at `/Users/meain/Documents/latest_screenshot.png`.

## Project Structure

- When working in any Veeam project (`/Users/meain/dev/veeam/`), refer to `/Users/meain/dev/mdocs/reference/veeam-repo-map.md` for how repos connect, dependency graphs, deployment flows, useful scripts/commands, and which repo to change for a given task. If you find any inaccuracies or missing information in that doc, update it.
- Each project will have `.mdocs` and `.mscripts` folders in the root containing docs or scripts related to the project. These provide insights into how the user works on it and how you can do certain things.
- Never reference `.mdocs`/`.mscripts` (or any file under them) from committed code, code comments, or any other file. The only things that may reference an `.mdocs`/`.mscripts` path are other `.mdocs`/`.mscripts` files (and taskwarrior tasks). If you need context that lives in `.mdocs`, inline the relevant detail rather than pointing at the file. Use the `save-research` skill when working with `.mdocs`.
- Cross-project research, guides, runbooks, reference docs, and workflows live in `/Users/meain/dev/mdocs/` (the global mdocs directory). It follows the same category structure as project-level `.mdocs/` folders. Refer to these if necessary. If you find outdated or incorrect information, suggest updates.
- Workflows in `/Users/meain/dev/mdocs/workflows/` define structured processes that combine multiple data sources (Jira, Confluence, vault) to provide intelligent recommendations and automation.
- We use nix in almost all projects. If you find a `flake.nix`, adding new tools/CLIs should be done using that instead of homebrew.

## Version Control & PRs

- Most projects are maintained using `jj` and not `git`. Use `jj` commands whenever possible instead of git commands. If using `gh` cli, set `GIT_DIR=$(jj git root 2>/dev/null || echo .git)` inline when calling it.
- A path under `/Users/meain/worksapces/<folder>/<repo>-<suffix>` is a jj workspace of `/Users/meain/dev/<folder>/<repo>`; the trailing `-<suffix>` is the workspace name. For example, `/Users/meain/worksapces/some-org/my-project-feature2` is the `feature2` workspace of `/Users/meain/dev/some-org/my-project`.
- Use the `/create-commit` skill when making jj commits in non-dotfiles repos.
- Do not squash changes or push to upstream without explicit user request.
- For long or multi-line commit/PR messages, pipe the content in via stdin from a temp file (e.g. `jj describe --stdin < /tmp/msg.txt`) rather than inlining a heredoc in the command.
- Branch names in Veeam repos should start with `meain/` (e.g., `meain/earn-prod-adx-sku-downsize`).
- When creating PRs, do not add "Generated with Claude Code" or similar labels/footers to the PR body.
- When creating a new PR in Veeam repos, look up the associated Jira ticket and link it in the PR description body (not as a comment). Use the `jira` CLI to fetch ticket details. To find the ticket, fetch all open tickets assigned to me (`jira issue list --assignee $(jira me) --status-not Done`) and look for one that matches the PR's purpose — do not do a keyword search.
- When creating a new PR, look up the repo's PR template first (`.github/pull_request_template.md`) and use it as the structure for the PR body. If there is none, include a summary of the changes and why they were made as two paragraphs. Include examples of code flow if it makes sense.
- I usually want to set `rs-malik` (Rabee Sohail Malik), `ryanfkeepers` (Ryan Keepers), `rami-veeam`, and `tford-veeam` (Tim Ford) as reviewers on PRs from repos in /Users/meain/dev/veeam. After creating the PR, check `.github/CODEOWNERS` for the changed paths — if all four are already auto-assigned via CODEOWNERS, skip the reviewer ask; otherwise ask if I want to add the missing ones.
- After you push a PR, ensure the CI is green in the background. If CI fails, notify the user proactively.

## Networking & TLS

- TLS/connection failures (curl exit 35, HTTP:000) when making HTTPS calls from Bash are caused by the sandbox blocking outbound network. Use `dangerouslyDisableSandbox: true` on the Bash tool call for any command that makes outbound network requests (curl, az CLI, etc.).

## External Tools

- Use the `/jira` and `/confluence` skills when working with Jira tickets and Confluence pages. These skills wrap the CLI tools with best practices and correct flags. Use `gh` (with GIT_DIR set) for GitHub. Prefer these over MCP tools, web fetching, or asking the user to look things up.
- When creating Jira tickets, always use `-p DP` as the default project unless the user explicitly specifies a different one.
- Use the `/web-search` skill for web searches when the built-in WebSearch tool is unavailable.
- Use the `/vibediff` skill when working with VibeDiff code review instances. VibeDiff defaults to localhost:8888 (verify with `curl http://localhost:8888/api/directory`) and provides REST APIs for comments, diffs, and revisions. When replying to comments, only provide `content` and `parentId` — other fields are inherited from the parent. If behavior seems wrong, check `http://localhost:8888/docs` for the instance-specific API reference.
- To open a URL in the browser, use `open <url>` (no sandbox needed). Reserve the `/firefox` skill for interactions that require UI scripting — tab switching, keyboard shortcuts, getting tab info.
- When debugging Emacs issues, connect to the running instance via `emacsclient --eval '(...)'` to inspect state, evaluate expressions, or test fixes without restarting.

## CLI Tool Preferences

- Use `rg` (ripgrep) instead of `grep` for searching file contents — it's faster and respects `.gitignore` by default, avoiding huge vendored/cache directories (e.g. `node_modules`, `elpaca`, `.git`).
- Use `fd` instead of `find` for locating files — simpler syntax, faster, and also respects `.gitignore` by default.
- If a search needs to include ignored files (e.g. inside a gitignored cache dir), pass `-uu`/`--no-ignore` to `rg`/`fd` explicitly rather than reaching for `grep`/`find`.
- Scope searches to the relevant directory rather than the whole repo root when possible, even with `rg`/`fd`.

## Workflow

- Ask me before you do action on a public forum like Jira or GitHub PRs/issues unless I have already explicitly asked you to do it.
- When the user corrects your approach or points out issues in how you work, suggest updating relevant instructions (AGENTS.md, skills, or both) to prevent recurrence. Always ask for confirmation before making the update. Note: CLAUDE.md is a symlink to AGENTS.md — always edit `/Users/meain/.agents/AGENTS.md` directly.
- Keep commit message subject lines short and descriptive — avoid listing every change in the summary. One concise phrase is enough.
- When committing in this dotfiles repo, git does not handle parallel commits well due to index locking. Issue `git add && git commit` calls sequentially, not in parallel.
- When committing nix flake.lock updates in this dotfiles repo, commit `home-manager/.config/home-manager/flake.lock` and `nix-darwin/.config/nix-darwin/flake.lock` separately with component-specific prefixes (`[home-manager]` and `[nix-darwin]`), not together as `[nix]`.
- For dotfiles commits that don't belong to a specific stow package subfolder (e.g., root-level meta files, shared configs), use `[]` with no component name.
- If in doubt about commit conventions, component names, or how to structure commits in this dotfiles repo, check `git log` for similar past changes to see how they were committed.

## Personal Notes

- My personal backlog is in `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`. You can refer to it as necessary. The tasks for today will be towards the top.
- Task notes with detailed research/context live in `/Users/meain/.local/share/sbdb/Tasks/YYYY-MM/<Task Name>.md`. They are referenced from the backlog using `[[Tasks/YYYY-MM/Task Name]]` wiki-links.
- Use the `/vault` skill when working with these notes.
- My dotfiles are managed at `/Users/meain/.dotfiles` and stowed from there to the appropriate locations in my home directory.
- Personal bookmarks are available at `/Users/meain/.local/share/bookmarks.md`

## User Context

- **User**: Abin Simon (aka meain)
- **Role**: Principal Engineer (Principal − 1) at Improving (formerly InfraCloud, acquired 2025), primarily writing Go. Currently working on the Veeam Data Cloud (VDC) control plane, owning the `user-management` service in `control-plane-backend` and contributing across other services and the EARN (Events, Aggregations, Reporting, Notifications) telemetry stack.

## Communication Preferences

- Prefer concise, high-signal communication over lengthy explanations
- Use direct, no-nonsense tone
- Strip away unnecessary complexity and focus on what's immediately useful
- Default to brief responses unless specifically asked for detail

## Additional tools

Use the `/codebase-query` skill (aliases: `/cbm`, `/code-graph`) to analyze codebases semantically. It uses `codebase-memory-mcp` to query code structure, complexity, dependencies, and architecture via a graph database. Prefer this over reading entire files or grep for understanding code relationships.

Examples:
- "show me high complexity functions in earn"
- "what calls GetOrgRegion?"
- "find implementations of EventRouter"
- "map service entry points"

The skill handles project detection, query construction, and result formatting. For direct CLI usage (when not in conversation), see `references/cookbook.md` inside the skill directory.

Common direct commands:
```
codebase-memory-mcp cli search_graph '{"project": "...", "name_pattern": ".*Handler.*", "label": "Function"}'
codebase-memory-mcp cli trace_path '{"project": "...", "function_name": "Search", "direction": "both"}'
codebase-memory-mcp cli query_graph '{"project": "...", "query": "MATCH (f:Function) WHERE f.complexity > 15 RETURN f.name, f.file_path LIMIT 10"}'
```

Note: CLI commands require explicit `"project"` parameter (e.g., `"Users-meain-dev-veeam-control-plane-backend"`). Use `list_projects` to find indexed project names.