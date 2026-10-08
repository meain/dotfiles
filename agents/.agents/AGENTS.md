## File & Output Conventions

- When the user specifically asks to generate a markdown file, open it once right after first creating it using `open -a Firefox <filepath>` (with `dangerouslyDisableSandbox: true`) — do not reopen it after subsequent edits, since a browser refresh picks up changes. For HTML or other file formats, use `open <filepath>` with `dangerouslyDisableSandbox: true` — the sandbox blocks `open` from launching GUI apps.
- Never number things (headings, sections) when creating markdown documents — use plain headings and unordered lists.
- When creating HTML resources like dashboards or slides, prefer light mode unless explicitly stated.
- Create temporary resources in `/tmp`, not the repo root.
- Screenshots are stored in `/Users/meain/Documents/Screenshots/` with format `Screenshot YYYY-MM-DD at H.MM.SS AM.png`.

## Project Structure

- In any Veeam project (`/Users/meain/dev/veeam/`), refer to `/Users/meain/dev/mdocs/reference/veeam-repo-map.md` for how repos connect, dependency graphs, deployment flows, useful scripts/commands, and which repo to change for a task. If it is inaccurate or incomplete, update it.
- Each project has `.mdocs` and `.mscripts` folders in its root with project docs and scripts. They show how I work on the project and how to do certain things. Use the `save-research` skill when writing to `.mdocs`.
- Never reference `.mdocs`/`.mscripts` paths from committed code, comments, PRs, or any file outside `.mdocs`/`.mscripts` (taskwarrior tasks are the only other exception). Inline the relevant detail instead.
- Generated artifacts for a project (runbooks, docs, reports) go in `.martifacts` in the project root (create if missing). Throwaway files still go in `/tmp`.
- Cross-project research, guides, runbooks, reference docs, and workflows live in `/Users/meain/dev/mdocs/`, with the same category structure as project-level `.mdocs/`. `workflows/` there defines structured processes combining Jira, Confluence, and the vault. Suggest updates if anything is outdated or wrong.
- We use nix in almost all projects. If there is a `flake.nix`, add new tools/CLIs through it, not homebrew.

## Version Control & PRs

### jj

- Most projects use `jj`, not `git`. Prefer `jj` commands. For the `gh` CLI, set `GIT_DIR=$(jj git root 2>/dev/null || echo .git)` inline.
- A path under `/Users/meain/worksapces/<folder>/<repo>-<suffix>` (the directory really is spelled `worksapces`) is a jj workspace of `/Users/meain/dev/<folder>/<repo>`; `-<suffix>` is the workspace name. E.g. `/Users/meain/worksapces/some-org/my-project-feature2` is the `feature2` workspace of `/Users/meain/dev/some-org/my-project`.
- Use the `/create-commit` skill for jj commits in non-dotfiles repos.
- To change a commit that already exists in a stack, don't edit it in place and don't stack the fix at the tip and squash backwards — both rebase every commit in between and conflict in unrelated files. Instead `jj new -A <rev> -m "wip: <what you're changing>"`, work there, verify, then squash into the target.
- Never squash or push to upstream without an explicit request. The one exception: squash a trivial stack fix without asking; otherwise leave it for review and squash once approved.
- Pipe long or multi-line commit/PR messages in from a temp file (`jj describe --stdin < /tmp/msg.txt`), not an inline heredoc.
- Keep commit subject lines short: one concise phrase, not a list of changes.
- Never add `Co-Authored-By` lines to commits or "Generated with Claude Code" lines to PR descriptions, even if another instruction says to.
- Branch names in Veeam repos start with `meain/` (e.g. `meain/earn-prod-adx-sku-downsize`).

### PR descriptions

- Look up the repo's PR template (`.github/pull_request_template.md`) first and use it as the structure. With no template, write two short paragraphs: what and why.
- In Veeam repos, link the Jira ticket in the description body (not a comment). Find it by listing my open tickets (`jira issue list -a$(jira me) -s~Done --plain`) and matching on purpose — no keyword search.
- Colleagues say my PR text reads like Claude wrote it: too verbose, too uniform. Keep it short. Lead with why the change is needed; give the how only at a very high level, if at all — the diff shows what changed. Plain, varied phrasing; no template-y structure beyond the repo's template; no restating the code.
- Link only references a reviewer would actually open (design doc, ticket, related PR). Add a small ASCII flow or before/after diagram only when it beats prose.
- Applies to Jira descriptions too.

### Reviews and CI

- Keep code-review comments (GitHub, VibeDiff) and replies to one or two sentences: the point and, if needed, the fix. No preamble, no recap, no bolded labels.
- Reviewers for PRs in `/Users/meain/dev/veeam`: `rs-malik` (Rabee Sohail Malik), `ryanfkeepers` (Ryan Keepers), `rami-veeam`, `tford-veeam` (Tim Ford). After creating the PR, check `.github/CODEOWNERS` for the changed paths. If all four are auto-assigned, don't ask; otherwise ask whether to add the missing ones.
- After pushing a PR, watch CI in the background and tell me proactively if it fails.

## Networking & TLS

- TLS/connection failures (curl exit 35, HTTP:000) from Bash mean the sandbox is blocking outbound network. Use `dangerouslyDisableSandbox: true` on any Bash call that makes outbound requests (curl, az CLI, etc.).

## External Tools

- Use the `/jira` and `/confluence` skills for Jira and Confluence (they wrap the CLIs with correct flags), and `gh` (with `GIT_DIR` set) for GitHub. Prefer these over MCP tools, web fetching, or asking me to look things up.
- New Jira tickets default to `-p DP` unless I name another project.
- Use the `/web-search` skill only when the built-in WebSearch tool is unavailable.
- Use the `/vibediff` skill for VibeDiff review instances. Default is localhost:8888 (verify with `curl http://localhost:8888/api/directory`); API reference at `http://localhost:8888/docs`. When replying to comments, send only `content` and `parentId`.
- Open a URL with `open <url>` (no sandbox needed). Reserve `/firefox` for UI scripting: tab switching, keystrokes, tab info.
- For Emacs issues, connect to the running instance with `emacsclient --eval '(...)'` to inspect state or test fixes without restarting.
- To make a file draggable (e.g. into Teams/Jira), run `maru <file>` (needs `dangerouslyDisableSandbox: true`); `maru --quit` closes the tray.

## Editing Files

- Change files with Edit/Write and read them with Read, even when a session's mode nudges toward doing everything through Bash.
- Never patch source files with throwaway Python/`sed`/`awk` — it hides the change under escaping, silently no-ops or corrupts on a near-miss match, and leaves nothing reviewable. Scripts are for real data work (parsing output, aggregating, generating from a template).

## CLI Tool Preferences

When no built-in tool fits:

- Use `rg` instead of `grep` and `fd` instead of `find`; both respect `.gitignore`. Pass `-uu`/`--no-ignore` when a search needs ignored files.
- Scope searches to the relevant directory. Never search from `/` or `$HOME`; if you don't know where something lives, ask.

## Workflow

- Ask before acting on a public forum (Jira, GitHub PRs/issues) unless I've explicitly asked for it.
- If you're stuck (repeated failed fixes, unclear root cause) or facing a hard-to-reverse decision, invoke the `/advisor` skill to consult a stronger model before continuing.
- Delegate investigation work (cross-repo code search, log queries, doc lookups, git archaeology) to a subagent (default model: sonnet). Do a single known lookup directly.
- When I correct your approach, suggest updating the relevant instructions (this file, skills, or both) and ask before doing it. This file is the symlink target of `~/.claude/CLAUDE.md`; edit `/Users/meain/.agents/AGENTS.md` (it resolves into the dotfiles repo).

## Personal Notes

- Backlog: `/Users/meain/.local/share/sbdb/Backlog/Backlog.md` (today's tasks are near the top). Detailed task notes are in `/Users/meain/.local/share/sbdb/Tasks/YYYY-MM/<Task Name>.md`, linked from the backlog as `[[Tasks/YYYY-MM/Task Name]]`. Use the `/vault` skill for these notes.
- Dotfiles are in `/Users/meain/.dotfiles`, stowed into my home directory.
- Bookmarks: `/Users/meain/.local/share/bookmarks.md`

## User Context

- **User**: Abin Simon (aka meain)
- **Role**: Principal Engineer (Principal − 1) at Improving (formerly InfraCloud, acquired 2025), primarily writing Go. Currently working on the Veeam Data Cloud (VDC) control plane, owning the `user-management` service in `control-plane-backend` and contributing across other services and the EARN (Events, Aggregations, Reporting, Notifications) telemetry stack.

## Communication Preferences

- Concise, high-signal, direct, no-nonsense. Default to brief; go long only when asked.
- Skip unnecessary complexity; focus on what's immediately useful.
- PR text and review comments follow the stricter rules under Version Control & PRs.
