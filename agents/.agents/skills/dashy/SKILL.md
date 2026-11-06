---
name: dashy
description: >
  Control the running Dashy app (native macOS app for many Claude Code agents) through `dashy-cli`:
  list agents, read what they're doing, start / message / fork / archive / delete them, approve their
  permissions, wait for them to finish, and set up scheduled agents (cron-style prompts).
  Triggers: /dashy, "start an agent in dashy", "which dashy agents need me", "message the dashy agent",
  "fork that dashy agent", "schedule an agent", "run this every morning in dashy", "wait for the dashy agent",
  "approve the dashy permission", "delete the dashy agent".
---

# dashy-cli

Remote control for the running Dashy app. It talks to the app over a Unix socket, so **Dashy must be running**
(`dashy-cli ping` checks). Run it with `dangerouslyDisableSandbox: true`: the sandbox blocks the socket.

## Binary

Use the first that exists:

- `dashy-cli` on PATH (`~/.local/bin/dashy-cli` links to the release bundle, refreshed by `make app`)
- `/Users/meain/dev/src/dashy/build/Dashy.app/Contents/MacOS/dashy-cli` (release bundle, after `make restart` / `make app`)
- `/Users/meain/dev/src/dashy/.build/debug/dashy-cli` (after `make cli`)

`dashy-cli help` prints the full reference. Source and docs: `/Users/meain/dev/src/dashy` (`docs/agents/remote-control.md`).
Other `dashy-cli` commands (`handshake`, `prompt`, `session`, `store`, `shared`, `rewind`, `control`) are the dev test harness. They drive real agents and cost money, so don't use them for remote control.

## Commands

```sh
dashy-cli agents [--all] [--state needs-you|working|idle|done|error|planned]
dashy-cli show AGENT [--tail 6]            # state, pending requests (with option ids), last messages, changed files
dashy-cli new "prompt" --project DIR [--isolation same|jj|git] [--model haiku|sonnet|opus] [--mode MODE] [--effort E] [--title T] [--plan] [--attach FILE]... [--wait]
dashy-cli send AGENT "text" [--interrupt] [--attach FILE]... [--wait]
dashy-cli wait AGENT [--timeout 600]       # blocks until done / needs you / timeout
dashy-cli approve AGENT [--always] [--request ID] [--option ID]
dashy-cli deny AGENT [--request ID]
dashy-cli stop AGENT                       # cancel the running turn
dashy-cli begin AGENT [--plan]             # start a planned agent (--plan: in plan mode)
dashy-cli fork AGENT ["prompt"] [--title T] [--wait]   # copy the conversation into a new agent
dashy-cli bookmark AGENT [--off]                # keep a chat findable: survives archiving; delete refuses bookmarked
dashy-cli agents --bookmarked --all         # open and archived bookmarks
dashy-cli pin AGENT [--off]                     # Pinned section at the top of Overview (open agents only; archiving unpins); `pinned` shows in show and --json
dashy-cli tag AGENT ["text"]                # label on the card's third line, coloured from a hash of the text; no text removes it
dashy-cli archive AGENT [--remove-workspace]
dashy-cli delete AGENT [--remove-workspace]            # archived agents only (planned ones too)
dashy-cli schedule add NAME --when "weekdays 09:00" --project DIR "prompt" [--isolation ..] [--model ..] [--mode ..] [--catch-up]
dashy-cli schedule edit NAME [--name N] [--prompt P] [--when SPEC] [--project DIR] [--isolation ..] [--model ..] [--mode ..] [--effort ..] [--catch-up|--no-catch-up]
dashy-cli schedule ls | runs NAME | rm NAME | run NAME | pause NAME | resume NAME
dashy-cli agents --schedule NAME          # only that schedule's runs
dashy-cli link new "prompt" [--project DIR] [--isolation same|jj|git] [--mode M] [--model M] [--effort E] [--title T]
dashy-cli link quick "prompt"                              # opens the ⇧⌘N quick-agent panel with the text filled in; the helper picks the repo from the text (a PR URL works) once the user presses ↩
dashy-cli link send AGENT "text"
dashy-cli link open AGENT [--tab thread|widgets|files]    # print a dashy:// URL; needs no running app, changes nothing
```

Add `--json` for machine-readable output. A `-` as the prompt or text reads stdin (use it for long or multi-line text).

- **AGENT** is an id, id prefix (4+ chars), exact title or part of the title. Keep the id printed by `new`: the adapter can retitle an agent after its first turn.
- **States** (for `--state`): needs-you, error, working, starting, done, idle, planned, archived.
- **Schedule refs:** every `schedule` subcommand takes a name or an id prefix (4+ chars); `schedule ls` shows ids. `schedule edit` changes only the flags you pass, and `default` clears model, mode or effort.
- **Exit codes** of `wait`, `new --wait`, `send --wait`: 0 finished, 1 error, 3 needs you (permission or question pending), 4 timed out.
- **`--when`:** `every 30m`, `hourly`, `daily 09:00`, `weekdays 09:00`, or a cron expression (`0 9 * * 1-5`). Local time. Cron also takes `L` for the last day of the month (`0 18 L * *`), `5L` for the last Friday and `5#1` for the first Friday; don't fake month-end with `28-31`. A plain day plus weekday (`1-7 * 5`) means either, so use `#` for "first Friday". Older Dashy builds reject `L`.

## Patterns

- **Delegate and collect:** `dashy-cli new "…" --project DIR --title "x" --wait` runs the agent and prints its final reply. For several in parallel, start each without `--wait` (save the ids), then `wait` on each.
- **Handle blocked agents:** `wait` exits 3 when an agent needs input. `show AGENT` lists the pending request. For a permission, `approve` (or `deny`). For a question, answer in words with `send AGENT "…"`, which declines the form and sends your text. Only approve what the user would approve: show them the command first unless they've said to go ahead.
- **Triage:** `dashy-cli agents --state needs-you` to see who's blocked.
- **Try an alternative:** `fork AGENT "try approach B instead"` leaves the original untouched. The fork shares the original's folder, so for risky edits start the original with `--isolation jj|git`.
- **Isolated work:** `--isolation jj` or `git` gives the agent its own workspace/worktree. `archive --remove-workspace` cleans it up.
- **Scheduled agents** run inside the app: each run is a new agent titled `NAME · date`. Runs are ordinary agents (clock icon in the app); the Schedules pill lists the schedules and the selected one's runs. A run is skipped while the previous one is still going, and missed runs (app closed) are skipped unless `--catch-up`. Use `schedule run NAME` to test one now.
- **Schedule default mode:** always pass `--mode auto` on `schedule add` (and when editing a schedule that has no mode) unless the user asks for a different mode. Don't use `acceptEdits`.
- **Schedule caveats:** runs only happen while Dashy is open and the Mac is awake (Dashy can hold off idle sleep while agents work, but not lid-close sleep, and it can't wake the Mac for a schedule). A run uses the schedule's mode, so one that hits a permission shows up as needs-you; prefer read-only prompts. Each schedule keeps its last 60 events (started, skipped, failed); `schedule runs NAME` shows them.
- **Plan first:** `new --plan` creates an agent without starting it; review it, then `begin AGENT` (or `begin AGENT --plan`).
- **Links (`dashy://`) for bookmarks, HTML reports and notes:** always build them with `dashy-cli link`, never by hand-encoding (it percent-encodes and refuses values the app would reject). `link new` makes a link that opens the New Agent sheet pre-filled; `link send AGENT "text"` one that puts text in an agent's message box (get your own id from the `whoami` MCP tool); `link quick "prompt"` one that opens the quick-agent panel filled in, where the repo is picked for you after the user presses ↩ (use it when the repo depends on the value, such as a PR URL); `link open` selects an agent. The user always presses the key themselves; a link never starts or sends anything alone. For a bookmark that takes a value, write `%s` in the prompt/title, then `sed 's/%25s/%s/g'` the output so the launcher can substitute it. Prompt and text cap at 8000 characters, so long ones belong in a file the prompt points to. Under Dashy Debug (`DASHY_DEBUG=1`) it prints `dashy-debug://` links. Builds older than the link feature have no `link` command and don't handle the URLs.
- **Socket:** `~/Library/Application Support/dashy/control.sock`; `DASHY_CONTROL_SOCKET` overrides it. The newest app instance owns it.

## Rules

- `new`, `send`, `fork`, `delete` and `schedule add` change state in the user's running app. Confirm first when the user didn't clearly ask for that exact action. `delete` is permanent (it also drops the agent's review comments).
- Don't pass `--model opus` or long-running prompts for tests; use `haiku` and tiny prompts.
- A remote start doesn't move the highlight in the app window, so tell the user the title and id of what you started.
- If `ping` says Dashy isn't running, tell the user. Don't launch the app yourself: quitting or relaunching it kills their running agents.
