---
name: monica
description: >
  Inspect and control other AI coding agents (Claude Code, pi) running in tmux panes via the
  `monica` CLI — list them, read their last message, send them a prompt, wait for them to finish,
  or focus their pane. Triggers: /monica, "what agents are running", "which agent needs me",
  "send this to the other agent", "ask the X agent", "wait for the other claude",
  "focus the oncall agent", "check on the other session".
---

# monica CLI

`monica` (on PATH, `~/.local/bin/monica`) talks to tmux directly; the menu bar app doesn't need to be running. Run `monica help` if unsure.

## Commands

- `monica ls [--json] [--status working|idle|waiting|quiet|stale]` — list agents (pane, state, agent, window, name, age).
- `monica show <target> [--json]` — path, branch, model, last prompt, tool activity and the agent's last message. For a fresh/cleared session it says so and shows the last 10 pane lines.
- `monica tail <target> [-n N]` — last N (default 20) lines on the agent's screen; use it to see what a send did.
- `monica send <target> [text…] [--wait] [--timeout SECS] [--grace SECS] [--force]` — paste text + Enter into the pane (stdin if no text or `-`). `--wait` blocks until the agent finishes its turn, or returns after the grace period (default 5s) if no turn started (slash commands like `/clear`). Refuses your own pane unless `--force`.
- `monica wait <target> [--timeout SECS]` — block until the agent is not working.
- `monica focus <target>` — switch the user's tmux client to the pane and raise the terminal.

`<target>` is a pane id (`%12`), a pid, or a case-insensitive substring of the agent's name/project/tmux session/window. A substring must match exactly one agent; on ambiguity the error lists matches, so retry with the pane id.

Exit codes: 0 ok, 1 error, 2 usage, 3 agent is waiting on the user (permission prompt etc.), 124 timeout.

## How to use it

- Start with `monica ls` to find the right pane; use the `%id` from there for later calls.
- Tmux commands (`monica`, `tmux`) need `dangerouslyDisableSandbox: true` — the sandbox blocks the tmux socket ("Operation not permitted"). Use it from the first call instead of failing once.
- Never target your own pane. `monica ls` marks it `(you)` (`isSelf` in `--json`), based on `$TMUX_PANE`; if that's unset no pane is marked. `send` refuses it unless `--force` — don't use `--force`. Sending to yourself injects text into your own input.
- To delegate and collect a result: `monica send <target> "<prompt>" --wait --timeout 900`, then `monica show <target>` for the reply. Exit 3 means the agent is blocked on a prompt — report that to the user rather than retrying.
- `--wait` also works for slash commands (`/compact`, `/clear`): no turn starts, so it returns after the grace period with a note on stderr. Then `monica tail <target>` to check the result.
- `send` types into a live conversation the user may be using. Confirm with the user before sending to an agent you weren't explicitly told to message, and never send secrets. Only send to agents in `idle` or `quiet` state unless told otherwise (both mean the agent is waiting at its prompt); sending while `working` queues text into the agent's input mid-turn.
- `focus` moves the user's screen — only do it when asked.
- Messages are pasted verbatim with a trailing Enter; multi-line text is fine.
- For machine-readable output use `--json` (fields: pane, pid, agent, state, status, waitingFor, name, project, path, tmuxSession, windowName, sessionId, updatedAt, isSelf; `show` adds gitBranch, model, lastPrompt, toolActivity, lastMessage, screen).
- `state` is the effective state: `waiting` (blocked on user), `working`, `idle` (finished, awaiting user), `quiet` (idle 15m–3h), `stale` (>3h or no status).
- `monica send` records when it typed into a pane and `ls`/`show` fold that into `updated`, so a pane you just sent to no longer reads `quiet`/`stale`. Other tools (the menu bar app) don't see this; and the state itself still only changes when the agent reports a status update, so check `monica tail` rather than the state to see what a send did.
