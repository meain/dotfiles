---
name: tmux-run
description: "Run a command in a tmux pane/window/session and monitor its output for errors. Triggers: /tmux-run, 'run in tmux', 'run in the other pane', 'run on the left', 'run in a new window', 'run in a new session', 'monitor tmux', 'watch tmux'"
user_invocable: true
---

# Tmux Run & Monitor

Run a command in a tmux pane and continuously monitor its output, notifying the user via system notification if something fails.

> **Sandbox**: All tmux commands require `dangerouslyDisableSandbox: true` (socket access at `/private/tmp/tmux-*/`).

## Step 1: Determine or create the target pane

The user may specify where to run the command. Handle each case:

### Case A: Existing sibling pane ("left pane", "other pane", "the pane on the right")

NEVER use relative targets like `-t left` or `-t right` — they resolve based on the focused pane, which is not yours. Instead, discover the correct pane ID (`%<number>`):

```bash
tmux list-panes -a -F '#{pane_id} #{pane_current_command} #{pane_current_path} #{window_name} #{pane_active}'
```

1. Find the pane running `claude` in the current working directory — that is **your** pane.
2. Find sibling panes in the **same window** (same `window_name`). The other pane (typically running `zsh`, `bash`, `make`, etc.) is the **target pane**.
3. If there are multiple candidates, prefer the one running a shell (`zsh`/`bash`) in the same or related project directory.
4. Record the pane ID (e.g., `%119`) and use it for ALL subsequent tmux commands.

### Case B: New split pane ("split a pane", "open a new pane")

```bash
# Split horizontally (side by side)
tmux split-window -h -t %<YOUR_PANE_ID> -P -F '#{pane_id}'
# Split vertically (top/bottom)
tmux split-window -v -t %<YOUR_PANE_ID> -P -F '#{pane_id}'
```

Capture the printed pane ID from `-P -F '#{pane_id}'` for monitoring.

### Case C: New window ("new window", "another window")

```bash
tmux new-window -P -F '#{pane_id}'
```

If the command should run in a specific directory:

```bash
tmux new-window -c '/path/to/dir' -P -F '#{pane_id}'
```

### Case D: New session ("new session", "separate session")

```bash
tmux new-session -d -s '<session-name>' -c '/path/to/dir' -P -F '#{pane_id}'
```

### After identifying or creating the target

Tell the user which pane ID you are using and why. Always store the `%<number>` ID — never rely on names or relative positions after this point.

## Step 2: Send the command

```bash
tmux send-keys -t %<PANE_ID> '<command>' Enter
```

- Quote the command properly to avoid shell interpretation issues.
- If the user wants to run the command in a specific directory and the pane is already open, `cd` first:
  ```bash
  tmux send-keys -t %<PANE_ID> 'cd /path/to/dir && <command>' Enter
  ```

## Step 3: Monitor output

Use periodic captures to check the pane output:

```bash
tmux capture-pane -t %<PANE_ID> -p -S -100
```

### Existing output in the pane

The target pane may already contain output from previous commands. On your **first capture** (right after sending the command), note what's already there so you can distinguish old output from new output. Look for your sent command in the pane to find where new output begins. Only evaluate errors/success from output **after** your command was sent.

### Timing

Choose wait times and polling intervals based on the command:
- **Fast commands** (ls, cat, simple scripts): no initial wait, poll every 5-10s
- **Medium commands** (build, test, npm install): 5s initial wait, poll every 15-30s
- **Long commands** (pulumi, terraform, make with infra): 10-15s initial wait, poll every 30-60s

### Completion detection

Look for:
- Shell prompt returning (the user's prompt pattern)
- `make: ***` pattern (Makefile failures)
- `error:`, `Error:`, `ERROR:`, `FAILED`, `fatal` in output
- The pane's current command reverting to a shell (check with `tmux list-panes -t %<PANE_ID> -F '#{pane_id} #{pane_current_command}'`)

### Scroll back

If the visible pane doesn't show enough, increase `-S` value (e.g., `-S -200`, `-S -500`).

### Background monitoring

Use `run_in_background: true` on sleep+capture commands so you can do other work while waiting.

### What to report

- **On success**: Briefly summarize what completed and any notable outputs.
- **On failure**: Send a system notification (see below) AND tell the user in chat with:
  - Which step/command failed
  - The error message
  - Your analysis of the root cause
  - Suggested fix if you can determine one (read relevant source files to diagnose)

## Step 4: System notification on failure

When a failure is detected, send a macOS notification:

```bash
osascript -e 'display notification "SUMMARY_HERE" with title "Command Failed" sound name "Basso"'
```

Keep the summary under 200 characters. Include the failing step name and a brief reason.

## Important notes

- **Only run the specifically requested command in tmux.** Any other commands needed for the task (e.g., looking up information, reading files, searching code) should be run directly in Claude Code using the normal tools. Tmux is only for the command the user explicitly asked to run there.
- The user may ask you to do other work while monitoring. Use `run_in_background: true` for the sleep+capture commands to avoid blocking.
- If the pane's working directory changes or a new command starts, re-evaluate whether you're still monitoring the right thing.
- If the pane is in a different directory than needed, `cd` before running the command.
