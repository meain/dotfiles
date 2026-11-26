---
name: dashy-debug-test
description: >
  Hand the user a test bed for a new Dashy feature: start a test agent in the Dashy Debug instance
  (`DASHY_DEBUG=1 dashy-cli`) that sets up the data or state needed to try the feature (widgets, messages,
  files), so the user can poke at it without touching their primary Dashy.
  Triggers: /dashy-debug-test, "ask an agent in the debug instance to create ... so I can test", "set up test
  widgets", "make a test agent in debug dashy", "give me something to test this with".
---

# Test a Dashy feature through the debug instance

Use after building a feature, when the user wants to try it by hand. The debug instance is a second app
("Dashy Debug", own data folder and socket) that never touches the primary's agents.

## Steps

- Always assume the debug instance is running with the latest build: the user keeps `make dev` going in the
  background. Don't check for it, don't start, quit or relaunch it, and don't ask them to run `make debug-open`.
  If a `DASHY_DEBUG=1 dashy-cli` call fails to connect, report that one error and stop.
- Work out what the feature needs as input: which MCP tool call, file or message produces the state to look at.
  Cover the happy path, the edge cases (bad input that must not render, empty, already-done) and one case that
  exercises the interaction (for example a block with `notify`, so a click sends a message back).
- Start the test agent from `/tmp` so it cannot touch the repo:

  ```sh
  cd /tmp && DASHY_DEBUG=1 dashy-cli new '<prompt>' --project /tmp --title "<feature> test"
  ```

  Needs `dangerouslyDisableSandbox: true` (control socket). Plain `dashy-cli` targets the primary; never use it here.
- Write the prompt so the agent does it exactly, with no guessing:
  - name the tool (`widget_set`, `board_post`, ...) and give ids, titles and the literal payloads;
  - say "then stop" so it doesn't wander;
  - ask it to call the read-back tool (for example `widget_list`) and report the output, and to repeat that
    read-back when told, so the user can check persistence of what they ticked or clicked.
- The user does the visual inspection in the debug instance; your job is to create the agent. Don't `--wait`
  for it unless you want to inspect something yourself.
- Inspect on your own when you can, and say what you covered: `DASHY_DEBUG=1 dashy-cli show <id>` to confirm
  the tool calls succeeded and read the read-back output, and `make snapshot SCENE=<scene>` (open the PNG with
  Read) for layout, if a scene shows the feature. You can't screenshot the live app, so anything only visible
  there (clicks, window chrome) stays with the user.

## Focusing the debug instance and its agent

When the user asks to focus the debug instance, or right after starting the test agent so they land on it:

- Bring the app forward by pid (a name match fails, since the process is called `dashy`):
  `osascript -e 'tell application "System Events" to set frontmost of (first process whose unix id is <pid>) to true'`,
  with the pid from `pgrep -fl "Dashy Debug.app/Contents/MacOS/dashy$"` (the line without `dashy-iroh`).
- Select the test agent: `open "$(DASHY_DEBUG=1 dashy-cli link open <agent-id>)"` (a `dashy-debug://open` link;
  it selects the agent and sends nothing).
- Both need `dangerouslyDisableSandbox: true`. You can't see the result, so say you ran them, not that the window is in front.

## Report

Reply short: the agent id and title, what each test widget or item is meant to show, and the two or three
things to try (what should happen on click, what should not render). State plainly what you did not verify
visually. Tell the user to quit the debug instance when done; don't quit it yourself.
