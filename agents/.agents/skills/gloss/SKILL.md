---
name: gloss
description: >
  What gloss is (the user's native macOS design-doc editor where a Claude agent co-writes
  MDX docs) and how to write a prompt for it. Use when the user asks for a prompt to give to
  gloss, wants to know what to ask gloss, or mentions gloss's chat, threads, decisions or
  slash commands. By default the output is a ready-to-paste prompt, not an edit to a doc. Also
  covers `gloss-cli`, the command line for the running app, for when you're asked to act on a
  doc yourself (read it, edit it, message its agent, answer its cards).
  Triggers: /gloss, "prompt for gloss", "give me a prompt for gloss", "what should I ask gloss",
  "write this up for gloss", "gloss prompt", "ask gloss to", "gloss-cli", "read the gloss doc",
  "send this to gloss", "answer the gloss decision", "update the gloss doc"
user_invocable: true
---

# gloss

gloss is the user's own macOS app for writing design docs together with a Claude agent (source: `/Users/meain/dev/src/gloss`, run with `open /Users/meain/dev/src/gloss/build/Gloss.app`; `docs/plan.md` there is the contract, `AGENTS.md` the dev guide). The user mostly uses this skill to have *you* write a prompt they paste into gloss. Unless they ask you to act on a doc, don't edit docs or drive gloss yourself; when they do ask, use `gloss-cli` (last section).

## What gloss is

- A doc is a folder: `~/Documents/Gloss/<Name>/index.mdx` plus `assets/` (images linked `./assets/<file>`). MDX, rendered as a WYSIWYG editor.
- Beside the doc: a **chat pane** (one ongoing Claude session per doc), **margin threads** (select text, ⌘J: a threaded chat about that passage, its own session), and inline **cards** the agent creates:
  - **Decision**: a question with 2–4 options, trade-offs and the agent's pick; the user answers on the card.
  - **Callout**: risk, question, note or blocker.
  - **Task**: a tracked task, only when asked.
- Diagrams are ```mermaid or ```svg code blocks; images go in `assets/`.
- The agent can read the doc and, if the user linked a repo (the repo chip in the top bar), read and edit that repo. It runs on the user's own `claude` login, with subagents for research.
- Edits the agent makes land directly in the doc and show as markers; the user can undo an agent turn, see History, and export Markdown or PDF.

## How gloss's agent behaves (shape prompts around this)

- **Edits only when explicitly asked.** Questions, reviews and research are answered in chat (or in the thread reply) with no edits. To change the doc, say so: "add", "rewrite", "put it in the Risks section".
- **Research answers stay in chat.** It will not write findings, tasks or callouts into the doc, and won't touch open Decision cards. If the user wants the result in the doc, the prompt must say that: "research X, then add the conclusion under Design".
- **Open choices become Decision cards**, not chat questions. Ask for "a decision on X" when you want a card; it frames options and recommends one but never answers it.
- **It does not know this conversation.** It only has the doc, its own chat history and (if linked) the repo. Put every needed fact, constraint, name and link in the prompt.
- It uses subagents for independent research questions when told to research; tell it the questions are independent and it'll parallelise.
- Chat replies are kept short. Threads answer briefly and only edit the doc when told to "apply".

## Slash commands in the chat composer

| command | does |
|---|---|
| `/research <question>` | researches with parallel subagents, answers in chat, no doc edits |
| `/draft <section>` | writes the section into the doc, turning guesses into Decision cards and risks into callouts |
| `/decide <topic>` | frames a Decision card with options and a pick |
| `/diagram <what>` | adds a mermaid diagram to the right section |
| `/review <focus>` | margin comments and callouts, no rewrite, five-bullet summary in chat |

## Writing the prompt

1. Decide where it goes and say so: **doc chat** (default), **a margin thread** (select the passage, ⌘J; use when the prompt is about one passage), or a **slash command** (when one fits, give it as `/draft …` so the template does the work).
2. State the intent first: *change the doc* or *just answer*. Use the explicit verb.
3. Name the target: the heading or section, or "the whole doc". Use the doc's own heading text.
4. Give the context the agent can't see: decisions already made, constraints, repo paths (absolute), ticket or doc links, names, numbers, what to leave alone.
5. Say what a good result looks like: format (bullets, a table, a mermaid flowchart), length, what to flag as a Decision or a risk.
6. Keep it short and concrete. No role-play preamble, no restating how gloss works, no tool names. One ask per prompt; split big jobs into numbered steps only if the order matters.
7. Don't ask it to "tell me" things it should put in the doc, or the reverse.

## Output format

Reply with:

- the prompt in **one fenced code block**, ready to paste (nothing inside it addressed to the user);
- one short line after it: where to paste it (doc chat, thread on <passage>, or the slash command) and anything the user should do first (link a repo, select a passage).

If the request is ambiguous about intent (edit vs answer) or target section, pick the likelier reading, say which in the line after the block, and don't ask unless the two readings would produce very different docs.

## Examples

Draft a section:

```
/draft Rollout: phased plan for the offline-sync launch. Context: the /sync endpoint ships behind the `offline_sync` flag; mobile 4.2 is the first client; ops wants a 5% → 25% → 100% ramp with a rollback owner named per stage. Turn anything you'd otherwise guess (ramp length, rollback trigger) into Decision cards. Add a risk callout for the notes backfill.
```

Research without touching the doc:

```
Research how Linear and Figma handle offline conflicts, and whether Automerge or Yjs has an official Swift package. These are independent. Reply here with a short sourced comparison and which you'd pick for our notes app; don't change the doc.
```

Research, then record it:

```
Check what the repo at /Users/meain/dev/src/acme/mobile does when a note is saved offline (start with src/data/notes.ts). Then add a short "Current state" section under Context describing it, and a risk callout if writes bypass the op-log.
```

Thread on one passage (select the sentence, ⌘J):

```
Is 2 seconds to sync after reconnect realistic on flaky 3G? Answer here; apply nothing to the doc yet.
```

## Prompts about gloss itself

If the user wants a prompt for an agent to *work on* gloss (not to use it), point that agent at `/Users/meain/dev/src/gloss`: read `AGENTS.md`, then `docs/plan.md` and `docs/status.md`; build with `swift build --build-system native`, test with `./scripts/test.sh`; never quit or touch the user's running instance or `~/Documents/Gloss`.

## Driving gloss with gloss-cli

Use it only when the user asks you to act on a gloss doc (read it, change it, message its agent, answer a card). It talks to the **running** gloss over a Unix socket (`gloss-cli ping` checks), so run it with `dangerouslyDisableSandbox: true`: the sandbox blocks the socket. It never launches or quits gloss. Never quit, kill or touch the user's running gloss.

Binary, first that exists: `gloss-cli` on PATH (`make link` in `/Users/meain/dev/src/gloss` links it into `~/.local/bin`), else `/Users/meain/dev/src/gloss/build/Gloss.app/Contents/MacOS/gloss-cli`. `gloss-cli help` has the full reference; `docs/plan.md` ("Control CLI") the contract. An older gloss build without the control socket answers "gloss isn't running": the user needs `make app` and a relaunch.

`<doc>` is an id, an id prefix (4+ characters), a title, part of a title, a path, or `current` (the doc on screen). `-` reads text from stdin; `--json` prints the raw result. Exit codes: 0 done, 1 failed, 2 usage, 3 the agent waits on a card, 4 timed out.

```sh
gloss-cli docs [--archived]                 # id, title, path; [on screen] marks the open one
gloss-cli new "Title" [--in DIR] [--body -] [--open]
gloss-cli read <doc> [--section "Heading"]  # MDX; outline <doc> lists the headings
gloss-cli edit-section <doc> "Heading" - <<'EOF'   # replace the body under a heading (stdin)
gloss-cli insert <doc> - --after-heading "H" | --after-text "exact phrase"
gloss-cli callout <doc> risk|question|note|blocker "text" [--after-heading H]
gloss-cli decision ask <doc> "Question?" --option "A: trade-off" --option "B" [--recommended 2]
gloss-cli send <doc> "message" --wait [--timeout 600]   # the doc's agent; prints its closing reply
gloss-cli status <doc> | chat <doc> --last 6 | cancel <doc> | wait <doc>
gloss-cli permission answer <doc> <card> allow|reject|cancel
gloss-cli question answer <doc> <card> "Blue" [--skip]
gloss-cli decisions <doc> | decision answer <doc> d1 "label" [--note ...]
gloss-cli threads <doc> | thread start <doc> --quote "exact text" "message" | thread reply|resolve <doc> <thread>
gloss-cli versions <doc> | restore <doc> latest | undo <doc>
gloss-cli export <doc> md|pdf <path> | move <doc> <folder> | archive <doc> | settings get
```

Things to know:

- **CLI edits write directly** like a user's edit: no agent turn, no markers. A `pre-cli` version is saved first, so `restore <doc> latest` (or a name from `versions`) steps back. Edit through the CLI rather than the file when the doc is open in gloss.
- **`send` asks gloss's own agent**, which is slower and costs money; to change the doc yourself use `edit-section` / `insert`. A message to a working agent is steered into its turn.
- **Exit 3** means the agent is blocked on a permission or question card: `status <doc>` lists them with ids; answer, then `wait <doc>`. Don't answer cards the user should decide unless they told you to.
- Research answers come back in chat (`send --wait` prints them); the agent doesn't write them into the doc unless asked.
- `decision answer` and `thread start|reply` start an agent turn; follow with `wait <doc>` if you need the result.
