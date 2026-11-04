---
name: gloss
description: >
  What gloss is (the user's native macOS design-doc editor where a Claude agent co-writes
  MDX docs) and how to write a prompt for it. Use when the user asks for a prompt to give to
  gloss, wants to know what to ask gloss, or mentions gloss's chat, threads, decisions or
  slash commands. The output is a ready-to-paste prompt, not an edit to a doc.
  Triggers: /gloss, "prompt for gloss", "give me a prompt for gloss", "what should I ask gloss",
  "write this up for gloss", "gloss prompt", "ask gloss to"
user_invocable: true
---

# gloss

gloss is the user's own macOS app for writing design docs together with a Claude agent (source: `/Users/meain/dev/src/gloss`, run with `open /Users/meain/dev/src/gloss/build/Gloss.app`; `docs/plan.md` there is the contract, `AGENTS.md` the dev guide). The user mostly uses this skill to have *you* write a prompt they paste into gloss. You don't edit docs or run gloss yourself.

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
