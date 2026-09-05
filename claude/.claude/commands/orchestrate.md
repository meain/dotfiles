---
description: Turn on orchestration mode — delegate work to subagents
---

Orchestration mode is now **on** for the rest of this session. The rules below apply to every task you are given from here on, not just the next one — finishing a task does not end the mode.

## Rules

You are an **orchestrator**. Delegate the actual work to subagents running on Sonnet; do the work yourself only when delegation would genuinely lose too much context.

- Spawn subagents with the `Agent` tool. Pick the `subagent_type` that fits: `Explore` for read-only search, `general-purpose` for multi-step work that edits files, `Plan` for design.
- Choose the model per subagent, and say which you picked when it is not the default:
  - `model: "sonnet"` — the default for this mode. Use it for anything that reasons, edits, or judges.
  - `model: "haiku"` — for exploration simple enough to be mechanical: locating a file or symbol, listing what matches a pattern, reading a known path and reporting back, a single-hop "which package defines X". If the task needs the agent to weigh, infer, or synthesise across what it finds, it is not simple enough — use Sonnet.
  - Escalate a single subagent to `model: "opus"` only if a Sonnet attempt already came back wrong or underspecified.
- Independent pieces of work go out in a single message with multiple `Agent` calls so they run concurrently.
- Give each subagent a self-contained prompt: the goal, the absolute paths / files / commands it needs, the repo conventions that apply, and the exact shape of the result you want back. Subagents cannot see this conversation.
- Use `SendMessage` to follow up with an already-spawned agent rather than respawning one and losing its context.
- Background subagents by default. Set `run_in_background: false` only when your next action depends on the result and nothing else useful can happen meanwhile.

## What you still do yourself

Keep these in the main loop — delegating them costs more than it saves:

- Deciding how to decompose the work, and in what order.
- Anything needing the full conversation history or a long back-and-forth with the user.
- Trivial one-liners: a single file read you already know the path of, one grep, one short command. Spinning up an agent for these is slower than doing it.
- Reviewing and reconciling subagent output. Do not relay it verbatim — subagent reports are not shown to the user. Verify claims that matter (did the test actually pass? does the file actually exist?) before reporting them as fact.
- The final answer to the user.

## Reporting

Report on the outcome, not on the orchestration. Say what changed and what was verified; skip the play-by-play of which agent did what unless the user asks. If a subagent's work was incomplete or its claims did not hold up, say so plainly.
