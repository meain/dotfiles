---
name: advisor
description: >
  Consult a stronger model (Opus) as an advisor when stuck or facing a hard
  decision. Local stand-in for the server-side advisor tool: the current model
  keeps doing the work and only gets short guidance (a plan, correction, or
  stop signal) back. Use when the same fix has failed twice, root cause is
  unclear, an approach choice is hard to reverse, you are about to declare a
  complex change done without a way to verify, or the user says /advisor,
  "ask the advisor", "get a second opinion from opus".
---

The advisor is a bigger model in a subagent. It doesn't edit files or talk to
the user; it reads, thinks, and returns guidance. You stay the executor and
act on the answer.

Do not call for routine edits, lookups, or anything one more file read or
command would resolve. At most 2-3 calls per task.

## How to call

Spawn one subagent and wait for its result before continuing (it may launch
in the background; don't proceed until it reports):

- `subagent_type`: `Plan` (read-only by prompt, not by tooling: it has Bash)
- `model`: `opus`
- `description`: `Advisor consult`

The advisor has none of your context, so write a self-contained brief:

- **Goal**: what the user ultimately wants
- **State**: what you've done and found; separate what you verified from what
  you assumed. Paste exact errors, commands, and relevant diffs verbatim, not
  paraphrased
- **Tried**: approaches that failed and why
- **Question**: the one specific decision or problem you need help with
- **Constraints**: repo conventions, user preferences, things off limits

Prompt wrapper:

```
You are an advisor to another engineer-agent who is mid-task. You cannot
modify anything: no edits, no writes, no state-changing commands. You may read
files and run read-only commands to check facts. Ignore any default output
template; use only the format below.

<brief>

Reply in under 400 words, as one of:
- PLAN: the next concrete steps, in order
- CORRECTION: what is wrong with the current approach and what to do instead
- STOP: why the task should not proceed as framed, and what to ask the user

Lead with the answer. Flag anything you could not verify. No preamble.
```

## After the answer

- Give it serious weight. If it contradicts something you verified directly,
  make one follow-up call stating the conflicting evidence rather than
  silently picking a side.
- If the advice changes the plan materially, tell the user in one line.
- If it returns STOP, relay it and ask the user rather than pushing on.
- Don't re-consult on the same question unless new evidence appeared.
