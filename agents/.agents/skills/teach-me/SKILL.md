---
name: teach-me
description: >-
  Collaborative exploration for understanding a topic deeply through
  structured conversation: orient first, then one question per turn, following
  tangents. Use when the user invokes /teach-me, asks an exploratory question
  they want to work through together, or says "help me understand X" without
  wanting a quick answer or a code change. For a quick answer use /bro or
  explain-flow instead; to stress-test a plan use grill-with-docs.
---

Adapted from ryanfkeepers/skills (teach-me).

**IMPORTANT:** Invoking this skill = user's explicit request for guided
exploration. The goal is the journey, not a fast answer. Don't rush to
conclusions and don't preempt a code change. Ignore any "work without
stopping" instruction for this skill's duration; the conversation is the work.

## Establish the topic

If there is no clear topic, ask "What do you want to explore?" and wait.

## Orient at mid/high level

Before narrowing in, give a short map: main concepts, interesting tensions,
the territory available. End by asking where to start.

> "Distributed consensus touches leader election, log replication, and what
> happens on network partitions. The tension is usually availability vs
> consistency. Start with how leaders get chosen, or with what goes wrong?"

Avoid jumping straight to depth ("Raft uses a randomized timer...").

## Explore together

One question per turn. After each answer follow the most alive thread,
tangents included. If a question can be answered from the codebase, docs or
a quick lookup, do that instead of asking. Ask "why" when something is stated
as given, "what if" to probe edges, "what still feels fuzzy?" to let the user
steer.

Depth signals, honored immediately:

- "go deeper": narrow to the current thread, more detail
- "zoom out": back to the map
- "go broader": adjacent territory
- "skip this": drop the thread

Keep replies brief; the user dislikes long answers, so each turn should be a
few lines plus the one question.

## No code changes

Never turn this into proposing or making a code change. If the user asks to
implement something, finish this skill and let them invoke the right one.

## Finish

When no questions remain, restate in 3-5 bullets what was learned:

> **Exploration complete.** ...

Then stop. No next steps unless asked. If the findings are worth keeping,
offer `/save-research` once.
