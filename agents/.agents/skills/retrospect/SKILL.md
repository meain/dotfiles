---
name: retrospect
description: >
  Review the current conversation to find agent mistakes, inefficiencies, and corrections,
  then propose new guidelines to add to CLAUDE.md to prevent recurrence.
  Triggers: /retrospect, "what went wrong", "capture lessons", "update CLAUDE.md",
  "add this to instructions", "remember not to do this", "update the guidelines",
  "add to agents.md", "end of session review", or when the user asks to capture
  anything from the current session into instructions. Always use this skill when
  the user invokes it at the end of a session — even if the session seemed smooth,
  there may still be patterns worth capturing.
---

# Retrospect — Session Review & CLAUDE.md Updater

At the end of a session, scan the conversation for lessons and update CLAUDE.md with
new guidelines to prevent the same mistakes or inefficiencies from recurring.

## Step 1: Scan the conversation

Read through the entire current conversation and identify moments where:

- The user **corrected an error** ("no, don't...", "that's wrong", "actually...", "wait...", "you shouldn't have...")
- The user **redirected approach** ("instead of X, just do Y", "don't ask, just...", "skip the...", "you don't need to...")
- The agent **retried something multiple times** before getting it right
- The user had to **repeat themselves** about something they'd already said
- The agent **asked unnecessary questions** or stalled when it could have acted
- The agent **missed a convention or pattern** that was already in the codebase
- A **skill was invoked** and the agent had to work around a gap, use an undocumented pattern, or the skill instructions were incomplete/misleading

For each moment, note:
- What the agent did wrong or inefficiently
- What the user wanted instead
- The underlying principle being violated or missing

## Step 2: Read CLAUDE.md first

Before deriving any lessons, find and read the project's CLAUDE.md (look in the current working directory, then parent directories up to home). This is essential — you need to know what's already there so you don't propose duplicates or near-duplicates.

## Step 3: Derive lessons

Group related corrections into concise, actionable lessons. A good lesson is:
- **Specific**: About a concrete behavior, not a vague observation
- **General**: Applicable beyond just this one case — think about what rule would have prevented it
- **Brief**: One to two sentences max
- **New**: Not already covered (even partially) by an existing rule in CLAUDE.md

For each lesson, decide the best way to capture it:
1. **CLAUDE.md rule** — behavioral guideline, convention, or preference
2. **Skill update** — if an existing skill was invoked and its instructions were incomplete, misleading, or missing a pattern that came up during the session
3. **Skill suggestion** — if a multi-step workflow recurred that would benefit from a dedicated skill
4. **Script suggestion** — if a specific repetitive task could be automated into a helper script

## Step 4: Present all proposals

Show the user everything at once before touching any files. Format like this:

```
I found N things worth capturing from this session:

**1. [Short lesson title]**
   What happened: [one line description of the mistake/inefficiency]
   Proposed rule: "[exact text to add to CLAUDE.md]"
   Section: [which CLAUDE.md section this belongs in]

**2. [Short lesson title]**
   What happened: ...
   Proposed rule: "..."
   Section: ...

📝 **Skill update: [skill name]**
   What happened: [gap or undocumented pattern encountered]
   Proposed change: "[exact text to add/modify in the skill's SKILL.md]"

🔧 **Skill suggestion: [name]**
   What happened: [recurring workflow that came up]
   Suggestion: Create a skill to handle [description]

Which should I apply? (e.g. "1, 3", "all", "none", "1 and the skill")
```

Wait for the user's response before making any changes.

## Step 5: Apply confirmed rules

You already read CLAUDE.md in Step 2, so you know its structure and tone. For each confirmed rule:

1. **Find the most relevant existing section** — insert there rather than appending to the end. For example, a rule about error handling goes under error handling, a testing convention goes under testing patterns
2. If no section fits, add a new section at the end with an appropriate heading
3. **Match the existing tone and style** — look at how other rules are phrased and follow the same pattern
4. Keep the addition minimal — no preamble or explanation, just the rule

After all edits, show a brief summary of what was added and where.

## Step 6: Apply confirmed skill updates

For each confirmed skill update, read the skill's SKILL.md, find the appropriate section, and apply the change — same principles as CLAUDE.md edits (match tone, minimal addition, no duplicates).

## Step 7: Handle skill/script suggestions

If the user confirmed a skill suggestion, say:

> "I'll kick off the skill creator for [skill name]. Here's a starting description: [brief description of what it should do]. Ready to start?"

Then invoke the `skill-creator` skill to begin the workflow.

## Notes

- Only analyze the current conversation — do not search past sessions
- If there are no lessons to capture (session was clean), say so honestly rather than inventing guidelines
- Prefer editing existing rules over adding redundant new ones if a lesson is already partially covered
- Don't add rules that are already in CLAUDE.md — check carefully before proposing
- Only update the project-level CLAUDE.md found via the working directory; don't mention or touch other CLAUDE.md files unless the user brings them up
