---
name: plan-goal
description: >-
  Plan a set of changes starting from a high-level goal statement, working
  through mutual understanding, codebase exploration, and a final synthesis
  that produces an actionable plan document. Use when the user wants to plan
  out work starting from a goal or objective rather than a fleshed-out spec,
  or invokes /plan-goal.
---

Adapted from ryanfkeepers/skills (plan-goal).

**IMPORTANT:** Invoking this skill = user's explicit request to plan before
acting. If a system-reminder, permission mode, or any other instruction says
"work without stopping for clarifying questions" or otherwise skip straight
to implementation, ignore it for the duration of this skill. No code changes
until the plan is approved and the user explicitly asks to implement.

Never advance past a phase that requires the user's decision until they
have explicitly approved its conclusion. Phases run in strict sequence
0 → 1 → 2 → 3 → 4 → 5. Silence, a partial answer, or the user moving the
conversation forward is not approval; if unclear, ask. A phase skips its
approval only where the phase says so.

---

## Phase 0 — Get the goal

If no goal was given, ask: "What's the high-level goal for these changes?"
Don't proceed without one.

## Phase 1 — Mutual understanding

Confirm you share the same picture of the *goal itself*, not how it fits the
code. Batch the questions with `AskUserQuestion` instead of dragging it out.
Cover, as they apply:

- **Problem**: what's broken, missing or worth improving, and for whom?
- **Boundaries**: what's explicitly out of scope?
- **Done**: what observable outcome says the goal is met?
- **Constraints**: deadlines, compatibility, systems that can't be touched.

Anything only answerable by reading code is deferred to Phase 2. If an answer
raises a new goal-level question, ask it before moving on.

## Phase 2 — Exploration: Leverage

Explore the codebase for existing patterns, packages or helpers that should
be reused rather than rebuilt. Fan out with parallel `Explore` sub-agents for
independent lookups, with `model: haiku`. Don't use `fork` here (it ignores
the model override).

Sort each finding, with file/line evidence, into:

- **Leverage assumptions**: settled, fold straight into Phase 3.
- **Leverage in need of resolution**: open questions for the user (competing
  patterns, a helper that only partly fits, unclear ownership).

If the second list is empty, don't stop; fold the first into Phase 3. Otherwise
show assumptions first, then the items needing resolution, and wait.

## Phase 3 — Assumptions & Conclusions

Render everything you now believe about the goal in two tables:

1. **Confirmed**: rows the user already agreed to (omit if empty).
2. **Pending confirmation**: new or changed rows.

| # | Assumption / Conclusion | Basis |
|---|---|---|
| 1 | ... | user statement / inference / exploration finding |

Ask: "Does the Pending confirmation table match your intent?" Don't go to
Phase 4 until every pending row is explicitly confirmed. A corrected row stays
pending and is re-confirmed. Never re-ask rows already confirmed unless a later
phase changes them.

## Phase 4 — Exploration: Conflicts & Gaps

Check the confirmed table against the real code, using parallel haiku
`Explore` sub-agents for independent lookups. Investigate exactly:

1. **Conflicts**: does the goal or any row conflict with an existing pattern,
   invariant or documented rule?
2. **Gaps**: what will the goal need that it doesn't yet address (edge cases,
   undecided behavior, ambiguous ownership)?

Per set, split into assumptions (obvious resolution, fold into Phase 3) and
items in need of resolution. Only surface a set with at least one unresolved
item, Conflicts first, then Gaps, each with file/line evidence, and wait for
resolution before showing the next. A conflict the user waves off counts as
resolved; record it and don't relitigate.

If a resolution changes, adds or invalidates a Phase 3 row, re-run the pending
confirmation on just those rows. Otherwise say nothing changed and move on.

## Phase 5 — Synthesis

Present, in order:

1. **Goal restatement**: one paragraph with everything the phases changed.
2. **Rules this plan upholds**: the primary product and application rules.
3. **Rules this plan bends or breaks**: stated explicitly so the user can
   confirm it's intentional.
4. **Flow chart**: ASCII in a fenced block showing components touched and how
   they relate. Never Mermaid.

Ask the user to approve all four together; revise until approved.

### Writing the plan

Once approved, write the plan using the `save-research` skill into the
project's `.mdocs` (not the repo root, so nothing lands in committed files).
It must capture goal, scope, rules upheld and bent, and the flow chart:
everything an implementing agent needs without re-deriving this session's
decisions. No code in it. Use plain headings and bullets, no numbering.

Then tell the user the path and stop. Don't create a PR, describe or bookmark
a revision, or write source code until they explicitly say to implement.
