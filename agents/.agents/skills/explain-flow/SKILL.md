---
name: explain-flow
description: >
  Explain how code flows with concrete input/output examples, ASCII diagrams, and
  before/after tables. Works on PRs, functions, modules, or any code path.
  Triggers: /explain-flow, "explain this PR", "explain this function", "explain this module",
  "how does this flow", "walk me through", "explain the change in"
---

# /explain-flow — Explain Code with Concrete Examples

Produce a clear explanation of how data flows through a PR, function, or module. The goal is to
build the reader's mental model first, then confirm it with code — not to narrate the diff line
by line.

## Step 1 — Gather the source material

- **PR**: `gh pr view` and `gh pr diff`. Read all changed files.
- **Function**: Read the function and its callers/callees one level in each direction.
- **Module/package**: Read the public API surface and key internal wiring.

For PRs and refactors, understand what the code did *before* the change and what it does *after*
before writing anything.

## Step 2 — Write the explanation

Work through these sections in order. Each one builds on the last.

### Background

Orient the reader before touching the code. Assume no prior knowledge of the surrounding system.
Cover:
- What the surrounding system does (one short paragraph)
- What was missing, broken, or constrained that prompted this change
- Any architectural rules or design decisions that shaped the approach

### Intuition

Explain the 2–4 core ideas in the change *before* showing any real code. Use toy examples and
analogies. For each concept:
- Name it
- Make it concrete with a minimal before/after or a simple example
- State why it matters

This section should make the Code section feel like confirmation rather than discovery. Don't
front-load implementation detail here — that belongs in the next section.

### Code walkthrough

One-paragraph summary of what the change does and why, then a step-by-step trace through the
key code path using realistic example data. Show actual values at each step:

```
Input: GET /api/items?filter=active
  1. Parse filters      → { status: "active" }
  2. Check permissions  → { orgIDs: ["org-1"], tenantIDs: ["t-1"] }
  3. Query DB           → SELECT ... WHERE status='active' AND org_id IN ('org-1')
  4. Return             → [{ id: "item-1", status: "active" }]
```

When a transformation spans multiple steps, show the data in both old and new form at *each*
step — not just the endpoints. The reader should see the value mutating stage by stage.

When there are meaningful branches, add a decision matrix:

```
| Input condition       | Path taken     | Result          |
|----------------------|----------------|-----------------|
| flag ON + authorized | new + old path | union of both   |
| flag OFF             | old path only  | legacy behavior |
| neither authorized   | early return   | empty response  |
```

### Other changes (PRs only)

Briefly list mechanical or supporting changes — test updates, config, helpers — that don't affect
the core flow.

## Guidelines

- Use realistic field names and values from the actual code, not generic placeholders.
- Keep examples short but complete: enough to see the transformation, not every field.
- Prefer ASCII flow notation over prose for multi-step pipelines.
- Pick the 2-3 most important code paths; don't trace every branch.
- For modules, lead with the happy path, then note error/edge cases.
