---
name: explain-flow
description: >
  Explain how code flows with concrete input/output examples, ASCII diagrams, and
  before/after tables. Works on PRs, functions, modules, or any code path.
  Triggers: /explain-flow, "explain this PR", "explain this function", "explain this module",
  "how does this flow", "walk me through", "explain the change in"
---

# /explain-flow — Explain Code with Concrete Examples

Produce a clear explanation of how data flows through a PR, function, or module using concrete
input/output examples. The goal is to make the reader *see* the data transforming at each step.

## Process

### 1. Identify the target

- **PR**: Fetch with `gh pr view` and `gh pr diff`. Read all changed files.
- **Function**: Read the function and its callers/callees (one level each direction).
- **Module/package**: Read the public API surface and key internal wiring.

### 2. Understand the before and after

For PRs and refactors, understand what the code did *before* the change and what it does *after*.
For functions/modules, understand the entry points, exit points, and key decision branches.

### 3. Write the explanation

Structure the output as follows:

#### a. One-paragraph summary
What does this change/code do and why? Lead with the motivation.

#### b. Step-by-step flow with concrete examples
Walk through the code path using realistic example data. Show the actual values at each step.

Use indented code blocks for data snapshots:

```
Request: GET /api/items?filter=active
  1. Parse filters → { status: "active" }
  2. Check permissions → { orgIDs: ["org-1"], tenantIDs: ["t-1"] }
  3. Query DB with filters + permissions → SELECT ... WHERE status='active' AND org_id IN ('org-1')
  4. Return → [{ id: "item-1", status: "active" }]
```

#### c. Decision matrix (when there are branches)
Use a markdown table showing different scenarios and their outcomes:

```
| Input condition       | Path taken     | Result              |
|----------------------|----------------|---------------------|
| flag ON + authorized | new + old path | union of both       |
| flag OFF             | old path only  | legacy behavior     |
| neither authorized   | early return   | empty response      |
```

#### d. Other changes (if PR)
Briefly list supporting/mechanical changes (test updates, helpers, config) that don't affect the core flow.

## Guidelines

- Use **realistic field names and values** from the actual code, not generic placeholders.
- Keep examples **short but complete** — show enough to understand the transformation, not every field.
- When explaining PRs, always contrast **before vs after** behavior.
- Prefer ASCII flow notation over prose for multi-step pipelines.
- If a function has many branches, pick the 2-3 most important paths to trace rather than being exhaustive.
- For modules, start with the "happy path" then mention error/edge paths.
