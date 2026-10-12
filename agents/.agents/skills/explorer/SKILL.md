---
name: explorer
description: >
  Delegate purely exploratory lookups to a cheap Haiku subagent that finds
  which files, symbols, or directories are relevant and reports locations back.
  It does not interpret or explain code. Use when you need to locate where
  something lives (across repos, many files, unfamiliar areas) before reading
  it yourself. Triggers: /explorer, "find where X is", "which files deal with
  X", "locate the code for X", "explore the repo for X".
---

Spin up a Haiku subagent to do the file lookup so the main thread keeps its
context for actually understanding the code.

## When to use

- Unknown location: "where is X handled / defined / configured / tested?"
- Broad sweeps: many files, several modules, or multiple repos
- Mapping the candidates before deciding what to read

Skip it for a single known lookup (one `rg` or `fd` call) — do that directly.

## Step 1: Write a narrow brief

Pass the subagent:

- **Question**: what you are trying to locate, in one or two sentences
- **Hints**: likely names, keywords, identifiers, error strings, or entry points
- **Scope**: directories or repos to search (e.g. `services/earn`, `../earn`,
  `../control-plane-platform`). Never `/` or `$HOME`.
- **Breadth**: quick (a few likely spots) or thorough (multiple naming
  conventions and locations)

## Step 2: Spawn the agent

Use the Agent tool with `subagent_type: "Explore"` and `model: "haiku"`. Run
independent explorations in parallel in a single message. Use this prompt
template, filling in the brief:

```
You are a file locator. Find where the following lives and report locations
only. Do NOT explain, summarize, or judge how the code works, and do NOT edit
anything.

Question: <question>
Hints: <hints>
Scope: <paths>
Breadth: <quick|thorough>

How to search:
- Use rg (not grep) and fd (not find). Scope every search to the given paths.
- Try several naming variants (camelCase, snake_case, singular/plural, env var
  and config-key spellings) before concluding something is absent.
- Read only enough of a file (a few lines around a match) to confirm it is
  relevant. Do not read whole files.

Report format (terse):
- Most relevant: up to ~10 entries, ranked, each as `path:line` plus a
  five-words-or-fewer note on why it matched (e.g. "defines Handler type",
  "test for retry").
- Also related: other candidate paths, one line each.
- Searched, nothing found: patterns/paths that came up empty.
- Unsure: anything ambiguous, flagged as a guess.
```

## Step 3: Use the result

- Treat the report as a map of leads, not as findings. Haiku locates; it does
  not understand.
- Read the top files yourself before drawing conclusions or making changes.
- Spot-check a `path:line` before relying on it. If the report is thin or
  off-target, re-run with better hints or a wider scope rather than guessing.
- Don't relay the raw report to the user unless they asked for locations.
