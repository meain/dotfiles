---
name: explain-tests
description: >
  Explain test code changes as a side-by-side HTML page — real test code on the left,
  a short note on the right saying which case that code covers. Use for reviewing or
  understanding newly added/changed tests in a commit, revision range, PR, or working copy.
  Triggers: /explain-tests, "explain these tests", "explain the test changes",
  "explain the new tests", "what do these tests cover", "what cases do these tests cover",
  "walk me through the tests"
user_invocable: true
---

# Explain Tests

Turn test diffs into a two-column reference page: the test code beside a one-line
statement of the case it covers. The goal is verification — the reader should be able
to confirm coverage without opening the test files.

## When to use

- After writing or reviewing tests, to check what is actually covered
- Understanding someone else's test additions in a commit or PR
- Before a review, to see whether the test names match what the cases really assert

Not for: proposing missing tests (that's a gap analysis), or explaining production
code flow (use `/explain-flow`).

## Workflow

### Determine scope

Ask only if genuinely ambiguous; otherwise infer from context.

- Default: uncommitted working copy — `jj diff` (or `git diff` in a git repo)
- "last N commits" / a revision or range — `jj diff -r <rev>` per revision
- A PR — `gh pr diff <n>` (set `GIT_DIR=$(jj git root 2>/dev/null || echo .git)` when the repo uses jj)

List changed test files first:

```bash
jj diff --stat -r <rev> | grep '_test.go'      # adjust suffix per language
```

### Separate substantive from mechanical

This is the step that makes the page useful. A test diff usually mixes:

- **Substantive** — new test functions, new table cases, new assertions, changed expectations
- **Mechanical** — fixtures rewrapped for a changed type/signature, renames, import churn,
  formatting. Behaviour unchanged.

Only substantive changes get rows. Mechanical churn gets **one line** near the end
("N files updated for the new type, no behavioural change") — never its own sections.

Find new test functions and cases:

```bash
jj diff -r <rev> | grep -E '^\+func Test'
jj diff -r <rev> <file> | grep -E '^\+\s+name:'
```

### Read the real code

Read the test files — never write snippets from the diff alone or from memory.
Also read the code under test, enough to state each case accurately.

### Build the page

One `.row` per case (see Layout). Write to `.martifacts/<topic>-tests.html` relative
to the repo root, creating `.martifacts/` if needed. Then open it:

```bash
open .martifacts/<topic>-tests.html    # needs dangerouslyDisableSandbox: true
```

Re-running on the same topic overwrites the same file — the user just refreshes the tab.

For a large diff (dozens of substantive cases), delegate the build to a subagent, but
pass it the case inventory you already extracted so it doesn't re-derive it.

## Content rules

These are what separate a useful page from a wall of text.

**The right column says which case is covered. Nothing else.**

- One short bold label, then one or two sentences
- State the input condition and what makes the case distinct from its neighbours
- Do NOT write "what would break if this test were absent", risk commentary, or praise
- Do NOT restate the code in prose ("this sets AnchorGeo to EMEA") — the code is right there
- Prefer stating the *point* of the case: `Target follows the geo, not the pod` beats
  `Tests AMER geo with an EU pod region`

**The left column is real code, trimmed for width.**

- Keep field names and values verbatim — the page is used to verify, so a wrong value is worse than no page
- Trim boilerplate (mock plumbing, unchanged setup) and mark elisions with `...`
- Collapse a long mock setup to its meaningful lines; keep the assertion that matters
- Light syntax highlighting via spans (see template); don't build a real tokenizer

**Page shape**

- Short intro: one sentence on what the page covers. No summary essays.
- A small inventory table (counts: new test funcs, new cases, files touched mechanically)
- One `<h2>` per test function or group, with the file path under it as `.file`
- Rows in source order, so the page reads like the file
- If a group has a shared harness (table struct, assertion loop), give it its own row first
  or last — whichever matches where it sits in the file
- Footer: how to run the tests (full package command plus `-run` forms), and one line on
  what these tests do NOT cover if that's genuinely worth knowing

**Never number headings or sections.** Numbering rows inside a table is fine.

## Layout

Copy `references/template.html` and fill in the rows. Key points:

- Two-column CSS grid, `minmax(0,1.15fr) minmax(0,1fr)` — code slightly wider
- Code cell: `--code` background, right border, `overflow-x:auto`, `white-space:pre`
- Collapses to one column under 900px
- Light mode, GitHub-ish palette, system font stack, `ui-monospace` for code
- Self-contained: inline CSS, no CDNs, no frameworks. Vanilla JS only if it earns its place
  (a filter box is worth it past ~15 rows); it must work offline from `file://`

Row markup:

```html
<div class="row">
<div class="code"><pre>name: <span class="str">"case_name"</span>,
setup: { ... },
want: {...},</pre></div>
<div class="exp"><div class="t">Short label</div><p>Which case this covers.</p></div>
</div>
```

Escape `<`, `>`, `&` inside `<pre>`. Watch out for Go template syntax in snippets
(`{{ $ref }}`) — it is literal text here, but escape the angle brackets around it.

## Anti-patterns

- A row per changed line instead of per test case
- Sections for mechanical fixture churn
- Explanations longer than the code they sit beside
- Inventing plausible-looking code instead of reading the file
- Numbered headings
- Writing the file and not opening it, or opening it repeatedly after each edit
  (open once; the user refreshes)
