---
name: refactorability
description: >
  Review code for refactorability — surface concrete, prioritized refactoring opportunities
  grounded in Martin Fowler's smell catalog and SOLID, augmented with static analysis tools
  (gocyclo, gocognit, dupl, etc.) when available. Go-first, polyglot-aware. Defaults to the
  current diff. Produces a High/Med/Low report with specific refactoring moves and a
  "Do NOT refactor" section to prevent over-eager suggestions.
  Triggers: /refactorability, "review for refactorability", "what should I refactor",
  "refactorability review", "find refactoring opportunities", "is this refactorable",
  "review this code for refactoring"
metadata:
  author: meain
  version: "1.0.0"
---

# /refactorability — Refactorability Review

Surface **concrete, prioritized** refactoring opportunities in code. The goal is not generic
"clean code" feedback — it is to point at specific smells, name the refactoring move that
fixes them, and rank by impact so the user knows what to actually touch.

The review is **opinionated about restraint**: it must call out cases where refactoring would
be premature, speculative, or net-negative.

## Process

### 1. Pick the scope

In order of preference:

1. **Explicit target** the user named (file, function, package).
2. **Current diff** (default when no target given):
   - `jj` repo: `jj diff --name-only -r @` (fallback to `jj diff --name-only`)
   - `git` repo: `git diff --name-only HEAD` (plus `--cached` if there's nothing unstaged)
   - PR context: `gh pr diff` if a PR is being reviewed
3. **Whole package/module** only if the user explicitly asks for it.

Print the resolved scope before continuing so the user can correct it.

### 2. Detect language and tools

Look at file extensions in scope. For each language present, check which of the tools
below are on `PATH` and run them only on **files in scope** (not the whole repo).

#### Go (primary)

| Tool       | Invocation (per file or dir)                           | What it gives you            |
|------------|--------------------------------------------------------|------------------------------|
| `gocyclo`  | `gocyclo -over 15 <files>`                             | Cyclomatic complexity > 15   |
| `gocognit` | `gocognit -over 15 <files>`                            | Cognitive complexity > 15    |
| `dupl`     | `dupl -threshold 50 <dirs>`                            | Duplicated token sequences   |
| `revive`   | `revive -formatter friendly <files>`                   | Style + some smell rules     |
| `golangci-lint` | `golangci-lint run --no-config --disable-all --enable=gocyclo,gocognit,dupl,gocritic,revive <pkg>` | Aggregated, if standalone tools are missing |

Also do a quick `grep`/AST scan for: functions > ~80 lines, > 5 params, deep nesting
(> 3 levels), `interface{}`/`any` over-use, and large switch/`if` ladders on type.

#### Python

`ruff check`, `radon cc -s -a <files>`, `radon mi <files>`, `vulture <files>`, `pylint --disable=all --enable=R <files>` for refactor-class warnings.

#### JS/TS

`eslint` with `eslint-plugin-sonarjs` if configured, `jscpd <dirs>` for duplication,
`tsc --noEmit` for type smells.

#### Polyglot fallback

`semgrep --config=p/code-smells` if installed; otherwise lean on LLM reading.

**If no tools are available**, proceed with LLM-only reading and say so in the report
(so the user knows the quantitative pass was skipped).

### 3. Apply the lens

Read the code in scope and map findings to the Fowler smell catalog + SOLID. Use
[`references/code-smells.md`](references/code-smells.md) as the authoritative list. Do
**not** invent smell names — pick from the catalog. For each finding, pick a concrete
refactoring move from the same catalog (e.g. "Extract Method", "Replace Conditional with
Polymorphism", "Introduce Parameter Object").

### 4. Score severity

Assign each finding a severity:

- **High** — actively blocks change/test/understand. Examples: function with cyclomatic > 20
  in a hot path, duplicated business logic across files, god-object that everything imports,
  shotgun-surgery pattern around a single concept.
- **Medium** — measurable maintenance tax but localized. Examples: long parameter list on a
  helper, feature envy across two files, modest duplication.
- **Low** — taste / future-proofing. Examples: primitive obsession on one type, slightly
  long method, comments hiding an obvious extract.

If a finding could be either, **prefer the lower severity**. The bar for High is "the next
person changing this will get hurt."

### 5. Decide what NOT to refactor

Before writing the report, explicitly identify code that *could* look refactorable but
**should be left alone** for this review. Put these in a dedicated section (see below).
Common cases:

- Working, stable code with no upcoming change — refactoring is pure risk.
- Code that's verbose because of a clear API contract (e.g. protobuf gen, handler boilerplate).
- A single duplication (rule of three not met).
- Abstraction that would only have one caller.
- Tests — they're allowed to be repetitive and explicit.
- Generated code.

### 6. Write the report

Use the output format below. Keep it **terse and scannable** — bullets, tables, file:line
references. No motivational filler.

## Output format

```markdown
# Refactorability Review

**Scope:** <files / diff / package being reviewed>
**Tools run:** gocyclo, gocognit, dupl  (or: "none — LLM-only pass")
**Overall:** <one sentence. e.g. "Two High-severity hotspots in user-management/handlers,
the rest is healthy.">

## High

### 1. <Smell name from catalog> — `path/to/file.go:123`
- **What:** <1 sentence describing the smell concretely>
- **Signal:** <metric or quote, e.g. "gocyclo=24, 142 LOC, 6 params">
- **Why it matters:** <1–2 sentences on change/test/understand cost>
- **Move:** <Refactoring name from catalog> — <1–2 sentence sketch>
- **Effort / Risk:** S/M/L — <1 short note on risk>

### 2. ...

## Medium

### 1. ...

## Low

### 1. ...

## Do NOT refactor

- `path/to/file.go:45` — Looks like primitive obsession, but it's a single call site behind
  a stable API. Wait until a second caller appears.
- `path/to/gen_pb.go` — Generated.
- ...

## Notes (optional)
Anything the user should know but isn't a finding (e.g. "gocognit not installed, skipped
cognitive complexity pass").
```

## Guidelines

- **Cite the catalog.** Every finding's smell name and refactoring move must come from
  `references/code-smells.md`. If you want to flag something not in the catalog, call it a
  "Note" instead of a finding.
- **Always include file:line.** No "in some handler somewhere".
- **Quote a signal.** A metric (`gocyclo=24`), a count (`6 parameters`), or 2–4 lines of code.
  Without a signal it's just opinion.
- **One paragraph per finding, max.** This is a triage tool, not a design doc.
- **Resist these temptations:**
  - Style / naming nits (use a linter for those)
  - "Add tests" — that's a separate review
  - Rewrites — only suggest local refactoring moves
  - "Use design pattern X" without a smell to justify it
  - Speculative generality ("what if we need to swap the DB later?")
  - Performance — separate review unless the smell *causes* the perf issue
- **High-severity ceiling.** Aim for ≤ 3 High findings per review. If everything is High,
  nothing is.
- **Honor existing project conventions.** If the codebase is consistently structured a certain
  way (e.g. Veeam VDC handlers), don't flag conformance to that style as a smell.

## When to fail loudly

- If the scope is empty (no diff, no target), ask the user what to review.
- If the user asks to review > ~50 files in one go, push back and propose narrowing.
- If running a tool errors out (e.g. `gocyclo` not installed), note it in "Notes" — do not
  silently skip.

## Reference

- [`references/code-smells.md`](references/code-smells.md) — Fowler smell catalog + refactoring moves,
  used as the authoritative vocabulary for findings.
