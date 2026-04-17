---
name: analyze-prs
description: Analyze my PRs across tracked repos, collect human reviewer feedback, identify recurring patterns and areas for improvement, and generate a visual HTML report. Accepts an optional time range argument (e.g., "last month", "last 2 weeks", "march") defaulting to last month.
disable-model-invocation: true
---

# PR Self-Review Analysis

Analyze PRs authored by the current user across all tracked repos, extract human reviewer feedback, identify improvement patterns, and produce an HTML report.

## Arguments

`$ARGUMENTS` is an optional time range string (e.g., "last 2 weeks", "last month", "march 2026"). Default to "last month" if empty.

## Workflow

### 1. Setup

```bash
# Get GitHub username
gh api user --jq '.login'

# Read repo list
cat ~/.dotfiles-private/datafiles/.config/datafiles/prs-repos
```

### 2. Fetch PRs

For each repo in the repos file, fetch PRs authored by the user within the time range:

```bash
gh pr list --repo <REPO> --author <USERNAME> --state all --limit 50 \
  --json number,title,state,createdAt,mergedAt,closedAt,reviewDecision,additions,deletions,changedFiles,url
```

Filter results to only PRs whose `createdAt` falls within the requested time range.

### 3. Fetch Review Comments

For each PR that has comments, fetch them in parallel across repos:

```bash
# Inline review comments
gh api repos/<REPO>/pulls/<NUMBER>/comments --jq '.[] | "[\(.user.login)] \(.body)"'

# Review-level comments (non-approval, non-empty)
gh api repos/<REPO>/pulls/<NUMBER>/reviews --jq '.[] | select(.state != "APPROVED" and .body != "") | "[\(.user.login)] [\(.state)] \(.body)"'
```

### 4. Filter to Human Reviewers

Discard comments from bots. Common bot suffixes/names to filter out:
- `[bot]` suffix (e.g., `copilot-pull-request-reviewer[bot]`, `coderabbitai[bot]`, `claude[bot]`)
- `Copilot` prefix comments (GitHub Copilot inline suggestions)

Keep only comments from human reviewers. Also keep the user's own replies for context on how feedback was addressed.

### 5. Analyze Patterns

Group human reviewer feedback into recurring themes. Common categories to look for:

- **Code readability**: nested conditionals, complex functions, missing comments
- **Codebase conventions**: test placement, existing helpers/libraries not used, style violations
- **Error handling**: log message style, error wrapping, error checking patterns
- **Feature rollout**: feature flag gating, config optionality, shadow mode logging
- **Naming/typos**: PR title typos, file name typos, variable naming
- **Schema/API consistency**: enum mismatches across specs, field name misalignment
- **PR hygiene**: closed-without-merge PRs, missing context for reviewers
- **Operational awareness**: e2e test alerting impact, startup dependency failures

For each pattern, include:
- Specific PR references (repo#number)
- Direct quotes from reviewers
- A concrete action item

### 6. Generate Report

Write the report as markdown to `/tmp/pr-self-review-<period>.md` where `<period>` is a slug of the time range (e.g., `march-2026`).

Structure:
```markdown
# PR Self-Review: Areas for Improvement (<period>)

<one-line summary: N PRs across M repos>

## 1. <Pattern Name>
- **<repo>#<number>**: <reviewer>: "<quote>"
**Action**: <concrete improvement>

...

## Summary Checklist
1. <actionable item>
...
```

### 7. Open Report

```bash
,markdown-to-html /tmp/pr-self-review-<period>.md
```

## Important Notes

- Fetch review comments in parallel across repos to avoid slow sequential API calls
- Use `gh api` for fetching comments (not `gh pr view`) to get full comment bodies
- A PR with 0 human review comments still counts for aggregate stats (PR size, merge time, closed-without-merge)
- Keep the report focused on actionable patterns, not exhaustive PR-by-PR summaries
- Do not include the report content in the chat response — just confirm it was opened
