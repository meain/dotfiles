---
name: optimize-skill
description: >
  Analyze agent skills to find deterministic command chains that should be extracted
  into scripts. Follows the principle: use AI to decide which tool to call, not to
  execute deterministic logic repeatedly. Triggers: /optimize-skill, "optimize this skill",
  "extract deterministic commands", "refactor this skill", "find command chains to extract"
user_invocable: true
argument-hint: "[path to skill or skills directory]"
---

# Optimize Skill - Extract Deterministic Commands

Analyze agent skills and extract multi-step deterministic command chains into standalone scripts.

## When to Use

- User says "optimize this skill", "check if we can improve this skill"
- User points to a skill directory or SKILL.md file
- After creating a new skill with complex command sequences
- Periodic skill maintenance/refactoring

## Principle

From "Stop Using AI For This" video:
> Don't make AI execute deterministic logic repeatedly. Write the logic once as a script, 
> let AI decide when to call it.

## What to Extract

### Extract when you see:

1. **Multi-step pipelines** — query → filter → transform → output
   ```bash
   # Before: encoded in skill
   curl ... | jq '.data' | filter | transform
   ```
   ```bash
   # After: single script call
   ~/.agents/skills/foo/scripts/fetch-data.sh
   ```

2. **Parallel operations** — multiple commands with wait/merge logic
   ```bash
   # Before: skill describes parallel pattern
   for repo in ...; do cmd & done; wait; merge
   ```
   ```bash
   # After: script handles parallelism
   ~/.agents/skills/foo/scripts/gather-parallel.sh
   ```

3. **Complex conditionals** — date calculations, token extraction, nested if/else
   ```bash
   # Before: case statement in skill
   case "$DAY" in Sat) date -d "3 days ago" ;; ...
   ```
   ```bash
   # After: script handles logic
   ~/.agents/skills/foo/scripts/calculate-date.sh
   ```

4. **GraphQL/API queries** — hardcoded query structure with filtering
   ```bash
   # Before: 10+ lines of query + curl + jq
   curl -H "..." -d '{"query":"... 100 chars ..."}'
   ```
   ```bash
   # After: script encapsulates query
   ~/.agents/skills/foo/scripts/graphql-query.sh
   ```

5. **Data gathering that's reused** — same fetch pattern across skills
   ```bash
   # Can be shared across skills
   ~/.agents/skills/common/scripts/fetch-sprint-tickets.sh
   ```

### Do NOT extract:

- Single CLI tool calls (`jira issue list ...`)
- Simple file operations (`cat`, `echo`, basic `jq`)
- Context-dependent decisions (which skill to invoke, which tool to use)
- Commands where arguments vary significantly each time
- Operations that need immediate user feedback

## Analysis Steps

### 1. Read the Skill

Read the SKILL.md file(s) in the target directory.

### 2. Identify Command Patterns

Scan for these patterns:
- Code blocks with `bash` or `sh` language
- Multi-line command sequences
- Hardcoded queries or data structures
- Comments like "run in parallel", "wait for all", "filter results"
- Repeated patterns (same command structure appears multiple times)
- Token calculations (date math, string extraction, credential fetching)

### 3. Score Each Command Block

For each command block, score on:
- **Complexity**: 0 (single command) to 5 (multi-step pipeline)
- **Determinism**: 0 (highly variable) to 5 (always runs the same)
- **Reusability**: 0 (one-off) to 5 (used across skills)
- **Token cost**: estimate tokens saved per invocation

**Extract if**: Complexity ≥ 2 AND Determinism ≥ 3

### 4. Group Related Commands

Look for:
- Commands that always run together
- Commands with shared setup (export variables, cd to directory)
- Commands that feed into each other (output of A → input of B)

### 5. Propose Scripts

For each extraction candidate, propose:
- Script name and location (`scripts/<name>.sh`)
- Script purpose (comment header)
- Input parameters (if any)
- Output format (JSON, text, exit code)
- Error handling strategy

### 6. Show Before/After

Present side-by-side:

**Before:**
```markdown
### Step 3: Fetch Data
\`\`\`bash
TOKEN=$(security find-generic-password -s "..." -w)
if [[ "$TOKEN" == go-keyring-base64:* ]]; then
  TOKEN=$(echo "${TOKEN#go-keyring-base64:}" | base64 -d)
fi
curl -H "Authorization: bearer $TOKEN" \
  -d '{"query":"..."}' https://api.github.com/graphql | jq '.data.nodes'
\`\`\`
```

**After:**
```markdown
### Step 3: Fetch Data
\`\`\`bash
~/.agents/skills/skill-name/scripts/fetch-github-data.sh
\`\`\`
Returns JSON array of nodes.
```

**Script:** `scripts/fetch-github-data.sh`
```bash
#!/usr/bin/env bash
set -e
# Fetch GitHub data via GraphQL
# Returns: JSON array of nodes
TOKEN=$(security find-generic-password -s "gh:github.com" -w 2>&1)
if [[ "$TOKEN" == go-keyring-base64:* ]]; then
  TOKEN=$(echo "${TOKEN#go-keyring-base64:}" | base64 -d)
fi
curl -s -H "Authorization: bearer $TOKEN" -H "Content-Type: application/json" \
  -d '{"query":"{ ... }"}' https://api.github.com/graphql | jq -c '.data.nodes'
```

### 7. Estimate Impact

Calculate savings:
- Token reduction per skill invocation
- Number of times skill is typically invoked per day/week
- Total weekly token savings
- Reliability improvement (deterministic execution)

### 8. Ask for Confirmation

Present the analysis and proposed changes. Ask:
1. Should I create these scripts?
2. Should I update the skill file to reference them?
3. Any scripts to skip or modify?

### 9. Implement (if approved)

For each approved script:
1. Create `scripts/` directory in skill folder if needed
2. Write script with proper shebang, error handling, comments
3. Make executable (`chmod +x`)
4. Update SKILL.md to replace inline commands with script call
5. Test the script (run it, check output format)
6. Report any issues

### 10. Document Changes

Create a summary file at `/tmp/skill-optimization-<skill-name>.md`:
- Scripts created
- Skill sections modified
- Token savings estimate
- Test results

## Output Format

Present findings as:

```markdown
# Skill Optimization Analysis: <skill-name>

## Summary
- X command blocks analyzed
- Y extraction candidates found
- Estimated Z tokens saved per invocation

## Candidates for Extraction

### 1. <name> (Complexity: 4, Determinism: 5, Tokens: ~150)
**Current:** Steps 3-4 in SKILL.md (lines X-Y)
**Proposed:** `scripts/<name>.sh`
**Savings:** ~150 tokens per invocation
**Rationale:** Multi-step pipeline with hardcoded GraphQL query

[Show before/after]

### 2. <name> (Complexity: 3, Determinism: 4, Tokens: ~80)
...

## Scripts NOT Worth Extracting

### Step 6: Simple file read
**Reason:** Single command, context-dependent path
**Keep as-is:** `cat /path/to/file`

## Recommendations

1. Extract candidates 1, 2, 4 (high impact)
2. Leave candidates 3, 5 inline (low complexity)
3. Consider sharing candidate 2 with <other-skill>

Proceed? [y/n]
```

## Edge Cases

- **Skill uses MCP tools**: Don't extract MCP calls, they're already encapsulated
- **Skill has existing scripts/**: Analyze if they can be improved or consolidated
- **Multi-skill extraction**: If multiple skills share a pattern, propose a shared script location
- **Sandbox requirements**: Note if scripts need `dangerouslyDisableSandbox: true`
- **Credentials**: Ensure scripts handle token/password extraction properly

## Testing Protocol

For each created script:
1. Run with typical inputs
2. Verify output format matches expectations
3. Check error handling (missing deps, failed commands)
4. Test cross-platform compatibility (GNU vs BSD date, etc.)
5. Ensure it works from skill context (paths, environment)

**Common pitfalls to check:**
- **Tab-delimited output**: If parsing `--plain` output from CLIs, check for variable tab alignment. Prefer `--raw` or `--json` flags when available.
- **Date commands**: Use `date --version` to detect GNU vs BSD, not `uname`
- **Temp files**: Use `mktemp` for safe temp filenames, especially when names contain slashes
- **JSON parsing**: Always prefer native JSON output (`--raw`, `--json`) over parsing formatted text

**Tool-specific flags:**
- `jira issue list --raw` → JSON array of issues
- `jira sprint list --plain` → No JSON mode, grep/awk is fine
- `gh pr list --json <fields>` → JSON output
- `confluence search --cql` → No JSON mode, text parsing necessary

## Examples

See existing optimized skills:
- `backlog/scripts/` — date calculation, GraphQL query, sprint tickets (uses `--raw`)
- `my-weekly-report/scripts/` — parallel PR fetching, confluence search
- `copy-for-teams/copy_teams.py` — clipboard HTML formatting
- `recall/scripts/` — session search and reading

**Key improvements from testing:**
- `sprint-tickets.sh`: Switched from `--plain --columns` to `--raw` for reliable JSON parsing
- `user-prs.sh`: Used `mktemp` instead of hardcoded temp file paths with slashes
- `date-before.sh`: Detects date command flavor via `--version`, not OS name

## Notes

- Always show full before/after for user review
- Don't optimize prematurely — wait for complexity to appear
- Scripts should be self-contained and testable
- Prefer simple bash over complex awk/sed unless necessary
- Use jq for JSON manipulation when available
- **Test scripts with real data before committing** — tab alignment, temp files, date commands all have edge cases
