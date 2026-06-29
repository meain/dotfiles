---
name: save-artifact
description: >
  Save an AI-generated artifact to the artifacts repository with structured metadata.
  Triggers: /save-artifact, "save this artifact", "store this artifact", "add to artifacts", "save to artifacts repo"
---

# save-artifact

Save an AI-generated artifact to the artifacts repository with structured metadata.

## Triggers

- `/save-artifact`
- "save this artifact"
- "store this artifact"
- "add to artifacts"
- "save to artifacts repo"

## Behavior

1. **Identify the artifact**: Determine what file/content to save (either just created, or user specifies a path)
2. **Collect metadata**: Gather required and optional fields
3. **Save the artifact**: Copy/write to `/Users/meain/dev/src/ai-artifacts/artifacts/`
4. **Update catalog**: Add entry to `/Users/meain/dev/src/ai-artifacts/artifacts.json`
5. **Confirm**: Show the artifact name and key metadata fields

## Required Metadata

- **type**: File extension or type (html, markdown, bash, python, json, yaml, etc.)
- **created**: ISO 8601 timestamp (use current time)
- **agent**: Agent harness (fin, codex, claude-code, etc.) - infer from context

## Optional Metadata (ask if not obvious)

- **description**: Brief description of what this does
- **tags**: Array of tags for categorization
- **repository**: Source repo context if relevant
- **model**: LLM model used (claude-3.5-sonnet, etc.)
- **purpose**: Why this was created - the problem it solves
- **dependencies**: External tools/libraries required
- **session_id**: Agent session ID if available
- **related**: Related artifact filenames
- **url**: Public URL if deployed
- **archived**: false (default)

## Filename Strategy

- Use descriptive kebab-case names
- Include version suffix if this is an iteration (e.g., `dashboard-v2.html`)
- Avoid generic names like `output.html` or `script.sh`
- If user doesn't specify, suggest a name based on content/purpose

## Implementation Notes

- Read existing `artifacts.json`, parse it, add the new entry, write it back with proper formatting (2-space indent, sorted keys)
- Use `jq` for JSON manipulation: `jq --indent 2 '.artifacts["<name>"] = <metadata>' artifacts.json`
- Preserve existing entries and formatting
- For the `created` timestamp, use ISO 8601 format: `date -u +"%Y-%m-%dT%H:%M:%SZ"`
- If artifact with same name exists, ask user whether to overwrite or create versioned copy

## Example

User: "save this dashboard to artifacts"

```bash
# Copy file
cp dashboard.html /Users/meain/dev/src/ai-artifacts/artifacts/adx-metrics-dashboard.html

# Update catalog
cd /Users/meain/dev/src/ai-artifacts
jq --indent 2 '.artifacts["adx-metrics-dashboard.html"] = {
  "type": "html",
  "description": "Interactive dashboard for ADX query metrics",
  "tags": ["visualization", "monitoring", "adx"],
  "repository": "veeam/control-plane-backend",
  "agent": "fin",
  "model": "claude-3.5-sonnet",
  "created": "2026-06-26T13:50:00Z",
  "purpose": "Debug ADX query performance issues",
  "session_id": "23051bf6-89b7-44cb-b19f-cd9019a967bc"
}' artifacts.json > artifacts.json.tmp && mv artifacts.json.tmp artifacts.json
```

Response: "Saved as `adx-metrics-dashboard.html` — interactive dashboard for ADX query metrics"
