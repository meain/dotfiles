---
name: save-research
description: >
  Summarize and save research findings from the current conversation into a well-structured
  markdown document. Triggers: /save-research, "save this research", "write up findings",
  "document what we found", "save these notes"
---

# /save-research -- Save Research Findings to Markdown

Collect, organize, and write all research findings from the current conversation into a
structured markdown document for future reference.

## When to Use

Use this skill when:
- You have been researching a topic and want to preserve the findings
- The conversation contains useful information worth saving as a reference
- You want to build up a knowledge base over time

## Instructions

### Phase 1: Gather Findings

1. Review the **entire conversation history** to identify all research findings, insights,
   code examples, links, decisions, and key takeaways.
2. Group related findings into logical sections.
3. Note any open questions or areas that need further research.

### Phase 2: Determine Output Location

Ask the user or infer from context:

- **Project-specific research** (related to the current codebase or project):
  Save to `.mdocs/<category>/` in the project root (see Phase 2b for categories).
  Create the directory if it does not exist.
- **Generic / cross-project research** (general knowledge, tools, techniques):
  Save to `/Users/meain/dev/mdocs/<category>/` using the same category system as project-specific research (see Phase 2b). Create the directory if it does not exist.

If it is unclear whether the research is project-specific or generic, **ask the user**.

### Phase 2b: Determine the Category

Project-specific documents must be placed in a category subdirectory under `.mdocs/`.
Pick the best-fit category based on the document's purpose:

| Category        | When to use                                                                 |
|-----------------|-----------------------------------------------------------------------------|
| `plan`          | Step-by-step plans for executing a task across repos/systems (has ordered steps, timelines, or rollout tiers) |
| `guide`         | Proactive how-to instructions — "I want to do X" (testing guides, operational procedures, style guides) |
| `runbook`       | Reactive incident response — "something broke, now what?" (triage steps, remediation, rollback procedures) |
| `design`        | Forward-looking proposals and architecture decisions (options analysis, solution designs, feature specs) |
| `reference`     | Explanations of how something already works (system behavior, patterns, tool docs, configuration references) |
| `investigation` | Analysis of bugs, performance issues, code quality, or root causes (debugging findings, violation reports, post-mortems) |
| `task`          | Tracking documents for specific tickets or work items (task checklists, ticket context, status tracking) |

If a document spans multiple categories, pick the **primary purpose**:
- A design doc that includes investigation context → `design`
- A guide that was born from an investigation → `guide`
- A plan driven by a specific ticket → `plan` (not `task`)

If genuinely unsure, **ask the user**.

### Phase 3: Determine the Filename

1. Derive a descriptive, kebab-case filename from the research topic.
   - Example: `.mdocs/reference/go-generics.md`, `.mdocs/design/react-server-components-patterns.md`
2. For project-specific research, the full path is: `.mdocs/<category>/<filename>.md`
3. If a file with the same name already exists, **ask the user** whether to:
   - Append to the existing file
   - Overwrite it
   - Use a different name

### Phase 4: Write the Document

Use the following structure for the markdown file. The title, date, tags, and status
should be in YAML front matter metadata rather than in the document body. This keeps
the `#` (h1) heading level available for use within the document content itself.

```markdown
---
title: <Title of Research Topic>
date: YYYY-MM-DD
tags: [tag1, tag2, tag3]
status: complete | in-progress | needs-review
claude-session-id: ${CLAUDE_SESSION_ID}  # or pi-session-id: <ask user> when under pi agent
---

## Summary

A 2-4 sentence high-level summary of what was researched and the key conclusions.

## Findings

### <Subtopic 1>

- Key points as bullet lists
- Include code snippets in fenced blocks where relevant

### <Subtopic 2>

...

## Key Takeaways

- Numbered or bulleted list of the most important conclusions

## Code Examples

Include any significant code snippets or commands discovered during research.
Only include this section if there are meaningful code examples.

## References

- Links to documentation, articles, or other resources consulted
- Only include this section if there are references to list

## Open Questions

- Anything unresolved or worth investigating further
- Only include this section if there are open questions
```

**Writing guidelines:**
- Always include a session ID in the YAML front matter so the source conversation can be traced back later. If `${CLAUDE_SESSION_ID}` is available (Claude Code), use `claude-session-id:` as the key. Otherwise, when running under pi agent (check `PI_CODING_AGENT=true` env var), ask the user to provide the session ID and use `pi-session-id:` as the key
- Be concise but thorough -- these are reference documents, not prose
- Prefer bullet points over paragraphs
- Include concrete code examples where applicable
- Use proper markdown formatting (headings, code fences, links)
- Add tags that would help with future search and discovery
- Use today's date for the Date field

### Phase 5: Verify

1. Read back the written file to confirm it was saved correctly.
2. Report the full file path to the user.
3. Provide a brief summary of what was captured (number of sections, key topics).

## Guidelines

- NEVER fabricate information that was not discussed in the conversation
- NEVER omit significant findings from the conversation
- If the conversation covered multiple unrelated topics, ask the user if they want separate files or one combined document
- Preserve technical accuracy -- do not paraphrase code or commands incorrectly
- Keep the document self-contained so it is useful without the original conversation context
- Omit sections from the template that have no content (e.g., skip "Open Questions" if there are none)
