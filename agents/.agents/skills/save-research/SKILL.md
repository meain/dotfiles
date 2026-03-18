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
  Save to `.mdocs/` in the project root. Create the directory if it does not exist.
- **Generic / cross-project research** (general knowledge, tools, techniques):
  Save to `/Users/meain/dev/docs/research/`. Create subdirectories as needed.

If it is unclear whether the research is project-specific or generic, **ask the user**.

### Phase 3: Determine the Filename

1. Derive a descriptive, kebab-case filename from the research topic.
   - Example: `understanding-go-generics.md`, `react-server-components-patterns.md`
2. If a file with the same name already exists, **ask the user** whether to:
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
