---
name: double-check
description: >
  Verify a claim, assumption, or answer before acting on it or reporting it
  to the user. Use when the user says /double-check, "double check that",
  "verify that", "are you sure", "check again", or when you've just made a
  factual claim that could be verified (e.g. "no reviewers added", "the file
  doesn't exist", "the test passed") and the user questions it.
---

Re-examine the claim or question from scratch. Don't rely on what was said
in the previous response — go back to the source.

- If the claim is about state (files, PRs, reviewers, CI, git history): re-run
  the relevant command or read the relevant file directly.
- If the claim is about code behaviour: read the code again and trace through it.
- If the claim is about a fact: look it up rather than reasoning from memory.

Report what you actually found, and correct the previous response if it was wrong.
