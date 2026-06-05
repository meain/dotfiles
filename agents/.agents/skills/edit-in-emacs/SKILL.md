---
name: edit-in-emacs
description: "Open a draft in emacsclient (blocking) so the user can review or edit it before further action. Triggers: /edit-in-emacs, 'open in emacs', 'open in emacsclient', 'let me edit', 'edit before submitting', 'open it in emacs and let me edit'"
user_invocable: true
---

# Edit in Emacs

Use this skill when the user wants to manually review or edit a draft (commit message, PR body, Jira description, prompt, etc.) before it is submitted or used downstream.

## Workflow

1. Write the draft to a temp file under `/tmp/`.
   - Pick a descriptive filename, e.g. `/tmp/jira-<topic>.md`, `/tmp/pr-body.md`, `/tmp/commit-msg.txt`.
   - If the draft has multiple structured parts (e.g. summary + body), separate them with a clear marker like a line `SUMMARY: ...` followed by `---`, and tell the user about the convention.

2. Open the file with **blocking** `emacsclient`:
   ```bash
   emacsclient /tmp/<file>
   ```
   - Do **not** use `-n` — blocking is the whole point. The Bash call returns only after the user does `C-x #` (`server-edit`) in Emacs.
   - Pass `timeout: 600000` (10 min) on the Bash tool call so it does not abort while the user is editing.
   - Always use `dangerouslyDisableSandbox: true` — `emacsclient` connects to a Unix socket and the sandbox blocks it.

3. After the Bash call returns, the edited file is on disk.
   - The harness usually emits a system-reminder showing the diff of any changes — use that if it is present.
   - If no diff was shown (e.g. user did not modify the file, or the change is large), Read the file to get the current contents.

4. Use the edited contents in the downstream action (jira create, gh pr create, commit, etc.). Do not re-prompt the user for the same content; trust their edits.

## Notes

- The user must have `emacs --daemon` (or a running GUI Emacs server) for `emacsclient` to attach. If `emacsclient` reports `can't find socket`, fall back to telling the user and offer `$EDITOR` instead.
- Keep the temp file under `/tmp/`, never in the repo root.
- If the user explicitly asks for a different editor, honour that and skip this skill.
- The skill is for *interactive editing of a draft*, not for opening existing source files — for those, just `Read` them.
