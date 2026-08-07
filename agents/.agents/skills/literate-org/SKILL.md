---
name: literate-org
description: "Create a literate org-mode document with executable bash source blocks (org-babel) so the user can re-run commands/queries themselves in Emacs, then open it via emacsclient. Triggers: /literate-org, 'literate org document', 'org file with runnable blocks', 'org doc I can execute', 'make this verifiable in org', 'create an org document with executable commands'"
user_invocable: true
---

# Literate Org Document

Use this skill when the user wants findings, verification steps, or a runbook
packaged as an org-mode file where each command is an executable org-babel
block they can run with `C-c C-c` in Emacs. Typical case: after an
investigation, turn the evidence trail into a self-verifiable document.

## Workflow

1. Collect the commands that support each claim — shell commands, CLI
   queries (az, kubectl, ES|QL via a wrapper script, etc.). Only include
   commands that were actually run and worked during the session; this is a
   reproduction document, not a brainstorm.

2. Write the org file to `/tmp/<topic>.org` (never the repo root) with this
   shape:

   ```org
   #+title: <One-line claim or task>
   #+date: <YYYY-MM-DD>
   #+property: header-args:bash :results output :exports both

   <Intro: the claim under test / purpose, prerequisites (sessions,
   VPN, credentials), and "run each block with C-c C-c".>

   * <Theme section>

   <One or two sentences: what this block shows and the expected outcome.>

   #+begin_src bash
   <command>
   #+end_src
   ```

3. Open it **non-blocking** in the user's running Emacs:
   ```bash
   emacsclient --no-wait /tmp/<topic>.org
   ```
   - Use `dangerouslyDisableSandbox: true` — emacsclient connects to a Unix
     socket and the sandbox blocks it.
   - Non-blocking is correct here (unlike `edit-in-emacs`): the user runs the
     blocks themselves; nothing is returned to the agent.

## Authoring rules

- **Prose before every block.** State what the block demonstrates and what
  output to expect ("Expect: only two caller IPs; no third IP"). The reader
  should know whether a result confirms or refutes the claim without asking.
- **Plain headings, no numbering** — sections by theme, unordered lists only.
- **`:dir` for cwd-dependent commands.** If a script must run from a repo,
  set it per block: `#+begin_src bash :dir ~/dev/<path>`. Prefer `~/...` over
  `/Users/<name>/...`.
- **Self-contained blocks.** Each block must run on its own — no reliance on
  shared shell state (env vars, `cd`) between blocks.
- **Chain values between blocks with org, don't make the user paste.** When a
  downstream block needs a value a previous block produced (an operation id, a
  pod name, a resource id), wire it through org-babel instead of hardcoding:
  - Name the producer block `#+name: <slug>` and make it print *only* the bare
    value (e.g. `... | jq -r '.Tables[0].Rows[][0]'`).
  - Consume it with a `:var` header: `#+begin_src bash :var X=<slug>`. Sanitize
    on the way in, since the value may carry a trailing newline:
    `ID=$(printf '%s' "$X" | tr -d '[:space:]')`.
  - **Caveat — resolving a `:var` that points at a src block re-executes that
    block.** So only chain this way when the producer is *idempotent and cheap*
    (a read, or an idempotent write like an `.set-or-append` no-op). State the
    re-run in prose ("resolving this var re-runs <slug>; it's idempotent").
    If the producer is expensive or has side effects, fall back to hardcoding
    the observed value and saying in prose how to refresh it.
- **Org escaping inside blocks:** any line starting with `*` or `#+` inside a
  src block must be prefixed with a comma (`,*`, `,#+`). Inline verbatim in
  prose uses `=text=` or `~text~`, not backticks.
- **Quoting:** prefer single-quoted command arguments so `$`, backticks, and
  `"` inside queries survive; escape literal backticks in az `--query` JMESPath
  as `\`[]\`` only when the block itself uses double quotes.
- **Multi-line queries** (ES|QL, KQL, SQL) go inside the block as a
  single-quoted argument to their runner CLI, formatted across lines for
  readability.
- **Caveats section.** End with what could *not* be verified this way
  (permissions, expired sessions, needs kubectl/VPN) so absence of output
  isn't misread, plus a short conclusion/fix section if this documents an
  investigation.

## Notes

- If `emacsclient` reports `can't find socket`, tell the user and give the
  file path instead; do not fall back to another editor unasked.
- Do not reopen the file after later edits — a revert/refresh in Emacs picks
  them up; just tell the user the file changed.
- This skill produces a document for the *user* to execute; do not run the
  blocks yourself after writing the file unless asked to sanity-check one.
