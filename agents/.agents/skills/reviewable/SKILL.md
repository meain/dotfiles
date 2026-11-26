---
name: reviewable
description: Take a finished implementation to a reviewable PR — commit, push, open a draft PR, wait for CI, mark it ready, wait up to 10 minutes for automated reviewer agents, then add human reviewers if needed and report back. Triggers: /reviewable, "make this reviewable", "get this ready for review", "create the PR and wait for CI".
---

# Reviewable

Run at the end of an implementation. Goal: a ready-for-review PR with green CI and agent feedback surfaced.

## Steps

- Preconditions: working copy has the change; find the Jira ticket (from the conversation, branch, or `jira issue list -a$(jira me) -s~Done --plain`). In `control-plane-backend`, run `/pseudolinter` on every commit first (go, then go-unit-test), fixing in the owning commits.
- Commit with `/create-commit` rules (jj, short subject, ticket key in brackets, no Co-Authored-By). Bookmark name starts with `meain/`. Push the bookmark (`dangerouslyDisableSandbox: true`).
- Create the PR as a draft: `GIT_DIR=$(jj git root 2>/dev/null || echo .git) gh pr create --draft --body-file /tmp/pr.md`. Follow `.github/pull_request_template.md` and the PR-description rules in the global instructions (short, why first, Jira link in the body, no stack notes). Then `open <pr-url>`.
- No reviewers on the draft.
- Wait for CI: poll `gh pr checks <n>` (sleep 30-60s between polls, use a background wait rather than blocking turns). Ignore `skipping`. If a check fails, read its log (`gh run view <id> --log-failed`), fix in the owning commit, push, and wait again. Stop and report if the same failure repeats twice or looks unrelated or flaky.
- When all required checks pass, mark ready: `gh pr ready <n>`.
- Wait for agent reviews, max 10 minutes from marking ready. Poll every ~60s: `gh pr view <n> --json reviews,comments` and `gh api repos/{owner}/{repo}/pulls/<n>/comments`. Agent reviewers are bots (Copilot, claude, etc.; author type Bot or login ending `[bot]`). Stop early once bot feedback has arrived and CI is still green, or when 10 minutes pass.
- Handle agent comments: valid ones, fix in the owning commit and push (then recheck CI); invalid ones, reply in one or two sentences. Ask me before replying on GitHub if unsure. Do not resolve threads.
- Reviewers: check `.github/CODEOWNERS` for changed paths. For `/Users/meain/dev/veeam` repos the set is rs-malik, ryanfkeepers, rami-veeam, tford-veeam. If all are auto-assigned, do nothing; otherwise ask which missing ones to add. Never before the PR is ready.
- Teams card: show an inline `widget_show` card for posting the review request. The channel exists for review requests, so no "PR for review:" prefix, no CI line, no greeting or sign-off. Proper capitalisation. Shape (markdown; a blank line between headline and bullets in the `copy` text; one line per bullet; the PR title without its ticket key):

  ```
  [<repo>#<n>](<pr-url>) – <short title>
  - Why: <the problem or finding, with [VMCP-123](https://veeam-vdc.atlassian.net/browse/VMCP-123)>
  - Fix: <what the change does, high level>
  ```

  Two buttons, labels without numbers: "Copy message" with `copy` set to that same markdown and `markdown: true` (Dashy then also puts rendered HTML on the clipboard, so Teams pastes it formatted), and "Open channel" with `open` set to the review channel's `msteams://` link. For the earn repo that is the EARN backend CodeReviews channel: `msteams://teams.microsoft.com/l/channel/19%3A5729597e716c441b8d30bcc3e4240b86%40thread.tacv2/project.EARN.backend.CodeReviews?groupId=7e735ea1-c2fb-430e-a0d3-395e63680899&tenantId=ba07baab-431b-49ed-add7-cbc3542f5140`; for other repos ask which channel. A button does one thing, so copy and open stay separate. Keep the display text and the `copy` text identical.
- Final report (short): PR URL, CI state, what agents said and what was done about it, reviewers state, anything needing me.

## Notes

- Never squash or merge. Never push to main.
- Use `set_status` for live progress; a widget is not needed.
- If a step is skipped or times out, say so plainly.
