---
name: check-pr-reviews
description: >
  Check for pending human review comments on my open PRs across configured repos.
  Triggers: /check-pr-reviews, "check my PRs", "any pending reviews", "PR review comments",
  "check review comments"
---

# /check-pr-reviews — Check Pending PR Review Comments

Check all open PRs authored by the current user across configured repos and report any pending review comments (from both humans and bots) that need attention.

## Repo List

Read the list of repos from `~/.config/datafiles/prs-repos` (one `owner/repo` per line).

## Steps

### 1. Get Current User

```bash
gh api user --jq '.login'
```

### 2. List Open PRs

For each repo in the repos file, run in parallel:

```bash
gh pr list --repo OWNER/REPO --author USERNAME --state open --json number,title,url
```

### 3. Check Review Comments

For each PR found, fetch both reviews and inline comments (include bot comments, exclude only self):

```bash
# Fetch inline review comments (exclude self)
gh api repos/OWNER/REPO/pulls/NUMBER/comments \
  --jq '.[] | select(.user.login != USERNAME) | "\(.user.login) [line \(.line // "general") in \(.path)]: \(.body | split("\r\n") | join(" ") | split("\n") | join(" ") | .[0:300])"'

# Fetch review-level comments with CHANGES_REQUESTED state (exclude self)
gh api repos/OWNER/REPO/pulls/NUMBER/reviews \
  --jq '.[] | select(.user.login != USERNAME and .state == "CHANGES_REQUESTED") | "\(.user.login) (\(.state)): \(.body | split("\r\n") | join(" ") | split("\n") | join(" ") | .[0:300])"'
```

### 4. Also Check Own Unanswered Questions

Look for comments by the current user that tag other people (contain `@`) — these may be waiting for a response. Note these as "waiting on others".

### 5. Report

Present a concise summary organized by repo:
- Skip repos/PRs with no pending comments
- For each PR with comments, show: PR title, URL, and all review comments
- Clearly distinguish between:
  - **Action needed**: Comments from reviewers you haven't responded to
  - **Waiting on others**: Questions you asked that haven't been answered
- At the end, give a one-line summary (e.g., "2 PRs need attention, 1 waiting on others")
