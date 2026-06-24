#!/usr/bin/env bash
set -e

# Fetch pending GitHub PR reviews for the user
# Returns JSON array of PRs where user is a direct reviewer
# The GraphQL query already filters for review-requested:meain and review:required

GH_TOKEN=$(security find-generic-password -s "gh:github.com" -w 2>&1)
if [[ "$GH_TOKEN" == go-keyring-base64:* ]]; then
    GH_TOKEN=$(echo "${GH_TOKEN#go-keyring-base64:}" | base64 -d)
fi

response=$(curl -s -H "Authorization: bearer $GH_TOKEN" -H "Content-Type: application/json" \
    -d '{"query":"{ search(query: \"is:pr is:open review-requested:meain review:required org:Veeam-VDC\", type: ISSUE, first: 30) { nodes { ... on PullRequest { number title url isDraft repository { nameWithOwner } author { login } } } } }"}' \
    https://api.github.com/graphql)

# Filter out drafts and format output
echo "$response" | jq -c '.data.search.nodes | map(
    select(.isDraft == false) | {
        number,
        title,
        url,
        repo: .repository.nameWithOwner,
        author: .author.login
    }
)'
