#!/usr/bin/env bash
set -e

# Fetch user's Confluence pages created or updated this week
# Outputs the raw confluence CLI results (not JSON)

week_start=$(date -d "last monday" "+%Y-%m-%d" 2>/dev/null || date -v-mon "+%Y-%m-%d")

confluence search "contributor = currentUser() AND lastmodified >= '$week_start' order by lastmodified desc" --cql --limit 10 2>/dev/null || echo "No results"
