#!/usr/bin/env bash
set -euo pipefail

# Fetch Confluence pages the user created/edited within a date range.
# Usage: confluence-updates.sh <start-date> [end-date]   (YYYY-MM-DD, inclusive)
#   end-date defaults to today.
#
# Requires dangerouslyDisableSandbox: true (network call to Confluence).
# Outputs the raw confluence CLI results (not JSON).

start="$1"
end="${2:-$(date +%Y-%m-%d)}"

confluence search "contributor = currentUser() AND lastmodified >= '$start' AND lastmodified <= '$end' order by lastmodified desc" \
    --cql --limit 10 2>/dev/null || echo "No results"
