#!/usr/bin/env bash
set -euo pipefail

# Find this week's "Updates for week of ..." Confluence status page and
# print its title (line 1) and plain URL (line 2) -- nothing else.
#
# Requires dangerouslyDisableSandbox: true (network call to Confluence).
#
# The page title's date is NOT the current week's Monday -- it's the Sunday
# immediately BEFORE that Monday. Confirmed against several real pages
# (each exactly 7 days apart): the week starting Monday 2026-08-10 is titled
# "Updates for week of 2026-08-09". Searching for "current Monday's date"
# (the old, wrong assumption) always returns nothing.
#
# Falls back to the most recently created page matching the title pattern
# if the exact date isn't found (e.g. the page hasn't been created yet).

SPACE='~712020cccc16439d5041339f95122d8d977f63'

dow=$(date +%u)
this_monday=$(date -d "-$((dow - 1)) days" "+%Y-%m-%d")
doc_date=$(date -d "$this_monday -1 days" "+%Y-%m-%d")

result=$(confluence search "title = \"Updates for week of $doc_date\" AND space = \"$SPACE\"" --cql 2>/dev/null || true)
line=$(printf '%s\n' "$result" | grep -m1 -E "^[0-9]+\. Updates for week of")

if [ -z "$line" ]; then
    result=$(confluence search "title ~ \"Updates for week of\" AND space = \"$SPACE\" order by created desc" --cql --limit 1 2>/dev/null || true)
    line=$(printf '%s\n' "$result" | grep -m1 -E "^[0-9]+\. Updates for week of")
fi

if [ -z "$line" ]; then
    echo "No weekly status page found (looked for date $doc_date)" >&2
    exit 1
fi

title=$(sed -E 's/^[0-9]+\. (.*) \(ID: [0-9]+\)$/\1/' <<<"$line")
page_id=$(sed -E 's/^[0-9]+\. .* \(ID: ([0-9]+)\)$/\1/' <<<"$line")
encoded_title=$(printf '%s' "$title" | tr ' ' '+')

echo "$title"
echo "https://veeam-vdc.atlassian.net/wiki/spaces/$SPACE/pages/$page_id/$encoded_title"
