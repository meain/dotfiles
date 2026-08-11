#!/usr/bin/env bash
set -euo pipefail

# Print backlog entries for a past week (Monday-Sunday) from the "## Before"
# archive section of Backlog/Backlog.md -- this is where COMPLETED work
# actually lives, not the "Today"/"This Cycle" sections at the top of the
# file (those are pending/future, not done).
#
# Every day of the week is printed with its weekday name and date --
# including Saturday/Sunday even when empty -- so the caller can see the
# full week shape and judge whether weekend work happened.
#
# Usage: last-week-backlog.sh [weeks_ago]
#   weeks_ago: 1 = the most recently completed Mon-Sun week (default) --
#              this is what "last week" means for the weekly report, since
#              reports are usually generated a day or more into the new week
#              0 = this week so far (Monday through today)
#
# Includes ALL items verbatim, personal or work, checked or not. This script
# does not filter anything out -- when it's ambiguous whether something is
# personal or work, that judgment call belongs to whoever reads this output,
# not the script.

BACKLOG="/Users/meain/.local/share/sbdb/Backlog/Backlog.md"
weeks_ago="${1:-1}"

dow=$(date +%u) # 1=Mon .. 7=Sun
this_monday=$(date -d "-$((dow - 1)) days" "+%Y-%m-%d")
target_monday=$(date -d "$this_monday -$((weeks_ago * 7)) days" "+%Y-%m-%d")

if [ "$weeks_ago" -eq 0 ]; then
    days=$dow
else
    days=7
fi

echo "# Backlog entries: week of $target_monday"
echo

for ((i = 0; i < days; i++)); do
    day_info=$(date -d "$target_monday +$i days" "+%Y-%m-%d %A")
    day_date="${day_info%% *}"
    day_name="${day_info#* }"

    items=$(awk -v date="$day_date" '
        /^## Before/ { in_before=1; next }
        in_before && $0 ~ ("^### " date) { in_day=1; next }
        in_before && in_day && /^###/ { in_day=0 }
        in_day && /^- / { print }
    ' "$BACKLOG")

    echo "### $day_date ($day_name)"
    if [ -z "$items" ]; then
        echo "(no entries)"
    else
        echo "$items"
    fi
    echo
done
