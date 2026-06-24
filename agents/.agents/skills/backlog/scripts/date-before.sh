#!/usr/bin/env bash
set -e

# Calculate the "Before" date for backlog archiving
# Handles weekend skipping: if yesterday was Sat/Sun, go back to Friday
# Returns: YYYY-MM-DD (DayOfWeek)

# Detect date command flavor
if date --version >/dev/null 2>&1; then
    # GNU date
    yesterday_day=$(date -d "yesterday" "+%A")
    case "$yesterday_day" in
        Saturday) date -d "3 days ago" "+%Y-%m-%d (%A)" ;;
        Sunday) date -d "2 days ago" "+%Y-%m-%d (%A)" ;;
        *) date -d "yesterday" "+%Y-%m-%d (%A)" ;;
    esac
else
    # BSD date
    yesterday_day=$(date -v-1d "+%A")
    case "$yesterday_day" in
        Saturday) date -v-3d "+%Y-%m-%d (%A)" ;;
        Sunday) date -v-2d "+%Y-%m-%d (%A)" ;;
        *) date -v-1d "+%Y-%m-%d (%A)" ;;
    esac
fi
