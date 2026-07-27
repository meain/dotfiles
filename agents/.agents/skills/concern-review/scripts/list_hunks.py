#!/usr/bin/env python3
"""Print the authoritative hunk-index map for a diff file.

Usage: list_hunks.py <diff-file> [path-substring-filter]

Read this output before writing a concerns JSON — the hunk indices printed
here are exactly what build_review.py expects in each file entry's "hunks"
list. Do not count "@@" lines by hand; use these indices.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from diffsplit import split_into_files, hunk_stats, is_new_file, is_deleted_file


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    diff_path = sys.argv[1]
    name_filter = sys.argv[2] if len(sys.argv) > 2 else None

    diff_text = Path(diff_path).read_text()
    files = split_into_files(diff_text)

    if not files:
        print("No files found in diff (is it a valid unified/git diff?)")
        sys.exit(1)

    for path, entry in files.items():
        if name_filter and name_filter not in path:
            continue

        total_add = sum(hunk_stats(h)[0] for h in entry["hunks"])
        total_del = sum(hunk_stats(h)[1] for h in entry["hunks"])
        flag = " (new file)" if is_new_file(entry["header"]) else \
               " (deleted)" if is_deleted_file(entry["header"]) else ""

        print(f"{path}{flag}  [{len(entry['hunks'])} hunks, +{total_add}/-{total_del}]")
        for i, h in enumerate(entry["hunks"]):
            add, del_ = hunk_stats(h)
            header_line = h[0]
            print(f"  {i:>2}  +{add}/-{del_}  {header_line}")
        print()


if __name__ == "__main__":
    main()
