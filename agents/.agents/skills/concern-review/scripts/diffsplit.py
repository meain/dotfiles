"""Shared unified-diff parsing for concern-review's list_hunks.py and build_review.py.

Both scripts must derive identical hunk indices for the same diff file, so the
splitting logic lives here once rather than being duplicated.
"""
import re
from collections import OrderedDict


def split_into_files(diff_text):
    """Split a multi-file unified diff into an OrderedDict keyed by the b/ path.

    Each value is {"header": [lines...], "hunks": [[lines incl. the @@ line], ...]}.
    Hunk indices are 0-based, in the order they appear in the file's diff.
    """
    file_chunks = re.split(r"(?=^diff --git )", diff_text, flags=re.M)
    files = OrderedDict()

    for chunk in file_chunks:
        if not chunk.strip():
            continue
        m = re.match(r"diff --git a/(\S+) b/(\S+)", chunk)
        if not m:
            continue
        path = m.group(2)

        lines = chunk.rstrip("\n").split("\n")
        i = 0
        header = []
        while i < len(lines) and not lines[i].startswith("@@"):
            header.append(lines[i])
            i += 1

        hunks = []
        cur = []
        for line in lines[i:]:
            if line.startswith("@@"):
                if cur:
                    hunks.append(cur)
                cur = [line]
            else:
                cur.append(line)
        if cur:
            hunks.append(cur)

        files[path] = {"header": header, "hunks": hunks}

    return files


def hunk_stats(hunk_lines):
    add = sum(1 for l in hunk_lines[1:] if l.startswith("+"))
    del_ = sum(1 for l in hunk_lines[1:] if l.startswith("-"))
    return add, del_


def is_new_file(header_lines):
    return any(l.startswith("new file mode") for l in header_lines)


def is_deleted_file(header_lines):
    return any(l.startswith("deleted file mode") for l in header_lines)


def slice_file_diff(file_entry, hunk_indices=None):
    """Reconstruct a diff snippet for one file: header + selected hunks.

    hunk_indices=None (or the string "all") returns every hunk, in original order.
    Otherwise hunk_indices is an iterable of 0-based ints, deduped and sorted
    ascending so the result always reflects the file's real line order
    regardless of the order the caller listed them in.
    """
    header = file_entry["header"]
    hunks = file_entry["hunks"]

    if hunk_indices is None or hunk_indices == "all":
        selected = hunks
    else:
        idxs = sorted(set(int(i) for i in hunk_indices))
        bad = [i for i in idxs if i < 0 or i >= len(hunks)]
        if bad:
            raise IndexError(f"hunk index out of range (file has {len(hunks)} hunks): {bad}")
        selected = [hunks[i] for i in idxs]

    body = "\n".join(header)
    for h in selected:
        body += "\n" + "\n".join(h)
    return body
