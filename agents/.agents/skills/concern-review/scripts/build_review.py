#!/usr/bin/env python3
"""Merge template.html + a diff file + a concerns JSON into a final review HTML.

Usage: build_review.py --diff <diff-file> --concerns <concerns.json> --out <output.html>

The concerns JSON never contains diff/code content — only file paths, summaries,
notes, and (optionally) which hunk indices apply. This script pulls the real
diff text from --diff so the rendered code is always exactly what's in the
actual diff, never something the model had to reproduce.

concerns.json shape:
{
  "pr": {"label": "org/repo #123", "subtitle": "TICKET-1 — short title"},
  "pathPrefix": "optional/common/prefix/to/strip/in/the/sidebar/",
  "categories": [
    {
      "key": "short-slug",
      "title": "Human title",
      "desc": "One or two sentence description of this concern.",
      "notes": ["Non-obvious/confusing point.", "..."],
      "files": [
        {"path": "path/as/it/appears/in/the/diff.go", "summary": "...", "hunks": "all"},
        {"path": "shared/file.go", "summary": "...", "hunks": [0, 1, 4]}
      ]
    }
  ]
}
"""
import argparse
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from diffsplit import split_into_files, slice_file_diff


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--diff", required=True)
    ap.add_argument("--concerns", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--template", default=str(Path(__file__).parent.parent / "template.html"))
    args = ap.parse_args()

    diff_text = Path(args.diff).read_text()
    files = split_into_files(diff_text)

    concerns = json.loads(Path(args.concerns).read_text())

    total_files = 0
    for cat in concerns.get("categories", []):
        for f in cat["files"]:
            path = f["path"]
            if path not in files:
                available = "\n  ".join(files.keys())
                raise SystemExit(
                    f"path not found in diff: {path!r} (category {cat.get('key')!r})\n"
                    f"available paths in diff:\n  {available}"
                )
            hunks = f.get("hunks", "all")
            try:
                f["diff"] = slice_file_diff(files[path], None if hunks == "all" else hunks)
            except IndexError as e:
                raise SystemExit(f"{e} (file {path!r}, category {cat.get('key')!r})")
            f.pop("hunks", None)
            total_files += 1

    template = Path(args.template).read_text()
    if "__DATA_JSON__" not in template:
        raise SystemExit(f"template missing __DATA_JSON__ placeholder: {args.template}")

    out_html = template.replace("__DATA_JSON__", json.dumps(concerns))
    Path(args.out).write_text(out_html)

    print(f"wrote {args.out} ({len(concerns.get('categories', []))} categories, {total_files} file entries)")


if __name__ == "__main__":
    main()
