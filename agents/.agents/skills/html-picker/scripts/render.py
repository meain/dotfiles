#!/usr/bin/env python3
"""Render an html-picker page from an options JSON file.

usage: render.py options.json output.html
"""
import json
import sys
from pathlib import Path

TEMPLATE = Path(__file__).resolve().parent.parent / "references" / "picker.html"
PLACEHOLDER = "/*OPTIONS_JSON*/"


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    src, dst = Path(sys.argv[1]), Path(sys.argv[2])
    cfg = json.loads(src.read_text())
    opts = cfg.get("options")
    if not opts:
        sys.exit("options.json needs a non-empty 'options' list")
    ids = [str(o.get("id")) for o in opts]
    if len(set(ids)) != len(ids) or "None" in ids:
        sys.exit("every option needs a unique 'id'")
    # '<' escaped so a mockup containing '</script>' cannot break out of the data block
    data = json.dumps(cfg, ensure_ascii=False).replace("<", "\\u003c")
    dst.parent.mkdir(parents=True, exist_ok=True)
    dst.write_text(TEMPLATE.read_text().replace(PLACEHOLDER, data))
    print(dst)


if __name__ == "__main__":
    main()
