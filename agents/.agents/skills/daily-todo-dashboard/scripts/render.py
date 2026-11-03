#!/usr/bin/env python3
"""Render the daily todo dashboard from a data JSON file.

usage: render.py [--board | --classic] data.json output.html

Default is the list design (List/Table/Cards views, filter by type, ignore).
--board renders the column board, --classic the old two-pane page with the scheduler.
"""
import json
import sys
from pathlib import Path

REFS = Path(__file__).resolve().parent.parent / "references"
TEMPLATES = {"--board": REFS / "dashboard-d.html", "--classic": REFS / "dashboard.html"}
DEFAULT = REFS / "dashboard-alt.html"
PLACEHOLDER = "/*DATA_JSON*/null"
SOURCES = {"Backlog", "Merge", "Chase review", "PR review", "Teams", "Blocked"}
REQUIRED = ("id", "src", "proj", "title", "urg", "imp", "age", "tags", "ctx", "links")


def fail(msg):
    sys.exit(f"render.py: {msg}")


def main():
    args = [a for a in sys.argv[1:] if a not in TEMPLATES]
    if len(args) != 2:
        sys.exit(__doc__)
    template = next((p for f, p in TEMPLATES.items() if f in sys.argv), DEFAULT)
    src, dst = Path(args[0]), Path(args[1])
    cfg = json.loads(src.read_text())
    for k in ("day", "title", "subtitle", "events", "todos"):
        if k not in cfg:
            fail(f"data needs '{k}'")
    todos = cfg["todos"]
    if not todos:
        fail("'todos' is empty")
    ids = [t.get("id") for t in todos]
    if len(set(ids)) != len(ids) or None in ids:
        fail("every todo needs a unique 'id'")
    for t in todos:
        missing = [k for k in REQUIRED if k not in t]
        if missing:
            fail(f"todo {t.get('id')} is missing {missing}")
        if t["src"] not in SOURCES:
            fail(f"todo {t['id']}: src must be one of {sorted(SOURCES)}")
        if not (1 <= t["urg"] <= 5 and 1 <= t["imp"] <= 5):
            fail(f"todo {t['id']}: urg/imp must be 1-5")
        t.setdefault("eff", 2)  # only the classic template uses it
        for l in t["links"]:
            if not (l.get("t") and l.get("u")):
                fail(f"todo {t['id']}: every link needs 't' and 'u'")
        t.setdefault("blocks", False)
        t.setdefault("held", False)
    # '<' escaped so a title containing '</script>' cannot break out of the data block
    data = json.dumps(cfg, ensure_ascii=False).replace("<", "\\u003c")
    html = template.read_text()
    if PLACEHOLDER not in html:
        fail("template placeholder missing")
    dst.parent.mkdir(parents=True, exist_ok=True)
    dst.write_text(html.replace(PLACEHOLDER, data))
    print(dst)


if __name__ == "__main__":
    main()
