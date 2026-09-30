#!/usr/bin/env python3
"""Small VibeDiff CLI for the common review-comment chores.

Usage (VIBEDIFF_URL defaults to http://localhost:8888; --dir defaults to cwd):
  vd.py comments [--dir D] [--rev R] [--author user|agent] [--all] [--json]
      Open threads (root + replies), optionally filtered by revision prefix
      and root author. --all includes resolved threads.
  vd.py post --file F --line N [--line-end M] --rev R [--dir D] [--name reviewer] TEXT
  vd.py reply ID [--name NAME] [--resolve] TEXT      (TEXT "-" reads stdin)
  vd.py resolve ID... | vd.py reopen ID...
  vd.py revisions [--dir D] [--limit N]
"""

import argparse
import json
import os
import sys
import urllib.error
import urllib.parse
import urllib.request

BASE = os.environ.get("VIBEDIFF_URL", "http://localhost:8888").rstrip("/")


def call(method, path, params=None, body=None):
    url = BASE + path
    if params:
        url += "?" + urllib.parse.urlencode({k: v for k, v in params.items() if v is not None})
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method, headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req) as resp:
            raw = resp.read()
    except urllib.error.HTTPError as err:
        sys.exit(f"{method} {path}: HTTP {err.code}: {err.read().decode().strip()}")
    return json.loads(raw) if raw.strip() else None


def text_arg(text):
    return sys.stdin.read().strip() if text == "-" else text


def all_comments(directory):
    return call("GET", "/api/review/comments", {"directory": directory}) or []


def cmd_comments(args):
    comments = all_comments(args.dir)
    roots = [c for c in comments if not c.get("parentId")]
    replies = {}
    for c in comments:
        if c.get("parentId"):
            replies.setdefault(c["parentId"], []).append(c)

    def keep(c):
        if not args.all and c.get("status") != "open":
            return False
        if args.rev and not c.get("revision", "").startswith(args.rev):
            return False
        return not args.author or c.get("author") == args.author

    roots = [c for c in roots if keep(c)]
    if args.json:
        print(json.dumps([{**c, "replies": replies.get(c["id"], [])} for c in roots], indent=2))
        return

    for c in sorted(roots, key=lambda c: (c.get("revision", ""), c.get("file", ""), c.get("line", 0))):
        who = c.get("author", "?") + (f":{c['authorName']}" if c.get("authorName") else "")
        span = f"{c.get('line')}" + (f"-{c['lineEnd']}" if c.get("lineEnd") and c["lineEnd"] != c.get("line") else "")
        print(f"[{c['id']}] {c.get('revision', '')[:8]} {c.get('file')}:{span} ({who}, {c.get('status')})")
        print("    " + c["content"].replace("\n", "\n    "))
        for r in replies.get(c["id"], []):
            rwho = r.get("author", "?") + (f":{r['authorName']}" if r.get("authorName") else "")
            print(f"    ↳ ({rwho}) " + r["content"].replace("\n", "\n      "))
        print()


def cmd_post(args):
    body = {
        "directory": args.dir,
        "file": args.file,
        "line": args.line,
        "lineEnd": args.line_end or args.line,
        "revision": args.rev,
        "content": text_arg(args.text),
        "author": "agent",
        "authorName": args.name,
    }
    print(call("POST", "/api/review/comment", body=body)["id"])


def cmd_reply(args):
    parent = next((c for c in all_comments(args.dir) if c["id"] == args.id), None)
    if parent is None:
        sys.exit(f"comment {args.id} not found in {args.dir}")
    body = {
        "directory": args.dir,  # required by the API even though replies inherit it
        "parentId": args.id,
        "content": text_arg(args.text),
        "author": "agent",
        "authorName": args.name,
    }
    reply = call("POST", "/api/review/comment", body=body)
    if args.resolve:
        call("POST", f"/api/review/comment/{args.id}/resolve")
    print(reply["id"])


def cmd_status(args, action):
    for cid in args.ids:
        call("POST", f"/api/review/comment/{cid}/{action}")
        print(f"{action}d {cid}")


def cmd_revisions(args):
    for r in call("GET", "/api/revisions", {"directory": args.dir, "limit": args.limit}) or []:
        print(r.get("id", ""), r.get("description", r.get("message", "")).splitlines()[0] if r.get("description") or r.get("message") else "")


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)

    def with_dir(sp):
        sp.add_argument("--dir", default=os.getcwd())
        return sp

    c = with_dir(sub.add_parser("comments"))
    c.add_argument("--rev")
    c.add_argument("--author", choices=["user", "agent"])
    c.add_argument("--all", action="store_true")
    c.add_argument("--json", action="store_true")

    po = with_dir(sub.add_parser("post"))
    po.add_argument("--file", required=True)
    po.add_argument("--line", type=int, required=True)
    po.add_argument("--line-end", type=int)
    po.add_argument("--rev", required=True)
    po.add_argument("--name", default="reviewer")
    po.add_argument("text")

    r = with_dir(sub.add_parser("reply"))
    r.add_argument("id")
    r.add_argument("--name", default="claude")
    r.add_argument("--resolve", action="store_true")
    r.add_argument("text")

    for action in ("resolve", "reopen"):
        s = sub.add_parser(action)
        s.add_argument("ids", nargs="+")

    rv = with_dir(sub.add_parser("revisions"))
    rv.add_argument("--limit", type=int, default=10)

    args = p.parse_args()
    if args.cmd == "comments":
        cmd_comments(args)
    elif args.cmd == "post":
        cmd_post(args)
    elif args.cmd == "reply":
        cmd_reply(args)
    elif args.cmd in ("resolve", "reopen"):
        cmd_status(args, args.cmd)
    else:
        cmd_revisions(args)


if __name__ == "__main__":
    main()
