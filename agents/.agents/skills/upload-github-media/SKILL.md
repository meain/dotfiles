---
name: upload-github-media
description: "Upload images/screenshots/media to GitHub and get durable github.com/user-attachments URLs for READMEs, PRs, issues or docs, instead of committing binaries to the repo. Uses the `gh image` extension and the repo's existing 'Media' issue. Triggers: /upload-github-media, 'upload screenshot', 'upload image to github', 'add screenshots to README', 'host this image', 'user-attachments link'"
user_invocable: true
---

# upload-github-media

GitHub has no API for `user-attachments` links, and `gh` alone can't make them. Use the `gh image` extension (`drogers0/gh-image`), which uploads with the browser's GitHub session cookie (no PAT scopes). Never commit images just to link them from a README or docs.

## Steps

1. Check the extension: `gh extension list | rg image`. If missing: `gh extension install drogers0/gh-image`.
2. Find the repo's media issue. meain usually keeps one open (or closed, which still accepts comments) per repo, titled "Media":
   `gh issue list --state all --json number,title | rg -i media`
   Known ones: meain/edo#1, meain/monica#4. If none exists, ask before creating one.
3. Upload one file per call so each URL maps to its file, with the repo set and `GIT_DIR=$(jj git root 2>/dev/null || echo .git)` for jj repos:
   `gh image --repo OWNER/REPO /path/to/file.png`
   It prints `![name.png](https://github.com/user-attachments/assets/<uuid>)`. Outbound network and the browser cookie need `dangerouslyDisableSandbox: true`.
4. Use the URL in the README/PR/doc, e.g. `<img src="URL" width="220" alt="descriptive alt text" />`. Write specific alt text describing what the image shows, not just a label.
5. Optional durable record: post the markdown as a comment on the media issue with `gh issue comment N --repo OWNER/REPO`. That is public, so ask first unless the user already asked for it.

## Notes

- Uploading itself is not an issue comment; the link works as soon as `gh image` prints it.
- If the extension fails with a token error, `gh image check-token`; the user may need to be logged in to GitHub in their browser.
- Resize large PNGs first (`magick in.png -resize 540x -strip out.png`) and put temp images in `/tmp`.
- Before uploading screenshots from a device or app, look at them for secrets and other private data (API keys, other projects' names, chat titles); the URLs are public.
