---
name: til
description: >
  Add a short "today I learned" note to the TIL folder of the Obsidian vault at ~/.local/share/sbdb.
  Triggers: /til, "add this as a til", "save as til", "til this", "note this down as a tidbit",
  "store this small how-to in the vault"
---

# /til -- Add a TIL note

**Folder**: `/Users/meain/.local/share/sbdb/TIL/`

One fact per file, named `TIL/<descriptive title>.md`. The filename is the title, so there is no heading and no frontmatter. Don't append tidbits to broad notes like `MacOS.md`. Code snippets go in `Snippets/` instead.

## Steps

- Take the content from the argument, or from what was just discussed in the conversation. Distil it to the useful bit: what happened or what to do, plus the fix or workaround.
- Pick a short, descriptive title in sentence case (e.g. `Changing swapfile on linux`, `Connecting to CosmosDB from local`). Run `ls /Users/meain/.local/share/sbdb/TIL` first. If a note on the same topic already exists, edit it instead of creating a duplicate.
- Write the file with the Write tool. Keep it short and plain: a sentence or two, a few bullets, or a link. Use `[[Wiki Link]]` for related vault notes, markdown links for URLs, and an optional `#tag` line at the top (e.g. `#linux`) when it fits.
- Don't add dates, headings, or "TIL:" prefixes.
- Open the note once in Obsidian (`dangerouslyDisableSandbox: true`), and don't reopen it after later edits:
  `open "obsidian://open?path=<url-encoded absolute path>"`
- Reply with the file path in one line.
