---
name: html-picker
description: >
  Let the user choose between options visually: build an HTML page with one card per
  option (mockup, description, tags), click-to-pick, a note box per card for the agent,
  and a button that copies the picks plus notes to paste back. Use when there are several
  ideas, designs or approaches to choose from and mockups beat a chat list.
  Triggers: /html-picker, "html picker", "let me pick visually", "picker page",
  "let me select options visually", "make a page where I can pick", "show options I can select"
user_invocable: true
---

# HTML Picker

Turn a list of options into a page the user can click through. They pick, leave notes
for you, copy a summary and paste it back. Picks and notes are independent: a note on
an unpicked card is still information, and a pick without a note is fine.

## When to use

- Several feature ideas, design variants or approaches, and the user should choose some
- The options are easier to judge as mockups than as prose
- Not for a single yes/no question (just ask), or for a comparison of two or three
  designs with a verdict (that is the mock-up rule in the global instructions: one
  side-by-side HTML page with a comparison table)

## Workflow

1. Write the options as JSON to `/tmp/<slug>-options.json` (schema below).
2. Render it:
   `python3 ~/.agents/skills/html-picker/scripts/render.py /tmp/<slug>-options.json <project>/.martifacts/<slug>-picker.html`
   Use `.martifacts` in the project root (create if missing); outside a project, `/tmp`.
3. Open it once with `open <file>` (`dangerouslyDisableSandbox: true`). Do not reopen
   after later edits; the user refreshes. Re-render over the same file when changing it.
4. Tell the user in two lines how to use it: click cards, optional notes, `copy picks +
   comments`, paste the result back. Mention anything non-obvious about the options
   (for example, which one you suggest and why).
5. When they paste back, it looks like:
   ```
   Picked: 10, 11, 12

   Notes for the agent (independent of picks):
   5. Pins and notes: <their text>
   ```
   Answer questions in the notes, apply the notes as requirements, and treat unpicked
   options as not wanted unless a note says otherwise.

## Options JSON

```json
{
  "id": "earndash-features",
  "title": "earndash: pick the features you want",
  "subtitle": "One sentence of context.",
  "mode": "multi",
  "columns": 2,
  "legend": "optional HTML shown above the cards (for example what the effort tags mean)",
  "options": [
    {
      "id": 1,
      "title": "Collapse repeats",
      "desc": "One or two sentences. HTML allowed.",
      "mockup": "<span class=\"d\">today</span>\n18:07 <span class=\"chip-r\"> PROD </span> ...",
      "mockup_kind": "terminal",
      "tags": [{"text": "small", "kind": "good"}],
      "meta": "Needs: nothing new",
      "group": "Optional section heading",
      "recommended": true
    }
  ]
}
```

- `mode`: `multi` (checkboxes, default) or `single` (radio, one choice).
- `id`: unique; use plain numbers so the user can reply "1, 3, 5". The page stores picks
  under `id` in the browser, so use a distinct top-level `id` per page.
- `mockup_kind`: `terminal` (default; content goes in a dark `<pre>`, supports the colour
  classes below), `html` (rendered as a boxed HTML block, for web UI mockups), or `text`
  (escaped plain text in the dark `<pre>`).
- `tags[].kind`: `good`, `warn`, `bad`, `rec` or empty. Typical use: effort (small = good,
  medium = warn, large = bad), risk, or "writes data".
- `group` starts a new heading when it changes between consecutive options.
- Page is light mode. Terminal mockups are dark on purpose.

## Terminal mockup classes

For `terminal` mockups, wrap text in spans: `c` cyan, `cb` bold cyan, `g` green, `r` red,
`rb` bold red, `y` yellow, `m` magenta, `d` dim, `b` bold; chips `chip-r` `chip-y`
`chip-g` `chip-d` (reverse video look); `tab` and `tabsel` for key hints and selected
tabs; `sel` for a selected row and `hl` for a highlighted row.

Keep each mockup to about 60 columns so it fits a half-width card without scrolling.
Align boxes by padding to a fixed width (count visible characters, not tags); if there
are many mockups, generate the lines with a small script instead of hand-aligning.
When mocking a TUI, only show what the framework can actually render.

## Rules

- Only offer options you would be willing to build. Put your suggestion first or set
  `recommended`, with a reason in `desc` or `meta`.
- Effort tags are estimates; say so in `legend`.
- Keep `desc` short. The mockup carries the idea.
- Never number headings in markdown docs you also write; option ids are the exception.
