---
name: html-picker
description: >
  Let the user choose between options visually: build an HTML page with one card per
  option (mockup, description, tags), click-to-pick, a note box per card for the agent,
  and a "Send to agent" button (a dashy://send link that fills this agent's message box
  in Dashy; copying the picks plus notes is the fallback). Use when there are several
  ideas, designs or approaches to choose from and mockups beat a chat list.
  Triggers: /html-picker, "html picker", "let me pick visually", "picker page",
  "let me select options visually", "make a page where I can pick", "show options I can select"
user_invocable: true
---

# HTML Picker

Turn a list of options into a page the user can click through. They pick, leave notes
for you, and click Send to agent: Dashy opens with the summary in your message box and
they press Enter. Copy is the fallback. Picks and notes are independent: a note on
an unpicked card is still information, and a pick without a note is fine.

## When to use

- Several feature ideas, design variants or approaches, and the user should choose some
- The options are easier to judge as mockups than as prose
- Not for a single yes/no question (just ask), or for a comparison of two or three
  designs with a verdict (that is the mock-up rule in the global instructions: one
  side-by-side HTML page with a comparison table)

## Workflow

1. Write the options as JSON to `/tmp/<slug>-options.json` (schema below). Inside Dashy,
   call the `whoami` MCP tool and put your agent id in the top-level `agent` field so the
   page gets the Send button (the `dashy://send` handler needs a Dashy build with link support; under
   Dashy Debug set `"scheme": "dashy-debug"`). Outside Dashy, leave `agent` out and the page
   only has Copy.
2. Render it:
   `python3 ~/.agents/skills/html-picker/scripts/render.py /tmp/<slug>-options.json <project>/.martifacts/<slug>-picker.html`
   Use `.martifacts` in the project root (create if missing); outside a project, `/tmp`.
3. Open it once with `open <file>` (`dangerouslyDisableSandbox: true`). Do not reopen
   after later edits; the user refreshes. Re-render over the same file when changing it.
4. Tell the user in two lines how to use it: click cards, optional notes, `Send to agent`
   (then Enter in Dashy; `Copy instead` if the link doesn't work, and they paste it back).
   Mention anything non-obvious about the options
   (for example, which one you suggest and why).
5. What comes back (sent from the page, or pasted) looks like:
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
  "agent": "your agent id from whoami (optional; enables the Send to agent button)",
  "scheme": "dashy (default) or dashy-debug",
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
- `agent`: when set, the summary is sent as `dashy://send?agent=<id>&text=<summary>`. Over
  about 7500 characters the button refuses and the user copies instead.
- `id`: unique; use plain numbers so the user can reply "1, 3, 5". The page stores picks
  under `id` in the browser, so use a distinct top-level `id` per page.
- `mockup_kind`: set it on every option, matching the medium of what is being proposed
  (see "Match the mockup to the medium" below). `terminal` (the fallback when unset;
  content goes in a dark `<pre>`, supports the colour classes below), `html` (rendered
  as a boxed HTML block, for web or GUI mockups), or `text` (escaped plain text in the
  dark `<pre>`).
- `tags[].kind`: `good`, `warn`, `bad`, `rec` or empty. Typical use: effort (small = good,
  medium = warn, large = bad), risk, or "writes data".
- `group` starts a new heading when it changes between consecutive options.
- Page is light mode. Terminal mockups are dark on purpose.

## Match the mockup to the medium

The mockup must look like what the user will actually get. `terminal` is only for TUIs,
CLI output and scripts. If the feature lives in a web page, dashboard, desktop or mobile
UI, use `html` and draw it as real UI (cards, buttons, chips, progress bars, tables), not
as monospaced ASCII in a dark box. Mixed lists are fine: a script-backed option can use
`terminal` for its output while the UI options beside it use `html`.

The `html` box only adds a border and padding, so style the mockup yourself with inline
`style` attributes (the page's own classes are not available). Keep it light mode, about
13px sans-serif, and reuse the host page's look when mocking an existing UI (same
colours, chip and button styles). Do not use the terminal colour classes in `html`.

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
- When options propose `dashy://` links (bookmarks, report buttons), generate every real link
  with `dashy-cli link new|send|open` (see the dashy skill) instead of hand-encoding, and show
  the readable form in the mockup. The picker's own Send button builds its link in the page.
- Never number headings in markdown docs you also write; option ids are the exception.
