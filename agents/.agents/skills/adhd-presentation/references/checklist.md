# Pre-delivery checklist

Run before handing over any document, report, dashboard or UI. Each item has a
test you can actually apply, not a vibe. Items marked **[hard]** are the ones
worth rejecting work over.

## First screen

- [ ] **[hard]** The conclusion is in the first screen, before any background,
      method or history. Test: delete everything after the first screenful —
      does the reader still know the answer?
- [ ] **[hard]** The single next action (or "nothing, FYI") is stated
      explicitly, with an owner. Test: can you point at one sentence?
- [ ] The deadline / date is in the first screen.
- [ ] A time estimate is given ("~6 min read", "about 10 min to run").
- [ ] First two words of the title and of every heading carry the meaning.
      Test: read only the first two words of each heading — is it an outline?
- [ ] The artifact has exactly one purpose. Test: name it in four words. If you
      need "and", split the document.

## Nothing held in the head

- [ ] **[hard]** No legends. Every series/category is labelled directly on the
      mark.
- [ ] **[hard]** No "as mentioned above", "see Figure 3", "per the thread",
      "as discussed". Test: grep for `above|below|earlier|previously|see Fig`.
- [ ] Units are inline with every number, not in a header or footnote alone.
- [ ] Acronyms are expanded at first use *in each section*, not once at the top.
- [ ] Tables have sticky headers (HTML) or repeat headers (long printed tables).
- [ ] Any choice shows the criterion and the options at the same time.
- [ ] Values the reader would otherwise compute are given: deltas, percentages,
      totals, the assembled command.
- [ ] Test: pick any section at random. Can someone read only that section and
      act on it? If no, it isn't self-contained.

## Built to re-entry

- [ ] A meaning-bearing subheading roughly every 3-5 short paragraphs.
- [ ] A status block exists (Status / Owner / Next step / Updated).
- [ ] Progress is shown as "N of M" **with the remaining items listed**.
- [ ] Completed items are greyed and struck, not removed.
- [ ] Nothing reorders, reflows, or relocates between loads.
- [ ] (Apps) Autosave on blur; scroll position and open panels restored;
      a "where you left off" block on return.
- [ ] (Docs > ~1,500 words) A linked table of contents at the top.

## Time

- [ ] **Every** date is absolute **and** relative: "Thu 27 Nov (in 2 days)".
      Test: grep for `tomorrow|next week|soon|recently|shortly` used alone.
- [ ] Durations have a visual length (bar/block), not only a number.
- [ ] Long horizons are broken into near-term milestones.
- [ ] "Now" is marked explicitly on any timeline.

## Visual hygiene

- [ ] Nothing autoplays, loops, carousels, parallaxes or animates unprompted.
      Test: load the page and don't touch it for 30 seconds. Did anything move?
- [ ] `prefers-reduced-motion`, `prefers-color-scheme` honoured.
- [ ] No element carries meaning by colour alone — there's a word, icon, shape
      or position too. Test: screenshot in greyscale; is it still readable?
- [ ] Body text ≥ 4.5:1 contrast (aim 7:1); no grey below `--fg-muted`.
- [ ] Text left-aligned, not justified; no ALL CAPS beyond a 1-3 word label; no
      long italic runs; no underline except links.
- [ ] Bold is still meaningful. Test: count bolded phrases per paragraph. More
      than one and it has stopped working.
- [ ] Line length 60-75 characters for prose.
- [ ] Decorative images, icons and backgrounds that carry no information have
      been removed.
- [ ] Layout survives a WCAG 1.4.12 text-spacing override (line-height 1.5,
      paragraph 2×, letter 0.12em, word 0.16em) without clipping.

## Charts

- [ ] Headline comparison is encoded as position or length, not area or angle.
- [ ] Title states the conclusion as a sentence, and the title is **true**.
- [ ] ≤4 series on a line chart; ≤5-7 colour-coded categories.
- [ ] Chart type matches the question: plain bars for value lookup; unit charts
      / pictographs for ratios; either for min/max.
- [ ] Any icon used is *the data*, not decoration on top of the data.
- [ ] Gridlines light and ≥8px apart, or replaced with direct value labels.
- [ ] Key values appear in text as well as in the graphic.
- [ ] Palette is colour-vision-safe (Okabe-Ito or equivalent).
- [ ] `role="img"` + an `aria-label` that states the trend and the values.

## Tone

- [ ] **[hard]** Nothing is framed as the reader's failure. No "you missed", no
      red overdue pile, no streak-break, no failure count as the headline.
- [ ] Errors state the fix before the problem.
- [ ] Red is reserved for genuine danger, not for "not done yet".
- [ ] No streaks, points, badges or leaderboards.

## Interaction (apps only)

- [ ] 3-5 options max where a choice is needed, one marked **Recommended** with
      a one-line reason, and a sensible default pre-selected.
- [ ] Progressive disclosure ≤2 levels, used only for rare detail; every
      "show more" names and counts its contents.
- [ ] Labels visible at all times (never placeholder-only).
- [ ] Validation on blur, not while typing; input never discarded.
- [ ] Notifications batched by default; immediate only for minutes-urgent
      items; delivered at task boundaries.
- [ ] Targets ≥24×24 CSS px.
- [ ] Density / motion / theme adjustable and remembered.

## Final sanity

- [ ] You cannot justify any choice here with "ADHD people prefer it".
      Preference does not track performance — if that's the only reason, cut it.
- [ ] You have not added stimulation. You have removed competitors for
      attention.
- [ ] Nothing in the artifact is labelled "ADHD mode" or explains the reader to
      themselves. Design for the demand, not the diagnosis.
