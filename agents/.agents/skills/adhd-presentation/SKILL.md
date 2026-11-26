---
name: adhd-presentation
description: >-
  How to present information and data so an ADHD reader can actually use it —
  for Markdown documents, HTML reports and dashboards, app and UI design, chart
  design, and written communication (PRs, tickets, runbooks, status updates).
  Use when asked to make something "ADHD friendly", when designing or generating
  an artifact for an ADHD reader, or when the user invokes /adhd-presentation.
  Also worth loading before building any long report or dense dashboard, since
  almost all of it is just good universal design applied strictly.
---

# Presenting information to ADHD readers

## Read this first: what the evidence actually supports

Most "ADHD design" advice online is invented. The honest position:

- **There is almost no ADHD-specific presentation research.** No peer-reviewed
  study has tested a typeface on a diagnosed ADHD sample. There is no ADHD
  typography, ADHD colour palette, or ADHD font.
- **The one good direct study found no ADHD-specific effect.** Tran et al.
  (CHI 2024, 70 ADHD vs 77 non-ADHD adults) tested chart hue, amount of text,
  and embellishment. None of them interacted with ADHD status. Irrelevant text
  and decorative images slowed *both* groups. The ADHD group was actually
  **faster** than controls on every task, at equal or better accuracy.
- **So: there is no separate "ADHD version" of good design.** There is good
  design, which ADHD readers need you to do *strictly* rather than *mostly*.

Three things genuinely are different, and they drive everything below:

| What differs | Evidence | Design consequence |
|---|---|---|
| Added working-memory load hurts comprehension ~3-4x more | Kofler 2018: comprehension drop d=-0.67 (ADHD) vs -0.18 (controls) | Rule 2 — nothing held in the head |
| Attention lapses are frequent, brief and start immediately | RT variability g≈0.46 (adults); vigilance deficit present from task start, not a late decay | Rule 3 — built to re-enter |
| Emotional reactions to framing are much stronger | Emotion dysregulation g≈1.17 in adult ADHD | Rule 7 — status without verdict |

**Why Tran's ADHD group outperformed controls matters more than it looks.** The
task was short (22 min), paid, explicitly goal-directed, with one clear question
per screen and immediate progression. That is close to the *best case* for an
ADHD brain. Real artifacts are the opposite: long, self-paced, no stated
question, no deadline, no payoff. So the highest-leverage move is not to
restyle the artifact — it is to **give it the properties of that task**: a
stated question, a short path, visible progress, an immediate payoff.

**Standing warning: preference does not track performance.** In Tran's study
ADHD participants *preferred* the heavily annotated chart that slowed them down,
and *disliked* the pictographs that raised their ratio accuracy from 55% to 80%.
Never justify a design choice with "ADHD people like X". Optimise the thing they
must extract for performance; spend the appeal budget on getting them to start.

---

## The seven rules

Apply in order. Rule 1 and Rule 2 are worth more than the rest combined.

### 1. Answer first

The conclusion, the ask, and the next action go in the first screen — before
background, method, or history. If the reader leaves after the first paragraph
they must still have the point.

- About 57% of page viewing time happens above the fold; 74% in the first two
  screenfuls (NN/g eye-tracking, n=120).
- The ADHD attention deficit is present *from the start of a task*, not only
  after a decay period. A recap at the end does not rescue a buried lead.
- Front-load the first two words of every heading, bullet and link. "Failed: 3
  jobs", not "There were 3 jobs that failed."
- One artifact, one purpose. Don't merge status + decision + history +
  instructions into one document; the reader can't tell which part is theirs.

### 2. Nothing held in the head

This is the single strongest ADHD-specific finding. Never require the reader to
carry a value, a definition, or a position from one place to another.

- **Direct labels, never legends.** Put the series name at the end of the line
  or on the bar.
- **Repeat, don't reference.** Restate the number, the term, the decision where
  it is used. Ban "as mentioned above", "see Figure 3", "per the thread".
- **Keep the question beside the options.** Any choice, form, or comparison must
  show the criterion and the candidates simultaneously.
- **Sticky table headers**, units inline, `scroll-padding-top` so a sticky
  header never hides the focused row.
- **Every section self-contained.** A reader must be able to land in section 4
  without having read 1-3.
- Define each acronym at first use *in the section where it's used*, not once at
  the top.

### 3. Built to re-enter

Assume the reader looks away mid-document and comes back cold. Design the
return, not the uninterrupted read.

- Descriptive headings act as re-entry points. One meaning-bearing subheading
  roughly every 3-5 short paragraphs; the headings alone should read as a
  summary.
- **Visible state everywhere**: done / in progress / blocked / not started, same
  vocabulary throughout, plus a count at the top ("4 of 7 done").
- **Stable layout.** Never reflow, reorder, or relocate controls between
  renders — spatial memory is what people use to find their place again.
- In apps: autosave on blur, restore exact scroll position and open panels, show
  a "where you left off" block with last action, timestamp and the single next
  step. (A ~1-minute "ready to resume" note measurably reduces attention
  residue; Leroy & Glomb 2018.)
- In documents: a status block at the top (Owner / Status / Next step / Last
  updated) and `- [x]` / `- [ ]` checkboxes.

### 4. Externalise at the point of performance

Barkley's core thesis, and the most under-used idea here: *"The information,
whatever it is that is key for you to remember right here right now, needs to be
outside of your brain in the visual field."* ADHD is a disorder of performance,
not knowledge — information that isn't present at the moment of action has no
effect.

- Put instructions next to the thing they govern, not in a preamble.
- **Make problem-solving manual.** Replace anything the reader would have to
  compute: give the delta as well as the two numbers, the percentage as well as
  the counts, the ready-to-run command rather than the parameters to assemble.
  Barkley: *"They can't hold things in mind and move them around as well as
  other people, so don't make them do it. Put it in their hands."*
- Prefer checkboxes, sortable/filterable tables, copy buttons, expand/collapse —
  things the reader can act on rather than hold.
- Never hide must-see state behind a click, hover, or tab.

### 5. Cut the irrelevant; don't add the stimulating

The "ADHD brains need stimulation" advice is mostly wrong as applied to layout.

- Irrelevant text and decorative images slowed both ADHD and non-ADHD readers
  (Tran). Adding more annotation increased response time monotonically.
- The real noise/stimulation effect is small (g≈0.25), **auditory**, and it
  *harms* non-ADHD readers (g≈-0.21). It is not a licence for busy visuals.
- Relevant embellishment helps; decorative embellishment hurts. An icon that
  *is* the data (unit/pictograph charts) is fine. An icon decorating a bar
  dropped ratio accuracy to 44%.
- The useful distinction is **relevant vs irrelevant** and **chosen vs imposed**
  — not more vs less. Put novelty in the *content*; keep the *chrome* calm.
- Zero tolerance for: autoplay, carousels, parallax, animated backgrounds,
  looping motion, unprompted modals, notification badges on non-actionable
  items, anything moving in peripheral vision while text is being read.

### 6. Make time and progress spatial

- Always give **absolute and relative together**: "Thu 27 Nov 2026 (in 2 days)".
  Never relative alone, never absolute alone.
- Show durations as a bar or proportional block, not just a number.
- Show "N of M" with the remaining items *listed*, not just a percentage.
- Give a time estimate up front ("~10 min read", "about 5 minutes to run") and
  be generous with it.
- Break long horizons into near-term milestones — delay discounting is
  reliable (d=0.43), so a deadline four weeks out does not drive action.
- Don't rely on streaks, Zeigarnik "open loops", or goal-gradient effects; the
  Zeigarnik memory effect failed meta-analytic replication (recall ratio ≈0.99),
  and constant progress bars do not reduce drop-off.

### 7. Status without verdict

Adult ADHD shows large emotion dysregulation (g≈1.17), and every ADHD-focused
product that survived converged on the same thing: report state, don't pass
judgement.

- No red "overdue" piles, no failure counts as the headline, no "you missed",
  no streak-break, no shame framing.
- State the fix before the problem: "Set `retries: 3` — currently unset", not
  "ERROR: retries missing".
- Neutral status colours; red reserved for genuine danger, not for "not done
  yet".
- Never use colour alone for status — add a word, icon, or position.

---

## Medium playbooks

### Markdown documents

```
# <Noun phrase naming the thing>

**Bottom line:** <one sentence — the conclusion or the ask>
**You need to:** <the single next action, or "nothing — FYI">
**Status:** <state> · **Owner:** <who> · **Updated:** <YYYY-MM-DD>

## <Descriptive heading>
...
```

- One idea per paragraph. Split anything over ~50 words (COGA 4.4.5).
- 3+ parallel items → bullet list. 3+ ordered steps → numbered list, one step
  per line, including the obvious ones.
- Bold only the words someone must *find*. At most one bolded phrase per
  paragraph — over-bolding destroys the anchor.
- No ALL CAPS beyond a 1-3 word label (it costs 10-19% reading speed). No
  italics for long runs. No underline except links.
- Tables beat prose for anything comparative. Put the recommendation in a
  "Recommended" column or a verdict line *before* the table, not after.
- `<details>` only for genuinely optional bulk (raw logs, full output), and the
  summary line must name the contents and the count: `<summary>Show 12 failed
  tests</summary>` — never "More".
- Docs over ~1,500 words: add a linked table of contents at the top.
- Front-load link text. Never "click here" / "see this".

### HTML reports and dashboards

Start from `references/report-template.html` and `references/adhd.css` — they
implement all of this. Key points:

- Single column, `max-width: 65ch` for prose. Dashboards may use a grid, but
  everything that must be compared belongs on one screen together.
- Above the fold: one headline sentence, then **3-4 KPIs maximum** (Cowan's ~4
  chunk limit), each with its comparison baseline and a status *word*.
- Each KPI: value, unit, comparison ("vs last week"), status word, and at most
  a sparkline. Group into labelled sections so each group reads as one chunk.
- Tables: ≤7 rows before "show more"; sorted by what matters, not
  alphabetically; numbers right-aligned with `font-variant-numeric: tabular-nums`;
  rounded to the precision the decision needs.
- Sticky section nav with the current section highlighted, anchors on headings,
  back-to-top for anything over ~3 screens.
- Honour `prefers-reduced-motion`, `prefers-color-scheme`, `prefers-contrast`.
  Default to no motion regardless.
- Offer a density/motion/theme toggle persisted in `localStorage`. COGA
  Objective 8 is explicit that adaptation is a user need.
- Reserve space for anything async (`aspect-ratio`, explicit width/height) so
  nothing shifts under the reader.
- Make it printable and copyable. Add a print stylesheet.

### Charts

- Encode the headline comparison as **position or length** — bars, dots, lines.
  Never pie/donut/bubble area for a value that must be read.
- **Title with the conclusion as a sentence**: "Churn fell 12% after the March
  change". Metric and unit go in the subtitle. Message-bearing titles measurably
  improve what readers take away and recall — but the title must be accurate,
  because it *determines* the takeaway.
- Max ~4 series on a line chart; ~5-7 categories anywhere colour-coded. Beyond
  that: highlight one in the accent colour, grey the rest, label the highlighted
  one directly.
- Direct labels at line/bar ends. No legend if avoidable; if unavoidable, place
  it adjacent and in the same order as the data.
- Never colour alone. Okabe-Ito is the safe categorical default:
  `#E69F00 #56B4E9 #009E73 #F0E442 #0072B2 #D55E00 #CC79A7 #000000`.
- Gridlines light and ≥8px apart, or dropped in favour of direct value labels.
- Match chart type to question (from Tran et al.):
  - **Look up a specific value** → plain bar chart. Pictographs are much worse
    here (15.5s vs 11.4s) because people start counting icons.
  - **Compare a ratio / part-to-whole** → pictograph / unit chart. 80% accuracy
    vs 55% for plain bars and 44% for decorated bars.
  - **Find the largest/smallest** → either; pictographs slightly faster.
- One or two annotation callouts stating the "so what". No more.
- Animate only for object constancy on a user-triggered change, ~0.3-0.5s.
  Never autoplay, never loop.

### App / product UI

- 3-5 options when a choice is needed; mark one **Recommended** with a one-line
  reason. Pre-select the sensible default (defaults are robust, d=0.68) and make
  changing it one click.
- Progressive disclosure: at most 2 levels, and only for genuinely rare detail.
  Prefer scrolling to accordions for content under ~3 screens.
- Forms: one question (or one tightly-related set) per page for long/branching
  flows, a check-your-answers page, visible labels always (never
  placeholder-only), autosave on blur, validation on blur not while typing,
  plain non-blaming error text, never discard input.
- Notifications: batched digest by default (3/day beat hourly in an n=237 field
  experiment); immediate only for things needing action in minutes; deliver at
  task boundaries; badge only actionable items.
- Response times: <0.1s no feedback; >1s show activity; 2-10s spinner; >10s
  determinate progress with named steps and a cancel.
- Separate capture from organisation: one field, no mandatory category or date.
- Show one thing, keep the rest reachable: a "start here" item with the list
  collapsed behind an explicit, counted control.
- Target size ≥24x24 CSS px (WCAG 2.5.8); 44x44pt on Apple platforms.

### Written communication (PRs, tickets, runbooks, status, chat)

- Subject/title carries the action and the deadline.
- The ask goes on its own line at the top. Numbered if there are several.
- **Lead with why**, not what. The diff shows what changed. ADHD engineers
  consistently report the missing "why" as the bigger problem than length.
- Keep it short, then structure what remains. The governing variable is *how
  much the reader must hold before getting value* — shortening reduces it,
  structure lets them skip and return. For a single-purpose message, short wins;
  for reference material, structure plus a summary wins and length is fine if
  the reader can enter anywhere.
- Runbooks: numbered steps, one action each, expected output after each step,
  copy-pasteable commands, jargon defined on first use in that section.
- Don't make someone categorise before they can file something.
- Offer a draft to react to rather than a blank template.

---

## The DON'T list

Never, in any medium:

- Bury the ask, the deadline, or the decision in prose.
- Open with background, history, or methodology.
- Write a wall of text — *or* a wall of undifferentiated bullets with no
  hierarchy. Both fail.
- Require a front-to-back read for later sections to make sense.
- Say "as discussed above" / "see the thread" / "per Figure 3".
- Split a label from its data (legends, footnoted units, keys).
- Use autoplay, carousels, parallax, animated backgrounds, unprompted modals, or
  anything moving in peripheral vision.
- Use decoration that carries no information.
- Use red/overdue piles, streaks, points, leaderboards, or "you missed" framing.
- Bold everything.
- Use placeholder text as the only label.
- Rely on colour alone for meaning.
- Reorder or relocate things between renders.
- Ship a long form with every field visible.
- Claim a design is good "because ADHD people prefer it".

## Things that sound right but aren't

Don't apply these, and push back if asked to:

- **Bionic Reading / bold-first-half text.** Multiple 2025-26 eye-tracking
  studies found no speed or comprehension benefit; one found it *increased*
  fixation count and reduced words per minute. Never tested on ADHD.
- **OpenDyslexic / Dyslexie fonts.** Meta-analysis of 15 studies, N=688:
  g = -0.04. No effect. Where a benefit appeared, it was the letter *spacing*,
  not the letterforms.
- **Lexend's "reading performance" claims.** One unreplicated creator-led
  dissertation. It's a fine wide-spaced sans; it is not an intervention.
- **Coloured overlays / Irlen syndrome.** Reviewed and not endorsable; effects
  are consistent with placebo.
- **"ADHD people can't focus for more than N minutes."** The time-on-task
  decrement is small; the deficit is present from the start. Put the point
  first rather than making the document shorter and hoping.
- **"ADHD brains need bright, stimulating visuals."** No ADHD-specific colour
  effect found. The stimulation evidence is auditory, small, and harms others.
- **7±2 as a UI cap.** Miller meant chunks and said it loosely. The working
  limit is ~4 chunks (Cowan), and it applies to what's held in the head, not
  what's displayed.
- **Zeigarnik / "open loops nag".** Failed meta-analytic replication. (The
  *tendency to resume* an interrupted task did hold up.)
- **Choice overload / the jam study.** Mean effect ≈0 across 50 experiments.
  Use "few options + a recommended default" because defaults work, not because
  choice is paralysing.
- **Ego depletion** as a justification for limiting decisions. d=0.04 in a
  23-lab replication. Justify via working memory instead.
- **"Chart junk is always bad"** and **"embellishment aids memory"** are both
  wrong as stated. Relevance is the variable.
- **Progress bars always help.** Constant bars did not reduce drop-off in a
  32-experiment meta-analysis.

## Before you deliver — self-check

Run `references/checklist.md`. The short version:

1. Can the reader get the conclusion and the next action in the first screen?
2. Is there anything they must carry from one place to another? (legend,
   cross-reference, "as above", units defined elsewhere)
3. Could they land in the middle, cold, and still use that section?
4. Is every date both absolute and relative? Is every duration shown as well as
   stated?
5. Does anything move, blink, autoplay, or shift?
6. Does any element carry meaning by colour alone?
7. Is anything framed as the reader's failure?
8. Is every emphasis (bold, caps, colour) still doing work, or has it inflated?

## Reference files

- `references/adhd.css` — drop-in stylesheet implementing the typographic and
  interaction spec. Light + dark, reduced-motion, print, Okabe-Ito tokens,
  status/KPI/callout/timeline components.
- `references/report-template.html` — a working report skeleton using it.
- `references/checklist.md` — the full pre-delivery review checklist.
- `references/evidence.md` — every claim above with its grade, numbers and
  source URL. Read it before defending or overriding any rule here.
