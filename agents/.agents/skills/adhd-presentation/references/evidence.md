# Evidence behind the rules

Every substantive claim in `SKILL.md`, with its grade, numbers and source.
Read this before overriding or defending a rule.

**Grades**
`[STRONG]` replicated experimental or meta-analytic · `[MODERATE]` a single
decent study or solid applied research with data · `[WEAK]` expert heuristic or
design guidance, no data · `[MYTH]` popular claim contradicted by evidence.

**Verification note.** Several publisher sites (ACM, PubMed, ResearchGate,
Reddit) block automated fetching. Claims marked † were read via abstract or a
secondary summary rather than full text. The Tran thesis, which carries the most
weight here, was read in full from the open University of Oklahoma copy.

---

## The headline: ADHD-specific presentation evidence is almost empty

| Claim | Grade | Source |
|---|---|---|
| No peer-reviewed study has tested a typeface on a clinically diagnosed ADHD sample | [STRONG] (absence confirmed across searches) | — |
| Chart hue, text amount and embellishment do **not** interact with ADHD status | [MODERATE] single study, n=147 | Tran, Lee & Park, CHI 2024. Open thesis: https://shareok.org/items/71f740d8-6f61-4bcd-9769-9547b17270e8 · DOI 10.1145/3613904.3642112 · author PDF: https://cs.ou.edu/~jpark/papers/ADHD.pdf |

### Tran et al. 2024 in detail — the single most on-point study

Crowdsourced survey via Prolific, 70 adults with ADHD vs 77 without, mean 22
minutes, self-reported diagnosis, medication not controlled, tasks not
counterbalanced for order.

- **ADHD group was *faster* on all three tasks** and equal or better on
  accuracy: colour 9.59s vs 11.23s (p=.035); text 30.61s vs 34.13s (p=.05),
  accuracy 84.6% vs 78.1%; embellishment 12.67s vs 13.14s (p=.03).
  The authors attribute this to a hyperfocus-friendly task: short, paid,
  explicitly goal-directed, one question per screen.
- **Colour:** no ADHD×hue interaction. Both groups ≥94% accurate. Green fastest
  (9.72s), blue slowest (11.1s). Red was *polarising* for the ADHD group — most
  often chosen as favourite **and** as least favourite.
- **Text amount:** response time rose monotonically with added annotation
  (L1 axes only: 21.0s → L4 prose paragraph: slowest). Accuracy L1 84.7% vs
  L4 73.5% (p=.006). **A chart with minimal labels beat a prose paragraph.**
  ADHD participants preferred L1 over L2 where controls preferred L2 — one
  called a trivial "maximum" annotation *"unnecessary and feels patronizing.
  It's better to have nothing."*
- **Embellishment × question type** (significant interaction, F(4,580)=24.66,
  p<.001):
  - *Search / look up a value*: pictographs much worse (15.5s) than plain bars
    (11.4s); decorated bars in between (12.3s). People start counting icons.
  - *Ratio / part-to-whole*: pictographs far more accurate — **80.3%** vs
    plain bars 54.6% vs decorated bars 44.4% (p<.001).
  - *Min/max*: pictographs marginally fastest (9.01s vs 9.36s).
- **Preference ≠ performance, consistently.** Both groups ranked the heavily
  annotated L3 chart as their favourite despite it being slower; both ranked
  pictographs *lowest* despite the large ratio-accuracy win.

**How to use this study:** it is the best evidence available and it argues
*against* a separate ADHD visual language. Its limitations (self-report
diagnosis, online, uncontrolled display size, short motivating task, order not
counterbalanced) are stated by the author.

---

## The three things that genuinely differ

| Claim | Numbers | Grade | Source |
|---|---|---|---|
| Added working-memory load hurts ADHD reading comprehension far more | comprehension drop d=-0.67 (ADHD) vs -0.18 (controls); reading *speed* unaffected; held after controlling for decoding and reading disability; children 8-13 | [MODERATE] | Kofler et al. 2018, https://link.springer.com/article/10.1007/s10802-018-0447-1 |
| Working memory is impaired, spatial more than verbal | meta-analysis 26 studies: spatial storage d=0.85, spatial central-executive d=1.06, verbal storage d=0.47, verbal CE d=0.43 | [STRONG] | Martinussen et al. 2005, https://pubmed.ncbi.nlm.nih.gov/15782085/ † |
| …though the deficit may be central-executive, not storage | bifactor modelling; phonological STM looks intact | [MODERATE] | Kofler et al. 2020 † |
| Attention lapses are frequent and brief | reaction-time variability g≈0.72-0.85 (children), g=0.46 (adults); only slightly above clinical controls (g=0.25), so not ADHD-specific | [STRONG] | Kofler et al. 2013, https://sciences.ucf.edu/psychology/childrenslearningclinic/wp-content/uploads/sites/24/2013/05/Kofler-et-al.-2013-RT-Variability-Meta-analysis.pdf |
| The vigilance deficit is present from task start, not a late decay | CPT meta-analysis, 47 studies; time-on-task decrement small-to-moderate; deficit is lower perceptual sensitivity (d′) and slower drift rate | [STRONG] | Huang-Pollock et al. 2012 † |
| Mind-wandering mediates the comprehension gap in long digital text | n=210 adults, ~half ADHD; metacognitive scaffolding at reading stages narrowed the gap | [MODERATE] | Brann & Sidi 2024, https://www.sciencedirect.com/science/article/abs/pii/S0959475224001786 † |
| Emotion dysregulation is large in adult ADHD | g=1.17; emotional lability g=1.20 | [STRONG] | Beheshti et al. 2020, https://pmc.ncbi.nlm.nih.gov/articles/PMC7069054/ |
| …but no study tests the *design* consequence | — | [WEAK] | The "status without verdict" rule is an inference, corroborated by product convergence (below) |

---

## Rule 1 — Answer first

| Claim | Grade | Source |
|---|---|---|
| 57% of page viewing time is above the fold; 74% in first 2 screenfuls (to 2160px); 81% in first 3; within the first screenful >65% of time is in its top half. n=120, >130,000 fixations | [MODERATE] | NN/g 2018, https://www.nngroup.com/articles/scrolling-and-attention/ |
| Users read the first two words of a line far more often than the third (F-pattern); n=232 | [MODERATE] | NN/g, https://www.nngroup.com/articles/f-shaped-pattern-reading-web-content-discovered/ |
| "Layer-cake" scanning (fixate headings, read the relevant section) is the most effective pattern | [MODERATE] | NN/g, https://www.nngroup.com/articles/text-scanning-patterns-eyetracking/ |
| Concise text +58% usability, scannable +47%, all three combined +124% — small, 1997, dated | [WEAK] | Nielsen & Morkes 1997, https://www.nngroup.com/articles/concise-scannable-and-objective-how-to-write-for-the-web/ |
| Clear page purpose stated up front, so it survives losing focus | [WEAK] standard | W3C COGA 4.2.1, https://www.w3.org/TR/coga-usable/ |
| Engineers with ADHD name the missing "why" as the core problem, above length | [MODERATE] qualitative, 19 interviewees | https://ar5iv.labs.arxiv.org/html/2312.05029 |

## Rule 2 — Nothing held in the head

| Claim | Grade | Source |
|---|---|---|
| Working-memory capacity is ~4 chunks (3-5), not 7±2; Miller's figure was "a rough estimate and a rhetorical device" | [STRONG], contested at the margins | Cowan 2001, https://memory.psych.missouri.edu/assets/doc/articles/2001/cowan-bbs-2001.pdf |
| Legend-compatibility problems hurt graph integration, modulated by concurrent processing demand; authors recommend direct labelling | [MODERATE] | https://link.springer.com/article/10.3758/s13414-011-0155-1 † |
| Direct labelling over legends as a split-attention remedy | [WEAK] — theory-derived; **no controlled chart experiment found** | EU Data Visualisation Guide, https://data.europa.eu/apps/data-visualisation-guide/cognitive-load-concepts-for-storytelling |
| Paragraphs over 50 words should be split; main point first; list at 3+ items; numbered list at 3+ steps; one step per block | [WEAK] standard | W3C COGA 4.4.5, 4.4.9 |
| Do not force users to remember things from previous pages | [WEAK] standard | UK Home Office dyslexia poster, https://github.com/UKHomeOffice/posters |
| "Put it in their hands" — do not require mental manipulation | [CLINICAL opinion] | Barkley, https://www.youtube.com/watch?v=_tpB-B8BXk0 |

## Rule 3 — Built to re-enter

| Claim | Grade | Source |
|---|---|---|
| Resumption lag is real even after 1s interruptions; suspended goals decay; keeping the task display visible speeds recovery; spatial memory ("where I was on screen") guides resumption; a 2-8s warning before an interruption reduces the lag | [STRONG] | Altmann & Trafton 2002 and follow-ups; index: https://www.interruptions.net/literature/ |
| ~41% of interrupted tasks are not resumed immediately; average 23 min 15s before returning | [MODERATE] | Mark, Gonzalez & Harris, CHI 2005, https://www.interruptions.net/literature/Mark-CHI05-p321-mark.pdf |
| A ~1-minute "ready to resume" plan reduces attention residue | [MODERATE] 2 experiments | Leroy & Glomb 2018, https://www.washington.edu/news/2018/01/16/task-interrupted-a-plan-for-returning-helps-you-move-on/ |
| Interruptions at task boundaries are less disruptive | [MODERATE] | Adamczyk & Bailey, CHI 2004 † |
| "The 25 minutes to refocus" figure | [MYTH as stated] | It is a rounded version of the 23-min *time before returning*, not a refocus cost |

## Rule 4 — Externalise at the point of performance

All [CLINICAL opinion] — Barkley's model, stated repeatedly and consistently,
but these are lecture claims, not a controlled study of information design.

- *"You have a disorder of performance not knowledge… your problem is not with
  knowing what to do, it's with doing what you know."*
- *"The information, whatever it is that is key for you to remember right here
  right now, needs to be outside of your brain in the visual field."*
- *"They can't hold things in mind and move them around as well as other people,
  so don't make them do it. Put it in their hands."*
- *"You have no clock, we got to put one in your visual field."*
- 80/20 rule: *"80% altering the point of performance, 20% training the skill."*

Sources: https://www.youtube.com/watch?v=YSfCdBBqNXY (2h51m lecture),
https://www.youtube.com/watch?v=_tpB-B8BXk0 (13m excerpt),
https://www.youtube.com/watch?v=W6JBgeFbYCc (adult-focused, ADDitude).

Corroborating: Barkley's claim that working-memory *training* doesn't
generalise matches the literature — so compensate in the artifact, don't expect
the reader to adapt.

**Discount from the same source:** his blood-glucose / "fuel tank" claims and
his strong "no internal motivation" framing are contested; don't build on them.

## Rule 5 — Cut the irrelevant; don't add the stimulating

| Claim | Grade | Source |
|---|---|---|
| Irrelevant chart text slowed **both** groups; more annotation = monotonically slower | [MODERATE] | Tran 2024 (above) |
| Decorative embellishment on bars dropped ratio accuracy to 44.4% | [MODERATE] | Tran 2024 |
| Superfluous images distract; pictographs that *are* the data cost nothing and are recalled better under working-memory load | [MODERATE] | Haroz, Kosara & Franconeri, CHI 2015, https://tableau.com/sites/default/files/2023-01/Haroz_CHI_2015.pdf |
| Relevant embellishment improved recall at 2-3 weeks with no accuracy cost (n=20, small) | [MODERATE] | Bateman et al., CHI 2010 "Useful Junk?", https://vis.csail.mit.edu/classes/6.859/readings/pdfs/Bateman-UsefulJunk.pdf |
| Colour, human-recognisable objects and visual density raise *memorability*; minimalist high-data-ink designs are least memorable (2,070 visualisations) | [MODERATE] | Borkin et al., InfoVis 2013, https://vcg.seas.harvard.edu/publications/20130101-what-makes-a-visualization-memorable † |
| White/pink noise helps ADHD g=+0.249 [0.135, 0.363] and **harms** non-ADHD g=-0.212 [-0.355,-0.069]; 13 studies, N=335; no brown-noise trials exist | [STRONG] on direction, small effect | Nigg et al. 2024, JAACAP, https://pubmed.ncbi.nlm.nih.gov/38428577/ † |
| Distractor interference in ADHD is greatest at *low* perceptual load and vanishes under high load | [MODERATE] lab visual search | Forster & Lavie † |
| **No study of web/page visual clutter in ADHD readers exists.** Closest are an n=3 participatory design study, a driving-dashboard perception study, and an unreviewed preprint | [WEAK] | — |
| Auto-forwarding carousels: content is missed; no auto-advance | [MODERATE] applied | NN/g, https://www.nngroup.com/articles/auto-forwarding/ |
| Short user-triggered animated transitions *improve* graphical perception | [MODERATE] | Heer & Robertson, InfoVis 2007, https://homes.cs.washington.edu/~jheer/files/2007-AnimatedTrans-InfoVis.pdf |

**The minimalism-vs-memorability conflict, resolved:** the studies measure
different outcomes (recall-later vs extract-now) and the moderating variable is
*relevance*, not quantity. Strip anything that carries no meaning; keep or add
anything that carries the message (conclusion title, one annotation, an icon
that *is* the data). Use Tran's task rule: lookup → plain; ratio → unit chart.

## Rule 6 — Time and progress made spatial

| Claim | Grade | Source |
|---|---|---|
| Time perception: less accurate (g>0.40), less precise (g=0.66), tends to overestimate; 27 studies, 1,620 ADHD vs 1,249 controls | [STRONG] | Zheng et al. 2020, https://hub.hku.hk/bitstream/10722/309307/1/content.pdf?accept=1 |
| Perceptual timing deficits are more consistent than *prospective verbal estimation*, where most studies find no group difference | [STRONG] | Marx et al., https://pmc.ncbi.nlm.nih.gov/articles/PMC10697718 |
| Delay discounting d=0.43, I²=7.6% (stable); 21 studies, N=3,913 | [STRONG] | Jackson & MacKillop 2016, https://pmc.ncbi.nlm.nih.gov/articles/PMC5049699 |
| Visible Time Timer lowered anticipatory anxiety (44 children, maths assessment) | [MODERATE] | https://www.ncbi.nlm.nih.gov/pmc/articles/PMC12731990/ |
| "Analog/area display beats digits for ADHD" | [WEAK] — **no study tests it.** Plausible via Barkley's externalisation logic | — |
| Zeigarnik effect (better memory for unfinished tasks) | [MYTH] — meta-analysis of 38 Zeigarnik publications, recall ratio ≈0.99 | Ghibellini & Meier 2025, DOI 10.1057/s41599-025-05000-w † |
| …but the *Ovsiankina* effect (tendency to resume) held up (~67% across 21 publications) | [MODERATE] | same |
| Constant progress bars do **not** reduce drop-off; fast-then-slow bars help; slow-then-fast bars hurt. 32 randomised experiments | [STRONG] | Villar, Callegaro & Yang 2013, https://research.google/pubs/where-am-i-a-meta-analysis-of-experiments-on-the-effects-of-progress-indicators-for-web-surveys/ |
| "The average time for booking an airline ticket is 15-30 minutes" — give a generous time estimate | [WEAK] standard | W3C COGA 4.6.4 |

## Rule 7 — Status without verdict

| Claim | Grade | Source |
|---|---|---|
| Emotion dysregulation g≈1.17 in adult ADHD | [STRONG] | Beheshti 2020 (above) |
| Rejection sensitivity associates with ADHD symptoms cross-sectionally (e.g. β=.47, n=304) | [MODERATE] for the association | https://link.springer.com/doi/10.1007/s00787-014-0560-9 † |
| "RSD" as a named syndrome | not a DSM diagnosis; no direct evidence as a distinct entity | — |
| Overdue lists and streak-breaks deepen self-blame; "gentle streaks" with preserved progress preferred | [MODERATE] qualitative, 22 interviews + 20-person speed-dating | Chen, Meng & Nie, CSCW 2026, https://arxiv.org/abs/2603.17258 |
| Product convergence: Tiimo dropped streaks/scores; Amazing Marvin shows its procrastination count "not to shame you"; Smarter Day has no red overdue; Structured moves missed blocks in one tap | [WEAK] vendor evidence, but 5+ independent products converged | https://www.tiimoapp.com/resource-hub/ai-co-planner-design · https://amazingmarvin.com/for/adhd/ |

## Typography and colour

| Claim | Grade | Source |
|---|---|---|
| WCAG 1.4.12 Text Spacing (AA): content must survive line-height 1.5×, paragraph 2×, letter 0.12×, word 0.16× | normative | https://www.w3.org/WAI/WCAG22/Understanding/text-spacing |
| WCAG 1.4.8 Visual Presentation (AAA): ≤80 characters per line, not justified, line spacing ≥1.5, paragraph spacing ≥1.5× line spacing, 200% resize | normative (AAA) | https://www.w3.org/TR/WCAG22/ |
| WCAG 1.4.3 (AA) 4.5:1 body / 3:1 large; 1.4.6 (AAA) 7:1 / 4.5:1 | normative | https://www.w3.org/WAI/WCAG22/Understanding/contrast-minimum |
| Sans-serif, monospaced and roman beat serif, proportional and italic for dyslexic readers; Helvetica, Courier, Arial, Verdana did well; italic hurt. 48 readers, 12 fonts, eye tracking | [MODERATE] dyslexia, not ADHD | Rello & Baeza-Yates, ASSETS 2013, https://typeset.io/pdf/good-fonts-for-dyslexia-2debffz8dl.pdf |
| Extra-large letter spacing improved dyslexic reading (74 children) | [MODERATE] dyslexia | Zorzi et al. 2012, https://pmc.ncbi.nlm.nih.gov/articles/PMC3396504 |
| ALL CAPS slows continuous reading ~10-19% | [MODERATE] | Tinker 1955; NN/g https://www.nngroup.com/articles/glanceable-fonts/ |
| Dark-on-light gave better acuity and proofreading than light-on-dark | [MODERATE] general adults | Piepenbrock 2013/2014 † |
| Okabe-Ito 8-colour palette as the colour-vision-safe default | [WEAK] convention, widely adopted | Wong, Nature Methods 2011 |
| ~5-7 as a practical categorical colour maximum | [WEAK] rule of thumb, not derived | Healey IEEE Vis '96 †; EU Data Visualisation Guide |
| Position > length > angle > area > colour for encoding accuracy; gridlines improve accuracy but error rises steeply below ~8px spacing | [STRONG] | Cleveland & McGill 1984; replicated Heer & Bostock, CHI 2010, https://homes.cs.washington.edu/~jheer/files/2010-MTurk-CHI.pdf |
| Message-bearing titles determine what readers take away and recall; readers rarely notice slant | [MODERATE] | Kong, Liu & Karahalios, CHI 2018, https://mail.zcliu.cs.umd.edu/vistitles/CHI18-VisTitles.pdf |

## Standards worth knowing

- **W3C COGA, "Making Content Usable"** — https://www.w3.org/TR/coga-usable/ —
  advisory, not normative. Most relevant: Objective 5 "Help Users Focus"
  (4.6.1 Limit Interruptions, 4.6.2 Make Short Critical Paths, 4.6.3 Avoid Too
  Much Content — *"five or less main choices on each screen"*, 4.6.4 Provide
  Information So a User Can Complete a Task); 4.3.3 Clear Page Structure;
  4.4.5 Keep Text Succinct; 4.4.10 Use White Spacing (with the warning not to
  push important content below the fold); 4.5.1 Controls Do Not Move
  Unexpectedly; 4.5.9 Avoid Data Loss; Objective 8 Adaptation. Persona 6.10
  "Yuki (AD(H)D)" is the ADHD persona.
- **WCAG 2.2** — the ADHD-relevant criteria are disproportionately **AAA**
  (1.4.6, 1.4.8, 2.2.4 Interruptions, 2.3.3 Animation from Interactions, 3.1.5
  Reading Level). AA-level ones that matter: 1.4.12, 2.2.1, 2.2.2, 3.2.3,
  3.3.1-3.3.4, and the 2.2 additions 2.4.11, 2.5.8, 3.3.7 Redundant Entry,
  3.3.8 Accessible Authentication. **"WCAG AA compliant" does very little for
  ADHD.**
- **There is no UK Home Office ADHD poster** — the series covers anxiety,
  autistic spectrum, dyslexia, low vision, motor, screen readers. Don't cite one.
- **Microsoft Inclusive Design for Cognition** —
  https://inclusive.microsoft.design/articles/inclusive-design-for-cognition-guidebook
  — *"motivation must equal or surpass cognitive load"*; protect scarce
  attention, minimise interruption cost, support different stimulus levels.
  It explicitly warns against designing around a diagnosis.
- **CSS**: `prefers-reduced-motion` (Baseline since Jan 2020),
  `prefers-contrast` (Baseline since May 2022),
  `prefers-reduced-transparency` (experimental),
  `prefers-reduced-data` (not Baseline, don't rely on it). There is **no**
  standard media feature for "reduce distractions" — provide an in-page toggle.

## Converging product patterns (weak evidence, strong signal)

Five or more ADHD-focused products independently arrived at each of these:

1. **Show one thing, hide the rest** behind an explicit counted control.
2. **Make time spatial** — shrinking area, proportional timeline, visible "now".
3. **Capacity warning before commit** — sum of estimates vs hours available.
4. **Time estimate attached to every item.**
5. **No-shame framing** of missed or overdue work.
6. **Task decomposition with a user-controlled granularity dial.**
7. **Capture separated from scheduling** (one field, nothing mandatory).
8. **Low density, strong defaults, everything optional.**

Best primary reads: https://www.tiimoapp.com/resource-hub/ai-co-planner-design
(13 explicit design decisions including what they deliberately *dropped*) ·
https://www.tiimoapp.com/resource-hub/sensory-design-neurodivergent-accessibility ·
https://amazingmarvin.com/for/adhd/ · https://design.intuit.com/adhd-and-design/

**Counter-evidence worth keeping:** a technical writer who tried chunking,
subheadings and a four-page journey map for ADHD readers reported it didn't
work — *"The journey map, it turned out, didn't make reading happen."* People
read long docs when a task they must finish depends on them.
https://www.idratherbewriting.com/trends/reading-long-documents.html

## Debunked, with verdicts

| Claim | Verdict | Source |
|---|---|---|
| Bionic Reading improves speed/comprehension | [MYTH] | Parker et al. 2026, Royal Society Open Science, n=90: no reading-time reduction, no reliable comprehension gain, 43/90 rated plain text most readable. Springer 2025: fewer words per minute. Možina 2025: no difference. **Never tested on ADHD.** https://link.springer.com/article/10.3758/s13414-025-03067-w |
| OpenDyslexic / Dyslexie help | [MYTH] | Meta-analysis 15 studies, 91 effect sizes, N=688: g=-0.04, 95% CI [-0.15, 0.07], p=0.5. https://strathprints.strath.ac.uk/97732/ · Kuster 2018 n=170 null · Marinus 2016's gain vanished once Arial's spacing was matched — it was *spacing*, not letterforms |
| Lexend improves reading performance | [WEAK/unproven] | One creator-led, unreplicated 2018 dissertation (~20 third-graders). Fine as a wide-spaced sans; not an intervention |
| Coloured overlays / Irlen | [MYTH] | Griffiths et al. 2016 reviewed 51 items: cannot be endorsed; benefits likely placebo/practice/Hawthorne |
| "ADHD can't focus for more than N minutes" | [MYTH] | Time-on-task decrement is small; the deficit is present from task start (Huang-Pollock 2012) |
| "Everyone with ADHD has executive dysfunction" | [MYTH] | EF deficits neither necessary nor sufficient; 83 studies, N=6,703, d=.46-.69. Willcutt 2005, https://pubmed.ncbi.nlm.nih.gov/15950006/ |
| "ADHD brains need bright/stimulating visuals" | [unsupported] | No ADHD-specific colour effect (Tran). Stimulation evidence is auditory, g≈0.25, and harms controls |
| "Brown noise helps ADHD" | [unsupported] | No brown-noise trials exist |
| 7±2 as a UI cap | [MYTH as applied] | ~4 chunks (Cowan), and it governs what's *held*, not what's *displayed* |
| Choice overload / the jam study | [MYTH-ish] | Scheibehenne 2010 meta-analysis: 50 experiments, N=5,036, mean d≈0.02. Chernev 2015 recovers an effect only under four moderators. Use defaults (d=0.68, 58 studies, N=73,675) instead |
| Ego depletion | [MYTH] | 23-lab registered replication, N=2,141, d=0.04 |
| Decoy effect | [MYTH-ish] | Repeated failures with realistic stimuli |
| Hyperfocus as a validated ADHD trait | [WEAK] | Self-report measures only; a 2022 study ties it closer to perseveration than flow |
| "Time blindness" as a clinical entity | [partial MYTH] | The timing deficit is real but modest and task-dependent; it is Barkley's term, not a diagnosis |
| Body doubling efficacy | [WEAK] | HCI reviews find no study clearly demonstrating it |

## Known gaps — do not fill these with confident advice

- No ADHD typography study of any kind.
- No ADHD study of page/web visual clutter, density, dark mode, or line length.
- No controlled direct-label-vs-legend chart experiment, for any population.
- No ADHD evidence for reader modes, focus/line-focus modes, or zen modes
  (Microsoft Immersive Reader's Line Focus is marketed for this; its ESSA
  report is Level IV, rationale only).
- No ADHD evidence on animation as a distractor specifically.
- No evidence on whether progress framing helps or backfires via shame.
- Adult ADHD data are generally thinner than child data; medication is rarely
  modelled; most samples skew male and under 25.
