# Analytical Design, Sparklines, and Layering

Extends `tufte-principles.md` with material from *Envisioning Information* (1990), *Visual Explanations* (1997), and *Beautiful Evidence* (2006).

---

## 1. The Six Principles of Analytical Design

From *Beautiful Evidence*. The most actionable framework Tufte produced — applies to any analytical presentation, not just charts.

1. **Show comparisons, contrasts, differences**
   The fundamental analytical act. Every display should answer "compared to what?"

2. **Show causality, mechanism, structure, explanation**
   Move beyond description. What's the *why* behind the pattern?

3. **Show multivariate data — more than 1 or 2 variables**
   Real problems are multivariate. Reducing to a single variable hides interactions.

4. **Completely integrate words, numbers, images, diagrams**
   Don't segregate by mode. Labels next to the data they describe; equations next to the curves they generate.

5. **Thoroughly describe the evidence**
   Provenance, authorship, scales, sources, measurements. Documentation enables trust.

6. **Analytical presentations ultimately stand or fall depending on the quality, relevance, and integrity of their content.**
   No amount of design fixes weak evidence. Content is paramount.

**Use in critique:** walk through all six. The lowest-scoring principle is usually the biggest improvement opportunity.

---

## 2. Sparklines

Word-sized, data-intense graphics. Tufte's signature *Beautiful Evidence* invention.

**Defining properties:**
- Typographic resolution — sized like text, embedded inline with prose or tables
- No axes, no labels, no decoration
- Endpoints often marked (start/end values, or min/max)
- Reveals shape, trend, variability at a glance

**Design rules:**
- Height ≈ x-height of surrounding text (~14-20px)
- Length ≈ a word or short phrase
- Use a single red/colored dot to flag a key point (current value, anomaly)
- Pair with the most recent numeric value: `120 ▁▂▃▅▇▇▆▅ 132`
- Stack in tables so eyes can scan vertically

**When to use:**
- Dashboards with many metrics (one row per metric: name | sparkline | current | delta)
- Inline prose: "revenue trended up ▁▂▄▆▇ over the quarter"
- Anywhere a full chart would dominate but trend matters

**When not to use:**
- When precise readings matter — sparklines show shape, not value
- For categorical or part-to-whole data

---

## 3. Layering and Separation

From *Envisioning Information*. The most useful concept for dense displays.

**The principle:** Visually distinct elements can coexist in the same space if they're *layered* — separated by value, weight, hue, or transparency rather than spatial isolation.

**Techniques:**
- **1+1=3 effect:** two heavy lines next to each other create a phantom third line. Lighten one to suppress this.
- **Hierarchy by weight:** primary data in dark/saturated; secondary in light gray; annotations even lighter.
- **Color for separation, not decoration:** distinct hues let overlapping data remain readable.
- **Whisper, don't shout:** grids, axes, reference lines should fade into the background — present but unobtrusive.

**Test:** squint at the graphic. The most important data should remain visible; chartjunk should disappear first.

---

## 4. Micro/Macro Design

Distinct from raw data density. A micro/macro graphic reveals **different stories at different viewing distances**.

- **Macro view** (zoomed out, peripheral): overall pattern, shape, trend
- **Micro view** (close inspection): individual data points, labels, exceptions

**Canonical examples:**
- Vietnam Memorial: macro = sweep of names; micro = a single name
- Galaxy maps: macro = structure; micro = individual stars
- Financial tables with sparklines: macro = which rows trended up; micro = the actual values

**Design implication:** don't choose between overview and detail — show both simultaneously by layering.

---

## 5. Escaping Flatland

The 2D page/screen is inherently flat; good information design adds dimensions *without* 3D gimmicks.

**Dimensions you can add on flat media:**
- Color (categorical or sequential)
- Size (continuous)
- Shape (categorical)
- Position (2-3 axes via projection)
- Time (small multiples, animation, or sparkline-style inline series)
- Layering (foreground/background via value)

**Anti-pattern:** 3D bar charts, pie charts with depth, isometric projections that distort proportions. These add visual dimension without adding information dimension — pure chartjunk.

---

## 6. Range-Frame and Dot-Dash Plot

Tufte's signature reinventions of standard chart elements. Direct applications of data-ink maximization.

**Range-frame:**
- Replace the full axis with a line that spans only the *range of actual data*
- Axis ends at min/max values, not arbitrary round numbers
- Tells the viewer the data extent without explicit annotation

**Dot-dash plot:**
- Scatter plot where the axes are replaced by marginal rug plots
- Each axis becomes a 1D distribution of the data on that variable
- Same ink, more information — the axes now show marginal density

**Pattern:** every standard chart element (axis, tick, gridline) can be redesigned to carry data.

---

## 7. Confections, Parallelism, Narrative

From *Visual Explanations*.

**Confections:** assemblages of disparate visual elements (images, maps, text, diagrams) into a single explanatory composition. Examples: Minard's Napoleon march, Snow's cholera map, exploded technical illustrations. They work when each element serves the argument.

**Parallelism:** repetition of visual structure to enable comparison — small multiples are one form, but parallelism extends to side-by-side maps, before/after states, repeated annotation styles.

**Narrative graphics of space and time:** combine spatial and temporal dimensions in one frame. Minard's Napoleon graphic encodes troop size, geography, direction, temperature, and time simultaneously.

---

## 8. Cause and Effect

From *Visual Explanations*. Causality is hard to visualize because it requires showing both the variables and the mechanism linking them.

**Techniques:**
- Show the intervention and the response in the same frame
- Annotate the causal mechanism directly on the data
- Use sequence (small multiples through time) to imply mechanism
- Pair the data display with a process diagram showing the proposed cause

**Worked example:** Challenger O-ring decision. The available data, plotted against temperature, showed catastrophic risk — but the engineers presented it in a way that hid the causal relationship. Tufte's redesign makes the causality unavoidable.

---

## Quick Reference: Extended Tufte Test

After applying the standard 7-question test in `tufte-principles.md`, add:

8. **Comparison:** Does the graphic answer "compared to what?"
9. **Causality:** Is the mechanism or explanation visible, not just the pattern?
10. **Multivariate:** Are interactions among variables shown, or has the problem been over-reduced?
11. **Integration:** Are words, numbers, and images interleaved — or segregated?
12. **Documentation:** Can a stranger evaluate the evidence (sources, scales, authorship)?
13. **Layering:** Do important elements dominate; do secondary elements recede?
14. **Micro/macro:** Does the display reward both a glance and a close read?
