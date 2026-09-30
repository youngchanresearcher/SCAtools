# NSCA

Necessary and Sufficient Condition Analysis: bivariate statements in which a
condition is both necessary and sufficient for an outcome.

Source and issue tracker:
<https://github.com/youngchanresearcher/NSCA>

## Installation

NSCA delegates necessity to NCA and sufficiency to SCAtools. All three are on
CRAN, and installing NSCA from CRAN brings the other two with it:

```r
install.packages("NSCA")
```

To install from a source tarball instead, put the dependencies in place first,
because `repos = NULL` does not resolve them:

```r
install.packages(c("NCA", "SCAtools", "ggplot2"))
install.packages("NSCA_0.4.5.tar.gz", repos = NULL, type = "source")
```

SCAtools lives in its own repository:
<https://github.com/youngchanresearcher/SCAtools>

Requires R (>= 3.5.0), NCA (>= 5.0.2) and SCAtools (>= 0.4.1).

## What it estimates

A necessary-and-sufficient statement fixes two empty corners at once. Writing sufficiency
as X implies Y and necessity as Y implies X, and contraposing each, puts the
two empty regions on diagonally opposite corners of the same scatter plot:

| NSCA number | Direction | Sufficiency corner | Necessity corner |
|---|---|---|---|
| 1 | `HH` | 4 lower-right | 1 upper-left |
| 2 | `LH` | 3 lower-left | 2 upper-right |
| 3 | `HL` | 2 upper-right | 3 lower-left |
| 4 | `LL` | 1 upper-left | 4 lower-right |

The NSCA number is a logical direction label; it is not a single physical
corner. For example, NSCA 1 (`HH`) requires physical corners 1 and 4.
If High/Low are exact complements, `HH` and `LL` describe the same
necessary-and-sufficient pattern, while `LH` and `HL` describe the other one. They should
not be counted as four independent hypothesis tests.

One frontier always bounds the point cloud from above and the other from below.
What lies between them is the region neither claim rules out.

```r
fit <- nsca_analysis(dat, "X", "Y", direction = "HH",
                     ceilings = c("ce_fdh", "cr_fdh"), test.rep = 1000)
fit
nsca_results(fit)        # necessity, sufficiency, and joint tables
nsca_thresholds(fit)
nsca_plot(fit)
```

## One identity behind every area

The two empty zones and the region between them tile the analytic scope, so by
inclusion and exclusion

```
admissible_region_share = 1 - d_nec - d_suf + overlap_share
joint_empty_zone_coverage = d_nec + d_suf - overlap_share = 1 - admissible_region_share
```

Everything else follows from this rather than being defined separately, and two
things follow immediately.

**The bounds are exact, and tight.** `admissible_region_share >= 0` gives
`d_nec + d_suf <= 1 + O`, hence

```
weakest_effect        <= 0.5 + O/2
balanced_joint_effect <= 1   + O
```

Both are *attained* by a perfect diagonal, where `O = 1/(n - 1)`. At n = 40 the
observed values are 0.5128 and 1.0256 against bounds of 0.5128 and 1.0256. The
excess is discretization, not evidence, and because it is an equality rather
than an order of magnitude you can subtract it instead of allowing for it.

**Only the coverage is bounded unconditionally.** `admissible_region_share` is a share
of the scope, so it and `joint_empty_zone_coverage` always lie in `[0, 1]`. The
other two are bounded only up to the overlap, which for a step frontier is
never exactly zero.

### `reconstruction_error`

The identity has two computational routes. `admissible_region_share` and
`overlap_share` are integrated from the frontiers this package rebuilds;
`d_nec` and `d_suf` are reported by the engines from their own. The residual
between the routes is reported per row.

It is the only available check that the reconstruction still matches what the
engine fitted, and it catches the one failure mode a plot cannot: a wrongly
rebuilt frontier still looks like a frontier and still yields plausible areas.
Only the identity refuses to close.

## Three summaries of the same two components

`d_nec` and `d_suf` are primary. Everything else is a way of putting them
together, and `nsca_table()` reports three such ways rather than presenting one
as *the* joint effect size, because nothing in the geometry of two empty zones
selects one.

| | what it asks | max | compensation | weakness |
|---|---|---|---|---|
| `weakest_effect` = `min(d_nec, d_suf)` | how strong is the weaker claim | about 0.5 | none | depressed by curvature |
| `balanced_joint_effect` = `2*sqrt(d_nec*d_suf)` | how strong **and** how balanced | 1 | partial | depressed by curvature, less so |
| `joint_empty_zone_coverage` = `1 - admissible_region_share` | how close to a deterministic function | 1 | full | inflated at the low end |

### `min`, the geometric mean and the sum are one family

They are the power means at `p = -Inf`, `p = 0` and `p = 1`, so the choice
between them is a choice of **how much compensation to allow**, not a choice
between a right and a wrong formula. `nsca_joint()` exposes the family:

```r
nsca_joint(0.40, 0.40, p = 0)     # 0.800  balanced
nsca_joint(0.70, 0.10, p = 0)     # 0.529  same sum, penalised for asymmetry
nsca_joint(0.40, 0.40, p = 1)     # 0.800  the sum cannot tell them apart:
nsca_joint(0.70, 0.10, p = 1)     # 0.800  that is the objection to it
```

The geometric mean is **partially compensatory**. A larger component does raise
it, at a diminishing rate, and it still collapses to zero if either component
does. Calling it a non-compensatory logical AND would overstate it; `min` is
the fully non-compensatory end of the same family.

### Why doubling is normalisation, not an assumption

For `p <= 1` the power mean is at most `(d_nec + d_suf)/2`, and the two empty
zones are disjoint subsets of one scope box, so `d_nec + d_suf <= 1` and the
average is at most 0.5. `balanced_joint_effect` therefore spans `[0, 1]`, and it reaches 1
**exactly** when `d_nec = d_suf = 0.5`: the single-line case, where the two
frontiers coincide and the band closes.

**Step frontiers pass slightly beyond the cap.** Between two consecutive
observations the two staircases cross, so the empty zones overlap by about one
tread and both components are inflated by a term of order `1/n`. A perfect
diagonal through 40 points gives `weakest_effect = 0.513` and `balanced_joint_effect = 1.03`. That
excess is reported in `overlap_share`, and the indices are left unclamped so it
stays visible rather than being tidied away.

### Where the three disagree

Curvature is the interesting case. `Y = X`, `Y = X^3` and `Y = X^(1/3)` are all
exact bijections, so all three are perfectly necessary and sufficient:

| | `d_nec` | `d_suf` | `weakest_effect` | `balanced_joint_effect` | `joint_empty_zone_coverage` |
|---|---|---|---|---|---|
| `Y = X` | 0.50 | 0.50 | 0.50 | 1.00 | 1.00 |
| `Y = X^3` | 0.75 | 0.25 | 0.25 | 0.87 | 1.00 |
| `Y = X^(1/3)` | 0.25 | 0.75 | 0.25 | 0.87 | 1.00 |

These are the limiting values on the unit square. A finite `ce_fdh` sample adds
the `1/n` staircase term to each component, so 150 points on `[0.001, 1]` give
0.5034 / 0.5034 for the diagonal and 0.7531 / 0.2536 for the cube.

None of these is an error. `joint_empty_zone_coverage` asks whether Y is pinned down by X,
and it is. The other two ask how strong the two claims are in the components'
own area metric, where a lopsided curve genuinely leaves one empty zone small.
The reverse case runs the other way: a loose diagonal scatter scores about 0.17
on `joint_empty_zone_coverage` where `weakest_effect` gives 0.02, because `joint_empty_zone_coverage` is roughly
the sum and is inflated at the low end.

| `weakest_effect` / `balanced_joint_effect` | `joint_empty_zone_coverage` | reading |
|---|---|---|
| high | high | strong necessary-and-sufficient |
| low | high | deterministic but lopsided; one side carries little content |
| low | low | loose relation |
| anything, `degenerate` true | | an axis does not vary |
| anything, p not significant | | no evidence against random pairing |

### No magnitude benchmarks

None are supplied for any of the three, and this is deliberate rather than an
omission. Conventions for a single NCA effect size do not transfer: the scales
differ, and under independence both components carry a positive finite-sample
bias that `balanced_joint_effect` amplifies rather than cancels, by an amount depending on
`n`, the frontier technique and the scope. Calibrate by simulation on the
design at hand before calling a value large or small.

## Two things called a horizontal line

They land in opposite places, and only one of them is a problem.

**A flat frontier over a filled scope.** X and Y both spread across the scope
and the ceiling sits flat against its top edge. There is no empty zone on
either side: `d_nec = d_suf = 0`, the band is the whole scope, and every index
is zero. Correct, and no special rule is needed.

**A flat data cloud: Y constant.** With no theoretical scope this is refused
outright, because a constant outcome has no scope of its own and every empty
area would be divided by zero height. With a scope imposed it runs, warns, and
is flagged `degenerate` — because the answer is now maximal rather than zero.
A constant at the centre of a unit scope leaves the upper half and the lower
half both empty, so `d_nec = d_suf = 0.5`, the band closes, and all three
indices report perfect joint support for data carrying no information. Move
the same line to `Y = 0.8` and `balanced_joint_effect` drops to 0.80 although nothing about
the data changed. That sensitivity is the tell.

This is the blind spot all three area summaries share, and `balanced_joint_effect` is the most
exposed of them, since `weakest_effect` at least follows the weaker side once the
constant sits off centre. Two things cover it: the `degenerate` flag, and the
permutation screen — permuting a constant outcome changes nothing, so every
resample reproduces the observed geometry and both p-values go to 1.

## Testing the conjunction

`p_nsca_iut = max(p_nec, p_suf)` passes only when **both** directional
component tests reject. This is the intersection-union rule: it is level-alpha
without any assumption that the two component p-values are independent, so no
correction is needed merely for combining two pre-specified tests. Multiplicity
across several conditions or frontier techniques is a separate matter.

It is also typically **conservative**, since the maximum of two p-values is
stochastically larger than either. An NSCA design therefore needs more
observations than either component alone — worth planning for rather than
discovering.

The scope of the result matters: the permutation test breaks the X-Y pairing
and tests a random-pairing/independence null. `p_nsca_iut` is a conjunction
screen for two directional empty-space tests, not a direct test of the full
logical null "not necessary or not sufficient", and not proof of a causal or
deterministic necessary-and-sufficient relation.

### The shared permutation sequence

`shared.test.rep` replaces the engines' two separate permutation runs with one
sequence driving both. Each replication shuffles the outcome once, refits both
engines on that same shuffled frame, and records `d_nec`, `d_suf` and their
minimum together.

```r
fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                     shared.test.rep = 999, shared.seed = 1)
```

This is what makes `p_weakest_perm` meaningful. The intersection-union rule
combines two p-values and needs no assumption about how they co-vary, but
`min(d_nec, d_suf)` is a statistic **of** both components and its null
distribution depends on exactly that co-variation. Two independent permutation
runs would throw away the constraint that the two empty zones are disjoint
subsets of one scope, and produce a null distribution no dataset could have
generated.

When the shared sequence is used, `p_nec` and `p_suf` come from it as well, so
all four p-values are mutually consistent; `p_source` says which regime
produced them. They carry the Phipson-Smyth correction, so none can be zero.

`p_weakest_perm` does **not** replace `p_nsca_iut`. Its null is random pairing
alone, which is a *proper subset* of the union null: that union contains states
such as necessary-but-not-sufficient which are not random pairing at all, so
calibrating against random pairing carries no level-alpha guarantee over it.
Treat it as a sensitivity analysis; the intersection-union rule is the
decision.

## The support decision

Four flags rather than one, so a failure says which requirement failed.

| flag | requires |
|---|---|
| `necessity_significant` | `p_nec <= alpha` |
| `necessity_supported` | significant, and `d_nec >= relevance` if one was set |
| `geometry_acceptable` | not degenerate, small `reconstruction_error`, overlap within the step frontier's own `1/(n - 1)` |
| `joint_support` | both components supported **and** acceptable geometry |

```r
fit <- nsca_analysis(dat, "X", "Y", test.rep = 1000,
                     relevance = c(necessity = 0.10, sufficiency = 0.10))
```

`relevance` is the pre-specified practical-relevance threshold. Without it,
support rests on significance alone, and `print()` says so rather than letting
you assume otherwise. A degenerate row can never be reported as supported.

## Names retired in 0.3.0 and 0.4.0

Old names remain reachable: `nsca_table(legacy = TRUE)` and
`nsca_thresholds(legacy = TRUE)` append them, `nsca_extract()` accepts them
with a warning, the retired arguments warn but still work, and
`nsca_legacy_names()` returns the mapping.

| retired | current | table |
|---|---|---|
| `d_nsca` | `weakest_effect` | joint |
| `j_nsca` | `balanced_joint_effect` | joint |
| `determinacy` | `joint_empty_zone_coverage` | joint |
| `weaker_side` | `weaker_component` | joint |
| `p_nsca` | `p_nsca_iut` | joint |
| `undetermined_share`, `data_zone_share` | `admissible_region_share` | joint |
| `nsca_supported` | `joint_support` | joint |
| `conjunction` | `joint_support_status` | joint |
| `undetermined_width`, `data_zone_width` | `necessity_sufficiency_interval` | thresholds |
| `region` | `threshold_status` | thresholds |
| `boundary` (argument) | `inequality` | thresholds |

0.3.0 retired names that stated an interpretation the number does not earn:
`determinacy` scores 1 for a constant outcome.

0.4.0 adopts the vocabulary of *condition analysis in degree*. The part of the
scope compatible with both components is the **admissible region** — the region
observations are still *admitted* in, which is not the claim that any were
*observed* there. The distance on the X scale between the two thresholds at one
outcome target is the **necessity-sufficiency interval**. Evidence for both
components is **joint support**. And a **boundary** is the theoretical line an
expected empty space is separated by — the thing a **frontier** estimates — so
the argument controlling the strictness of a reported inequality is now called
`inequality`.

`nsca_terms()` lists the whole vocabulary beside the name this package uses for
each term.

## Three reported analyses

`print(fit)`, `summary(fit)`, and `nsca_results(fit)` report:

1. the Necessary Condition Analysis table;
2. the Sufficiency Condition Analysis table; and
3. the joint NSCA table, including `weakest_effect`, `balanced_joint_effect`, `joint_empty_zone_coverage`,
   `p_nsca_iut`, the `degenerate` flag, and the joint-support decision.

The third table is meaningful only as a combination of the first two; it is not
a third independent data analysis.

## One frontier technique per row

The descriptive joint index compares two empty areas. A comparison is only
interpretable if both sides were measured the same way. Envelopment frontiers
(`ce_fdh`, `ce_vrs`) hug the data and produce the largest empty areas;
regression frontiers (`cr_fdh`, `cr_vrs`, `cols`, `qr`, `c_lp`) cut into them.
On the same 80 points the same side scores 0.298 to 0.400 depending only on the
technique. Mixing techniques across sides therefore decides which side looks
weaker by the choice of technique rather than by the evidence.

So `ceilings` applies to both sides, and several techniques give several rows,
each internally matched. Comparing rows is a sensitivity analysis; comparing
across sides within a row is impossible by construction.

`nsca_plot()` is exempt. A chart compares nothing numerically, so every
requested technique is drawn on both sides.

## Dual thresholds

For each outcome level the two components mark out three regions on the
condition axis. For a high-X direction:

| Region | Meaning |
|---|---|
| below `necessity_threshold` | the level is out of reach |
| between the two | possible, but not guaranteed |
| above `sufficiency_threshold` | guaranteed by the fitted frontier |

For a low-X direction the inequalities reverse: values above the necessity
threshold are out of reach, while values below the sufficiency threshold are
guaranteed. `threshold_gap_actual` is direction-oriented, so a positive value
means an admissible interval in all four directions. A negative value is
reported as `frontiers overlap` rather than silently truncated to zero.

`necessity_sufficiency_interval` is the width of the middle region: the
distance on the X scale between the two thresholds at that outcome target. It
collapses to zero where the relation is deterministic, which `threshold_status`
reports as `exact correspondence`. Neither NCA nor SCA alone produces this
table: each supplies one edge.

## Performance

The engine's purity metrics are off by default (`purity = TRUE` turns them
on). It computes them only for a corner with neither axis flipped, so at most
one side of an NSCA model can have them and which side depends on the
direction. They are also slow when many observations lie on the frontier,
which is precisely the near-deterministic case this package exists to
describe: a perfect diagonal makes every point a frontier point.

## Interpretation

- Effect sizes are areas relative to the declared scope, and depend on it.
- Area-based measures are not invariant to monotone rescaling of either
  variable. This is inherited from NCA and applies to every quantity here.
- A permutation test asks whether an empty zone this large is unusual once the
  X-Y pairing is broken. It does not establish a mechanism.
- An empty-space pattern is not proof of a deterministic necessary-and-sufficient relation.
  Temporal order, design, measurement quality, scope and out-of-sample
  validation still matter.

## Attribution

Necessity is computed by NCA (Jan Dul, Govert Buijs), sufficiency by SCAtools.

- Dul, J. (2016). Necessary Condition Analysis (NCA): Logic and methodology of
  "necessary but not sufficient" causality. *Organizational Research Methods,
  19*(1), 10-52. https://doi.org/10.1177/1094428115584005
- Berger, R. L. (1982). Multiparameter hypothesis testing and acceptance
  sampling. *Technometrics, 24*(4), 295-300.

## The reference line: OLS is drawn, never counted

`"ols"` is refused in `ceilings` and always has been: it estimates central
tendency, not an empty space, so it can be neither the necessity nor the
sufficiency component. Since 0.4.1 it can still be drawn:

```r
fit <- nsca_analysis(
  dat, "X", "Y", direction = "HH",
  ceilings = "ce_fdh",
  reference = "ols"
)

nsca_reference(fit)   # intercept, slope, R-squared
nsca_plot(fit)        # dotted grey line, outside the component legend
```

Nothing about the two components changes: the reference line enters no joint
index, no threshold, no p value and no support decision. It is drawn because
average-effect and condition analysis answer different questions about the same
relationship, and showing them together is more informative than merging them
into one number.

---

The Traditional Chinese version of this README is installed with the package:

```r
file.show(system.file("docs", "README_zh-TW.md", package = "NSCA"))
```
