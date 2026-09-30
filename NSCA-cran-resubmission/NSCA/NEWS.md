# NSCA 0.4.5

## A missing condition value no longer moves that condition's scope

Each condition is fitted by the engines on its own complete pairs, but the
outcome bounds NSCA measured the admissible region against were taken from
every observed outcome. A row with an outcome and no condition value therefore
widened the box on one side of the identity and not on the other. On one such
dataset `admissible_region_share` fell from 0.167 to 0.056, the area identity
missed by 0.11, and `geometry_acceptable` -- and with it `joint_support` --
turned `FALSE`. With two conditions the effect is not confined to edge cases:
missing values in one condition stretched its bounds to the outcome range of
the other, and the identity missed by 0.65. Outcome bounds are now taken per condition, from the
same pairs and the same scope segment the engines use. Complete data is
unaffected.

## Smaller fixes

- `relevance` given as a named character vector, such as
  `c(necessity = "0.1", sufficiency = "0.2")`, stayed character, and the
  support rule then compared effect sizes with thresholds as strings. Named
  input is now reordered after it has been converted to numbers.
- `shared.seed` restored the caller's random stream only when one already
  existed. In a fresh session it left behind the stream it had created, so every
  later draw followed from that seed. It now removes it, as documented.
- `nsca_extract()` said every retired name had been renamed in 0.3.0; five of
  them were renamed in 0.4.0. The warning now names the right release.
- `nsca_extract()` refuses a frontier that was not estimated on every route.
  The `nec:` and `suf:` routes used to return `NA` for it, which looks like a
  missing measurement; the joint route now gives the same "was not estimated"
  message as `nsca_thresholds()` and `nsca_plot()`.

## Examples

CRAN review asked that `\dontrun{}` be kept for code that genuinely cannot
run. The one example that used it, `nsca_extract()`, relied on a `fit` it never
created; it now builds its own model and runs unwrapped.

`nsca()`, `nsca_table()`, `nsca_results()` with its `print()` and `summary()`
methods, `nsca_thresholds()` and `nsca_plot()` with its `plot()` method had no
examples and now have runnable ones. The `nsca()` example keeps the default
1000 permutations, which take longer than CRAN's five-second budget, so it is
wrapped in `\donttest{}`: it still runs under `example()` and under
`R CMD check --as-cran`.

`nsca_reference()`'s example and its list of returned columns had been added
to the `.Rd` file by hand and were missing from the roxygen source, so the next
`roxygen2::roxygenise()` would have dropped them. They are now in the source,
and every help page is regenerated from it.

## Documentation

- The area identity in `?nsca_table` still used the name retired in 0.4.0,
  `data_zone_share`; it now reads `admissible_region_share`.
- `legacy` in `nsca_table()` and `nsca_thresholds()` was documented as
  appending the names of one release each; both append those of 0.3.0 and
  0.4.0.
- Both READMEs installed `NSCA_0.4.3.tar.gz` and described SCAtools as a
  source-only package. SCAtools is on CRAN, so installation is now
  `install.packages("NSCA")`, with the source-tarball route kept below it.

# NSCA 0.4.4

`URL`, `BugReports` and both READMEs pointed at
`youngchanresearcher/NSCA_condition_analysis`. They now point at the package's
repository, <https://github.com/youngchanresearcher/NSCA>. Nothing else changed;
the entry for 0.4.3 below had been relabelled 0.4.4 in that release and is
restored to its own version here.

# NSCA 0.4.3

## Authorship and maintainer

`Authors@R` named a placeholder, `NSCA Contributors <maintainer@example.org>`,
which CRAN rejects: the maintainer must be a real person at a working address.
Both are now Young Chan <youngchanresearcher@gmail.com>.

The package had no `inst/CITATION`, so `citation("NSCA")` fell back to the
entry R generates from `DESCRIPTION`. That entry was correct but silent about
where the numbers come from. A CITATION file now names NSCA, SCAtools and the
NCA method article together, and reads the version from `DESCRIPTION`.

## `region` was retired but never reachable

0.3.0 renamed the threshold-table column `region` to `threshold_status`, and
that rename is printed in the table below, in both READMEs and in the package
guide. The entry was never added to the legacy map, so `nsca_legacy_names()`
omitted it, `nsca_thresholds(legacy = TRUE)` did not append a `region` column,
and `nsca_extract("region")` reported it as an unknown parameter rather than a
retired name — the one thing the retirement policy promises. It is now in the
map, and the test pins the whole documented mapping rather than a sample of it,
so a missing entry cannot pass again.

## Documentation

Both READMEs gained the installation section they never had. NSCA needs NCA and
SCAtools in place first, and the order matters, which is worth stating where a
reader starts.

`DESCRIPTION` gained `URL` and `BugReports`, and both READMEs now link the
repository. SCAtools is released separately, so its own link is given too:

    URL: https://github.com/youngchanresearcher/NSCA_condition_analysis
    BugReports: https://github.com/youngchanresearcher/NSCA_condition_analysis/issues

## The interval and the overlap now follow `scale` (breaking)

`nsca_thresholds()` reported `necessity_threshold` and `sufficiency_threshold`
on the requested `scale`, but `necessity_sufficiency_interval` and
`overlap_width` were left in actual units on every scale. The three columns
print side by side, so a reader who subtracted the two thresholds did not get
the interval: on the default `"percentage.range"` the two thresholds were
percentages of the range and the width between them was in the units of X.
The behaviour was documented -- the help page said the interval is "expressed
in the units of X" and told the reader to normalise -- but the normalised
number was the one the package would not produce, and nothing in the table
said the row mixed two kinds of column.

Both columns are now reported on `scale`, so a row can be checked by
subtraction. `necessity_sufficiency_interval_actual` and `overlap_width_actual`
are new and hold the previous values, whatever `scale` is asked for; the signed
`threshold_gap_actual` is unchanged.

Only the reported magnitude moved. The sign is still settled in actual units
and carried over, because under `convention = "directional"` a low-X direction
is mirrored: taking the sign from the reported difference would swap
`"admissible interval"` and `"frontiers overlap"` in exactly the views where
the mirroring applies. `threshold_status`, `thresholds_compatible` and every
component and joint index are untouched.

Under `scale = "percentile"` the column is a difference of ranks, so it reads
as the share of cases between the two thresholds rather than as a width. The
help page now says so, and points at `"percentage.range"` or `"sd"` for a
normalised width.

To restore the old numbers, read the `_actual` columns or ask for
`scale = "actual"`.

# NSCA 0.4.2

Three fixes found by the first full `R CMD check --as-cran` of the 0.4.x line.
All three predate 0.4.1. No code, argument, column, default or reported
quantity changed.

## `nsca_table()` documentation was malformed (WARNING)

`checkRd` refused `nsca_table.Rd`: `\emph` is not valid inside `\code`. The
formula for `balanced_joint_effect` had been written to the `.Rd` as
`\code{2 \emph{ sqrt(d_nec } d_suf)}` -- a markdown converter had read the two
multiplication signs in `2 * sqrt(d_nec * d_suf)` as emphasis markers and
turned the text between them into `\emph`. The `.Rd` now carries the formula
it was meant to carry. The roxygen source was already correct, so nothing in
`R/results.R` needed changing.

## A test used an argument its expectation does not have (ERROR)

`test-invariants.R` passed `info = name` to `expect_gte()` and `expect_lte()`.
The ordered comparisons take `label` and `expected.label`, not `info`, so
current testthat raises "unused argument" -- which aborts the whole
`test_that()` block rather than failing one expectation. This is why the suite
reported 1 error against 1032 passes: the assertion itself was fine and had
never actually run. Both calls now pass `label`.

The neighbouring `expect_true(..., info = ...)` calls are correct and are left
alone; `expect_true()` does take `info`.

## `README_zh-TW.md` moves into `inst/docs/` (NOTE)

`R CMD check` counts any unrecognised top-level file as a NOTE. From `inst/`
the translation is still shipped and is now reachable from R:
`file.show(system.file("docs", "README_zh-TW.md", package = "NSCA"))`.

## Not fixed, because it is not the package

The "unable to verify current time" NOTE is `R CMD check` failing to reach a
time server. It says nothing about the package and no package change can
remove it. Set `_R_CHECK_SYSTEM_CLOCK_ = 0` before checking to skip it.

# NSCA 0.4.1

## "biconditional" is prose; `biconditional` stays a column name

The article calls the joint claim *necessary-and-sufficient*, and uses
"biconditional" only in contrast: section 2.1 says each component claim "is a
conditional, not a biconditional". Using the same word for the joint claim
therefore reads against the article it follows. All prose now says
necessary-and-sufficient -- the DESCRIPTION, the roxygen documentation, the
code comments, the printed inference notes, and both READMEs.

Nothing was renamed. The `biconditional` column of `nsca_direction_map()` keeps
its name: it holds the joint statement written out, and the short logical word
is the more usable column name. `necessary_and_sufficient_statement` would be
three times as long for the same content, in a data frame whose other columns
are one or two words. Every field name, list element and status value is
unchanged, including the `necessary_and_sufficient` element of
`nsca_results()`.

## OLS is available as a reference line

`"ols"` is an ordinary least-squares regression of the outcome on the
condition, fitted to all the data. It has always been refused as a component,
and still is: it summarises central tendency, not an empty space, so it can be
neither the necessity nor the sufficiency side. Since 0.4.1 it can be drawn
alongside them. Section 6.1 of the framework keeps average-effect and condition
analysis side by side rather than merging them into one number, and cites the
combined importance-performance map as the precedent for showing two logics on
one picture without collapsing them.

- `nsca_analysis()` and `nsca()` gain `reference`. `reference = "ols"` draws
  the regression line on `nsca_plot()` as a dotted grey line, outside the
  component legend, with the caption saying what it is.
- `ceilings` still refuses `"ols"`; the warning now says where the line
  belongs.
- New `nsca_reference()` reports its intercept, slope, R-squared and the
  observations it was fitted to. It is available whether or not the line was
  drawn.
- `nsca_terms()` gains a *central tendency* row.
- No joint index, threshold, p value or support decision is affected. The
  reference line is reported apart from `nsca_table()` for that reason.

Requires SCAtools >= 0.4.1.

# NSCA 0.4.0

This release adopts the vocabulary of *condition analysis in degree*, the
framework that places SCA and NSCA beside NCA. Terminology follows the article;
the package's own names survive only where the article has nothing to say.
Nothing was removed: every retired name still works.

## The admissible region and the necessity-sufficiency interval

The framework names the two quantities 0.3.0 had called the data zone.

- The part of the scope compatible with both components is the **admissible
  region**, so `data_zone_share` becomes `admissible_region_share`.
- For a fixed outcome target, the distance on the X scale between the two
  thresholds is the **necessity-sufficiency interval**, so `data_zone_width`
  becomes `necessity_sufficiency_interval`.

Two `threshold_status` values change with them: `"determinate"` becomes
`"exact correspondence"`, the limiting case in which one X value is both the
minimum required and the sufficient level, and `"data zone"` becomes
`"admissible interval"`.

The documentation now states three things the framework is careful about and
0.3.0 was not. The interval is expressed in the units of X, so it is
scale-dependent and comparable across studies only after normalisation. It is
not a confidence interval and carries no statistical uncertainty. And an
estimated interval of zero is *consistent* with exact correspondence but does
not by itself establish it; a small non-zero interval should be called near
correspondence only when a substantively justified tolerance has been defined.

## Joint support

Evidence for both components is **joint support**, which is a claim about the
content of the joint hypothesis rather than a demand that the components have
identical effects or thresholds.

- `nsca_supported` becomes `joint_support`.
- `conjunction` becomes `joint_support_status`.

## The disjointness check

Section 5.2 of the framework states a consistency check the package did not
implement. Whenever the two frontiers do not cross, the floor lies at or below
the ceiling throughout the scope, so no point belongs to both expected empty
spaces; because each component effect size is that space's share of the same
scope, the two shares cannot sum to more than one. Component effect sizes
summing above one therefore say the frontiers have crossed and the result needs
diagnosis, not that it is unusually strong.

- `component_sum` reports `d_nec + d_suf`, so the framework's own rule can be
  applied directly by any reader.
- `frontiers_crossed` answers the same question from the measurement rather
  than from the sum.

The sum is a proxy for a crossing that is not otherwise available. Here it is
available: `overlap_share` is the area on which the two empty spaces actually
overlap, integrated from the reconstructed frontiers. Deciding from the
measurement avoids the proxy's failure mode, which is that the sum comes from
the engines and the comparison threshold from the geometry, so the two can
disagree in the last few digits on exactly the configuration the check exists
to bless.

That configuration is the perfect diagonal. A step frontier overlaps itself by
construction: between two consecutive observations the upper staircase still
holds the earlier value while the lower one has already moved, so the two
spaces overlap on every tread, exactly `1 / (n - 1)` of the scope summed over
the `n - 1` treads, and a perfect diagonal attains it. That allowance is
subtracted before the comparison. For every smooth frontier the allowance is
zero and any overlap at all is a genuine crossing.

`frontiers_crossed` asks whether the frontiers crossed at all;
`geometry_acceptable` asks whether they crossed by more than
`geometry.max_overlap`. A row can be flagged as crossed and still have
acceptable geometry, which is the useful distinction: one says something
happened, the other says whether it matters.

## Ceiling, floor, and the empty space

The framework distinguishes a **boundary**, the theoretical line separating an
expected empty space from the compatible region, from a **frontier**, that
boundary estimated from data, and names the direction in which Y is bounded a
**ceiling** or a **floor**. The package estimated the right things but never
said which was which, because the argument is called `ceilings` in every
direction.

`nsca_table()`, `nsca_results()`, `nsca_corners()` and `nsca_direction_map()`
now report, in the wording of Table 1:

- `relationship` -- the direction as a claim about the theorised X-Y
  relationship, with the user's own variable names, for example
  `"Higher training for higher score"`. This is a claim about the relationship,
  not about the shape of the estimated frontier, which may be a step function
  or a straight line under any of the four types.
- `necessity_empty_space` and `sufficiency_empty_space` -- what each expected
  empty space would contain.
- `necessity_boundary` and `sufficiency_boundary` -- `"ceiling"` or `"floor"`.
  The two are always one of each, because the corners are diagonally opposite.

`nsca_thresholds()` gains `relationship` and records the `inequality` it used.

## Boundary is no longer an argument name

`nsca_thresholds()` renames its `boundary` argument to `inequality`, matching
SCAtools 0.4.0. A boundary is the theoretical line an empty space is separated
by; it has nothing to do with whether a reported rule uses `>` or `>=`. The old
argument still works and warns.

## Retired names still work

- `nsca_legacy_names()` now covers the 0.4.0 retirements as well as the 0.3.0
  ones.
- `nsca_thresholds()` gains `legacy = TRUE`, which `nsca_table()` already had.
  Each table appends only the retired names whose replacement it carries.
- `nsca_extract()` asked for a dual-threshold column now explains that such a
  column is defined per outcome level and points at `nsca_thresholds()`,
  instead of reporting a name it knows about as unknown.

## Also

- `nsca_terms()` is a new machine-readable glossary linking each term of the
  framework to the name this package uses for it.
- Requires SCAtools >= 0.4.0, whose `sufficiency_threshold` columns this
  release reads. The two packages now use one name for one quantity.

# NSCA 0.3.0

This release adopts the unified conceptual specification: the reported
quantities now follow one geometric identity instead of being defined
separately, the vocabulary names the geometry rather than an interpretation,
and a statistic of both components can be tested properly for the first time.

## The identity everything now follows

The two empty zones and the region between them tile the analytic scope, so by
inclusion and exclusion

    data_zone_share = 1 - d_nec - d_suf + overlap_share

Every other area quantity is derived from this rather than asserted. Two
consequences are new:

- **The bounds are exact and tight.** `data_zone_share >= 0` gives
  `d_nec + d_suf <= 1 + O`, hence `weakest_effect <= 0.5 + O/2` and
  `balanced_joint_effect <= 1 + O`. Both are attained exactly by a perfect
  diagonal, where `O` is `1/(n - 1)`: at n = 40 the observed values are 0.5128
  and 1.0256 against bounds of 0.5128 and 1.0256. Earlier versions could only
  say the excess was "of order 1/n"; it is now an equality, so a reader can
  subtract the discretization instead of allowing for it.
- **Only the coverage is bounded unconditionally.** `data_zone_share` is a
  share of the scope, so it and `joint_empty_zone_coverage` always lie in
  `[0, 1]`; the other two indices are bounded only up to the overlap. That is a
  technical reason to report the coverage next to them, additional to its
  answering a different question.

## New: `reconstruction_error`

The identity has two computational routes. `data_zone_share` and
`overlap_share` are integrated from the frontiers this package reconstructs;
`d_nec` and `d_suf` are reported by the engines from their own internal
frontiers. The residual between the routes is now reported per row.

It is the only available check that the reconstruction still matches what the
engine actually fitted, and it catches the one failure mode a plot cannot: a
wrongly rebuilt frontier still looks like a frontier and still yields plausible
areas. Only the identity refuses to close. This was previously asserted on
fixtures in the test suite; it now runs on the user's own data.

## Renamed columns

Old names remain reachable: `nsca_table(legacy = TRUE)` appends them as
duplicates, `nsca_extract()` accepts them with a warning, and
`nsca_legacy_names()` returns the mapping.

| retired | current |
|---|---|
| `d_nsca` | `weakest_effect` |
| `j_nsca` | `balanced_joint_effect` |
| `determinacy` | `joint_empty_zone_coverage` |
| `undetermined_share` | `data_zone_share` |
| `weaker_side` | `weaker_component` |
| `p_nsca` | `p_nsca_iut` |
| `undetermined_width` (thresholds) | `data_zone_width` |
| `region` (thresholds) | `threshold_status` |

`determinacy` had to go: it named an interpretation the number does not earn,
since a constant outcome scores 1 on it. The region between the frontiers is
the **data zone** — the region observations are still admitted in, which is
not the claim that any were observed there, and the documentation now says so
explicitly. The `threshold_status` value `"undetermined"` is correspondingly
now `"data zone"`.

## New: `p_weakest_perm` and the shared permutation sequence

`shared.test.rep` replaces the engines' two separate permutation runs with one
sequence driving both. Each replication shuffles the outcome once, refits both
engines on that same shuffled frame, and records `d_nec`, `d_suf` and their
minimum together.

This is what makes a p-value for `min(d_nec, d_suf)` meaningful. An
intersection-union rule combines two p-values and needs no assumption about how
they co-vary, but the minimum is a statistic *of* both components and its null
distribution depends on exactly that co-variation. Two independent permutation
runs would discard the constraint that the two empty zones are disjoint subsets
of one scope, and produce a null distribution no dataset could have generated.

When the shared sequence is used, `p_nec` and `p_suf` come from it too, so all
four p-values are mutually consistent; `p_source` reports which regime produced
them. The p-values carry the Phipson-Smyth correction, so none can be zero.

`p_weakest_perm` does **not** replace `p_nsca_iut`, and the documentation now
gives the sharp reason rather than the vague one. Its null is random pairing
alone, which is a *proper subset* of the union null "not necessary or not
sufficient": that union contains states, such as necessary-but-not-sufficient,
which are not random pairing at all. Calibrating against random pairing
therefore carries no level-alpha guarantee over the union. It is a sensitivity
analysis; the intersection-union rule remains the decision.

## New: the support decision

`necessity_supported`, `sufficiency_supported`, `geometry_acceptable` and
`nsca_supported` are reported separately, so a failure says which of the three
requirements failed rather than collapsing into one boolean.

- `relevance` takes pre-specified practical-relevance thresholds for the
  component effect sizes. Support then needs an effect at least that large as
  well as a significant test. Without it, support rests on significance alone
  and `print()` says so rather than letting the reader assume otherwise.
- `geometry_acceptable` requires a non-degenerate axis, a small
  `reconstruction_error`, and frontier overlap no larger than the `1/(n - 1)`
  a step frontier produces by construction. A degenerate row can therefore
  never be reported as supported.

## New: `balance`

`balanced_joint_effect` penalises asymmetry but does not measure it: `(0.50,
0.125)` and `(0.25, 0.25)` both score 0.50. `balance` is the geometric mean
over the arithmetic mean, which is 1 exactly when the two components are equal.
The name of the index promised something only this column delivers.

## Documentation is now generated from one source

Earlier versions carried hand-written `.Rd` files alongside roxygen comments.
Running `devtools::document()` silently replaced the `.Rd` files with roxygen
output and lost their content, and without `Roxygen: list(markdown = TRUE)` the
result rendered literal backticks and `[function()]` links on every help page.
The roxygen blocks are now the single source, the shipped `.Rd` files are
generated from them and carry the roxygen2 header, and markdown is enabled.

## Also

- `print()` separates description from decision: section 3 reports the three
  joint summaries side by side, section 4 the p-values and support flags.
- The `nsca_analysis()` documentation gains sections on matched estimation,
  degenerate scopes and the shared sequence.
- Tests: `test-decision.R` and `test-shared.R` are new. The four tests
  distinguishing the joint indices by curvature, low-end inflation and the
  constant-outcome blind spot are restored.

# NSCA 0.2.0

- Adds `j_nsca = 2 * sqrt(d_nec * d_suf)` to `nsca_table()`, the balanced joint
  strength. It penalises asymmetry rather than only tracking the weaker side:
  at an equal component sum of 0.80, `(0.40, 0.40)` gives 0.80 while
  `(0.70, 0.10)` gives 0.529.

  The doubling is normalisation, not a new assumption. The two empty zones are
  disjoint subsets of one scope box, so `d_nec + d_suf <= 1`, and by AM-GM
  `sqrt(d_nec * d_suf) <= (d_nec + d_suf)/2 <= 0.5`. The index therefore spans
  `[0, 1]` and reaches 1 exactly when both components equal 0.5, which is the
  single-line case where the two frontiers coincide and the band closes. It is
  left unclamped so that the step frontier's `1/n` self-overlap stays visible.

- Adds `nsca_joint()`, which exposes the whole power-mean family that `min`,
  the geometric mean and the sum belong to, at `p = -Inf`, `p = 0` and `p = 1`.

  This is a correction to how earlier versions framed the choice. `d_nsca` was
  described as though the minimum were the uniquely correct joint effect size
  and the alternatives were mistakes. They are not: they are the same family at
  different degrees of compensation, and nothing in the geometry of two empty
  zones fixes the degree. The documentation now names the parameter instead of
  hiding a choice inside a formula.

- The geometric mean is documented as **partially compensatory**, not as a
  non-compensatory logical AND. A larger component does raise `j_nsca`, at a
  diminishing rate. `min` is the fully non-compensatory end of the family and
  the sum the fully compensatory end; `j_nsca` sits between them, and saying
  otherwise would overstate what a geometric mean does.

- No magnitude benchmarks are asserted for `d_nsca`, `j_nsca` or
  `determinacy`, and the documentation now says why rather than staying silent.
  Conventions for a single NCA effect size do not transfer: the scales differ,
  and under independence both components carry a positive finite-sample bias
  that `j_nsca` amplifies rather than cancels, by an amount depending on `n`,
  the frontier technique and the scope. Calibration is a simulation question.

- Adds a `degenerate` column and a warning at analysis time for an axis that
  does not vary inside the declared scope. This is the blind spot all three
  area summaries share: a constant outcome at the centre of an imposed scope
  leaves the upper and lower halves both empty, so `d_nec = d_suf = 0.5`, the
  band closes, and every index reports a perfect biconditional for data with no
  information in it. `j_nsca` is the most exposed of the three, since `d_nsca`
  at least follows the weaker side when the constant sits off centre. A second,
  milder warning fires when the observations span less than five per cent of
  the declared scope on either axis.

  The other thing called a horizontal line — a flat frontier over a filled
  scope — needs no special rule and gets none. There is no empty zone on either
  side, so every index is correctly zero.

- `print()` and `summary()` now show all three summaries side by side with a
  one-line statement of what each measures, and say that no benchmarks exist.

# NSCA 0.1.2

- Adds a `determinacy` column to `nsca_table()`, equal to
  `1 - undetermined_share`: the share of the scope the two claims between them
  rule out. It is the complement of the band rather than the raw sum
  `d_nec + d_suf`, which measures the same thing but inherits the step
  frontier's own overlap and can exceed 1.

  It exists because `d_nsca` and the band answer different questions and
  neither dominates the other. `d_nsca` is depressed by curvature: a perfect
  bijection such as `Y = X^3` scores 0.25 because the sufficiency zone it
  leaves really is small. `determinacy` is curvature-neutral and reaches 1 for
  `Y = X` and `Y = X^3` alike, but it is inflated at the low end, where a loose
  scatter already scores about 0.2.

  `determinacy` is not an effect size and the documentation says so. A constant
  outcome closes the band completely and scores 1 while satisfying neither
  component. The permutation screen is what rules that out, not the index.

# NSCA 0.1.1

- `nsca_results()` and the print/summary methods now expose all three requested
  layers explicitly: necessity, sufficiency, and the joint NSCA table.
- The joint table now reports `p_nsca = max(p_nec, p_suf)` and matching
  component/joint significance flags. Documentation now states the correct
  scope of this conjunction: the underlying NCA p-values test a
  random-pairing/independence null and do not directly prove biconditionality.
- Low-X (`LH` and `LL`) threshold gaps now use the reversed condition-axis
  ordering. Negative gaps are reported as frontier overlap instead of being
  silently changed to zero.
- `.nsca_band()` now returns the documented `crossed` flag, fixing the package's
  own failing geometry test.
- The multi-condition regression test now uses an actually independent `X2`.
  The earlier visual test used `sort(runif(n))`, which made `X2` monotonically
  related to the already ordered outcome and invalidated the expected wide-band
  check.
- Declared ggplot2 data-mask variables remove the R CMD check NOTE.

# NSCA 0.1.0

First release.

- `nsca_analysis()` estimates both components of a biconditional statement on
  diagonally opposite corners of the same scatter plot: necessity through
  `NCA`, sufficiency through `SCAtools`.
- `nsca_table()` reports both component effect sizes, the descriptive joint index,
  the undetermined share of the scope, and an intersection-union verdict.
- `nsca_thresholds()` gives dual thresholds and the undetermined region for
  every outcome level.
- `nsca_plot()` draws both frontiers and shades the undetermined band.
- `nsca_corners()` and `nsca_direction_map()` expose the corner mapping.

## Design decisions worth knowing

- **The descriptive joint index is the weaker component.**
  `min(d_nec, d_suf)` falls to zero as soon as either component does and stays
  on the components' own scale. The undetermined band remains separate because
  it answers how much of the scope neither claim rules out.
- **The 0.5 ceiling is approximate for step frontiers.** The two empty zones
  are disjoint only where the frontiers do not invert. A step frontier overlaps
  itself by about one tread between consecutive observations, inflating both
  components by a term of order `1/n`: a perfect diagonal through 40 points
  gives 0.513. `overlap_share` reports the overlap, and the frontier-inversion
  warning fires only for smooth frontiers, where an inversion means something.
- **A constant outcome is refused, not scored.** It has no scope of its own, so
  every empty area would be divided by zero height. With a theoretical scope
  imposed the analysis runs, but the answer depends on where the constant sits
  inside that scope, so the error says so.
- **Both sides always use the same frontier technique.** The descriptive joint index
  is a comparison, and envelopment and regression frontiers do not measure the
  same thing. Mixing them across sides would let the technique decide which
  side looks weaker. Several techniques may be requested; each produces its own
  internally matched row.
- **Plots are exempt.** A chart compares nothing numerically, so `nsca_plot()`
  draws every requested technique on both sides.
- **The conjunction requires both directional screens.** Their maximum p-value
  passes only if both reject. Because the underlying NCA permutation null is
  random pairing/independence, this does not directly establish logical or
  causal biconditionality.
- **The engine's purity metrics are off by default.** It computes them only
  for a corner with neither axis flipped. Of the two corners an NSCA model
  uses, at most one qualifies, and which one depends on the direction: `HH`
  gets them on the necessity side, `LL` on the sufficiency side, `LH` and `HL`
  on neither. They are also expensive when many observations sit on the
  frontier, which is exactly the near-deterministic case NSCA is about. No
  NSCA statistic uses them. Set `purity = TRUE` to compute them anyway.
- **`ols` is refused.** It estimates central tendency, not an empty space, so
  it yields no component of either claim.
