# Joint results: component effects, the three joint summaries, the admissible region,
# the geometry diagnostics and the joint-support decision.

# Conjunction rule for the two directional component tests. Requiring both to
# pass is an intersection-union combination, which is level-alpha without any
# assumption that the two are independent. The underlying null is random
# pairing, so this remains an empirical screen rather than a direct test of
# the full logical null "not necessary or not sufficient".
.nsca_verdict <- function(p_nec, p_suf, alpha, tested) {
  if (!isTRUE(tested)) {
    return("not tested")
  }
  if (!is.finite(p_nec) || !is.finite(p_suf)) {
    return("not tested")
  }
  if (p_nec <= alpha && p_suf <= alpha) {
    return("supported")
  }
  if (p_nec > alpha && p_suf > alpha) {
    return("neither component")
  }
  if (p_nec > alpha) "necessity fails" else "sufficiency fails"
}

# Names retired in 0.3.0 and 0.4.0, mapped to their replacements. Kept
# reachable rather than merely documented, because a renamed column that
# silently returns NULL turns a working script into a wrong one instead of a
# broken one.
.nsca_legacy_columns <- c(
  # 0.3.0: name the geometry rather than the interpretation.
  d_nsca = "weakest_effect",
  j_nsca = "balanced_joint_effect",
  determinacy = "joint_empty_zone_coverage",
  weaker_side = "weaker_component",
  p_nsca = "p_nsca_iut",
  # 0.4.0: adopt the vocabulary of condition analysis in degree.
  undetermined_share = "admissible_region_share",
  data_zone_share = "admissible_region_share",
  data_zone_width = "necessity_sufficiency_interval",
  undetermined_width = "necessity_sufficiency_interval",
  nsca_supported = "joint_support",
  conjunction = "joint_support_status",
  # `region` was renamed in 0.3.0 along with the rest of this block, and the
  # rename is listed in NEWS and in both READMEs, but the entry was never added
  # here -- so the one promise the retirement policy makes, that an old name
  # still leads somewhere, did not hold for it until 0.4.3.
  region = "threshold_status",
  boundary = "inequality"
)

# The release that retired each name, for the nsca_extract() warning.
.nsca_legacy_since <- c(
  d_nsca = "0.3.0", j_nsca = "0.3.0", determinacy = "0.3.0",
  weaker_side = "0.3.0", p_nsca = "0.3.0", undetermined_share = "0.3.0",
  undetermined_width = "0.3.0", region = "0.3.0",
  data_zone_share = "0.4.0", data_zone_width = "0.4.0",
  nsca_supported = "0.4.0", conjunction = "0.4.0", boundary = "0.4.0"
)

#' Names retired in NSCA 0.3.0 and 0.4.0
#'
#' The 0.3.0 vocabulary followed the geometry rather than the interpretation:
#' the region between the two frontiers was named for what it is and the two
#' joint indices for what they do to a weak component rather than for a verdict.
#'
#' 0.4.0 goes further and adopts the vocabulary of *condition analysis in
#' degree*. The region compatible with both components is the **admissible
#' region**; the distance on the X scale between the two thresholds at one
#' outcome target is the **necessity-sufficiency interval**; evidence for both
#' components is **joint support**; and *boundary* is reserved for the
#' theoretical line an expected empty space is separated by, so the argument
#' controlling the strictness of a reported inequality became `inequality`.
#'
#' Old names remain usable. [nsca_table()] and [nsca_thresholds()] append them
#' as duplicate columns when called with `legacy = TRUE`, [nsca_extract()]
#' accepts them with a warning, and the retired arguments warn but still work.
#'
#' @return A named character vector: names are the retired names, values the
#'   current ones.
#' @seealso [nsca_terms()]
#' @export
#' @examples
#' nsca_legacy_names()
nsca_legacy_names <- function() {
  .nsca_legacy_columns
}

#' Terminology of condition analysis in degree
#'
#' A machine-readable glossary linking the vocabulary of the condition analysis
#' in degree framework to the names this package uses. Names retired along the
#' way are listed by [nsca_legacy_names()].
#'
#' @return A data frame with one row per term.
#' @seealso [nsca_legacy_names()], [nsca_direction_map()], [nsca_table()]
#' @export
#' @examples
#' nsca_terms()
nsca_terms <- function() {
  data.frame(
    term = c(
      "scope", "expected empty space", "boundary", "frontier",
      "ceiling", "floor", "necessity threshold", "sufficiency threshold",
      "admissible region", "necessity-sufficiency interval",
      "threshold correspondence", "exact correspondence", "joint support",
      "component effect size", "frontier crossing", "central tendency"
    ),
    meaning = c(
      "Region of the X-Y space bounded by the theoretical or observed extremes of X and Y",
      "Part of the scope that should hold no observations if the relation holds",
      "Theoretical line separating the expected empty space from the compatible region",
      "The boundary as estimated from data",
      "A boundary that limits how high Y can be for a given X",
      "A boundary that limits how low Y can be for a given X",
      "X level below which a stated outcome target cannot be attained",
      "X level at which the condition is enough for that target",
      "Part of the scope compatible with both components",
      "Distance on the X scale between the two thresholds at one outcome target",
      "How close the two thresholds are, asked only after both components are supported",
      "The limiting case in which one X value is both required and sufficient",
      "Credible evidence for both components under their respective criteria",
      "One component's empty space as a proportion of the shared scope",
      "The two frontiers invert, so the component effect sizes can sum above one",
      "How the expected outcome moves with the condition; an average effect, not a boundary"
    ),
    package_name = c(
      "scope", "empty_zone_area", "(theoretical; not estimated)", "ceiling",
      "boundary_type == \"ceiling\"", "boundary_type == \"floor\"",
      "necessity_threshold", "sufficiency_threshold",
      "admissible_region_share", "necessity_sufficiency_interval",
      "threshold_status", "threshold_status == \"exact correspondence\"",
      "joint_support", "d_nec, d_suf", "frontiers_crossed, overlap_share",
      "reference = \"ols\", nsca_reference()"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Central-tendency reference line
#'
#' Fits the ordinary least-squares regression of the outcome on each condition,
#' on the same observations both frontiers were fitted to. This is the line
#' `lm(y ~ x)` returns, and it is what `reference = "ols"` draws on
#' [nsca_plot()].
#'
#' It is reported apart from [nsca_table()] because it is a different kind of
#' quantity. Regression describes how the expected outcome moves with the
#' condition; the two frontiers describe which condition-outcome combinations
#' are absent. Section 6.1 of the condition analysis in degree framework keeps
#' the two logics side by side rather than merging them into one number, and
#' section 4.3 notes that a frontier is fixed by the most extreme observations
#' rather than by the central tendency. A reference line therefore has a slope
#' and an intercept but no empty zone, no effect size, no p value, no
#' threshold, and no part in the joint-support decision.
#'
#' The line is computed on request, so it is available whether or not
#' `reference = "ols"` was set; `drawn` records whether it also appears on the
#' plot.
#'
#' @param model An object returned by [nsca_analysis()].
#' @param x Optional condition names or positions. All conditions by default.
#' @return A data frame with one row per condition: `condition`, `outcome`,
#'   `line`, `line_type`, `observations`, `intercept`, `slope`, `r_squared`
#'   and `drawn`.
#' @seealso [nsca_analysis()], [nsca_table()], [nsca_terms()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", reference = "ols")
#' nsca_reference(fit)
nsca_reference <- function(model, x = NULL) {
  metadata <- .nsca_metadata(model)
  conditions <- .nsca_select_conditions(model, x)
  drawn <- isTRUE("ols" %in% metadata$reference)

  rows <- lapply(conditions, function(condition) {
    item <- metadata$raw$conditions[[condition]]
    xv <- as.numeric(item$x)
    yv <- as.numeric(item$y)
    keep <- is.finite(xv) & is.finite(yv)
    xv <- xv[keep]
    yv <- yv[keep]

    intercept <- NA_real_
    slope <- NA_real_
    r_squared <- NA_real_
    if (length(xv) >= 2L && stats::var(xv) > 0) {
      fitted <- stats::lm(yv ~ xv)
      coefficients <- stats::coef(fitted)
      intercept <- unname(coefficients[[1L]])
      slope <- unname(coefficients[[2L]])
      if (stats::var(yv) > 0) {
        r_squared <- suppressWarnings(stats::cor(xv, yv))^2
      }
    }

    data.frame(
      condition = condition,
      outcome = metadata$outcome,
      line = "ols",
      line_type = "central tendency",
      observations = length(xv),
      intercept = intercept,
      slope = slope,
      r_squared = as.numeric(r_squared),
      drawn = drawn,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.nsca_global_value <- function(fit, condition, parameter) {
  item <- fit$summaries[[condition]]
  if (is.null(item) || is.null(item$global) ||
      !(parameter %in% rownames(item$global))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(
    item$global[match(parameter, rownames(item$global)), 1L]
  ))
}

.nsca_component_table <- function(model, component) {
  metadata <- .nsca_metadata(model)
  if (identical(component, "sufficiency")) {
    source <- SCAtools::sca_table(model$sufficiency)
    if (nrow(source) == 0L) {
      return(source)
    }
    source$analysis <- "sufficiency"
    source$nsca_number <- unname(.nsca_direction_number[source$direction])
    source$statement <- vapply(
      seq_len(nrow(source)),
      function(i) .nsca_sufficiency_statement(
        source$direction[[i]], source$condition[[i]], source$outcome[[i]]
      ),
      character(1L)
    )
    return(source[c(
      "analysis", "condition", "outcome", "direction", "relationship",
      "nsca_number", "empty_corner", "location", "empty_space",
      "boundary_type", "statement", "frontier",
      "observations", "empty_zone_area", "effect_size", "p_value",
      "p_accuracy", "frontier_accuracy", "fit", "slope", "intercept"
    )])
  }

  rows <- list()
  index <- 0L
  for (condition in metadata$conditions) {
    direction <- unname(metadata$direction[[condition]])
    corner <- unname(metadata$necessity_corner[[condition]])
    for (ceiling in metadata$ceilings) {
      index <- index + 1L
      rows[[index]] <- data.frame(
        analysis = "necessity",
        condition = condition,
        outcome = metadata$outcome,
        direction = direction,
        relationship = .nsca_relationship_text(
          direction, condition, metadata$outcome
        ),
        nsca_number = unname(.nsca_direction_number[[direction]]),
        empty_corner = corner,
        location = unname(.nsca_corner_location[[as.character(corner)]]),
        empty_space = .nsca_empty_space_text(
          corner, condition, metadata$outcome
        ),
        boundary_type = .nsca_boundary_type(corner),
        statement = .nsca_necessity_statement(
          direction, condition, metadata$outcome
        ),
        frontier = ceiling,
        observations = .nsca_global_value(
          model$necessity, condition, "Number of observations"
        ),
        empty_zone_area = .nsca_nca_param(
          model$necessity, condition, ceiling, "Ceiling zone"
        ),
        effect_size = .nsca_nca_param(
          model$necessity, condition, ceiling, "Effect size"
        ),
        p_value = .nsca_nca_param(
          model$necessity, condition, ceiling, "p-value"
        ),
        p_accuracy = .nsca_nca_param(
          model$necessity, condition, ceiling, "p-accuracy"
        ),
        frontier_accuracy = .nsca_nca_param(
          model$necessity, condition, ceiling, "Ceiling accuracy"
        ),
        fit = .nsca_nca_param(model$necessity, condition, ceiling, "Fit"),
        slope = .nsca_nca_param(model$necessity, condition, ceiling, "Slope"),
        intercept = .nsca_nca_param(
          model$necessity, condition, ceiling, "Intercept"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.nsca_band_for <- function(metadata, model, condition, ceiling) {
  raw <- metadata$raw$conditions[[condition]]
  x_bounds <- raw$x_bounds
  y_bounds <- raw$y_bounds
  grid <- seq(x_bounds[[1L]], x_bounds[[2L]], length.out = .nsca_grid_size)

  nec_corner <- unname(metadata$necessity_corner[[condition]])
  suf_corner <- unname(metadata$sufficiency_corner[[condition]])

  nec <- .nsca_frontier(
    raw$x, raw$y, nec_corner, ceiling, grid,
    slope = .nsca_nca_param(model$necessity, condition, ceiling, "Slope"),
    intercept = .nsca_nca_param(model$necessity, condition, ceiling, "Intercept"),
    peers = model$necessity$peers[[ceiling]][[condition]]
  )
  suf <- .nsca_frontier(
    raw$x, raw$y, suf_corner, ceiling, grid,
    slope = .nsca_sca_param(model$sufficiency, condition, ceiling, "Slope"),
    intercept = .nsca_sca_param(model$sufficiency, condition, ceiling, "Intercept"),
    peers = model$sufficiency$peers[[ceiling]][[condition]]
  )

  if (identical(.nsca_corner_side(nec_corner), "upper")) {
    band <- .nsca_band(nec, suf, grid, y_bounds)
  } else {
    band <- .nsca_band(suf, nec, grid, y_bounds)
  }
  scope_area <- diff(x_bounds) * diff(y_bounds)
  share <- function(value) {
    if (is.finite(value) && scope_area > 0) value / scope_area else NA_real_
  }
  list(
    share = share(band$area),
    overlap = share(band$overlap),
    # A step frontier always overlaps itself by roughly one tread; only a
    # smooth frontier that inverts is reporting a problem.
    suspect = !(ceiling %in% .nsca_step_ceilings) &&
      isTRUE(share(band$overlap) > 0.01)
  )
}

# Numerical slack for the "component effect sizes sum above one" check. Both
# shares are integrals over a grid, so an exactly saturating pair can land a
# few ulps above one; flagging that as crossed frontiers would be a false
# alarm about the one configuration the check is meant to bless.
.nsca_sum_tolerance <- 1e-9

# Columns that belong to nsca_thresholds() rather than nsca_table(). They are
# defined per outcome level, so nsca_extract() cannot return one of them for a
# condition and frontier and says so instead of calling the name unknown.
.nsca_threshold_columns <- c(
  "convention", "condition_scale", "outcome_scale", "outcome_level",
  "outcome_level_actual", "necessity_threshold", "sufficiency_threshold",
  "necessity_threshold_actual", "sufficiency_threshold_actual",
  "necessity_status", "sufficiency_status", "threshold_gap_actual",
  "necessity_sufficiency_interval", "overlap_width",
  "necessity_sufficiency_interval_actual", "overlap_width_actual",
  "thresholds_compatible",
  "threshold_status", "sufficiency_rule", "inequality"
)

# The overlap a step frontier produces by construction, which is not a fault
# and must not count against the geometry. Between two consecutive
# observations the upper staircase still holds the earlier point's value while
# the lower staircase already holds the later one, so the two empty zones
# overlap on every tread. Summed over the n - 1 treads this is exactly
# 1 / (n - 1) of the scope, and it is attained by a perfect diagonal.
.nsca_step_allowance <- function(ceiling, observations) {
  if (!(ceiling %in% .nsca_step_ceilings)) {
    return(0)
  }
  if (!is.finite(observations) || observations < 2) {
    return(0)
  }
  1 / (observations - 1)
}

#' Joint necessity and sufficiency results
#'
#' One row per condition and frontier technique. Both sides of a row always use
#' the same technique, the same observations and the same scope.
#'
#' @section What is primary:
#' The component effect sizes `d_nec` and `d_suf` are primary. Everything else
#' is a way of putting them together, and three such ways are reported rather
#' than one, because nothing in the geometry of two empty zones selects one.
#'
#' \describe{
#'   \item{`weakest_effect`}{`min(d_nec, d_suf)`, the fully non-compensatory
#'   summary. It falls to zero as soon as either component does and stays on
#'   the components' own scale, so its maximum is about 0.5 rather than 1.}
#'   \item{`balanced_joint_effect`}{`2 * sqrt(d_nec * d_suf)`, normalised onto
#'   `[0, 1]`. Partially compensatory: a larger component raises it at a
#'   diminishing rate, and it still collapses to zero if either component
#'   does. At an equal component sum of 0.80, `(0.40, 0.40)` gives 0.80 and
#'   `(0.70, 0.10)` gives 0.529. It is *not* a measure of balance: `(0.50,
#'   0.125)` and `(0.25, 0.25)` both score 0.50. The `balance` column measures
#'   that separately.}
#'   \item{`joint_empty_zone_coverage`}{The share of the scope that at least
#'   one claim rules out, `1 - admissible_region_share`. Curvature-neutral: it reaches
#'   1 whenever the admissible region closes, whatever the shape of the relation.}
#' }
#'
#' `min` and the geometric mean are the power means at `p = -Inf` and `p = 0`,
#' so choosing between them is choosing how much compensation to allow; the
#' component sum sits at `p = 1`, the fully compensatory end, and is a poor
#' conjunction summary because one strong component can carry it.
#' [nsca_joint()] exposes the family directly.
#'
#' @section The area identity:
#' The two empty spaces and the admissible region tile the scope, so by inclusion and
#' exclusion
#'
#' \deqn{\mathrm{admissible\_region\_share} = 1 - d_{nec} - d_{suf} + O}
#'
#' where \eqn{O} is `overlap_share`. Everything else follows from it rather
#' than being asserted separately. Because `admissible_region_share` is a share of the
#' scope it lies in `[0, 1]` unconditionally, and therefore so does
#' `joint_empty_zone_coverage`. The other two indices are bounded only up to
#' the overlap:
#'
#' \deqn{d_{nec} + d_{suf} \le 1 + O, \quad
#'       \mathrm{weakest\_effect} \le 0.5 + O/2, \quad
#'       \mathrm{balanced\_joint\_effect} \le 1 + O}
#'
#' Both bounds are tight and are attained exactly by a perfect diagonal, where
#' `O` is `1 / (n - 1)`: at `n = 40` the observed values are 0.5128 and 1.0256
#' against bounds of 0.5128 and 1.0256. The excess is discretization, not
#' evidence, and the indices are left unclamped so that it stays visible.
#'
#' @section Vocabulary:
#' The column names follow the condition analysis in degree framework.
#' `relationship` states the direction as a claim about the theorised X-Y
#' relationship, as in Table 1 of that framework, not about the shape of the
#' estimated frontier. `necessity_empty_space` and `sufficiency_empty_space`
#' name what each expected empty space would contain, and
#' `necessity_boundary` and `sufficiency_boundary` say whether the boundary
#' separating it is a ceiling, which limits how high the outcome can be for a
#' given condition value, or a floor, which limits how low. The two are always
#' one of each, because the two corners are diagonally opposite.
#'
#' @section The disjointness check:
#' Whenever the two frontiers do not cross, the floor lies at or below the
#' ceiling throughout the scope, so no point belongs to both expected empty
#' spaces. Because each component effect size is that space's share of the same
#' scope, the two shares cannot sum to more than one. Component effect sizes
#' summing above one therefore say the frontiers have crossed and the joint
#' result needs diagnosis before interpretation, rather than that it is
#' unusually strong. `component_sum` reports `d_nec + d_suf` so that this rule
#' can be applied directly.
#'
#' `frontiers_crossed` answers the same question from the measurement rather
#' than from the sum. The sum is a proxy for a crossing that is not otherwise
#' available; here it is available, because `overlap_share` is the area on
#' which the two empty spaces actually overlap, integrated from the
#' reconstructed frontiers. Using the measurement avoids the proxy's failure
#' mode, which is that the sum and the allowance below are reached by different
#' routes and can disagree in the last few digits on exactly the configuration
#' the check is meant to bless.
#'
#' That allowance exists because a step frontier overlaps itself by
#' construction. Between two consecutive observations the upper staircase still
#' holds the earlier value while the lower one has already moved, so the two
#' spaces overlap on every tread; summed over the `n - 1` treads that is
#' exactly `1 / (n - 1)` of the scope, attained by a perfect diagonal, which is
#' the cleanest relation there is rather than a failure. For every smooth
#' frontier the allowance is zero and any overlap at all is a genuine crossing.
#'
#' `frontiers_crossed` asks whether the frontiers crossed at all;
#' `geometry_acceptable` asks whether they crossed by more than
#' `geometry.max_overlap`. A row can be flagged as crossed and still have
#' acceptable geometry.
#'
#' @section Geometry diagnostics:
#' `reconstruction_error` is the residual of the identity above. The two sides
#' come from different places: `admissible_region_share` and `overlap_share` are
#' integrated from the frontiers this package reconstructs, while `d_nec` and
#' `d_suf` are reported by the engines from their own internal frontiers. A
#' residual near zero says the two agree; a large one says the reconstruction
#' has drifted from what the engine actually fitted, which is the one failure
#' mode that a plot cannot reveal, because a wrongly reconstructed frontier
#' still looks like a frontier.
#'
#' `degenerate` marks a condition whose axis does not vary inside the declared
#' scope. `geometry_acceptable` combines both diagnostics with the excess
#' frontier overlap, allowing a step frontier the `1 / (n - 1)` it produces by
#' construction.
#'
#' @section Decisions:
#' `p_nsca_iut` is `max(p_nec, p_suf)`, the intersection-union combination. It
#' passes only when both components reject at `test.p_threshold`, and it is
#' level-alpha without any assumption that the two component tests are
#' independent, so no correction is needed merely for combining two
#' pre-specified tests. Multiplicity across several conditions or frontiers is
#' a separate matter. The combination is also typically conservative, so an
#' NSCA design needs more observations than either component alone.
#'
#' `p_weakest_perm` tests `min(d_nec, d_suf)` directly and is available only
#' when `shared.test.rep` was used, because the null distribution of a
#' statistic of both components depends on how they co-vary under random
#' pairing. It answers a different question from the intersection-union rule
#' and does not replace it: its null is random pairing alone, which is a proper
#' subset of the union null "not necessary or not sufficient", so it carries no
#' level-alpha guarantee over that union. Treat it as a sensitivity analysis.
#' `p_source` reports whether the component p-values came from the engines'
#' own separate runs or from the shared sequence.
#'
#' `necessity_supported` and `sufficiency_supported` require significance and,
#' when a `relevance` threshold was pre-specified, an effect at least that
#' large. `joint_support` additionally requires `geometry_acceptable`, so a
#' degenerate or badly reconstructed row cannot be reported as supported.
#'
#' @section Blind spot:
#' All three joint summaries are areas over the declared scope and share one
#' blind spot. A constant outcome at the centre of an imposed scope leaves the
#' upper and lower halves both empty, so both components reach 0.5, the data
#' zone closes, and every index reports perfect joint support for data
#' carrying no information. `balanced_joint_effect` is the most exposed, since
#' `weakest_effect` at least follows the weaker side once the constant sits off
#' centre. The `degenerate` flag and the permutation screen are what cover it:
#' permuting a constant outcome reproduces the same geometry every time, so
#' every p-value goes to 1.
#'
#' No magnitude benchmarks are supplied for any index. Conventions for a single
#' NCA effect size do not transfer: the scales differ, and under independence
#' both components carry a positive finite-sample bias that
#' `balanced_joint_effect` amplifies rather than cancels, by an amount
#' depending on the sample size, the frontier technique and the scope.
#' Calibrate by simulation on the design at hand.
#'
#' @param model An object returned by [nsca_analysis()].
#' @param legacy Append the column names retired in 0.3.0 and 0.4.0 as
#'   duplicates. See [nsca_legacy_names()].
#' @return A data frame of joint results.
#' @seealso [nsca_results()], [nsca_joint()], [nsca_extract()],
#'   [nsca_legacy_names()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
#' nsca_table(fit)
nsca_table <- function(model, legacy = FALSE) {
  metadata <- .nsca_metadata(model)
  shared <- metadata$shared
  relevance <- metadata$relevance
  tolerances <- metadata$geometry
  rows <- list()
  index <- 0L
  crossed_any <- character(0L)

  for (condition in metadata$conditions) {
    observations <- .nsca_global_value(
      model$necessity, condition, "Number of observations"
    )
    for (ceiling in metadata$ceilings) {
      d_nec <- .nsca_nca_param(model$necessity, condition, ceiling, "Effect size")
      d_suf <- .nsca_sca_param(model$sufficiency, condition, ceiling, "Effect size")
      weakest <- suppressWarnings(min(d_nec, d_suf))
      balanced <- nsca_joint(d_nec, d_suf, p = 0)
      component_sum <- d_nec + d_suf
      # The geometric mean over the arithmetic mean: 1 exactly when the two
      # components are equal, and the part of balanced_joint_effect that the
      # name promises but the index alone does not deliver.
      balance <- if (is.finite(component_sum) && component_sum > 0) {
        balanced / component_sum
      } else {
        NA_real_
      }

      band <- .nsca_band_for(metadata, model, condition, ceiling)
      if (isTRUE(band$suspect)) {
        crossed_any <- c(crossed_any, sprintf("%s/%s", condition, ceiling))
      }
      coverage <- if (is.finite(band$share)) 1 - band$share else NA_real_
      reconstruction <- if (is.finite(band$share) && is.finite(band$overlap) &&
                            is.finite(d_nec) && is.finite(d_suf)) {
        abs(1 - band$share - d_nec - d_suf + band$overlap)
      } else {
        NA_real_
      }
      allowance <- .nsca_step_allowance(ceiling, observations)
      excess_overlap <- if (is.finite(band$overlap)) {
        max(0, band$overlap - allowance)
      } else {
        NA_real_
      }

      # The disjointness check of the framework, section 5.2. Whenever the two
      # frontiers do not cross, the floor lies at or below the ceiling
      # throughout the scope, so no point belongs to both expected empty
      # spaces, and because each component effect size is that space's share of
      # the same scope the two cannot sum to more than one. The framework
      # therefore proposes d_nec + d_suf > 1 as a consistency check.
      #
      # That form of the check is a proxy: it infers crossing from the sum
      # because the crossing itself is not otherwise available. Here it is
      # available. overlap_share is the area on which the two empty spaces
      # actually overlap, integrated from the reconstructed frontiers, so the
      # crossing is measured rather than inferred. Reporting component_sum
      # leaves the framework's own rule applicable by any reader; deciding
      # frontiers_crossed from the measurement avoids the proxy's failure mode,
      # which is that the sum and the idealized allowance are computed by
      # different routes and can disagree in the last few digits exactly on the
      # perfect diagonal the check is meant to bless.
      #
      # The step allowance is already subtracted in excess_overlap. A step
      # frontier overlaps itself by construction: between two consecutive
      # observations the upper staircase still holds the earlier value while
      # the lower one has already moved, so the two spaces overlap on every
      # tread, exactly 1 / (n - 1) of the scope summed over the n - 1 treads.
      # For every smooth frontier the allowance is zero and any overlap at all
      # is a genuine crossing.
      frontiers_crossed <- if (is.finite(excess_overlap)) {
        excess_overlap > .nsca_sum_tolerance
      } else {
        NA
      }

      degenerate <- isTRUE(metadata$degeneracy[[condition]]$degenerate)
      geometry_acceptable <- !degenerate &&
        is.finite(excess_overlap) &&
        excess_overlap <= tolerances[["max_overlap"]] &&
        is.finite(reconstruction) &&
        reconstruction <= tolerances[["max_reconstruction"]]

      if (is.null(shared)) {
        p_nec <- .nsca_nca_param(model$necessity, condition, ceiling, "p-value")
        p_suf <- .nsca_sca_param(model$sufficiency, condition, ceiling, "p-value")
        p_weakest <- NA_real_
        p_source <- if (isTRUE(metadata$test.rep > 0)) "engine" else "none"
      } else {
        p_nec <- .nsca_permutation_p(shared$necessity[, condition, ceiling], d_nec)
        p_suf <- .nsca_permutation_p(shared$sufficiency[, condition, ceiling], d_suf)
        p_weakest <- .nsca_permutation_p(
          shared$weakest[, condition, ceiling], weakest
        )
        p_source <- "shared"
      }
      tested <- !identical(p_source, "none")
      p_iut <- if (is.finite(p_nec) && is.finite(p_suf)) {
        max(p_nec, p_suf)
      } else {
        NA_real_
      }

      nec_significant <- if (tested && is.finite(p_nec)) {
        p_nec <= metadata$alpha
      } else {
        NA
      }
      suf_significant <- if (tested && is.finite(p_suf)) {
        p_suf <= metadata$alpha
      } else {
        NA
      }
      nsca_significant <- if (is.na(nec_significant) || is.na(suf_significant)) {
        NA
      } else {
        nec_significant && suf_significant
      }

      relevant <- function(value, threshold) {
        if (is.na(threshold)) {
          return(TRUE)
        }
        isTRUE(is.finite(value) && value >= threshold)
      }
      nec_supported <- if (is.na(nec_significant)) {
        NA
      } else {
        nec_significant && relevant(d_nec, relevance[["necessity"]])
      }
      suf_supported <- if (is.na(suf_significant)) {
        NA
      } else {
        suf_significant && relevant(d_suf, relevance[["sufficiency"]])
      }
      joint_support <- if (is.na(nec_supported) || is.na(suf_supported)) {
        NA
      } else {
        nec_supported && suf_supported && geometry_acceptable
      }

      index <- index + 1L
      rows[[index]] <- data.frame(
        condition = condition,
        outcome = metadata$outcome,
        direction = unname(metadata$direction[[condition]]),
        equivalence_class = unname(
          .nsca_equivalence_class[[metadata$direction[[condition]]]]
        ),
        relationship = .nsca_relationship_text(
          unname(metadata$direction[[condition]]), condition, metadata$outcome
        ),
        statement = unname(metadata$statement[[condition]]),
        ceiling = ceiling,
        necessity_corner = unname(metadata$necessity_corner[[condition]]),
        necessity_empty_space = .nsca_empty_space_text(
          unname(metadata$necessity_corner[[condition]]),
          condition, metadata$outcome
        ),
        necessity_boundary = .nsca_boundary_type(
          unname(metadata$necessity_corner[[condition]])
        ),
        sufficiency_corner = unname(metadata$sufficiency_corner[[condition]]),
        sufficiency_empty_space = .nsca_empty_space_text(
          unname(metadata$sufficiency_corner[[condition]]),
          condition, metadata$outcome
        ),
        sufficiency_boundary = .nsca_boundary_type(
          unname(metadata$sufficiency_corner[[condition]])
        ),
        observations = observations,
        d_nec = d_nec,
        d_suf = d_suf,
        component_sum = component_sum,
        # Two points on one power-mean family, p = -Inf and p = 0. The minimum
        # is left as a literal min() rather than routed through nsca_joint():
        # the two agree on every value an effect size can take, but nsca_joint()
        # returns NA for a negative input where min() returns the negative
        # number, and an engine that ever reported a negative area should
        # surface it here rather than have it become a missing value.
        weakest_effect = weakest,
        weaker_component = if (!is.finite(d_nec) || !is.finite(d_suf)) {
          NA_character_
        } else if (d_nec <= d_suf) "necessity" else "sufficiency",
        balanced_joint_effect = balanced,
        balance = balance,
        admissible_region_share = band$share,
        joint_empty_zone_coverage = coverage,
        overlap_share = band$overlap,
        frontiers_crossed = frontiers_crossed,
        degenerate = degenerate,
        geometry_acceptable = geometry_acceptable,
        reconstruction_error = reconstruction,
        p_source = p_source,
        p_nec = p_nec,
        p_suf = p_suf,
        p_nsca_iut = p_iut,
        p_weakest_perm = p_weakest,
        necessity_significant = nec_significant,
        sufficiency_significant = suf_significant,
        nsca_significant = nsca_significant,
        necessity_supported = nec_supported,
        sufficiency_supported = suf_supported,
        joint_support = joint_support,
        joint_support_status = .nsca_verdict(p_nec, p_suf, metadata$alpha, tested),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  if (length(crossed_any) > 0L) {
    warning(
      sprintf(
        paste0(
          "The two frontiers invert for %s by more than 1%% of the scope, so ",
          "the expected empty spaces genuinely overlap rather than partition ",
          "the scope, and d_nec + d_suf will exceed 1. This is not the small ",
          "overlap a step frontier always has; see frontiers_crossed and ",
          "inspect the plot before reading these areas."
        ),
        paste(unique(crossed_any), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  .nsca_append_legacy(out, legacy)
}

# Append retired names as duplicate columns. Only names whose replacement this
# particular table carries are added, so one helper serves both the joint table
# and the dual-threshold table.
.nsca_append_legacy <- function(out, legacy) {
  if (!isTRUE(legacy)) {
    return(out)
  }
  for (old in names(.nsca_legacy_columns)) {
    new <- .nsca_legacy_columns[[old]]
    if (new %in% names(out)) {
      out[[old]] <- out[[new]]
    }
  }
  out
}

#' All three NSCA result layers
#'
#' Returns the necessity analysis, the sufficiency analysis, and their joint
#' necessary-and-sufficient analysis as three explicit tables. The third does
#' not represent an independent third experiment: its `p_nsca_iut` is the
#' conjunction `max(p_nec, p_suf)` and can pass only when both component
#' screens pass.
#'
#' @param model An object returned by [nsca_analysis()].
#' @param object An object returned by [nsca_analysis()].
#' @param x An object returned by [nsca_analysis()], for the print method.
#' @param ... Additional arguments reserved for methods.
#' @return A named list containing `necessity`, `sufficiency`, and
#'   `necessary_and_sufficient` data frames. The methods print, or return a
#'   summary object.
#' @seealso [nsca_table()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
#' layers <- nsca_results(fit)
#' names(layers)
#' fit
#' summary(fit)
nsca_results <- function(model) {
  .nsca_metadata(model)
  list(
    necessity = .nsca_component_table(model, "necessity"),
    sufficiency = .nsca_component_table(model, "sufficiency"),
    necessary_and_sufficient = nsca_table(model)
  )
}

#' Extract one value from an NSCA result
#'
#' @param model An object returned by [nsca_analysis()].
#' @param x Condition name. Defaults to the first condition.
#' @param ceiling Frontier technique. Defaults to the first requested.
#' @param param A column of [nsca_table()], one of the retired names in
#'   [nsca_legacy_names()], or a parameter name understood by the underlying
#'   engines, prefixed with `"nec:"` or `"suf:"`.
#' @return The selected value.
#' @seealso [nsca_table()], [nsca_legacy_names()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
#' nsca_extract(fit, param = "weakest_effect")
#' nsca_extract(fit, param = "nec:Ceiling accuracy")
nsca_extract <- function(model, x = NULL, ceiling = NULL,
                         param = "weakest_effect") {
  metadata <- .nsca_metadata(model)
  if (is.null(x)) {
    x <- metadata$conditions[[1L]]
  }
  if (!(x %in% metadata$conditions)) {
    stop(sprintf("Unknown condition '%s'.", x), call. = FALSE)
  }
  if (is.null(ceiling)) {
    ceiling <- metadata$ceilings[[1L]]
  }
  # Checked here rather than left to the table lookup, because the nec: and
  # suf: routes read the engines directly and would otherwise return NA for a
  # frontier that was never estimated.
  if (length(ceiling) != 1L || !(ceiling %in% metadata$ceilings)) {
    stop(
      sprintf(
        "Frontier '%s' was not estimated. Available: %s.",
        paste(ceiling, collapse = ", "), paste(metadata$ceilings, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (grepl("^nec:", param)) {
    return(.nsca_nca_param(model$necessity, x, ceiling, sub("^nec:", "", param)))
  }
  if (grepl("^suf:", param)) {
    return(.nsca_sca_param(model$sufficiency, x, ceiling, sub("^suf:", "", param)))
  }

  if (param %in% names(.nsca_legacy_columns)) {
    replacement <- .nsca_legacy_columns[[param]]
    warning(
      sprintf(
        "'%s' was renamed to '%s' in NSCA %s. See nsca_legacy_names().",
        param, replacement, .nsca_legacy_since[[param]]
      ),
      call. = FALSE
    )
    param <- replacement
  }

  table <- nsca_table(model)
  row <- table[table$condition == x & table$ceiling == ceiling, , drop = FALSE]
  if (nrow(row) == 0L) {
    stop(sprintf("No result for condition '%s' with frontier '%s'.", x, ceiling),
         call. = FALSE)
  }
  if (!(param %in% names(row))) {
    # A dual-threshold column is defined per outcome level, not per condition
    # and frontier, so there is no single value to return. Say that rather than
    # reporting the name as unknown, which it is not.
    if (param %in% .nsca_threshold_columns) {
      stop(
        sprintf(
          paste0(
            "'%s' is a column of nsca_thresholds(), which has one row per ",
            "outcome level rather than one value per condition and frontier. ",
            "Call nsca_thresholds() and select the column."
          ),
          param
        ),
        call. = FALSE
      )
    }
    stop(
      sprintf(
        paste0(
          "Unknown parameter '%s'. Use a column of nsca_table(), a retired ",
          "name from nsca_legacy_names(), or prefix an engine parameter with ",
          "'nec:' or 'suf:'."
        ),
        param
      ),
      call. = FALSE
    )
  }
  row[[param]][[1L]]
}

#' @rdname nsca_results
#' @export
print.nsca_result <- function(x, ...) {
  metadata <- .nsca_metadata(x)
  tables <- nsca_results(x)
  joint <- tables$necessary_and_sufficient
  cat("\nNecessary and Sufficient Condition Analysis (NSCA)\n")
  cat("Engines: NCA ", metadata$engines[["NCA"]],
      " (necessity), SCAtools ", metadata$engines[["SCAtools"]],
      " (sufficiency)\n", sep = "")

  mapping <- data.frame(
    condition = metadata$conditions,
    direction = unname(metadata$direction[metadata$conditions]),
    nec_corner = unname(metadata$necessity_corner[metadata$conditions]),
    suf_corner = unname(metadata$sufficiency_corner[metadata$conditions]),
    statement = unname(metadata$statement[metadata$conditions]),
    stringsAsFactors = FALSE
  )
  print(mapping, row.names = FALSE)

  cat("\n1. Necessary Condition Analysis\n")
  print(
    tables$necessity[c(
      "condition", "frontier", "effect_size", "p_value",
      "frontier_accuracy", "fit"
    )],
    row.names = FALSE
  )

  cat("\n2. Sufficiency Condition Analysis\n")
  print(
    tables$sufficiency[c(
      "condition", "frontier", "effect_size", "p_value",
      "frontier_accuracy", "fit"
    )],
    row.names = FALSE
  )

  cat("\n3. Necessary and Sufficient Condition Analysis\n")
  cat("Three ways of combining the same two components; none supersedes them.\n")
  cat("  weakest_effect            = min(d_nec, d_suf)       non-compensatory, max ~0.5 + O/2\n")
  cat("  balanced_joint_effect     = 2*sqrt(d_nec*d_suf)     partially compensatory, max ~1 + O\n")
  cat("  joint_empty_zone_coverage = 1 - admissible_region_share     curvature-neutral, always in [0, 1]\n")
  print(
    joint[c(
      "condition", "ceiling", "d_nec", "d_suf", "component_sum",
      "weakest_effect", "balanced_joint_effect", "balance",
      "admissible_region_share", "joint_empty_zone_coverage",
      "overlap_share", "frontiers_crossed"
    )],
    row.names = FALSE
  )
  cat(
    "\nMagnitude benchmarks for these indices are not established. Report them",
    "\nwith both components, and calibrate by simulation before naming a value",
    "\nlarge or small.\n",
    sep = ""
  )

  cat("\n4. Decision\n")
  cat("p_nsca_iut = max(p_nec, p_suf): the intersection-union combination,\n")
  cat("which passes only when both component tests reject. Support adds any\n")
  cat("pre-specified relevance threshold and requires acceptable geometry.\n")
  print(
    joint[c(
      "condition", "ceiling", "p_source", "p_nec", "p_suf", "p_nsca_iut",
      "p_weakest_perm", "necessity_supported", "sufficiency_supported",
      "geometry_acceptable", "joint_support"
    )],
    row.names = FALSE
  )

  if (identical(joint$p_source[[1L]], "none")) {
    cat("\nNo permutation test was run, so no joint-support verdict is available.\n")
    cat("Set test.rep to test both components; joint support needs both.\n")
  }
  if (all(is.na(metadata$relevance))) {
    cat("\nNo practical-relevance threshold was pre-specified, so support rests\n")
    cat("on significance alone. Set 'relevance' to require an effect size too.\n")
  }
  if (any(joint$degenerate)) {
    cat(
      "\nDegenerate row(s) present: an axis does not vary inside the declared",
      "\nscope, so every index above measures the scope, not the data, and it",
      "\nmeasures it flatteringly. Read the p-values instead.\n",
      sep = ""
    )
  }
  bad <- joint$reconstruction_error >
    metadata$geometry[["max_reconstruction"]]
  if (isTRUE(any(bad, na.rm = TRUE))) {
    cat(
      "\nThe area identity does not close for at least one row. The frontiers",
      "\nthis package reconstructs disagree with the ones the engines fitted,",
      "\nso the admissible region and the plots may be wrong even though they",
      "\nlook plausible. Inspect reconstruction_error.\n",
      sep = ""
    )
  }
  cat(
    "\nInference note: component and joint p-values screen the ",
    "random-pairing/independence null; they do not by themselves prove a ",
    "causal or deterministic necessary-and-sufficient relation.\n",
    sep = ""
  )
  invisible(x)
}

#' @rdname nsca_results
#' @export
summary.nsca_result <- function(object, ...) {
  metadata <- .nsca_metadata(object)
  tables <- nsca_results(object)
  out <- list(
    call = metadata$call,
    map = nsca_direction_map(),
    necessity = tables$necessity,
    sufficiency = tables$sufficiency,
    necessary_and_sufficient = tables$necessary_and_sufficient,
    results = tables$necessary_and_sufficient,
    alpha = metadata$alpha,
    relevance = metadata$relevance,
    test.rep = metadata$test.rep,
    shared.test.rep = metadata$shared.test.rep
  )
  class(out) <- "summary_nsca_result"
  out
}

#' @rdname nsca_results
#' @export
print.summary_nsca_result <- function(x, ...) {
  cat("\nNecessary and Sufficient Condition Analysis summary\n\n")
  if (!is.null(x$call)) {
    cat("Call:\n")
    print(x$call)
    cat("\n")
  }
  cat("1. Necessary Condition Analysis:\n")
  print(x$necessity, row.names = FALSE)
  cat("\n2. Sufficiency Condition Analysis:\n")
  print(x$sufficiency, row.names = FALSE)
  cat("\n3. Necessary and Sufficient Condition Analysis:\n")
  print(x$necessary_and_sufficient, row.names = FALSE)
  cat("\nThe joint-support screen uses p_nsca_iut = max(p_nec, p_suf) at alpha = ",
      format(x$alpha), "; both directional component tests must reject.\n",
      sep = "")
  cat("This is not a direct test of a causal necessary-and-sufficient relation.\n")
  cat("weakest_effect, balanced_joint_effect and joint_empty_zone_coverage are\n")
  cat("three summaries of the same two components, differing in how much\n")
  cat("compensation they allow and in how they treat curvature. None has\n")
  cat("established magnitude benchmarks.\n")
  invisible(x)
}
