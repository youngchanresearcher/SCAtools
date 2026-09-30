# Dual thresholds.
#
# For a given outcome level, necessity and sufficiency answer two different
# questions, and together they cut the condition axis into three regions:
#
# For a high-X statement, low values are out of reach and high values are
# guaranteed. For a low-X statement the ordering reverses. In both cases the
# direction-oriented gap is positive when the two frontiers leave an admitted
# interval between them.
#
# The middle region is the admissible region at that outcome level: the
# interval the frontiers still admit. Its width is the necessity-sufficiency
# interval, the row-level counterpart of admissible_region_share, and it
# collapses to zero exactly where the relation is deterministic at that level.
# "Admits" is the right word: the interval says observations are not ruled out
# there, not that any were seen there.

# Read a threshold column from an engine bottleneck table in actual units.
# The exact values are carried on an attribute; the printed text is the
# fallback and is rounded to three decimals.
.nsca_actual_column <- function(table, condition, rows) {
  column <- table[[condition]]
  actual <- attr(column, "mpx.actual")
  if (is.null(actual)) {
    actual <- attr(table[condition], "mpx.actual")
  }
  if (!is.null(actual)) {
    actual <- as.numeric(actual)
    if (length(actual) == rows) {
      return(list(values = actual, exact = TRUE))
    }
  }
  text <- trimws(as.character(column))
  text[is.na(text)] <- "NA"
  parsed <- suppressWarnings(as.numeric(text))
  # "NN" is an infinite requirement in the engine's flipped coordinates and
  # "NA" a requirement outside the scope; both are non-finite here, and the
  # side-specific readings are attached by the caller.
  parsed[!is.finite(parsed)] <- ifelse(text[!is.finite(parsed)] == "NN", Inf, NA_real_)
  list(values = parsed, exact = FALSE)
}

# Necessity markers keep their ordinary necessity reading, because the
# necessity side is being read as necessity. Only a necessity computation read
# as sufficiency needs the meanings inverted, which is what SCAtools handles.
.nsca_necessity_status <- function(values, x_letter) {
  ifelse(
    is.na(values), "unattainable",
    ifelse(
      is.infinite(values),
      if (identical(x_letter, "H")) "no_minimum" else "no_maximum",
      "estimable"
    )
  )
}

#' Dual thresholds and the necessity-sufficiency interval
#'
#' For every outcome level, reports the necessity and sufficiency thresholds
#' and the direction-oriented distance between them.
#'
#' For a fixed outcome target, the *necessity-sufficiency interval* is the
#' distance on the condition axis between the two thresholds: if a target
#' requires at least 40 units of the condition but 70 units are enough for it,
#' the interval runs from 40 to 70. It is not a confidence interval: it carries
#' no statistical uncertainty.
#'
#' `necessity_sufficiency_interval` and `overlap_width` are reported on
#' `scale`, the same scale as the two threshold columns they are the distance
#' between, so a row can be checked by subtraction.
#' `necessity_sufficiency_interval_actual` and `overlap_width_actual` keep the
#' same two quantities in the units of the data whatever `scale` is asked for,
#' and `threshold_gap_actual` is the signed actual-unit gap the pair is split
#' from. Before 0.4.3 the two unsuffixed columns held actual units on every
#' scale, which put three columns of two different kinds in one printed row.
#'
#' Under `scale = "percentile"` the reported column is a difference of ranks
#' rather than a distance, so it reads as the share of cases lying between the
#' two thresholds. That is a useful quantity, but it is not a width: equal
#' percentile gaps in a dense and in a sparse part of the distribution stand
#' for very different distances on the condition axis. For a normalised width
#' that is comparable across studies use `"percentage.range"` or `"sd"`.
#'
#' The interval is also distinct from the component effect sizes. Effect sizes
#' aggregate exclusion over the whole scope, whereas the interval is read at one
#' outcome target, so large component effects do not guarantee a narrow interval
#' at any particular level and a narrow interval at one target says nothing
#' about the others. Reporting both, at several targets, is not redundant.
#'
#' @section The three regions:
#' Necessity and sufficiency answer different questions about the same outcome
#' level, and together they cut the condition axis into three parts. For a
#' high-X direction:
#'
#' \describe{
#'   \item{below `necessity_threshold`}{The outcome level is out of reach.}
#'   \item{between the two thresholds}{It is admitted but not guaranteed. This
#'   is the admissible interval, and `necessity_sufficiency_interval` is its
#'   width. It is the horizontal cross-section, at one outcome level, of the
#'   region whose area `admissible_region_share` reports.}
#'   \item{above `sufficiency_threshold`}{The fitted frontier guarantees it.}
#' }
#'
#' For a low-X direction these inequalities reverse: values above the necessity
#' threshold are out of reach and values below the sufficiency threshold are
#' guaranteed. `threshold_gap_actual` is direction-oriented, so a positive
#' value means an admitted interval in all four directions. A negative value is
#' reported as `"frontiers overlap"` in `threshold_status`, with its magnitude
#' in `overlap_width` and `overlap_width_actual`, rather than being silently
#' truncated to zero: the two
#' thresholds are then mutually incompatible at that level, which is a
#' modelling problem and not a narrow admissible interval.
#'
#' The interval collapses to zero exactly where the relation is deterministic
#' at that level, reported as `"exact correspondence"`: one X value is then both
#' the minimum required and the sufficient level. An estimated interval of zero
#' is *consistent* with exact correspondence but does not by itself establish
#' it, and a small non-zero interval should be called near correspondence only
#' when a substantively justified tolerance has been defined. A positive
#' interval does not negate joint support; it indicates imperfect threshold
#' correspondence. Neither NCA nor SCA alone can produce this table: each
#' supplies one edge of the interval.
#'
#' The two out-of-range markers read differently on the two sides, because
#' their meaning inverts under contraposition. On the necessity side they keep
#' their necessity reading, giving `"no_minimum"`, `"no_maximum"` and
#' `"unattainable"`; on the sufficiency side SCAtools supplies the sufficiency
#' reading.
#'
#' @param model An object returned by [nsca_analysis()].
#' @param ceiling A single frontier technique, applied to both sides.
#' @param x Optional condition names or positions.
#' @param scale,outcome_scale Reporting scales, defaulting to those requested
#'   in [nsca_analysis()]. See [SCAtools::sca_scales()].
#' @param convention `"absolute"` or `"directional"`.
#' @param inequality Strict or inclusive inequalities. Renamed from `boundary`
#'   in 0.4.0, which is reserved for the theoretical line an expected empty
#'   space is separated by.
#' @param boundary Deprecated. Former name of `inequality`.
#' @param digits Significant digits in the formatted text.
#' @param legacy Append the column names retired in 0.3.0 and 0.4.0 as
#'   duplicates. See [nsca_legacy_names()].
#' @return A data frame with one row per condition and outcome level.
#' @seealso [nsca_table()], [nsca_analysis()], [nsca_legacy_names()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
#' nsca_thresholds(fit)
nsca_thresholds <- function(
    model,
    ceiling = NULL,
    x = NULL,
    scale = NULL,
    outcome_scale = NULL,
    convention = NULL,
    inequality = c("strict", "inclusive"),
    digits = 6L,
    legacy = FALSE,
    boundary = NULL) {
  metadata <- .nsca_metadata(model)
  inequality <- .nsca_resolve_inequality(
    inequality, boundary, !missing(inequality)
  )
  if (is.null(ceiling)) {
    ceiling <- metadata$ceilings[[1L]]
  }
  if (length(ceiling) != 1L) {
    stop("Select exactly one frontier technique.", call. = FALSE)
  }
  if (!(ceiling %in% metadata$ceilings)) {
    stop(
      sprintf(
        "Frontier '%s' was not estimated. Available: %s.",
        ceiling, paste(metadata$ceilings, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # The sufficiency side is produced by SCAtools, which already applies the
  # direction-aware scale conversion and the sufficiency reading of the
  # out-of-range markers.
  sufficiency <- SCAtools::sca_thresholds(
    model$sufficiency,
    ceiling = ceiling,
    x = x,
    scale = scale,
    outcome_scale = outcome_scale,
    convention = convention,
    inequality = inequality,
    digits = digits
  )
  condition_names <- unique(sufficiency$condition)

  nec_table <- model$necessity$bottlenecks[[ceiling]]
  if (is.null(nec_table)) {
    stop(
      sprintf("The necessity side produced no threshold table for '%s'.", ceiling),
      call. = FALSE
    )
  }
  nec_levels <- as.numeric(nec_table[[1L]])

  rows <- list()
  for (i in seq_along(condition_names)) {
    condition <- condition_names[[i]]
    suf_rows <- sufficiency[sufficiency$condition == condition, , drop = FALSE]

    nec_column <- .nsca_actual_column(nec_table, condition, length(nec_levels))
    # Align the necessity rows to the sufficiency rows by actual outcome level;
    # both sides were given the same levels, but the engine orders them by
    # corner.
    position <- match(
      round(suf_rows$outcome_level_actual, 10L), round(nec_levels, 10L)
    )
    nec_actual <- nec_column$values[position]
    direction <- unique(suf_rows$direction)
    x_letter <- substr(direction[[1L]], 1L, 1L)
    nec_status <- .nsca_necessity_status(nec_actual, x_letter)
    nec_actual[!is.finite(nec_actual)] <- NA_real_

    suf_actual <- suf_rows$sufficiency_threshold_actual
    gap <- if (identical(x_letter, "H")) {
      suf_actual - nec_actual
    } else {
      nec_actual - suf_actual
    }
    x_span <- diff(metadata$raw$conditions[[condition]]$x_bounds)
    tolerance <- .Machine$double.eps^0.5 * max(1, abs(x_span))
    width_actual <- ifelse(is.finite(gap), pmax(gap, 0), NA_real_)
    overlap_actual <- ifelse(is.finite(gap), pmax(-gap, 0), NA_real_)

    threshold_status <- ifelse(
      nec_status == "estimable" & suf_rows$status == "estimable",
      ifelse(
        is.finite(gap) & gap < -tolerance,
        "frontiers overlap",
        ifelse(is.finite(gap) & abs(gap) <= tolerance,
               "exact correspondence", "admissible interval")
      ),
      ifelse(nec_status == "unattainable", "out of reach",
        ifelse(suf_rows$status == "no_threshold", "never guaranteed",
          ifelse(nec_status == "no_minimum", "no minimum required",
            ifelse(nec_status == "no_maximum", "no maximum required",
                   "not estimable")
          )
        )
      )
    )

    rows[[i]] <- data.frame(
      condition = condition,
      outcome = suf_rows$outcome,
      direction = suf_rows$direction,
      relationship = .nsca_relationship_text(
        unname(metadata$direction[[condition]]), condition, metadata$outcome
      ),
      ceiling = ceiling,
      convention = suf_rows$convention,
      inequality = inequality,
      condition_scale = suf_rows$condition_scale,
      outcome_scale = suf_rows$outcome_scale,
      outcome_level = suf_rows$outcome_level,
      outcome_level_actual = suf_rows$outcome_level_actual,
      necessity_threshold_actual = nec_actual,
      sufficiency_threshold_actual = suf_actual,
      necessity_status = nec_status,
      sufficiency_status = suf_rows$status,
      threshold_gap_actual = gap,
      necessity_sufficiency_interval_actual = width_actual,
      overlap_width_actual = overlap_actual,
      thresholds_compatible = ifelse(
        is.finite(gap), gap >= -tolerance, NA
      ),
      threshold_status = threshold_status,
      sufficiency_rule = suf_rows$rule,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  out <- do.call(rbind, rows)

  # Express both thresholds on the requested scale with the very same converter
  # SCAtools used, so the two columns are directly comparable. Anything else
  # risks the two sides differing by a convention rather than by the evidence.
  out$necessity_threshold <- .nsca_rescale(
    metadata, out, "necessity_threshold_actual", scale, convention, inequality
  )
  out$sufficiency_threshold <- .nsca_rescale(
    metadata, out, "sufficiency_threshold_actual", scale, convention, inequality
  )

  # The interval and the overlap are distances between the two threshold
  # columns, so they are reported on the scale those columns are reported on.
  # A width in actual units printed beside two percentages cannot be checked by
  # subtraction, and the Rd told the reader to normalise without offering the
  # normalised number.
  #
  # The magnitude is taken from the reported columns and the sign is carried
  # over from threshold_gap_actual. Every scale is a monotone transformation of
  # X, so the magnitude survives it; but under the "directional" convention a
  # low-X direction is mirrored, and reading the sign off the reported
  # difference would then invert admissible interval and frontiers overlap.
  # The sign is geometry, not presentation, and is settled in actual units.
  reported_gap <- abs(out$sufficiency_threshold - out$necessity_threshold) *
    sign(out$threshold_gap_actual)
  out$necessity_sufficiency_interval <- ifelse(
    is.finite(reported_gap), pmax(reported_gap, 0), NA_real_
  )
  out$overlap_width <- ifelse(
    is.finite(reported_gap), pmax(-reported_gap, 0), NA_real_
  )

  order_columns <- c(
    "condition", "outcome", "direction", "relationship", "ceiling",
    "convention", "inequality",
    "condition_scale", "outcome_scale", "outcome_level", "outcome_level_actual",
    "necessity_threshold", "sufficiency_threshold",
    "necessity_threshold_actual", "sufficiency_threshold_actual",
    "necessity_status", "sufficiency_status", "threshold_gap_actual",
    "necessity_sufficiency_interval", "overlap_width",
    "necessity_sufficiency_interval_actual", "overlap_width_actual",
    "thresholds_compatible",
    "threshold_status", "sufficiency_rule"
  )
  out <- out[, order_columns, drop = FALSE]
  rownames(out) <- NULL
  .nsca_append_legacy(out, legacy)
}

# Accept the retired 'boundary' argument for one more release cycle. The name
# was retired because the framework uses "boundary" for the theoretical line
# that separates an expected empty space from the compatible region -- the
# thing a frontier estimates -- not for the strictness of a reported
# inequality.
.nsca_resolve_inequality <- function(inequality, boundary, supplied) {
  if (!is.null(boundary)) {
    .Deprecated(
      new = "inequality",
      package = "NSCA",
      msg = paste0(
        "'boundary' is deprecated in NSCA 0.4.0 and will be removed in a ",
        "future release; use 'inequality' instead. In the condition analysis ",
        "vocabulary a boundary is the theoretical line an expected empty ",
        "space is separated by, not the strictness of a reported inequality."
      )
    )
    if (isTRUE(supplied)) {
      warning(
        "Both 'boundary' and 'inequality' were supplied; using 'inequality'.",
        call. = FALSE
      )
    } else {
      inequality <- boundary
    }
  }
  match.arg(inequality, c("strict", "inclusive"))
}

# Convert an actual-unit column to the reporting scale, one condition at a
# time, because the percentile and standard-deviation scales depend on that
# condition's own data.
.nsca_rescale <- function(metadata, out, column, scale, convention, inequality) {
  scale <- if (is.null(scale)) metadata$threshold.x else scale
  convention <- if (is.null(convention)) metadata$convention else convention
  values <- rep(NA_real_, nrow(out))
  for (condition in unique(out$condition)) {
    rows <- out$condition == condition
    raw <- metadata$raw$conditions[[condition]]
    letter <- substr(unname(metadata$direction[[condition]]), 1L, 1L)
    values[rows] <- SCAtools::sca_rescale(
      out[[column]][rows],
      reference = raw$x,
      bounds = raw$x_bounds,
      scale = scale,
      letter = letter,
      convention = convention,
      inequality = inequality
    )
  }
  values
}
