.nsca_metadata <- function(model) {
  metadata <- attr(model, "nsca")
  if (is.null(metadata)) {
    stop("The supplied object is not a valid 'nsca_result'.", call. = FALSE)
  }
  metadata
}

# Lines that produce no empty-zone estimate, and therefore no component of a
# necessary-and-sufficient claim.
#
# "ols" is an ordinary least-squares regression of the outcome on the
# condition, fitted to all the data: the line lm(y ~ x) returns. It summarises
# central tendency, not an empty space, so it can be neither the necessity nor
# the sufficiency component. Since 0.4.1 it is still available, through
# `reference`, as a comparison line: section 6.1 of the condition analysis in
# degree framework treats average-effect and condition analysis as different
# targets that are most informative shown together rather than merged into one
# number. Drawing it changes no NSCA statistic.
.nsca_unusable_ceilings <- c("ols")
.nsca_reference_lines <- c("ols")

.nsca_validate_reference <- function(reference) {
  if (is.null(reference)) {
    return(character(0L))
  }
  reference <- as.character(reference)
  reference <- unique(tolower(trimws(reference[!is.na(reference)])))
  reference <- setdiff(reference, c("none", ""))
  if (length(reference) == 0L) {
    return(character(0L))
  }
  unknown <- setdiff(reference, .nsca_reference_lines)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown reference line(s): %s. Available: %s, or NULL for none.",
        paste(unknown, collapse = ", "),
        paste(.nsca_reference_lines, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  reference
}

.nsca_validate_ceilings <- function(ceilings) {
  if (!is.character(ceilings) || length(ceilings) == 0L) {
    stop("'ceilings' must name at least one frontier technique.", call. = FALSE)
  }
  ceilings <- unique(tolower(trimws(ceilings)))
  dropped <- intersect(ceilings, .nsca_unusable_ceilings)
  if (length(dropped) > 0L) {
    warning(
      sprintf(
        paste0(
          "Ignoring %s in 'ceilings': it is a central-tendency line, not an ",
          "empty-space frontier, so it yields no necessity or sufficiency ",
          "component. Pass reference = \"ols\" to draw it for comparison."
        ),
        paste(dropped, collapse = ", ")
      ),
      call. = FALSE
    )
    ceilings <- setdiff(ceilings, dropped)
  }
  if (length(ceilings) == 0L) {
    stop("No usable frontier technique remains.", call. = FALSE)
  }
  ceilings
}

# Practical-relevance thresholds for the two components. Significance and
# relevance are different questions, and a support decision that silently
# collapses them is the classic way to turn a large sample into a finding.
.nsca_validate_relevance <- function(relevance) {
  if (is.null(relevance)) {
    return(c(necessity = NA_real_, sufficiency = NA_real_))
  }
  values <- suppressWarnings(as.numeric(relevance))
  if (length(values) == 1L) {
    values <- c(values, values)
  }
  if (length(values) != 2L || anyNA(values) || any(values < 0)) {
    stop(
      paste0(
        "'relevance' must be NULL, one non-negative number applied to both ",
        "components, or two of them as c(necessity, sufficiency)."
      ),
      call. = FALSE
    )
  }
  if (!is.null(names(relevance)) && all(c("necessity", "sufficiency") %in%
                                        names(relevance))) {
    # Reorder the numbers already validated above. Reading the raw input again
    # would keep a character vector such as c(necessity = "0.1", ...) as
    # character, and the later effect >= threshold test would then compare
    # strings.
    values <- values[match(c("necessity", "sufficiency"), names(relevance))]
  }
  stats::setNames(values, c("necessity", "sufficiency"))
}

# Read one engine parameter without failing when it is absent.
.nsca_param <- function(fit, condition, ceiling, param, extractor) {
  value <- tryCatch(
    extractor(fit, x = condition, ceiling = ceiling, param = param),
    error = function(e) NA_real_
  )
  suppressWarnings(as.numeric(value)[[1L]])
}

.nsca_nca_param <- function(fit, condition, ceiling, param) {
  .nsca_param(fit, condition, ceiling, param, NCA::nca_extract)
}

.nsca_sca_param <- function(fit, condition, ceiling, param) {
  .nsca_param(fit, condition, ceiling, param, SCAtools::sca_extract)
}

#' Necessary and sufficient condition analysis
#'
#' Estimates the two empty-space frontiers implied by a
#' necessary-and-sufficient statement and assesses them jointly. Sufficiency is delegated to
#' [SCAtools::sca_analysis()] and necessity to [NCA::nca_analysis()], on
#' diagonally opposite corners of the same scatter plot.
#'
#' @section Matched estimation:
#' Both sides are always estimated with the same frontier technique, the same
#' theoretical scope, the same observations and the same outcome levels. Every
#' joint index compares the two empty zones, and a comparison is only
#' meaningful when both sides are measured the same way: envelopment frontiers
#' hug the data and yield the largest empty areas, while regression frontiers
#' cut into them, so mixing techniques across sides would decide which side
#' looks weaker by the choice of technique rather than by the evidence.
#' Supplying several techniques produces one internally matched row per
#' technique, which is the right way to check whether a conclusion survives
#' that choice.
#'
#' @section Degenerate scopes:
#' A constant outcome is refused outright when no theoretical scope is given,
#' because it has no scope of its own and every empty area would be measured
#' against a scope of zero height. With a scope imposed the analysis runs but
#' warns, and marks the affected rows `degenerate` in [nsca_table()]. An axis
#' that does not move inside the declared scope makes every reported area a
#' property of the scope rather than of the data, and it does so flatteringly:
#' a constant outcome at the centre of the scope leaves the upper and lower
#' halves both empty, so both components reach 0.5, the admissible region
#' closes, and every joint index reports perfect joint support for data
#' carrying no information. A milder warning fires when the observations span less than
#' five per cent of the declared scope on either axis.
#'
#' @section The shared permutation sequence:
#' `shared.test.rep` replaces the engines' two separate permutation runs with
#' one sequence that drives both. Each replication shuffles the outcome once,
#' refits both engines on that same shuffled frame, and records `d_nec`,
#' `d_suf` and their minimum together. This is what makes `p_weakest_perm`
#' meaningful: the null distribution of a statistic of both components depends
#' on how they co-vary under random pairing, and independent permutations
#' would destroy exactly that dependence. When it is used, `p_nec` and `p_suf`
#' are taken from the same sequence, the `p_source` column reports `"shared"`,
#' and all four p-values are mutually consistent. It costs two engine fits per
#' replication, so it is off by default.
#'
#' @param data A data frame or object coercible to a data frame.
#' @param x Columns containing one or more conditions.
#' @param y A single outcome column.
#' @param direction Necessary-and-sufficient direction(s): `"HH"`, `"LH"`,
#'   `"HL"`, or
#'   `"LL"`. The first letter is the condition level and the second the outcome
#'   level.
#' @param ceilings One or more empty-space frontier techniques, applied
#'   identically to both sides. `"ols"` is rejected here because it estimates
#'   central tendency rather than an empty space; pass it as `reference`
#'   instead.
#' @param reference Central-tendency lines drawn beside the two frontiers for
#'   comparison, or `NULL` for none. `"ols"` is the ordinary least-squares
#'   regression of the outcome on the condition, fitted to all the data. It is
#'   an average-effect summary, enters no joint index, no threshold and no
#'   support decision, and is reported by [nsca_reference()].
#' @param scope Optional theoretical scope `c(xmin, xmax, ymin, ymax)`, shared
#'   by both sides.
#' @param threshold.x,threshold.y Reporting scales for [nsca_thresholds()], as
#'   in [SCAtools::sca_analysis()].
#' @param convention Reporting convention, as in [SCAtools::sca_analysis()].
#' @param steps Number of outcome levels, or an explicit vector of levels on
#'   the `threshold.y` scale.
#' @param step.size Optional spacing between outcome levels.
#' @param cutoff How out-of-range threshold values are represented.
#' @param qr.tau Quantile used by the quantile-regression frontier.
#' @param test.rep Number of permutation resamples for the engines' own
#'   component tests. Zero skips testing, and without it no joint-support verdict
#'   can be reached.
#' @param test.p_confidence Confidence level for permutation p-value accuracy.
#' @param test.p_threshold Significance level used by the intersection-union
#'   combination of the two directional component tests.
#' @param relevance Optional pre-specified practical-relevance thresholds for
#'   the component effect sizes, as one number applied to both or
#'   `c(necessity, sufficiency)`. Support requires an effect at least this
#'   large as well as a significant test. `NULL` means no relevance criterion
#'   was pre-specified, and support then rests on significance alone.
#' @param shared.test.rep Number of replications of the shared permutation
#'   sequence. Zero, the default, skips it; `p_weakest_perm` is then `NA` and
#'   the component p-values come from the engines.
#' @param shared.seed Optional seed for the shared sequence. The caller's
#'   random stream is restored afterwards.
#' @param geometry.max_overlap Largest excess frontier overlap, beyond what a
#'   step frontier produces by construction, that still counts as acceptable
#'   geometry.
#' @param geometry.max_reconstruction Largest residual of the area identity
#'   that still counts as acceptable geometry.
#' @param purity Compute the engine's extra purity metrics where it offers
#'   them. Off by default: the engine computes them only for a corner with
#'   neither axis flipped, so exactly one side of an NSCA model can ever have
#'   them, and which side depends on the direction. No NSCA statistic uses
#'   them, and they are expensive on data with many frontier points.
#' @return An object of class `nsca_result`.
#' @seealso [nsca_table()], [nsca_results()], [nsca_joint()],
#'   [nsca_thresholds()], [nsca_corners()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' fit <- nsca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")
#' fit
nsca_analysis <- function(
    data,
    x,
    y,
    direction = "HH",
    ceilings = c("ce_fdh", "cr_fdh"),
    reference = NULL,
    scope = NULL,
    threshold.x = "percentage.range",
    threshold.y = "percentage.range",
    convention = c("absolute", "directional"),
    steps = 10,
    step.size = NULL,
    cutoff = 0,
    qr.tau = 0.95,
    test.rep = 0,
    test.p_confidence = 0.95,
    test.p_threshold = 0.05,
    relevance = NULL,
    shared.test.rep = 0,
    shared.seed = NULL,
    geometry.max_overlap = 0.01,
    geometry.max_reconstruction = 0.02,
    purity = FALSE) {

  frame <- as.data.frame(data)
  x_names <- colnames(frame[x])
  y_name <- colnames(frame[y])
  if (length(y_name) != 1L) {
    stop("'y' must select exactly one outcome column.", call. = FALSE)
  }
  if (length(x_names) == 0L) {
    stop("At least one condition must be selected in 'x'.", call. = FALSE)
  }

  direction <- .nsca_validate_direction(direction, length(x_names))
  y_letters <- unique(substr(direction, 2L, 2L))
  if (length(y_letters) > 1L) {
    stop(
      paste0(
        "All conditions in one analysis must use the same outcome direction. ",
        "Run separate models for high-Y and low-Y statements."
      ),
      call. = FALSE
    )
  }
  ceilings <- .nsca_validate_ceilings(ceilings)
  reference <- .nsca_validate_reference(reference)
  convention <- match.arg(convention)
  relevance <- .nsca_validate_relevance(relevance)
  if (!is.numeric(shared.test.rep) || length(shared.test.rep) != 1L ||
      is.na(shared.test.rep) || shared.test.rep < 0) {
    stop("'shared.test.rep' must be a single non-negative number.",
         call. = FALSE)
  }
  shared.test.rep <- as.integer(shared.test.rep)

  necessity_corner <- unname(.nsca_necessity_corner[direction])
  sufficiency_corner <- unname(.nsca_sufficiency_corner[direction])

  raw <- .nsca_raw_data(frame, x_names, y_name, scope)
  if (diff(raw$y_bounds) <= 0) {
    stop(
      paste0(
        "The outcome takes a single value, so it has no scope of its own and ",
        "neither component is defined: every empty area would be measured ",
        "against a scope of zero height. Supply a theoretical 'scope' with ",
        "distinct ymin and ymax if the outcome is bounded by design. Note that ",
        "the result then depends on where the constant sits inside that scope."
      ),
      call. = FALSE
    )
  }

  # The engine computes its purity metrics only when neither axis is flipped.
  # Of the two corners an NSCA model uses, at most one is unflipped, so these
  # metrics can never be available symmetrically, and which side gets them is
  # decided by the direction rather than by anything substantive. They also
  # cost a great deal on data with many frontier points. Off unless asked for.
  if (!isTRUE(purity)) {
    previous <- Sys.getenv("NCA_SKIP_PURITY", unset = NA)
    Sys.setenv(NCA_SKIP_PURITY = "TRUE")
    on.exit(
      if (is.na(previous)) {
        Sys.unsetenv("NCA_SKIP_PURITY")
      } else {
        Sys.setenv(NCA_SKIP_PURITY = previous)
      },
      add = TRUE
    )
  }

  # Sufficiency first: it settles the outcome levels, which the necessity side
  # then reuses so that the two threshold tables line up row for row.
  sufficiency <- SCAtools::sca_analysis(
    data = frame,
    x = x,
    y = y,
    direction = direction,
    ceilings = ceilings,
    reference = reference,
    scope = scope,
    threshold.x = threshold.x,
    threshold.y = threshold.y,
    convention = convention,
    steps = steps,
    step.size = step.size,
    cutoff = cutoff,
    qr.tau = qr.tau,
    test.rep = test.rep,
    test.p_confidence = test.p_confidence,
    test.p_threshold = test.p_threshold
  )
  sca_meta <- attr(sufficiency, "sca")
  outcome_levels <- sca_meta$outcome_levels

  necessity <- NCA::nca_analysis(
    data = frame,
    x = x,
    y = y,
    ceilings = ceilings,
    corner = necessity_corner,
    scope = scope,
    bottleneck.x = "actual",
    bottleneck.y = "actual",
    steps = outcome_levels,
    step.size = NULL,
    cutoff = cutoff,
    qr.tau = qr.tau,
    effect_aggregation = 1,
    test.rep = test.rep,
    test.p_confidence = test.p_confidence,
    test.p_threshold = test.p_threshold
  )

  condition_names <- names(sufficiency$summaries)
  outcome_name <- sca_meta$outcome
  names(direction) <- condition_names
  names(necessity_corner) <- condition_names
  names(sufficiency_corner) <- condition_names

  raw$conditions <- raw$conditions[match(condition_names, names(raw$conditions))]
  names(raw$conditions) <- condition_names

  # Every reported quantity is an area over the declared scope. When one axis
  # barely moves inside that scope, the areas are decided by the scope and not
  # by the data, and they are decided in the flattering direction. Flag it
  # here, once, rather than leaving it to be read off a table.
  degeneracy <- lapply(condition_names, function(condition) {
    item <- raw$conditions[[condition]]
    x_coverage <- .nsca_axis_coverage(item$x, item$x_bounds)
    y_coverage <- .nsca_axis_coverage(item$y, item$y_bounds)
    worst <- min(c(x_coverage, y_coverage))
    list(
      x_coverage = x_coverage,
      y_coverage = y_coverage,
      degenerate = !is.finite(worst) || worst <= .nsca_degenerate_tolerance,
      thin = is.finite(worst) && worst > .nsca_degenerate_tolerance &&
        worst < .nsca_thin_coverage
    )
  })
  names(degeneracy) <- condition_names

  flagged <- function(field) {
    condition_names[vapply(degeneracy, function(item) isTRUE(item[[field]]),
                           logical(1L))]
  }
  flat <- flagged("degenerate")
  if (length(flat) > 0L) {
    warning(
      sprintf(
        paste0(
          "%s: the condition or the outcome does not vary inside the declared ",
          "scope. Every empty area is then decided by where that constant sits ",
          "in the scope rather than by the data, and it is decided ",
          "flatteringly: a constant outcome at the centre of an imposed scope ",
          "gives d_nec = d_suf = 0.5, closes the admissible region, and scores ",
          "1 on every joint index while carrying no information. Read the ",
          "permutation p-values instead; nsca_table() marks these rows ",
          "degenerate and geometry_acceptable is FALSE."
        ),
        paste(flat, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  thin <- flagged("thin")
  if (length(thin) > 0L) {
    warning(
      sprintf(
        paste0(
          "%s: the observations span less than %g%% of the declared scope on ",
          "at least one axis, so most of every reported area is unobserved ",
          "scope rather than evidence. Check that this is the intended scope."
        ),
        paste(thin, collapse = ", "), 100 * .nsca_thin_coverage
      ),
      call. = FALSE
    )
  }

  statements <- vapply(
    condition_names,
    function(condition) {
      .nsca_statement(direction[[condition]], condition, outcome_name)
    },
    character(1L)
  )

  # One permutation sequence for both engines, when asked for.
  shared <- NULL
  if (shared.test.rep > 0L) {
    complete <- frame[stats::complete.cases(frame[c(x_names, y_name)]), ,
                      drop = FALSE]
    fitted_counts <- vapply(raw$conditions, function(item) length(item$x),
                            integer(1L))
    if (nrow(complete) < max(fitted_counts)) {
      warning(
        paste0(
          "The shared permutation sequence uses listwise-complete rows (",
          nrow(complete), "), while at least one component was fitted on more ",
          "(", max(fitted_counts), "). Its p-values therefore describe a ",
          "slightly smaller sample than the observed effect sizes."
        ),
        call. = FALSE
      )
    }
    # Address the conditions by the engines' own names throughout, so that the
    # direction and the corner cannot drift out of step with the column order
    # if an engine ever returns its summaries in a different order than it was
    # given them.
    shared <- .nsca_shared_null(
      frame = complete, x_names = condition_names, y_name = y_name,
      direction = unname(direction[condition_names]), ceilings = ceilings,
      scope = scope,
      necessity_corner = unname(necessity_corner[condition_names]),
      cutoff = cutoff, qr.tau = qr.tau,
      reps = shared.test.rep, seed = shared.seed
    )
  }

  model <- list(necessity = necessity, sufficiency = sufficiency)
  attr(model, "nsca") <- list(
    call = match.call(),
    direction = direction,
    necessity_corner = necessity_corner,
    sufficiency_corner = sufficiency_corner,
    statement = statements,
    conditions = condition_names,
    outcome = outcome_name,
    outcome_letter = y_letters[[1L]],
    ceilings = ceilings,
    reference = reference,
    outcome_levels = outcome_levels,
    threshold.x = threshold.x,
    threshold.y = threshold.y,
    convention = convention,
    cutoff = cutoff,
    purity = isTRUE(purity),
    degeneracy = degeneracy,
    relevance = relevance,
    geometry = c(max_overlap = geometry.max_overlap,
                 max_reconstruction = geometry.max_reconstruction),
    test.rep = test.rep,
    shared.test.rep = shared.test.rep,
    shared = shared,
    alpha = test.p_threshold,
    raw = raw,
    engines = c(
      NCA = as.character(utils::packageVersion("NCA")),
      SCAtools = as.character(utils::packageVersion("SCAtools"))
    )
  )
  class(model) <- "nsca_result"
  model
}

#' Quick necessary and sufficient condition analysis
#'
#' A thin wrapper over [nsca_analysis()] with a permutation test attached.
#'
#' @inheritParams nsca_analysis
#' @return An object of class `nsca_result`.
#' @seealso [nsca_analysis()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' \donttest{
#' # The default 1000 permutations take several seconds.
#' nsca(dat, "X", "Y")
#' }
nsca <- function(data, x, y, direction = "HH", ceilings = "ce_fdh",
                 reference = NULL, test.rep = 1000) {
  nsca_analysis(
    data = data, x = x, y = y, direction = direction,
    ceilings = ceilings, reference = reference, test.rep = test.rep
  )
}
