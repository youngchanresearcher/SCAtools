# The dual-threshold table: reporting arguments and the signed gap.
#
# The gap between the two thresholds is the only place the package reports a
# quantity whose sign carries meaning. A negative gap means the frontiers have
# crossed at that outcome level, which is a modelling problem rather than a
# narrow admissible interval; clamping it to zero, as an earlier version did,
# would present the failure as a perfect result.

th_data <- function(n = 60, noise = 0.12, seed = 8, decreasing = FALSE) {
  set.seed(seed)
  x <- sort(runif(n))
  y <- pmin(pmax(x + rnorm(n, 0, noise), 0), 1)
  data.frame(X = x, Y = if (decreasing) 1 - y else y)
}

test_that("the table has one row per outcome level and stays ordered", {
  fit <- nsca_analysis(th_data(), "X", "Y", ceilings = "ce_fdh", steps = 6)
  rules <- nsca_thresholds(fit)
  expect_equal(nrow(rules), length(attr(fit, "nsca")$outcome_levels))
  expect_false(is.unsorted(rules$outcome_level_actual))
  expect_false(anyDuplicated(rules$outcome_level_actual) > 0)
})

test_that("the gap keeps its sign and splits into two non-negative widths", {
  fit <- nsca_analysis(th_data(), "X", "Y", ceilings = "ce_fdh", steps = 6)
  rules <- nsca_thresholds(fit)
  finite <- is.finite(rules$threshold_gap_actual)

  expect_equal(rules$necessity_sufficiency_interval_actual[finite],
               pmax(rules$threshold_gap_actual[finite], 0))
  expect_equal(rules$overlap_width_actual[finite],
               pmax(-rules$threshold_gap_actual[finite], 0))
  # At most one of the two can be positive on any row.
  expect_true(all(rules$necessity_sufficiency_interval[finite] *
                    rules$overlap_width[finite] == 0))
  expect_true(all(rules$necessity_sufficiency_interval[finite] >= 0))
  expect_true(all(rules$overlap_width[finite] >= 0))
})

test_that("compatibility, threshold_status and the gap tell the same story", {
  fit <- nsca_analysis(th_data(), "X", "Y", ceilings = "ce_fdh", steps = 6)
  rules <- nsca_thresholds(fit)
  finite <- is.finite(rules$threshold_gap_actual)

  # A row flagged incompatible must have a real overlap behind it, and must be
  # labelled as such rather than passed off as a narrow admissible interval.
  incompatible <- finite & !rules$thresholds_compatible
  if (any(incompatible)) {
    expect_true(all(rules$overlap_width[incompatible] > 0))
  }
  expect_true(all(rules$thresholds_compatible[finite &
                    rules$threshold_gap_actual > 0]))
  if (any(incompatible)) {
    expect_true(all(rules$threshold_status[incompatible] == "frontiers overlap"))
  }
  expect_true(all(rules$threshold_status %in% c(
    "exact correspondence", "admissible interval", "frontiers overlap", "out of reach",
    "never guaranteed", "no minimum required", "no maximum required",
    "not estimable"
  )))
})

test_that("the gap is direction-oriented, not a raw subtraction", {
  # For a high-X statement the sufficiency threshold sits above the necessity
  # one; for a low-X statement the ordering reverses. A raw suf - nec would
  # therefore be negative for every low-X row and report a permanent failure.
  for (direction in c("HH", "LH", "HL", "LL")) {
    decreasing <- direction %in% c("LH", "HL")
    dat <- th_data(decreasing = decreasing)
    fit <- nsca_analysis(dat, "X", "Y", direction = direction,
                         ceilings = "ce_fdh", steps = 5)
    rules <- nsca_thresholds(fit)
    both <- rules$necessity_status == "estimable" &
      rules$sufficiency_status == "estimable"
    if (!any(both)) {
      next
    }
    x_letter <- substr(direction, 1L, 1L)
    raw <- rules$sufficiency_threshold_actual - rules$necessity_threshold_actual
    expected <- if (x_letter == "H") raw else -raw
    expect_equal(rules$threshold_gap_actual[both], expected[both],
                 info = direction)
  }
})

test_that("the necessity status names the bound the direction implies", {
  # An out-of-range necessity threshold is "no minimum required" for a high-X
  # statement and "no maximum required" for a low-X one. Reusing one label
  # would tell a low-X reader to look at the wrong end of the axis.
  for (direction in c("HH", "LL")) {
    dat <- th_data(decreasing = FALSE)
    rules <- nsca_thresholds(
      nsca_analysis(dat, "X", "Y", direction = direction,
                    ceilings = "ce_fdh", steps = 5)
    )
    allowed <- if (substr(direction, 1L, 1L) == "H") {
      c("estimable", "unattainable", "no_minimum")
    } else {
      c("estimable", "unattainable", "no_maximum")
    }
    expect_true(all(rules$necessity_status %in% allowed), info = direction)
  }
})

test_that("actual values are the invariant and the scale is only a view", {
  fit <- nsca_analysis(th_data(), "X", "Y", ceilings = "ce_fdh", steps = 5)
  actual <- nsca_thresholds(fit, scale = "actual")
  for (scale in c("percentage.range", "percentage.max", "percentile", "sd")) {
    view <- nsca_thresholds(fit, scale = scale)
    expect_equal(view$necessity_threshold_actual,
                 actual$necessity_threshold_actual, info = scale)
    expect_equal(view$sufficiency_threshold_actual,
                 actual$sufficiency_threshold_actual, info = scale)
    expect_equal(view$threshold_gap_actual, actual$threshold_gap_actual,
                 info = scale)
    expect_identical(view$condition_scale[[1L]], scale)
  }
})

test_that("both threshold columns go through the same converter", {
  dat <- th_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", steps = 5)
  rules <- nsca_thresholds(fit, scale = "percentile")
  # The engine coerces every column through as.character(), which can move the
  # last bits of a double. A percentile is a step function, so at a threshold
  # sitting exactly on an observation that shift changes the count by one case.
  # The reference must therefore be the coerced data.
  reference <- as.numeric(as.character(dat$X))
  for (side in c("necessity", "sufficiency")) {
    actual <- rules[[paste0(side, "_threshold_actual")]]
    shown <- rules[[paste0(side, "_threshold")]]
    ok <- is.finite(actual)
    expect_equal(
      shown[ok],
      SCAtools::sca_rescale(actual[ok], reference = reference,
                            bounds = range(reference), scale = "percentile",
                            letter = "H"),
      info = side
    )
  }
})

test_that("the outcome scale can be chosen independently of the condition", {
  fit <- nsca_analysis(th_data(), "X", "Y", ceilings = "ce_fdh", steps = 5)
  mixed <- nsca_thresholds(fit, scale = "sd", outcome_scale = "percentile")
  expect_identical(mixed$condition_scale[[1L]], "sd")
  expect_identical(mixed$outcome_scale[[1L]], "percentile")
  expect_true(all(mixed$outcome_level >= -1e-9 & mixed$outcome_level <= 100 + 1e-9))
})

test_that("the inequality convention reaches the rule text", {
  fit <- nsca_analysis(th_data(), "X", "Y", ceilings = "ce_fdh", steps = 5)
  strict <- nsca_thresholds(fit, inequality = "strict")
  inclusive <- nsca_thresholds(fit, inequality = "inclusive")
  usable <- !is.na(strict$sufficiency_rule) & !is.na(inclusive$sufficiency_rule)
  expect_true(any(usable))
  expect_true(all(grepl(">", strict$sufficiency_rule[usable], fixed = TRUE)))
  expect_true(all(grepl(">=", inclusive$sufficiency_rule[usable], fixed = TRUE)))
  # The underlying numbers do not move with the reporting convention.
  expect_equal(strict$necessity_threshold_actual,
               inclusive$necessity_threshold_actual)
})

test_that("a specific frontier and condition can be selected", {
  dat <- th_data()
  two <- data.frame(X1 = dat$X, X2 = runif(nrow(dat)), Y = dat$Y)
  fit <- nsca_analysis(two, c("X1", "X2"), "Y",
                       ceilings = c("ce_fdh", "cr_fdh"), steps = 4)

  expect_setequal(nsca_thresholds(fit)$condition, c("X1", "X2"))
  expect_identical(unique(nsca_thresholds(fit, x = "X2")$condition), "X2")
  expect_identical(unique(nsca_thresholds(fit, ceiling = "cr_fdh")$ceiling),
                   "cr_fdh")
  # The default frontier is the first that was requested.
  expect_identical(unique(nsca_thresholds(fit)$ceiling), "ce_fdh")
})

test_that("the row-level gap and the scope-level band agree in direction", {
  # admissible_region_share summarises the same geometry the per-row widths
  # describe, so a tighter relation must shrink both.
  tight <- nsca_analysis(th_data(noise = 0.03), "X", "Y", ceilings = "ce_fdh",
                         steps = 8)
  loose <- nsca_analysis(th_data(noise = 0.30), "X", "Y", ceilings = "ce_fdh",
                         steps = 8)
  mean_width <- function(fit) {
    rules <- nsca_thresholds(fit)
    mean(rules$necessity_sufficiency_interval[is.finite(rules$necessity_sufficiency_interval)])
  }
  expect_lt(mean_width(tight), mean_width(loose))
  expect_lt(nsca_table(tight)$admissible_region_share,
            nsca_table(loose)$admissible_region_share)
})

test_that("the interval is reported on the scale the thresholds are on", {
  # The three columns sit side by side in one printed row, so the width must be
  # the distance between the two thresholds as printed. Before 0.4.3 it was in
  # actual units on every scale, and the row could not be checked by
  # subtraction.
  fit <- nsca_analysis(th_data(), "X", "Y", ceilings = "ce_fdh", steps = 6)
  actual <- nsca_thresholds(fit, scale = "actual")
  for (scale in c("actual", "percentage.range", "percentage.max",
                  "percentile", "sd")) {
    rules <- nsca_thresholds(fit, scale = scale)
    finite <- is.finite(rules$threshold_gap_actual) &
      is.finite(rules$necessity_threshold) &
      is.finite(rules$sufficiency_threshold)

    expect_equal(
      rules$necessity_sufficiency_interval[finite] -
        rules$overlap_width[finite],
      abs(rules$sufficiency_threshold[finite] -
            rules$necessity_threshold[finite]) *
        sign(rules$threshold_gap_actual[finite]),
      info = scale
    )
    expect_true(all(rules$necessity_sufficiency_interval[finite] >= 0),
                info = scale)
    expect_true(all(rules$overlap_width[finite] >= 0), info = scale)

    # The actual-unit pair is the invariant and does not move with the view,
    # and neither does the reading of the geometry.
    expect_equal(rules$necessity_sufficiency_interval_actual,
                 actual$necessity_sufficiency_interval_actual, info = scale)
    expect_equal(rules$overlap_width_actual, actual$overlap_width_actual,
                 info = scale)
    expect_equal(rules$threshold_status, actual$threshold_status, info = scale)
  }

  # On the actual scale the reported pair and the actual pair are one thing.
  expect_equal(actual$necessity_sufficiency_interval,
               actual$necessity_sufficiency_interval_actual)
  expect_equal(actual$overlap_width, actual$overlap_width_actual)
})

test_that("a mirrored directional view does not invert the interval", {
  # Under convention = "directional" a low-X direction is mirrored, so the
  # difference between the two reported columns changes sign while the geometry
  # does not. Reading the sign off the reported columns would report an
  # admissible interval as crossed frontiers.
  fit <- nsca_analysis(th_data(decreasing = TRUE), "X", "Y", direction = "LH",
                       ceilings = "ce_fdh", steps = 6)
  base <- nsca_thresholds(fit, scale = "actual")
  view <- nsca_thresholds(fit, scale = "percentage.range",
                          convention = "directional")
  finite <- is.finite(view$threshold_gap_actual)

  expect_true(all(view$necessity_sufficiency_interval[finite] >= 0))
  expect_true(all(view$overlap_width[finite] >= 0))
  expect_equal(view$threshold_status, base$threshold_status)
  expect_equal(
    sign(view$necessity_sufficiency_interval[finite] -
           view$overlap_width[finite]),
    sign(base$necessity_sufficiency_interval_actual[finite] -
           base$overlap_width_actual[finite])
  )
})
