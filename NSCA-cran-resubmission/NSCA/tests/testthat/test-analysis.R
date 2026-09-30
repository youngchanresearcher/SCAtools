make_data <- function(n = 80, noise = 0.10, seed = 3) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, noise), 0), 1))
}

test_that("both sides reproduce their own engine on the mapped corner", {
  dat <- make_data()
  fit <- nsca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")

  nca_fit <- NCA::nca_analysis(dat, "X", "Y", corner = 1, ceilings = "ce_fdh")
  sca_fit <- SCAtools::sca_analysis(dat, "X", "Y", direction = "HH",
                                    ceilings = "ce_fdh")
  expect_equal(
    nsca_extract(fit, param = "d_nec"),
    as.numeric(NCA::nca_extract(nca_fit, "X", "ce_fdh", "Effect size"))
  )
  expect_equal(
    nsca_extract(fit, param = "d_suf"),
    as.numeric(SCAtools::sca_extract(sca_fit, "X", "ce_fdh", "Effect size"))
  )
})

test_that("the two joint indices are the two power means they claim to be", {
  dat <- make_data()
  table <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  expect_equal(table$weakest_effect, pmin(table$d_nec, table$d_suf))
  expect_equal(table$balanced_joint_effect, 2 * sqrt(table$d_nec * table$d_suf))
  # They are the same family at p = -Inf and p = 0, so the ordering between
  # them is fixed: min <= geometric mean, and doubling preserves it.
  expect_lte(2 * table$weakest_effect, table$balanced_joint_effect + 1e-12)
  expect_equal(table$weakest_effect,
               nsca_joint(table$d_nec, table$d_suf, p = -Inf,
                          normalize = FALSE))
  expect_equal(table$balanced_joint_effect, nsca_joint(table$d_nec, table$d_suf, p = 0))
})

test_that("complementary directions swap components but preserve the joint pattern", {
  positive <- make_data()
  hh <- nsca_table(nsca_analysis(
    positive, "X", "Y", direction = "HH", ceilings = "ce_fdh"
  ))
  ll <- nsca_table(nsca_analysis(
    positive, "X", "Y", direction = "LL", ceilings = "ce_fdh"
  ))
  expect_equal(hh$d_nec, ll$d_suf)
  expect_equal(hh$d_suf, ll$d_nec)
  expect_equal(hh$weakest_effect, ll$weakest_effect)
  expect_equal(hh$balanced_joint_effect, ll$balanced_joint_effect)
  expect_equal(hh$admissible_region_share, ll$admissible_region_share)

  negative <- positive
  negative$Y <- 1 - negative$Y
  lh <- nsca_table(nsca_analysis(
    negative, "X", "Y", direction = "LH", ceilings = "ce_fdh"
  ))
  hl <- nsca_table(nsca_analysis(
    negative, "X", "Y", direction = "HL", ceilings = "ce_fdh"
  ))
  expect_equal(lh$d_nec, hl$d_suf)
  expect_equal(lh$d_suf, hl$d_nec)
  expect_equal(lh$weakest_effect, hl$weakest_effect)
  expect_equal(lh$balanced_joint_effect, hl$balanced_joint_effect)
  expect_equal(lh$admissible_region_share, hl$admissible_region_share)
})

test_that("a deterministic relation approaches the 0.5 ceiling", {
  # A step frontier overlaps itself by about one tread between consecutive
  # observations, so both components are inflated by a term of order 1/n and
  # the cap is only approached, not respected exactly. The excess must shrink
  # as the sample grows; that is what distinguishes discretization from a bug.
  excess <- vapply(
    c(20L, 40L, 200L),
    function(n) {
      dat <- data.frame(X = seq(0, 1, length.out = n),
                        Y = seq(0, 1, length.out = n))
      table <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
      expect_equal(table$admissible_region_share, 0, tolerance = 1e-6)
      # The normalised index inherits exactly twice the same excess, which is
      # the point of leaving it unclamped: the discretization stays visible.
      expect_equal(table$balanced_joint_effect, 2 * table$weakest_effect, tolerance = 1e-4)
      table$weakest_effect - 0.5
    },
    numeric(1L)
  )
  expect_true(all(excess > 0))
  expect_true(all(diff(excess) < 0))
  expect_lt(excess[[3L]], 0.01)
  expect_lt(abs(excess[[2L]] - 1 / (2 * 39)), 0.002)
})

test_that("a constant outcome is refused with an explanation", {
  # A constant outcome has no scope of its own, so every empty area would be
  # measured against zero height. Guessing a scope here would produce a number
  # that depends entirely on where the constant happens to sit.
  dat <- data.frame(X = seq(0, 1, length.out = 40), Y = rep(0.5, 40))
  expect_error(
    nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"),
    "single value"
  )
  # With a scope imposed it runs, but it warns, and the numbers are exactly as
  # scope-dependent as the error message warns.
  expect_warning(
    centred_fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                 scope = c(0, 1, 0, 1)),
    "does not vary"
  )
  centred <- nsca_table(centred_fit)
  low <- data.frame(X = dat$X, Y = rep(0.02, 40))
  edged <- nsca_table(suppressWarnings(
    nsca_analysis(low, "X", "Y", ceilings = "ce_fdh", scope = c(0, 1, 0, 1))
  ))
  expect_gt(centred$weakest_effect, edged$weakest_effect + 0.3)

  # Both rows must carry the flag, and the centred one is the flattering case:
  # a line with no information at the centre of the scope maxes out every
  # index at once. This is the reason the flag exists.
  expect_true(centred$degenerate)
  expect_true(edged$degenerate)
  expect_equal(centred$d_nec, 0.5, tolerance = 0.01)
  expect_equal(centred$d_suf, 0.5, tolerance = 0.01)
  expect_equal(centred$balanced_joint_effect, 1, tolerance = 0.02)
  expect_equal(centred$joint_empty_zone_coverage, 1, tolerance = 0.01)
  # Moving the same uninformative line off centre changes the answer, which is
  # the tell: nothing about the data moved.
  expect_lt(edged$balanced_joint_effect, centred$balanced_joint_effect - 0.2)
})

test_that("a flat frontier over a filled scope is the opposite case", {
  # The other thing people call a horizontal line. Here the data fills the
  # scope and both frontiers are flat against its edges, so there is no empty
  # zone on either side. Every index must be at its floor, not its ceiling,
  # and nothing should be flagged degenerate.
  set.seed(11)
  n <- 400
  filled <- data.frame(X = runif(n), Y = runif(n))
  row <- nsca_table(nsca_analysis(filled, "X", "Y", ceilings = "ce_fdh"))

  expect_false(row$degenerate)
  expect_lt(row$d_nec, 0.05)
  expect_lt(row$d_suf, 0.05)
  expect_lt(row$weakest_effect, 0.05)
  expect_lt(row$balanced_joint_effect, 0.10)
  expect_lt(row$joint_empty_zone_coverage, 0.10)
  expect_gt(row$admissible_region_share, 0.90)
})

test_that("a scope far larger than the data is warned about", {
  dat <- make_data()
  expect_warning(
    nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                  scope = c(-50, 50, -50, 50)),
    "less than 5% of the declared scope"
  )
  # A merely generous scope is not warned about.
  seen <- character(0L)
  withCallingHandlers(
    nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", scope = c(-1, 2, -1, 2)),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("declared scope", seen)))
})

test_that("one technique per row, applied to both sides", {
  dat <- make_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = c("ce_fdh", "cr_fdh"))
  table <- nsca_table(fit)
  expect_equal(nrow(table), 2L)
  expect_setequal(table$ceiling, c("ce_fdh", "cr_fdh"))
  # There is no way to ask for different techniques on the two sides: the
  # column is single-valued per row by construction.
  expect_true(all(table$ceiling %in% c("ce_fdh", "cr_fdh")))
})

test_that("ols is refused because it is not an empty-space frontier", {
  dat <- make_data()
  expect_warning(
    fit <- nsca_analysis(dat, "X", "Y", ceilings = c("ols", "ce_fdh")),
    "central-tendency"
  )
  expect_identical(nsca_table(fit)$ceiling, "ce_fdh")
  # The same call warns before it stops, so both conditions must be caught.
  expect_warning(
    expect_error(nsca_analysis(dat, "X", "Y", ceilings = "ols"), "No usable"),
    "central-tendency"
  )
})

test_that("joint support needs both components to reject", {
  dat <- make_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", test.rep = 200)
  table <- nsca_table(fit)
  expect_equal(table$p_nsca_iut, max(table$p_nec, table$p_suf))
  expect_identical(
    table$nsca_significant,
    table$necessity_significant && table$sufficiency_significant
  )
  expect_true(table$joint_support_status %in%
    c("supported", "necessity fails", "sufficiency fails", "neither component"))
  if (table$joint_support_status == "supported") {
    expect_true(table$p_nec <= 0.05 && table$p_suf <= 0.05)
  }
  # Without a test there can be no verdict.
  untested <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  expect_identical(untested$joint_support_status, "not tested")
  expect_true(is.na(untested$p_nsca_iut))
  expect_true(is.na(untested$nsca_significant))
})

test_that("all three analysis layers are reported explicitly", {
  dat <- make_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  tables <- nsca_results(fit)
  expect_named(
    tables,
    c("necessity", "sufficiency", "necessary_and_sufficient")
  )
  expect_equal(unname(vapply(tables, nrow, integer(1L))), c(1L, 1L, 1L))
  expect_equal(
    tables$necessity$effect_size,
    tables$necessary_and_sufficient$d_nec
  )
  expect_equal(
    tables$sufficiency$effect_size,
    tables$necessary_and_sufficient$d_suf
  )
  printed <- paste(capture.output(print(fit)), collapse = "\n")
  expect_match(printed, "1\\. Necessary Condition Analysis")
  expect_match(printed, "2\\. Sufficiency Condition Analysis")
  expect_match(
    printed,
    "3\\. Necessary and Sufficient Condition Analysis"
  )
})

test_that("dual thresholds bracket the admissible region", {
  dat <- make_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", steps = 5)
  rules <- nsca_thresholds(fit)

  both <- rules$necessity_status == "estimable" &
    rules$sufficiency_status == "estimable"
  # A level can never require more of X to be reachable than to be guaranteed.
  expect_true(all(
    rules$necessity_threshold_actual[both] <=
      rules$sufficiency_threshold_actual[both] + 1e-9
  ))
  expect_true(all(rules$necessity_sufficiency_interval[both] >= -1e-9))
  expect_true(all(rules$threshold_status %in% c(
    "exact correspondence", "admissible interval", "out of reach", "never guaranteed",
    "no minimum required", "no maximum required", "frontiers overlap",
    "not estimable"
  )))
  expect_false(is.unsorted(rules$outcome_level_actual))
})

test_that("low-X threshold widths use the reversed ordering", {
  dat <- make_data(noise = 0.12, seed = 9)
  for (direction in c("LH", "LL")) {
    if (identical(direction, "LH")) {
      dat$Y <- 1 - dat$Y
    }
    fit <- nsca_analysis(
      dat, "X", "Y", direction = direction, ceilings = "ce_fdh", steps = 5
    )
    rules <- nsca_thresholds(fit)
    both <- rules$necessity_status == "estimable" &
      rules$sufficiency_status == "estimable"
    expect_true(all(
      rules$necessity_threshold_actual[both] >=
        rules$sufficiency_threshold_actual[both] - 1e-9
    ), info = direction)
    expect_equal(
      rules$threshold_gap_actual[both],
      rules$necessity_threshold_actual[both] -
        rules$sufficiency_threshold_actual[both],
      info = direction
    )
    expect_true(all(rules$necessity_sufficiency_interval[both] > 0), info = direction)
    expect_true(all(rules$threshold_status[both] == "admissible interval"),
                info = direction)
    if (identical(direction, "LH")) {
      dat$Y <- 1 - dat$Y
    }
  }
})

test_that("both threshold columns use the same converter", {
  dat <- make_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", steps = 5)
  rules <- nsca_thresholds(fit, scale = "percentile")
  # The engine coerces every column through as.character() before use, which
  # can move the last bits of a double. A percentile is a step function, so at
  # a threshold that sits exactly on an observation that shift changes the
  # count by one case. The reference here must therefore be the coerced data,
  # which is what both the engine and this package actually work with.
  raw <- as.numeric(as.character(dat$X))
  ok <- is.finite(rules$necessity_threshold_actual)
  expect_equal(
    rules$necessity_threshold[ok],
    SCAtools::sca_rescale(
      rules$necessity_threshold_actual[ok], reference = raw,
      bounds = range(raw), scale = "percentile", letter = "H"
    ),
    tolerance = 1e-9
  )
})

test_that("plots may mix techniques even though the computation may not", {
  dat <- make_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = c("ce_fdh", "cr_fdh"))
  chart <- nsca_plot(fit)
  expect_s3_class(chart, "ggplot")
  expect_match(chart$labels$subtitle, "necessity corner 1")
  expect_error(nsca_plot(fit, ceilings = "qr"), "not estimated")
})

test_that("mixed outcome directions are refused", {
  dat <- data.frame(X1 = 1:10, X2 = 10:1, Y = 1:10)
  expect_error(
    nsca_analysis(dat, c("X1", "X2"), "Y", direction = c("HH", "HL")),
    "same outcome direction"
  )
})

test_that("an actually unrelated second condition has a wide band", {
  set.seed(11)
  n <- 70L
  x <- sort(runif(n))
  y <- pmin(pmax(x + rnorm(n, 0, 0.11), 0), 1)
  dat <- data.frame(X1 = x, X2 = runif(n), Y = y)
  fit <- nsca_analysis(dat, c("X1", "X2"), "Y", ceilings = "ce_fdh")
  table <- nsca_table(fit)
  expect_gt(table$admissible_region_share[table$condition == "X2"], 0.4)
  expect_gt(
    table$admissible_region_share[table$condition == "X2"],
    table$admissible_region_share[table$condition == "X1"] + 0.2
  )
})
