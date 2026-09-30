# The 0.3.0 reporting layer: renamed columns, the support decision, and the
# geometry diagnostics.
#
# A rename is only safe if the old name still leads somewhere, and a support
# flag is only useful if it can be false for each of its reasons separately.
# These tests hold both of those in place.

# Collect the conditions an expression signals without letting a warning
# unwind the call, so that a warning followed by an error can both be checked.
# Nesting expect_warning() around expect_error() is not reliable for that: the
# error unwinds before the outer handler is reached.
conditions_of <- function(expr) {
  warnings <- character(0)
  error <- NA_character_
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error <<- conditionMessage(e)
      NULL
    }
  )
  list(value = value, warnings = warnings, error = error)
}

decision_data <- function(n = 60, noise = 0.10, seed = 4) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, noise), 0), 1))
}

test_that("every retired name still leads to its replacement", {
  fit <- nsca_analysis(decision_data(), "X", "Y", ceilings = "ce_fdh",
                       test.rep = 100)
  joint <- nsca_table(fit)
  rules <- nsca_thresholds(fit)
  map <- nsca_legacy_names()

  # Every replacement exists somewhere, and no retired name is still in use.
  live <- union(names(joint), names(rules))
  expect_gt(length(map), 0L)
  expect_true(all(map %in% live))
  expect_false(any(names(map) %in% live))

  # Each retired name belongs to whichever table carries its replacement, and
  # legacy = TRUE appends it there and only there.
  for (table_name in c("joint", "rules")) {
    current <- if (table_name == "joint") joint else rules
    legacy <- if (table_name == "joint") {
      nsca_table(fit, legacy = TRUE)
    } else {
      nsca_thresholds(fit, legacy = TRUE)
    }
    mine <- map[map %in% names(current)]
    expect_gt(length(mine), 0L)
    for (old in names(mine)) {
      expect_true(old %in% names(legacy), info = paste(table_name, old))
      expect_equal(legacy[[old]], legacy[[mine[[old]]]],
                   info = paste(table_name, old))
    }
    # Nothing from the other table leaked in.
    expect_setequal(setdiff(names(legacy), names(current)), names(mine))
  }
})

test_that("nsca_extract() routes retired names to the right explanation", {
  fit <- nsca_analysis(decision_data(), "X", "Y", ceilings = "ce_fdh",
                       test.rep = 100)
  joint <- nsca_table(fit)
  map <- nsca_legacy_names()

  for (old in names(map)) {
    replacement <- map[[old]]
    got <- conditions_of(nsca_extract(fit, param = old))
    expect_true(any(grepl("renamed", got$warnings)), info = old)
    if (replacement %in% names(joint)) {
      # A joint-table name is extractable; the value is the replacement's.
      expect_true(is.na(got$error), info = old)
      expect_equal(got$value, joint[[replacement]][[1L]], info = old)
    } else {
      # A dual-threshold name is defined per outcome level, so there is no one
      # value to return. Warn about the rename, then say why, rather than
      # calling a name we know about unknown.
      expect_match(got$error, "nsca_thresholds|Unknown parameter", info = old)
    }
  }
})

test_that("the area identity closes, and its residual is reported", {
  # admissible_region_share is integrated from the frontiers this package rebuilds;
  # d_nec and d_suf come from the engines' own. The identity is exact in
  # theory, so the residual measures whether the reconstruction still matches
  # what the engine fitted. Nothing else in the package can detect that.
  for (seed in c(4, 17, 31)) {
    dat <- decision_data(seed = seed)
    row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
    residual <- 1 - row$admissible_region_share - row$d_nec - row$d_suf +
      row$overlap_share
    expect_equal(row$reconstruction_error, abs(residual), tolerance = 1e-12,
                 info = as.character(seed))
    expect_lt(row$reconstruction_error, 0.01)
    expect_equal(row$joint_empty_zone_coverage, 1 - row$admissible_region_share)
  }
})

test_that("the two bounds hold everywhere and are attained by the diagonal", {
  # d_nec + d_suf <= 1 + O follows from admissible_region_share >= 0, and the two
  # index bounds follow from that. For a perfect diagonal the overlap is
  # exactly 1/(n - 1) and both bounds are equalities, which is what makes the
  # excess discretization rather than evidence.
  for (n in c(20L, 40L, 200L)) {
    dat <- data.frame(X = seq(0, 1, length.out = n),
                      Y = seq(0, 1, length.out = n))
    row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
    expect_equal(row$overlap_share, 1 / (n - 1), tolerance = 1e-3,
                 info = as.character(n))
    expect_equal(row$weakest_effect, 0.5 + row$overlap_share / 2,
                 tolerance = 1e-3, info = as.character(n))
    expect_equal(row$balanced_joint_effect, 1 + row$overlap_share,
                 tolerance = 1e-3, info = as.character(n))
    # The step overlap is expected, so it must not count against the geometry.
    expect_true(row$geometry_acceptable, info = as.character(n))
  }

  # And the bounds are not merely equalities on that one shape.
  for (seed in c(4, 17, 31)) {
    row <- nsca_table(nsca_analysis(decision_data(seed = seed), "X", "Y",
                                    ceilings = "ce_fdh"))
    expect_lte(row$d_nec + row$d_suf, 1 + row$overlap_share + 1e-9)
    expect_lte(row$weakest_effect, 0.5 + row$overlap_share / 2 + 1e-9)
    expect_lte(row$balanced_joint_effect, 1 + row$overlap_share + 1e-9)
    # Only the coverage is bounded unconditionally, which is the reason it is
    # worth reporting next to the other two.
    expect_gte(row$joint_empty_zone_coverage, -1e-9)
    expect_lte(row$joint_empty_zone_coverage, 1 + 1e-9)
  }
})

test_that("balance separates what balanced_joint_effect cannot", {
  # The index penalises imbalance but does not measure it: (0.50, 0.125) and
  # (0.25, 0.25) both score 0.50. The balance column is the geometric mean
  # over the arithmetic one, which is 1 exactly when the components are equal.
  expect_equal(nsca_joint(0.50, 0.125), nsca_joint(0.25, 0.25))
  expect_lt(2 * sqrt(0.50 * 0.125) / 0.625, 2 * sqrt(0.25 * 0.25) / 0.50)

  row <- nsca_table(nsca_analysis(decision_data(), "X", "Y",
                                  ceilings = "ce_fdh"))
  expect_equal(row$balance,
               row$balanced_joint_effect / (row$d_nec + row$d_suf))
  expect_gte(row$balance, 0)
  expect_lte(row$balance, 1 + 1e-9)

  # A symmetric relation is perfectly balanced; a lopsided one is not.
  grid <- seq(0.001, 1, length.out = 150)
  symmetric <- nsca_table(nsca_analysis(data.frame(X = grid, Y = grid),
                                        "X", "Y", ceilings = "ce_fdh"))
  lopsided <- nsca_table(nsca_analysis(data.frame(X = grid, Y = grid^4),
                                       "X", "Y", ceilings = "ce_fdh"))
  expect_gt(symmetric$balance, 0.99)
  expect_lt(lopsided$balance, 0.85)
})

test_that("support needs significance, relevance and geometry, separately", {
  dat <- decision_data()

  # Significance alone, with no relevance threshold pre-specified.
  plain <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                    test.rep = 200))
  expect_identical(plain$necessity_supported, plain$necessity_significant)
  expect_identical(plain$sufficiency_supported, plain$sufficiency_significant)

  # A relevance threshold nothing can meet removes support without touching
  # significance. That separation is the whole point of having both.
  strict <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                     test.rep = 200, relevance = 0.99))
  expect_identical(strict$necessity_significant, plain$necessity_significant)
  expect_false(strict$necessity_supported)
  expect_false(strict$sufficiency_supported)
  expect_false(strict$joint_support)

  # The two components can be held to different standards.
  split <- nsca_table(nsca_analysis(
    dat, "X", "Y", ceilings = "ce_fdh", test.rep = 200,
    relevance = c(necessity = 0, sufficiency = 0.99)
  ))
  expect_identical(split$necessity_supported, split$necessity_significant)
  expect_false(split$sufficiency_supported)

  expect_error(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                             relevance = "large"), "'relevance' must be")
  expect_error(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                             relevance = -1), "'relevance' must be")
})

test_that("a degenerate row can never be reported as supported", {
  flat <- data.frame(X = seq(0, 1, length.out = 60), Y = rep(0.5, 60))
  fit <- suppressWarnings(nsca_analysis(
    flat, "X", "Y", ceilings = "ce_fdh", scope = c(0, 1, 0, 1), test.rep = 200
  ))
  row <- nsca_table(fit)

  # Every index is at its maximum, which is exactly why the flag matters.
  expect_gt(row$balanced_joint_effect, 0.99)
  expect_gt(row$joint_empty_zone_coverage, 0.99)
  expect_true(row$degenerate)
  expect_false(row$geometry_acceptable)
  expect_false(isTRUE(row$joint_support))
})

test_that("without a test there is no decision, only a description", {
  row <- nsca_table(nsca_analysis(decision_data(), "X", "Y",
                                  ceilings = "ce_fdh"))
  expect_identical(row$p_source, "none")
  expect_true(is.na(row$p_nsca_iut))
  expect_true(is.na(row$p_weakest_perm))
  expect_true(is.na(row$necessity_supported))
  expect_true(is.na(row$joint_support))
  expect_identical(row$joint_support_status, "not tested")
  # The description is still there, and geometry is still checkable.
  expect_true(is.finite(row$weakest_effect))
  expect_true(row$geometry_acceptable)
})

test_that("the decision block reaches the printed output", {
  fit <- nsca_analysis(decision_data(), "X", "Y", ceilings = "ce_fdh",
                       test.rep = 100)
  printed <- paste(capture.output(print(fit)), collapse = "\n")
  expect_match(printed, "4\\. Decision")
  expect_match(printed, "p_nsca_iut")
  expect_match(printed, "geometry_acceptable")
  expect_match(printed, "joint_support")
  # A user who set no relevance threshold is told so rather than left to infer
  # that one was applied.
  expect_match(printed, "No practical-relevance threshold")
})

# ---------------------------------------------------------------- 0.4.0 terms

test_that("the joint table carries the framework vocabulary", {
  fit <- nsca_analysis(decision_data(), "X", "Y", ceilings = "ce_fdh")
  row <- nsca_table(fit)

  expect_true(all(c("relationship", "necessity_empty_space",
                    "necessity_boundary", "sufficiency_empty_space",
                    "sufficiency_boundary", "component_sum",
                    "frontiers_crossed", "joint_support",
                    "joint_support_status", "admissible_region_share") %in%
                    names(row)))

  # HH: necessity sits upper-left, so its boundary is a ceiling and the
  # sufficiency boundary on the opposite corner is a floor.
  expect_equal(row$relationship[[1L]], "Higher X for higher Y")
  expect_equal(row$necessity_empty_space[[1L]], "Low X with high Y")
  expect_equal(row$necessity_boundary[[1L]], "ceiling")
  expect_equal(row$sufficiency_empty_space[[1L]], "High X with low Y")
  expect_equal(row$sufficiency_boundary[[1L]], "floor")
})

test_that("component_sum is the sum, and a clean relation is not flagged", {
  for (seed in c(4, 17, 31)) {
    row <- nsca_table(nsca_analysis(decision_data(seed = seed), "X", "Y",
                                    ceilings = "ce_fdh"))
    expect_equal(row$component_sum, row$d_nec + row$d_suf)
    # A step frontier is allowed the 1 / (n - 1) it produces by construction,
    # so a relation whose frontiers never invert is not flagged.
    expect_false(row$frontiers_crossed)
    expect_lte(row$overlap_share, 1 / (row$observations - 1) + 1e-9)
  }
})

test_that("a perfect diagonal saturates the bound without being flagged", {
  # The bound d_nec + d_suf <= 1 + O is attained here, so this is the case the
  # check must not mistake for crossed frontiers. The two sides are reached by
  # different routes -- the sum from the engines, the overlap from the
  # reconstructed frontiers -- so they agree to about a grid cell, not exactly.
  n <- 40
  dat <- data.frame(X = seq(0, 1, length.out = n), Y = seq(0, 1, length.out = n))
  row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  allowance <- 1 / (n - 1)
  expect_equal(row$component_sum, 1 + allowance, tolerance = 1e-3)
  expect_equal(row$overlap_share, allowance, tolerance = 1e-3)
  expect_false(row$frontiers_crossed)
  expect_true(row$geometry_acceptable)
})

test_that("frontiers_crossed is decided by the measured overlap", {
  # It is the direct question -- did the two frontiers invert at all, beyond
  # the overlap a staircase produces by construction -- rather than the
  # sum-based proxy. geometry_acceptable asks the separate question of whether
  # any inversion was larger than the tolerance.
  for (seed in c(4, 17)) {
    dat <- decision_data(seed = seed)
    row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
    allowance <- 1 / (row$observations - 1)
    excess <- max(0, row$overlap_share - allowance)
    expect_identical(row$frontiers_crossed, excess > 1e-9)
  }
  # A smooth frontier gets no allowance, so any inversion at all counts.
  dat <- decision_data(seed = 4)
  smooth <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "cr_fdh"))
  expect_identical(smooth$frontiers_crossed, smooth$overlap_share > 1e-9)
})

test_that("the dual-threshold table uses the framework's status vocabulary", {
  fit <- nsca_analysis(decision_data(), "X", "Y", ceilings = "ce_fdh",
                       steps = 4)
  rules <- nsca_thresholds(fit)
  expect_true("necessity_sufficiency_interval" %in% names(rules))
  expect_true("inequality" %in% names(rules))
  expect_true(all(rules$threshold_status %in% c(
    "exact correspondence", "admissible interval", "frontiers overlap",
    "out of reach", "never guaranteed", "no minimum required",
    "no maximum required", "not estimable"
  )))
  # The interval is the width of the admissible interval at that outcome level.
  ok <- rules$threshold_status == "admissible interval"
  if (any(ok)) {
    expect_true(all(rules$necessity_sufficiency_interval[ok] > 0))
  }
})

test_that("the retired inequality argument still works", {
  fit <- nsca_analysis(decision_data(), "X", "Y", ceilings = "ce_fdh",
                       steps = 4)
  inclusive <- nsca_thresholds(fit, inequality = "inclusive")

  got <- conditions_of(nsca_thresholds(fit, boundary = "inclusive"))
  expect_length(got$warnings, 1L)
  expect_true(is.na(got$error))
  expect_equal(got$value$sufficiency_rule, inclusive$sufficiency_rule)
  expect_equal(unique(got$value$inequality), "inclusive")

  # Supplying both warns twice -- once for the deprecation, once for the
  # conflict -- and keeps the new argument.
  both <- conditions_of(
    nsca_thresholds(fit, inequality = "strict", boundary = "inclusive")
  )
  expect_length(both$warnings, 2L)
  expect_true(any(grepl("deprecated", both$warnings)))
  expect_true(any(grepl("Both", both$warnings)))
  expect_equal(unique(both$value$inequality), "strict")
})
