# Invariants that must hold on any data, not just on a fixture.
#
# The fixtures elsewhere are tidy: sorted, well spread, no missing values. Real
# data is not, and an invariant that only holds on a tidy fixture is not an
# invariant. Each block below re-checks a structural property across several
# random datasets and every direction, so a violation shows up as a seed rather
# than as a hand-picked counterexample nobody thought to write down.

random_frame <- function(seed, n = 45, shape = c("band", "curved", "noisy")) {
  shape <- match.arg(shape)
  set.seed(seed)
  x <- runif(n)
  y <- switch(
    shape,
    band = x + rnorm(n, 0, 0.12),
    curved = x^2 + rnorm(n, 0, 0.08),
    noisy = x + rnorm(n, 0, 0.35)
  )
  data.frame(X = x, Y = pmin(pmax(y, 0), 1))
}

directions <- c("HH", "LH", "HL", "LL")
seeds <- c(101, 202, 303)

test_that("the joint index is bounded by both components everywhere", {
  for (seed in seeds) {
    for (shape in c("band", "curved", "noisy")) {
      dat <- random_frame(seed, shape = shape)
      for (direction in directions) {
        row <- nsca_table(nsca_analysis(dat, "X", "Y", direction = direction,
                                        ceilings = "ce_fdh"))
        label <- paste(seed, shape, direction)
        expect_equal(row$weakest_effect, min(row$d_nec, row$d_suf), info = label)
        expect_gte(row$weakest_effect, 0)
        # AM-GM, in both directions: the minimum never exceeds the geometric
        # mean, and the geometric mean never exceeds the arithmetic one. This
        # is what licenses doubling the geometric mean onto [0, 1].
        expect_equal(row$balanced_joint_effect, 2 * sqrt(row$d_nec * row$d_suf),
                     info = label)
        expect_lte(2 * row$weakest_effect, row$balanced_joint_effect + 1e-12)
        expect_lte(row$balanced_joint_effect, row$d_nec + row$d_suf + 1e-12)
        # Both empty zones and the band together cannot exceed the scope by
        # more than the step frontier's own overlap.
        expect_lte(
          row$d_nec + row$d_suf + row$admissible_region_share - row$overlap_share,
          1 + 0.01
        )
      }
    }
  }
})

test_that("shares are proportions and the two frontiers account for the scope", {
  for (seed in seeds) {
    dat <- random_frame(seed)
    row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
    for (column in c("d_nec", "d_suf", "weakest_effect", "joint_empty_zone_coverage",
                     "admissible_region_share", "overlap_share")) {
      value <- row[[column]]
      expect_true(is.finite(value), info = column)
      expect_gte(value, -1e-9)
      expect_lte(value, 1 + 1e-9)
    }
    # balanced_joint_effect is bounded by 1 for the same reason the components sum to at most
    # the scope, so it inherits the step frontier's overlap in the same way and
    # is allowed exactly that much slack, not an arbitrary tolerance.
    expect_true(is.finite(row$balanced_joint_effect))
    expect_gte(row$balanced_joint_effect, -1e-9)
    expect_lte(row$balanced_joint_effect, 1 + row$overlap_share + 1e-9)
    # For an exact envelope the decomposition holds once the staircase overlap
    # is added back.
    expect_equal(
      row$d_nec + row$d_suf + row$admissible_region_share - row$overlap_share,
      1, tolerance = 5e-3, info = as.character(seed)
    )
  }
})

test_that("weaker_component names the component that produced the minimum", {
  for (seed in seeds) {
    for (direction in directions) {
      row <- nsca_table(nsca_analysis(random_frame(seed), "X", "Y",
                                      direction = direction,
                                      ceilings = "ce_fdh"))
      expected <- if (row$d_nec <= row$d_suf) "necessity" else "sufficiency"
      expect_identical(row$weaker_component, expected,
                       info = paste(seed, direction))
    }
  }
})

test_that("reversing the outcome swaps the two components", {
  # HH on Y and HL on 1 - Y describe the same geometry read from the other
  # end, so the components must trade places while the joint index is
  # unchanged. This catches a corner mapping that is self-consistent but wrong.
  for (seed in seeds) {
    dat <- random_frame(seed)
    up <- nsca_table(nsca_analysis(dat, "X", "Y", direction = "HH",
                                   ceilings = "ce_fdh"))
    down <- nsca_table(nsca_analysis(
      data.frame(X = dat$X, Y = 1 - dat$Y), "X", "Y", direction = "HL",
      ceilings = "ce_fdh"))
    expect_equal(up$d_nec, down$d_nec, tolerance = 1e-8,
                 info = as.character(seed))
    expect_equal(up$d_suf, down$d_suf, tolerance = 1e-8)
    expect_equal(up$weakest_effect, down$weakest_effect, tolerance = 1e-8)
    expect_equal(up$admissible_region_share, down$admissible_region_share,
                 tolerance = 1e-8)
  }
})

test_that("a monotone rescaling of the axes leaves the corner logic intact", {
  # Effect sizes are areas and are not invariant to rescaling, which the
  # documentation says. The corner mapping and the ordering of the thresholds
  # must nonetheless survive an affine change of units.
  dat <- random_frame(101)
  shifted <- data.frame(X = 10 * dat$X + 3, Y = 5 * dat$Y - 2)
  a <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  b <- nsca_table(nsca_analysis(shifted, "X", "Y", ceilings = "ce_fdh"))

  expect_identical(a$necessity_corner, b$necessity_corner)
  expect_identical(a$sufficiency_corner, b$sufficiency_corner)
  expect_identical(a$weaker_component, b$weaker_component)
  # An affine change of units rescales both axes uniformly, so the ratios of
  # areas to the scope are unchanged even though the areas are not.
  expect_equal(a$d_nec, b$d_nec, tolerance = 1e-6)
  expect_equal(a$d_suf, b$d_suf, tolerance = 1e-6)
})

test_that("incomplete pairs are dropped exactly as the engine drops them", {
  dat <- random_frame(202)
  holed <- dat
  holed$X[c(3, 11)] <- NA
  holed$Y[c(7, 11, 20)] <- NA
  complete <- holed[stats::complete.cases(holed), , drop = FALSE]

  with_holes <- nsca_analysis(holed, "X", "Y", ceilings = "ce_fdh")
  without <- nsca_analysis(complete, "X", "Y", ceilings = "ce_fdh")

  expect_equal(nsca_table(with_holes)$d_nec, nsca_table(without)$d_nec)
  expect_equal(nsca_table(with_holes)$d_suf, nsca_table(without)$d_suf)
  # The stored data used for scale conversion must match the fitted sample.
  expect_equal(
    length(attr(with_holes, "nsca")$raw$conditions[["X"]]$x),
    nrow(complete)
  )
  expect_equal(
    nsca_results(with_holes)$necessity$observations, nrow(complete)
  )
})

test_that("non-numeric entries become missing rather than an error", {
  dat <- random_frame(303)
  messy <- data.frame(X = as.character(dat$X), Y = dat$Y,
                      stringsAsFactors = FALSE)
  messy$X[5] <- "not a number"
  fit <- nsca_analysis(messy, "X", "Y", ceilings = "ce_fdh")
  expect_equal(nsca_results(fit)$necessity$observations, nrow(dat) - 1L)
})

test_that("the analysis is deterministic when no test is requested", {
  dat <- random_frame(101)
  first <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  second <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  expect_equal(first, second)

  thr <- function() nsca_thresholds(
    nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", steps = 5)
  )
  expect_equal(thr(), thr())
})

test_that("purity metrics change the reported extras but never the estimates", {
  # The engine computes them only for an unflipped corner, so they are
  # available on at most one side. They must not touch anything NSCA reports.
  dat <- random_frame(101, n = 30)
  off <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  on <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                 purity = TRUE))
  expect_equal(off$d_nec, on$d_nec)
  expect_equal(off$d_suf, on$d_suf)
  expect_equal(off$admissible_region_share, on$admissible_region_share)
})

test_that("the environment is left as it was found", {
  # The purity switch works by setting an engine environment variable. It must
  # restore whatever was there before, including nothing at all.
  dat <- random_frame(101, n = 30)
  Sys.unsetenv("NCA_SKIP_PURITY")
  nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  expect_identical(Sys.getenv("NCA_SKIP_PURITY", unset = NA), NA_character_)

  Sys.setenv(NCA_SKIP_PURITY = "sentinel")
  on.exit(Sys.unsetenv("NCA_SKIP_PURITY"), add = TRUE)
  nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  expect_identical(Sys.getenv("NCA_SKIP_PURITY"), "sentinel")
})

test_that("a theoretical scope is accepted and applied to both sides", {
  # Effect sizes are areas relative to the declared scope, so widening it
  # changes them. What must not change is that both sides see the same scope:
  # a scope applied to one side only would make the comparison meaningless.
  dat <- random_frame(101)
  tight <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  wide <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                        scope = c(-1, 2, -1, 2))
  wide_row <- nsca_table(wide)

  expect_false(isTRUE(all.equal(nsca_table(tight)$d_nec, wide_row$d_nec)))
  for (column in c("d_nec", "d_suf", "weakest_effect", "admissible_region_share")) {
    expect_gte(wide_row[[column]], -1e-9)
    expect_lte(wide_row[[column]], 1 + 1e-9)
  }
  bounds <- attr(wide, "nsca")$raw$conditions[["X"]]
  expect_equal(bounds$x_bounds, c(-1, 2))
  expect_equal(bounds$y_bounds, c(-1, 2))
})

test_that("the convenience wrapper is the analysis with a test attached", {
  dat <- random_frame(101, n = 30)
  quick <- nsca(dat, "X", "Y", ceilings = "ce_fdh", test.rep = 50)
  expect_s3_class(quick, "nsca_result")
  row <- nsca_table(quick)
  expect_true(is.finite(row$p_nec) && is.finite(row$p_suf))
  expect_equal(row$p_nsca_iut, max(row$p_nec, row$p_suf))
})

test_that("joint_empty_zone_coverage is the complement of the band and stays in [0, 1]", {
  for (seed in seeds) {
    for (shape in c("band", "curved", "noisy")) {
      dat <- random_frame(seed, shape = shape)
      row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
      label <- paste(seed, shape)
      expect_equal(row$joint_empty_zone_coverage, 1 - row$admissible_region_share, info = label)
      expect_gte(row$joint_empty_zone_coverage, -1e-9)
      expect_lte(row$joint_empty_zone_coverage, 1 + 1e-9)
    }
  }
})

test_that("the three summaries order themselves by how they treat curvature", {
  # Y = X, Y = X^3 and Y = X^(1/3) are all exact bijections, so all three are
  # perfectly necessary and sufficient. joint_empty_zone_coverage must score them alike;
  # weakest_effect must not, because a lopsided curve really does leave one empty zone
  # small; balanced_joint_effect must sit between the two, which is the whole point of a
  # partially compensatory aggregator. None of the three is wrong: they are
  # answering different questions, and this test pins the differences down.
  x <- seq(0.001, 1, length.out = 150)
  curves <- list(linear = x, convex = x^3, concave = x^(1 / 3))
  rows <- lapply(curves, function(y) {
    nsca_table(nsca_analysis(data.frame(X = x, Y = y), "X", "Y",
                             ceilings = "ce_fdh"))
  })

  joint_empty_zone_coverage <- vapply(rows, function(r) r$joint_empty_zone_coverage, numeric(1L))
  smallest <- vapply(rows, function(r) r$weakest_effect, numeric(1L))
  balanced <- vapply(rows, function(r) r$balanced_joint_effect, numeric(1L))

  # Every curve closes the band, so every curve shows exact correspondence.
  expect_true(all(joint_empty_zone_coverage > 0.99), info = paste(round(joint_empty_zone_coverage, 3),
                                                    collapse = " "))
  expect_lt(diff(range(joint_empty_zone_coverage)), 0.02)

  # The minimum, by contrast, separates them sharply by curvature.
  expect_gt(smallest[["linear"]], 0.45)
  expect_lt(smallest[["convex"]], 0.35)
  expect_lt(smallest[["concave"]], 0.35)
  expect_gt(diff(range(smallest)), 0.15)

  # The geometric mean is less severe than the minimum on the lopsided curves
  # but still does not treat them as equivalent to the diagonal. Partial
  # compensation is exactly this: between the other two, not the same as
  # either.
  for (name in names(curves)) {
    # `label`, not `info`: the ordered comparisons take a label for the object
    # and have no `info` argument, so passing one is an error rather than an
    # annotation.
    expect_gte(balanced[[name]], 2 * smallest[[name]] - 1e-9, label = name)
    expect_lte(balanced[[name]], joint_empty_zone_coverage[[name]] + 0.05,
               label = name)
  }
  expect_gt(balanced[["convex"]], 0.75)
  expect_lt(balanced[["convex"]], joint_empty_zone_coverage[["convex"]] - 0.02)
  expect_gt(balanced[["linear"]], balanced[["convex"]])
})

test_that("joint_empty_zone_coverage is inflated at the low end, which is why nothing is quoted alone", {
  # A loose scatter scores an order of magnitude higher on joint_empty_zone_coverage than on
  # weakest_effect. Reporting joint_empty_zone_coverage as the effect size would present that as a
  # moderate result; reporting weakest_effect alone understates a deterministic but
  # lopsided relation. That is the case for reporting all of them.
  dat <- random_frame(101, shape = "noisy")
  row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  expect_gt(row$joint_empty_zone_coverage, row$weakest_effect)
  expect_gt(row$joint_empty_zone_coverage / max(row$weakest_effect, 1e-6), 3)
  # joint_empty_zone_coverage is roughly the component sum, and the geometric mean can never
  # exceed that sum. It can come arbitrarily close to it when the two
  # components are balanced, so this is the bound that actually holds; a strict
  # inequality between the two would be an artefact of one fixture.
  expect_lte(row$balanced_joint_effect, row$d_nec + row$d_suf + 1e-12)
})

test_that("a constant outcome fools every index and is caught by the flag and the screen", {
  # The band closes completely for a constant outcome placed at the centre of
  # an imposed scope, so all three summaries report perfect joint support
  # for data with no information in it. This is the documented blind spot they
  # share. Two things cover it: the degeneracy flag and the permutation
  # screen. Neither is optional.
  flat <- data.frame(X = seq(0, 1, length.out = 60), Y = rep(0.5, 60))
  fit <- suppressWarnings(nsca_analysis(
    flat, "X", "Y", ceilings = "ce_fdh", scope = c(0, 1, 0, 1), test.rep = 100
  ))
  row <- nsca_table(fit)

  expect_gt(row$joint_empty_zone_coverage, 0.99)          # every index is fooled
  expect_gt(row$balanced_joint_effect, 0.99)
  expect_gt(row$weakest_effect, 0.49)
  expect_true(row$degenerate)               # the flag is not
  expect_gte(row$p_nec, 0.5)                # nor is the screen
  expect_gte(row$p_suf, 0.5)
  expect_false(isTRUE(row$nsca_significant))
})

test_that("balanced_joint_effect penalises asymmetry at a fixed sum", {
  # The substantive claim behind choosing p = 0 over p = 1. Two datasets with
  # nearly the same component sum but very different balance must not receive
  # the same joint strength, and the balanced one must score higher.
  x <- seq(0.001, 1, length.out = 200)
  balanced <- nsca_table(nsca_analysis(
    data.frame(X = x, Y = x), "X", "Y", ceilings = "ce_fdh"
  ))
  lopsided <- nsca_table(nsca_analysis(
    data.frame(X = x, Y = x^4), "X", "Y", ceilings = "ce_fdh"
  ))

  # Both are deterministic, so both have a closed band and a component sum of
  # about 1: the sum cannot tell them apart at all.
  expect_equal(balanced$d_nec + balanced$d_suf,
               lopsided$d_nec + lopsided$d_suf, tolerance = 0.05)
  expect_gt(balanced$balanced_joint_effect, lopsided$balanced_joint_effect + 0.05)
  expect_gt(abs(lopsided$d_nec - lopsided$d_suf),
            abs(balanced$d_nec - balanced$d_suf))
})
