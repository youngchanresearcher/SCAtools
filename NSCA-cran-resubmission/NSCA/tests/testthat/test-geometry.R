set.seed(7)
x <- sort(runif(120))
y <- pmin(pmax(x + rnorm(120, 0, 0.12), 0), 1)
grid <- seq(min(x), max(x), length.out = 2001L)

test_that("each corner selects the matching running extreme", {
  upper_left <- NSCA:::.nsca_step_frontier(x, y, 1L, grid)
  lower_right <- NSCA:::.nsca_step_frontier(x, y, 4L, grid)
  expect_false(is.unsorted(upper_left))           # non-decreasing
  expect_false(is.unsorted(lower_right))          # non-decreasing
  expect_true(all(upper_left >= lower_right - 1e-9))
  expect_equal(upper_left[[length(grid)]], max(y))
  expect_equal(lower_right[[1L]], min(y))
})

test_that("the empty areas and the band decompose the scope", {
  # Exact envelopment frontiers cannot overlap, so the two empty zones plus the
  # admissible region must account for the whole scope. This is the identity
  # the package refuses to rely on for regression frontiers.
  for (direction in c("HH", "LH", "HL", "LL")) {
    yy <- if (direction %in% c("HH", "LL")) y else 1 - y
    corners <- nsca_corners(direction)
    nec <- NSCA:::.nsca_step_frontier(x, yy, corners$necessity_corner, grid)
    suf <- NSCA:::.nsca_step_frontier(x, yy, corners$sufficiency_corner, grid)
    bounds <- c(min(yy), max(yy))
    span <- diff(range(grid)) * diff(bounds)

    empty <- function(values, side) {
      NSCA:::.nsca_integrate(
        if (side == "upper") bounds[[2L]] - values else values - bounds[[1L]],
        grid
      )
    }
    d_nec <- empty(nec, corners$necessity_bound) / span
    d_suf <- empty(suf, corners$sufficiency_bound) / span
    band <- if (corners$necessity_bound == "upper") {
      NSCA:::.nsca_band(nec, suf, grid, bounds)
    } else {
      NSCA:::.nsca_band(suf, nec, grid, bounds)
    }
    expect_equal(d_nec + d_suf + band$area / span, 1, tolerance = 1e-6,
                 info = direction)
    expect_false(isTRUE(band$crossed), info = direction)
  }
})

test_that("crossing frontiers are reported rather than silently absorbed", {
  crossing_upper <- grid
  crossing_lower <- 1 - grid
  band <- NSCA:::.nsca_band(crossing_upper, crossing_lower, grid, c(0, 1))
  expect_true(band$crossed)
  expect_true(band$area > 0)          # only the positive part is counted
  expect_true(band$area < 0.5)
})

test_that("a deterministic relation leaves no band", {
  band <- NSCA:::.nsca_band(grid, grid, grid, c(0, 1))
  expect_equal(band$area, 0, tolerance = 1e-9)
})

test_that("rows without a condition value do not widen that condition's scope", {
  # The engines fit each condition on its own complete pairs. An outcome
  # observed only where the condition is missing must not stretch the box the
  # admissible region is measured in, or the area identity fails to close and
  # joint support is refused for no reason in the data.
  set.seed(2)
  n <- 50
  x <- runif(n)
  y <- pmin(pmax(x + rnorm(n, 0, 0.1), 0), 1)
  base <- data.frame(X1 = x, X2 = runif(n), Y = y)
  padded <- rbind(base, data.frame(X1 = 0.5, X2 = NA, Y = c(-1, 2)))

  single <- nsca_table(nsca_analysis(
    rbind(data.frame(X = x, Y = y), data.frame(X = NA, Y = c(-1, 2))),
    "X", "Y", ceilings = "ce_fdh"
  ))
  clean <- nsca_table(nsca_analysis(data.frame(X = x, Y = y), "X", "Y",
                                    ceilings = "ce_fdh"))
  expect_equal(single$admissible_region_share, clean$admissible_region_share)
  expect_lt(single$reconstruction_error, 0.02)
  expect_true(single$geometry_acceptable)

  both <- nsca_table(nsca_analysis(padded, c("X1", "X2"), "Y",
                                   ceilings = "ce_fdh"))
  alone <- nsca_table(nsca_analysis(base[c("X2", "Y")], "X2", "Y",
                                    ceilings = "ce_fdh"))
  expect_equal(both$admissible_region_share[both$condition == "X2"],
               alone$admissible_region_share)
  expect_true(all(both$reconstruction_error < 0.02))
})
