# The joint index family.
#
# The point of nsca_joint() is that min, the geometric mean and the sum are one
# family indexed by how much compensation they allow. If that claim is true the
# function must reproduce each of them exactly at its own p, and the values
# must be monotone in p. If it is not true the function is just three formulas
# in a coat, so these tests are about the claim as much as the arithmetic.

test_that("each named member of the family is reproduced exactly", {
  a <- c(0.10, 0.25, 0.40, 0.50, 0.00, 0.30)
  b <- c(0.70, 0.25, 0.40, 0.50, 0.45, 0.00)

  expect_equal(nsca_joint(a, b, p = -Inf, normalize = FALSE), pmin(a, b))
  expect_equal(nsca_joint(a, b, p = Inf, normalize = FALSE), pmax(a, b))
  expect_equal(nsca_joint(a, b, p = 0, normalize = FALSE), sqrt(a * b))
  expect_equal(nsca_joint(a, b, p = 1, normalize = FALSE), (a + b) / 2)

  harmonic <- ifelse(a > 0 & b > 0, 2 * a * b / (a + b), 0)
  expect_equal(nsca_joint(a, b, p = -1, normalize = FALSE), harmonic)
})

test_that("the family is monotone in p, which is what makes p a dial", {
  a <- 0.30
  b <- 0.60
  degrees <- c(-Inf, -4, -2, -1, 0, 0.5, 1)
  values <- vapply(degrees, function(p) {
    nsca_joint(a, b, p = p, normalize = FALSE)
  }, numeric(1L))
  expect_true(all(diff(values) > 0))
  expect_equal(values[[1L]], min(a, b))
  expect_equal(values[[length(values)]], (a + b) / 2)
})

test_that("a zero component annihilates every non-compensatory member", {
  # Raising zero to a negative power gives Inf, and the mean would come back
  # NaN. A joint_support_status with a dead half is zero, not missing.
  for (p in c(-Inf, -3, -1, 0)) {
    expect_equal(nsca_joint(0, 0.5, p = p, normalize = FALSE), 0, info = p)
    expect_equal(nsca_joint(0.5, 0, p = p, normalize = FALSE), 0, info = p)
    expect_equal(nsca_joint(0, 0, p = p, normalize = FALSE), 0, info = p)
  }
  # The fully compensatory end does not, and that is the objection to it.
  expect_equal(nsca_joint(0, 0.5, p = 1, normalize = FALSE), 0.25)
})

test_that("doubling maps the index onto [0, 1] and reaches 1 only at the line", {
  # The AM-GM chain: sqrt(ab) <= (a+b)/2 <= 0.5 whenever the two empty zones
  # are disjoint subsets of one scope. Equality throughout requires a = b = 0.5,
  # which is the single-line case.
  expect_equal(nsca_joint(0.5, 0.5), 1)
  levels <- (0:10) / 20                     # 0, 0.05, ..., 0.50
  grid <- expand.grid(a = levels, b = levels)
  feasible <- grid[grid$a + grid$b <= 1 + 1e-12, , drop = FALSE]
  values <- nsca_joint(feasible$a, feasible$b)
  expect_true(all(values >= -1e-12))
  expect_true(all(values <= 1 + 1e-12))
  expect_equal(max(values), 1, tolerance = 1e-9)
  top <- feasible[values > 1 - 1e-9, , drop = FALSE]
  expect_equal(nrow(top), 1L)
  expect_equal(unname(unlist(top)), c(0.5, 0.5), tolerance = 1e-9)
})

test_that("asymmetry is penalised at a fixed sum", {
  # The substantive difference from the sum, stated as a number rather than as
  # a preference. Both pairs sum to 0.80.
  expect_equal(nsca_joint(0.40, 0.40), 0.80)
  expect_equal(nsca_joint(0.70, 0.10), 2 * sqrt(0.07))
  expect_lt(nsca_joint(0.70, 0.10), nsca_joint(0.40, 0.40))
  expect_equal(round(nsca_joint(0.70, 0.10), 3), 0.529)
  # But it is only a partial penalty. A fully non-compensatory index would put
  # the lopsided pair at twice its weaker half; the geometric mean puts it
  # strictly higher, and saying otherwise would overstate what it does.
  expect_gt(nsca_joint(0.70, 0.10), 2 * min(0.70, 0.10))
})

test_that("missing, infinite and negative inputs give NA rather than a number", {
  expect_true(is.na(nsca_joint(NA_real_, 0.4)))
  expect_true(is.na(nsca_joint(0.4, NA_real_)))
  expect_true(is.na(nsca_joint(Inf, 0.4)))
  expect_true(is.na(nsca_joint(-0.1, 0.4)))
  mixed <- nsca_joint(c(0.4, NA, 0.2), c(0.4, 0.4, -1))
  expect_equal(mixed[[1L]], 0.8)
  expect_true(all(is.na(mixed[-1L])))
})

test_that("arguments are recycled and the degree is validated", {
  expect_equal(nsca_joint(0.4, c(0.4, 0.1)), nsca_joint(c(0.4, 0.4), c(0.4, 0.1)))
  expect_length(nsca_joint(c(0.1, 0.2, 0.3), 0.4), 3L)
  expect_equal(nsca_joint(numeric(0L), 0.4), numeric(0L))
  expect_error(nsca_joint(0.4, 0.4, p = c(0, 1)), "single number")
  expect_error(nsca_joint(0.4, 0.4, p = NA), "single number")
  # Above p = 1 the normalisation guarantee no longer holds, so it says so
  # instead of quietly returning a number above 1.
  expect_warning(nsca_joint(0.5, 0.5, p = 2), "p <= 1")
})

test_that("the table columns are exactly two members of this family", {
  set.seed(5)
  x <- sort(runif(50))
  dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(50, 0, 0.12), 0), 1))
  row <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))

  expect_equal(row$weakest_effect,
               nsca_joint(row$d_nec, row$d_suf, p = -Inf, normalize = FALSE))
  expect_equal(row$balanced_joint_effect, nsca_joint(row$d_nec, row$d_suf, p = 0))
  # And the fully compensatory member, which the package deliberately does not
  # report, would rank a one-sided relation alongside a balanced one.
  one_sided <- nsca_joint(0.9, 0.05, p = 1)
  balanced <- nsca_joint(0.5, 0.45, p = 1)
  expect_lt(abs(one_sided - balanced), 0.03)
  expect_gt(nsca_joint(0.5, 0.45) - nsca_joint(0.9, 0.05), 0.3)
})
