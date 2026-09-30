# Input validation and error paths.
#
# Every stop() in the package is a decision about what the analysis refuses to
# do. A refusal that stops working is worse than a missing feature, because it
# turns a guarded case into a silently wrong number, so each one is pinned here
# by its message.

validation_data <- function(n = 40, noise = 0.10, seed = 5) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, noise), 0), 1))
}

test_that("directions are validated", {
  dat <- validation_data()
  expect_error(nsca_analysis(dat, "X", "Y", direction = "ZZ"),
               "Invalid direction")
  expect_error(nsca_analysis(dat, "X", "Y", direction = NA_character_),
               "must contain one or more")
  expect_error(nsca_analysis(dat, "X", "Y", direction = character()),
               "must contain one or more")
  # Recycling is allowed only from length one.
  expect_error(
    nsca_analysis(data.frame(X1 = 1:10, X2 = 10:1, Y = 1:10),
                  c("X1", "X2"), "Y", direction = c("HH", "HH", "HH")),
    "length 1 or the same length"
  )
})

test_that("direction input is normalised, not merely accepted", {
  expect_identical(nsca_corners("  ll  ")$direction, "LL")
  expect_identical(nsca_corners(c("hh", "Lh"))$direction, c("HH", "LH"))
  expect_identical(nsca_corners(factor("HL"))$direction, "HL")
})

test_that("the outcome must be exactly one column", {
  dat <- data.frame(X = 1:10, Y1 = 1:10, Y2 = 10:1)
  expect_error(nsca_analysis(dat, "X", c("Y1", "Y2")), "exactly one outcome")
})

test_that("at least one condition is required", {
  dat <- validation_data()
  expect_error(nsca_analysis(dat, character(), "Y"),
               "At least one condition")
})

test_that("ceilings are validated and ols is rejected as a frontier", {
  dat <- validation_data()
  expect_error(nsca_analysis(dat, "X", "Y", ceilings = character()),
               "at least one frontier technique")
  # ols estimates central tendency, so it is not a component of either claim.
  expect_warning(
    expect_error(nsca_analysis(dat, "X", "Y", ceilings = "ols"), "No usable"),
    "central-tendency"
  )
  # Mixed with a usable technique it is dropped, not fatal.
  expect_warning(
    fit <- nsca_analysis(dat, "X", "Y", ceilings = c("OLS", "ce_fdh")),
    "central-tendency"
  )
  expect_identical(nsca_table(fit)$ceiling, "ce_fdh")
})

test_that("ceiling names are case-insensitive and deduplicated", {
  dat <- validation_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = c("CE_FDH", "ce_fdh"))
  expect_identical(nsca_table(fit)$ceiling, "ce_fdh")
})

test_that("a single analysis cannot mix outcome directions", {
  dat <- data.frame(X1 = 1:10, X2 = 10:1, Y = 1:10)
  expect_error(
    nsca_analysis(dat, c("X1", "X2"), "Y", direction = c("HH", "HL")),
    "same outcome direction"
  )
  # Mixing condition directions is fine: only the outcome must agree.
  expect_s3_class(
    nsca_analysis(dat, c("X1", "X2"), "Y", direction = c("HH", "LH"),
                  ceilings = "ce_fdh"),
    "nsca_result"
  )
})

test_that("a constant outcome is refused with a message that says what to do", {
  flat <- data.frame(X = seq(0, 1, length.out = 30), Y = rep(0.4, 30))
  expect_error(nsca_analysis(flat, "X", "Y", ceilings = "ce_fdh"),
               "single value")
  expect_error(nsca_analysis(flat, "X", "Y", ceilings = "ce_fdh"),
               "scope")
  # The suggested remedy works, but it is not silent: an outcome that does not
  # move inside the supplied scope still makes every area a property of the
  # scope, so the analysis proceeds under a warning rather than quietly.
  expect_warning(
    fit <- nsca_analysis(flat, "X", "Y", ceilings = "ce_fdh",
                         scope = c(0, 1, 0, 1)),
    "does not vary"
  )
  expect_s3_class(fit, "nsca_result")
  expect_true(nsca_table(fit)$degenerate)
})

test_that("the accessors refuse objects that are not NSCA results", {
  for (fn in list(nsca_table, nsca_results, nsca_thresholds, nsca_plot,
                  nsca_extract)) {
    expect_error(fn(list(a = 1)), "not a valid 'nsca_result'")
  }
  expect_error(summary(structure(list(), class = "nsca_result")),
               "not a valid 'nsca_result'")
})

test_that("unknown conditions and frontiers are named in the error", {
  dat <- validation_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  expect_error(nsca_extract(fit, x = "Z"), "Unknown condition 'Z'")
  expect_error(nsca_plot(fit, x = "Z"), "Unknown condition\\(s\\): Z")
  expect_error(nsca_plot(fit, ceilings = "qr"), "not estimated")
  expect_error(nsca_thresholds(fit, ceiling = "qr"), "was not estimated")
  expect_error(nsca_thresholds(fit, ceiling = c("ce_fdh", "cr_fdh")),
               "exactly one frontier")
  expect_error(nsca_thresholds(fit, x = "Z"), "Unknown condition")
})

test_that("scale and convention arguments are validated by SCAtools", {
  dat <- validation_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", steps = 4)
  expect_error(nsca_thresholds(fit, scale = "percent"), "Invalid")
  expect_error(nsca_thresholds(fit, convention = "relative"), "Invalid")
  expect_error(nsca_analysis(dat, "X", "Y", threshold.x = "nonsense"),
               "Invalid")
})

test_that("ols is available as a reference line rather than a frontier", {
  dat <- validation_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", reference = "ols")

  # It changes nothing about the two components.
  expect_identical(nsca_table(fit)$ceiling, "ce_fdh")

  line <- nsca_reference(fit)
  expected <- stats::coef(stats::lm(Y ~ X, data = dat))
  expect_equal(nrow(line), 1L)
  expect_identical(line$line_type, "central tendency")
  expect_equal(line$intercept, unname(expected[[1L]]))
  expect_equal(line$slope, unname(expected[[2L]]))
  expect_equal(line$r_squared, stats::cor(dat$X, dat$Y)^2)
  expect_true(line$drawn)

  # Computable without having been drawn, and refused when it is not a line
  # this package knows.
  plain <- nsca_reference(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  expect_false(plain$drawn)
  expect_equal(plain$slope, line$slope)
  expect_error(
    nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", reference = "qr"),
    "Unknown reference line"
  )
})

test_that("the glossary names the central-tendency line", {
  terms <- nsca_terms()
  expect_true("central tendency" %in% terms$term)
  expect_match(
    terms$package_name[terms$term == "central tendency"],
    "nsca_reference"
  )
})

test_that("a named relevance is reordered as numbers, whatever its type", {
  set.seed(3)
  x <- sort(runif(40))
  dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(40, 0, 0.1), 0), 1))
  reversed <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                            relevance = c(sufficiency = 0.2, necessity = 0.1))
  expect_identical(attr(reversed, "nsca")$relevance,
                   c(necessity = 0.1, sufficiency = 0.2))
  # Before 0.4.5 a character input survived as character, and the support
  # rule then compared effect sizes with thresholds as strings.
  text <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                        relevance = c(necessity = "0.1", sufficiency = "0.2"))
  expect_type(attr(text, "nsca")$relevance, "double")
  expect_identical(attr(text, "nsca")$relevance,
                   c(necessity = 0.1, sufficiency = 0.2))
})
