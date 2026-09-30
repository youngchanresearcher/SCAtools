# nsca_extract(): the single-value accessor.
#
# Two things matter here. It must never invent a number, and it must be honest
# about which engine a value came from. The nec:/suf: prefixes exist so that a
# reader can tell a joint quantity from a component one at the call site, and
# these tests hold that distinction in place.

extract_data <- function(n = 45, seed = 31) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(
    X1 = x,
    X2 = runif(n),
    Y = pmin(pmax(x + rnorm(n, 0, 0.10), 0), 1)
  )
}

test_that("columns of the joint table are reachable by name", {
  fit <- nsca_analysis(extract_data(), "X1", "Y", ceilings = "ce_fdh")
  row <- nsca_table(fit)
  for (param in c("d_nec", "d_suf", "weakest_effect", "balanced_joint_effect", "joint_empty_zone_coverage",
                  "degenerate", "admissible_region_share",
                  "overlap_share", "weaker_component", "joint_support_status",
                  "equivalence_class", "necessity_corner")) {
    expect_equal(nsca_extract(fit, param = param), row[[param]][[1L]],
                 info = param)
  }
})

test_that("the prefixes reach the engines and disagree with each other", {
  dat <- extract_data()
  fit <- nsca_analysis(dat, "X1", "Y", direction = "HH", ceilings = "ce_fdh")

  expect_equal(
    nsca_extract(fit, param = "nec:Effect size"),
    as.numeric(NCA::nca_extract(
      NCA::nca_analysis(dat, "X1", "Y", corner = 1, ceilings = "ce_fdh"),
      "X1", "ce_fdh", "Effect size"
    ))
  )
  expect_equal(nsca_extract(fit, param = "nec:Effect size"),
               nsca_extract(fit, param = "d_nec"))
  expect_equal(nsca_extract(fit, param = "suf:Effect size"),
               nsca_extract(fit, param = "d_suf"))
  # The two sides are different analyses; a prefix that silently read the same
  # object would make them agree.
  expect_false(isTRUE(all.equal(
    nsca_extract(fit, param = "nec:Ceiling zone"),
    nsca_extract(fit, param = "suf:Ceiling zone")
  )))
})

test_that("defaults pick the first condition and the first frontier", {
  fit <- nsca_analysis(extract_data(), c("X1", "X2"), "Y",
                       ceilings = c("ce_fdh", "cr_fdh"))
  expect_equal(nsca_extract(fit), nsca_extract(fit, x = "X1", ceiling = "ce_fdh"))
  expect_identical(nsca_extract(fit, param = "condition"), "X1")
  expect_identical(nsca_extract(fit, param = "ceiling"), "ce_fdh")
  # And the non-defaults really do select something else.
  expect_identical(nsca_extract(fit, x = "X2", param = "condition"), "X2")
  expect_identical(nsca_extract(fit, ceiling = "cr_fdh", param = "ceiling"),
                   "cr_fdh")
})

test_that("an unknown parameter is refused rather than returned as NA", {
  fit <- nsca_analysis(extract_data(), "X1", "Y", ceilings = "ce_fdh")
  expect_error(nsca_extract(fit, param = "d_total"), "Unknown parameter")
  expect_error(nsca_extract(fit, param = "d_total"), "nec:")
  expect_error(nsca_extract(fit, x = "X9"), "Unknown condition")
  expect_error(nsca_extract(fit, ceiling = "cr_fdh"), "was not estimated")
  # The engine routes are guarded too: before 0.4.5 they returned NA for a
  # frontier that was never estimated.
  expect_error(nsca_extract(fit, ceiling = "cr_fdh", param = "nec:Effect size"),
               "was not estimated")
  expect_error(nsca_extract(fit, ceiling = "cr_fdh", param = "suf:Effect size"),
               "was not estimated")
})

test_that("an engine parameter the engine does not know returns NA, not junk", {
  # The prefixed path goes straight to the engine, which has its own parameter
  # vocabulary. A name it does not recognise must come back as NA so that a
  # typo cannot be mistaken for a measurement.
  fit <- nsca_analysis(extract_data(), "X1", "Y", ceilings = "ce_fdh")
  expect_true(is.na(nsca_extract(fit, param = "nec:No Such Parameter")))
  expect_true(is.na(nsca_extract(fit, param = "suf:No Such Parameter")))
})

test_that("extracted values match the table for every condition and frontier", {
  fit <- nsca_analysis(extract_data(), c("X1", "X2"), "Y",
                       ceilings = c("ce_fdh", "cr_fdh"))
  table <- nsca_table(fit)
  for (i in seq_len(nrow(table))) {
    condition <- table$condition[[i]]
    ceiling <- table$ceiling[[i]]
    expect_equal(
      nsca_extract(fit, x = condition, ceiling = ceiling, param = "weakest_effect"),
      table$weakest_effect[[i]],
      info = paste(condition, ceiling)
    )
  }
})

test_that("a retired name warns with the release that retired it", {
  fit <- nsca_analysis(extract_data(), "X1", "Y", ceilings = "ce_fdh")
  expect_warning(nsca_extract(fit, param = "d_nsca"), "in NSCA 0\\.3\\.0")
  expect_warning(nsca_extract(fit, param = "data_zone_share"),
                 "in NSCA 0\\.4\\.0")
  expect_warning(nsca_extract(fit, param = "nsca_supported"),
                 "in NSCA 0\\.4\\.0")
  # Every retired name has a release on record, and nothing else does.
  expect_setequal(names(NSCA:::.nsca_legacy_since),
                  names(nsca_legacy_names()))
})
